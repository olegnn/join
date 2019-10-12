# `union!`

`union!` - one macro to rule them all. Combines sync/async results, transforms tuple of results in result of tuple, provides single and multi thread (sync/async) step by step execution of branches and useful shortcut combinators.

You can specify handler - one of 

- `map` => will act as results.map(|(result0, result1, ..)| handler(result0, result1, ..))

- `and_then` => will act as results.and_then(|(result0, result1, ..)| handler(result0, result1, ..))

- `then` => will act as handler(result0, result1, ..)

or not specify - then Result<(result0, result1, ..), Error> or Option<(result0, result1, ..)> will be returned.

## Single thread combinations

### Simple sync results combination

Converts input in series of chained results and joins them step by step.

```rust
extern crate union;

use std::error::Error;
use union::union;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

fn action_1() -> Result<u16> {
    Ok(1)
}

fn action_2() -> Result<u8> {
    Ok(2)
}

fn main() {
    let sum = union! {
        action_1(),
        action_2().map(|v| v as u16),
        action_2().map(|v| v as u16 + 1).and_then(|v| Ok(v * 4)),
        action_1().and_then(|_| Err("5".into())).or(Ok(2)),
        map => |a, b, c, d| a + b + c + d
    }.expect("Failed to calculate sum");

    println!("Calculated: {}", sum);
}
```

### Async results combination 

Each branch will represent chain of tasks. All branches will be joined using `join!` macro and macro will return `unpolled` future.

```rust
#![recursion_limit="256"]

extern crate union;
extern crate futures;
extern crate tokio;

use std::error::Error;
use union::union_async;
use futures::future::{ok, err};

type Result<T> = std::result::Result<T, Box<dyn Error>>;

async fn action_1() -> Result<u16> {
    Ok(1)
}
async fn action_2() -> Result<u8> {
    Ok(2)
}

#[tokio::main]
async fn main() {
    let sum = union_async! {
        action_1(),
        action_2().and_then(|v| ok(v as u16)),
        action_2().map(|v| v.map(|v| v as u16 + 1)).and_then(|v| ok(v * 4u16)),
        action_1().and_then(|_| err("5".into())).or_else(|_| ok(2u16)),
        map => |a, b, c, d| a + b + c + d
    }.await.expect("Failed to calculate sum");

    println!("Calculated: {}", sum);
}
```

## Multi-thread combinations

To execute several tasks in parallel you could use `union_spawn!` (`spawnion!`) for sync tasks
and `union_async_spawn!` (`spasynction!`) for async tasks. Since `union_async` already provides parallel futures execution in one thread, `union_async_spawn!` spawns every branch into `tokio` executor so they will be evaluated in multi-threaded executor.

### Multi-thread sync branches

`union_spawn` spawns one `::std::thread` per each step of each branch (number of branches is the max thread count at the time).

```rust
extern crate union;

use std::error::Error;
use union::union_spawn;

type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;

fn action_1() -> Result<usize> {
    Ok(1)
}

fn action_2() -> Result<u16> {
    Ok(2)
}

fn main() {
    // Branches will be executed parallel
    let sum = union_spawn! {
        action_1(),
        action_2().map(|v| v as usize),
        action_2().map(|v| v as usize + 1).and_then(|v| Ok(v * 4)),
        action_1().and_then(|_| Err("5".into())).or(Ok(2)),
        map => |a, b, c, d| a + b + c + d
    }.expect("Failed to calculate sum");

    println!("Calculated: {}", sum);
}
```

`union_async_spawn!` uses `::tokio::spawn` function to spawn tasks so it should be done inside `tokio` runtime 
(number of branches is the max count of `tokio` tasks at the time).

### Multi-thread async branches

```rust
#![recursion_limit="256"]
extern crate union;
extern crate futures;
extern crate tokio;
 
use std::error::Error;
use union::union_async_spawn;
use futures::future::{ok, err};
 
type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;
 
async fn action_1() -> Result<u16> {
     Ok(1)
}
 
async fn action_2() -> Result<u8> {
     Ok(2)
}
 
#[tokio::main]
async fn main() {
     // Branches will be executed parallel
     let sum = union_async_spawn! {
         action_1(),
         action_2().and_then(|v| ok(v as u16)),
         action_2().map(|v| v.map(|v| v as u16 + 1)).and_then(|v| ok(v * 4u16)),
         action_1().and_then(|_| err("5".into())).or_else(|_| ok(2u16)),
         map => |a, b, c, d| a + b + c + d
     }.await.expect("Failed to calculate sum");
 
     println!("Calculated: {}", sum);
}
```

## Combinators

- Map: `|>` expr - value.map(expr)

- AndThen: `=>` expr - value.and_then(expr),

- Then: `->` expr - expr(value)

- Dot: `>.` expr - value.expr

- Or: `<|` expr - value.or(expr)

- OrElse: `<=` expr - value.or_else(expr)  

- MapErr: `!>` expr - value.map_err(expr)

- Inspect: `?>` expr - (|value| { expr(value); value })(value)

where `value` is the previous value

Every combinator prefixed by `~` will act as deferred action (all actions will wait until completion in every step and only after move to the next one).

Using combinators we can rewrite first example like

```rust
extern crate union;

use std::error::Error;
use union::union;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

fn action_1() -> Result<u16> {
    Ok(1)
}

fn action_2() -> Result<u8> {
    Ok(2)
}

fn main() {
    let sum = union! {
        action_1(),
        action_2() |> |v| v as u16,
        action_2() |> |v| v as u16 + 1 => |v| Ok(v * 4),
        action_1() => |_| Err("5".into()) <| Ok(2),
        map => |a, b, c, d| a + b + c + d
    }.expect("Failed to calculate sum");

    println!("Calculated: {}", sum);
}
```

By separating chain in actions, you will make actions wait for completion of all of them in current step before go to the next step.

```rust
extern crate union;

use std::error::Error;
use union::union;

type Result<T> = std::result::Result<T, u8>;

fn action_1() -> Result<u16> {
    Ok(1)
}

fn action_2() -> Result<u8> {
    Ok(2)
}

fn main() {
    let sum = union! {
        action_1(),
        let result_1 = action_2() ~|> |v| v as u16 + 1,
        action_2() ~|> move |v| {
            // `result_1` now is the result of `action_2()` [Ok(1u8)]
            if result_1.is_ok() { 
                v as u16 + 1 
            } else {
                0 
            }
        }  ~=> move |v| {
            // `result_1` now is the result of `|v| v as u16 + 1` [Ok(2u16)]
            if let Ok(result_1) = result_1 {
                Ok(v * 4 + result_1)
            } else {
                unreachable!()
            }
        },
        action_1() ~=> |_| Err(5) <| Ok(2),
        map => |a, b, c, d| a + b + c + d
    }.expect("Failed to calculate sum");
    println!("Calculated: {}", sum);
}

```
