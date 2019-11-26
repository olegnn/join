# Exports of the `join!`, `join_async!`, `join_spawn!`, `join_async_spawn!`, `async_spawn!`, `try_join!`, `try_join_async!`, `try_join_spawn!`, `try_join_async_spawn!`, `try_async_spawn!` macros which are reexported by `join` crate.

# `join!`

**Macros** which provide useful shortcut combinators, combine sync/async chains, support single and multi thread (sync/async) step by step execution of branches, transform tuple of results to result of tuple.

- `join!` macros will just return final values. Use it if you are working with iterators/streams etc.
- `try_join!` macros will transpose tuple of `Option`s/`Result`s in `Option`/`Result` of tuple. Use it when you are dealing with results or options. If one of branches produces `None`/`Err`  at the end of step, next steps execution will be aborted. In case of `async` macro you can only provide `Result`s because `::futures::try_join` doesn't support `Option`s.

[![Docs][docs-badge]][docs-url]
[![Crates.io][crates-badge]][crates-url]
[![MIT licensed][mit-badge]][mit-url]
[![Build Status][travis-badge]][travis-url]

[docs-badge]: https://docs.rs/join/badge.svg
[docs-url]: https://docs.rs/join
[crates-badge]: https://img.shields.io/crates/v/join.svg
[crates-url]: https://crates.io/crates/join
[mit-badge]: https://img.shields.io/badge/license-MIT-blue.svg
[mit-url]: LICENSE
[travis-badge]: https://travis-ci.org/olegnn/join.svg?branch=master
[travis-url]: https://travis-ci.org/olegnn/join

**Use [these docs](https://docs.rs/join) for development, they are more convenient.**

- [Macros](#macros)
- [Combinators](#combinators)
- [Nested combinators](#nested-combinators)
- [Handler](#handler)
- [Let pattern](#let-pattern)
- [Block captures](#block-captures)
- [Demos](#demos)
    - [Sync](#sync-demo)
    - [Async](#futures-demo)
- [Single thread examples](#single-thread-combinations)
    - [Sync](#sync-branches)
    - [Async](#futures)
- [Multi thread examples](#multi-thread-combinations)
    - [Sync](#sync-threads)
    - [Async](#future-tasks)
- [Detailed steps example](#detailed-steps-example)

## Macros

- `try_join!` - combines results/options, transposes tuple of results/options in result/option of tuple.
```rust
assert_eq!(try_join!(Ok::<_,u8>(1), Ok::<_,u8>("2"), Ok::<_,u8>(3.0)), Ok::<_,u8>((1, "2", 3.0)));
```
- `try_join_async!` - combines futures, transposes tuple of results in result of tuple.
```rust
assert_eq!(try_join_async!(ok::<_,u8>(1), ok::<_,u8>("2"), ok::<_,u8>(3.0)).await, Ok::<_,u8>((1, "2", 3.0)));
```
- `try_join_spawn!` - spawns `std::thread` per each branch and joins results, transposes tuple of results in result of tuple.
```rust
assert_eq!(try_join_spawn!(Ok::<_,u8>(1), Ok::<_,u8>("2"), Ok::<_,u8>(3.0)), Ok::<_,u8>((1, "2", 3.0)));
```
- `try_spawn!` - alias for `try_join_spawn!`.
- `try_join_async_spawn!` - spawns tokio task using `tokio::spawn` per each branch, transposes tuple of results in result of tuple.
```rust
assert_eq!(try_join_async_spawn!(ok::<_,u8>(1), ok::<_,u8>("2"), ok::<_,u8>(3.0)).await, Ok::<_,u8>((1, "2", 3.0)));
```
- `try_async_spawn!` - alias for `try_join_async_spawn!`.
- `join!` - combines values.
```rust
assert_eq!(join!(1, "2", 3.0), (1, "2", 3.0));
```
- `join_async!` - combines futures.
```rust
assert_eq!(join_async!(ready(1), ready("2"), ready(3.0)).await, (1, "2", 3.0));
```
- `join_spawn!` - spawns `std::thread` per each branch.
```rust
assert_eq!(join_spawn!(1, "2", 3.0), (1, "2", 3.0));
```
- `spawn!` - alias for `join_spawn!`.
- `join_async_spawn!` -  spawns tokio task using `tokio::spawn` per each branch.
```rust
assert_eq!(join_async_spawn!(ready(1), ready("2"), ready(3.0)).await, (1, "2", 3.0));
```
- `async_spawn!` - alias for `join_async_spawn!`.

## Combinators

- Map: **`|>`**
```rust no_run
join! { value |> expr }; // => value.map(expr)
```

- AndThen: **`=>`**
```rust no_run
join! { value => expr }; // => value.and_then(expr)
```

- Then: **`->`**
```rust no_run
join! { value -> expr }; // => expr(value)
```

- Filter: **`?>`**
```rust no_run
join! { value ?> expr }; // => value.filter(expr)
```

- Dot: **`..`** or **`>.`**
```rust no_run
join! { value .. expr }; // => value.expr
join! { value >. expr }; // => value.expr
```

- Or: **`<|`**
```rust no_run
join! { value <| expr }; // => value.or(expr)
```

- OrElse: **`<=`**
```rust no_run
join! { value <= expr }; // => value.or_else(expr)  
```

- MapErr: **`!>`**
```rust no_run
join! { value !> expr }; // => value.map_err(expr)
```

- Collect: **`=>[]`** (type is optional)
```rust no_run
join! { value =>[] T }; // => value.collect::<T>()
join! { value =>[] }; // => value.collect()
```

- Chain: **`>@>`**
```rust no_run
join! { value >@> expr }; // => value.chain(expr)
```

- FindMap: **`?|>@`**
```rust no_run
join! { value ?|>@ expr }; // => value.find_map(expr)
```

- FilterMap: **`?|>`**
```rust no_run
join! { value ?|> expr }; // => value.filter_map(expr)
```

- Enumerate: **`|n>`**
```rust no_run
join! { value |n> }; // => value.enumerate()
```

- Partition: **`?&!>`**
```rust no_run
join! { value ?&!> expr }; // => value.partition(expr)
```

- Flatten: **`^^>`**
```rust no_run
join! { value ^^> }; // => value.flatten()
```

- Fold: **`^@`**
```rust no_run
join! { value ^@ init_expr, fn_expr }; // => value.fold(init_expr, fn_expr)
```

- TryFold: **`?^@`**
```rust no_run
join! { value ?^@ init_expr, fn_expr }; // => value.try_fold(init_expr, fn_expr)
```

- Find: **`?@`**
```rust no_run
join! { value ?@ expr }; // => value.find(expr)
```

- Zip: **`>^>`**
```rust no_run
join! { value >^> expr }; // => value.zip(expr)
```

- Unzip: **`<->`** (types are optional)
```rust no_run
join! { value <-> A, B, FromA, FromB }; // => value.unzip::<A, B, FromA, FromB>()
join! { value <-> }; // => value.unzip()
```

- Inspect: **`??`** 
```rust no_run 
join! { value ?? expr }; // => (|value| { (expr)(&value); value })(value) // for sync
join_async! { value ?? expr }; // => value.inspect(expr) // for async 
```

where `value` is the previous value.

**Every combinator prefixed by `~` will act as deferred action (all actions will wait until completion in every step and only after move to the next one).**

## Nested combinators

- Wrap: `combinator` **`>>>`** `combinator`(s)...
```rust
try_join! { value => >>> |> |v| v + 2 } // => value.and_then(|value| value.map(|v| v + 2))
```
Use to enter to nested constructions like 
```rust
a.and_then(
    // >>>
    |b| b.and_then(
        // >>>
        |c| c.and_then(
            |v| Ok(v + 2)
        )
    )
)
```

- Unwrap: **`<<<`**
```rust
try_join! { 
    value 
    => >>> 
        |> |v| v + 2 
    <<<
    |> |v| Some(v + 4)  
} // => value.and_then(|value| value.map(|v| v + 2)).map(|v| Some(v + 4))
```
Use to move out of nested constructions
```rust
a.and_then(
    // >>>
    |b| b.and_then(
        // >>>
        |c| c.and_then(
            |v| Ok(v + 2)
        )
        // <<<
    )
    // <<<
).map(
    |v| v + 1
)
```

## Handler

might be one of

- `map` => **Only valid for `try` macros.** Will act as `results.map(|(result0, result1, ..)| handler(result0, result1, ..))`

```rust
assert_eq!(try_join! { Some(1), Some(2), Some(3), map => |a, b, c| a + b + c }, Some(6));
```
- `and_then` => **Only valid for `try` macros.** Will act as `results.and_then(|(result0, result1, ..)| handler(result0, result1, ..))`

```rust
assert_eq!(try_join! { Some(1), Some(2), Some(3), and_then => |a, b, c| Some(a + b + c) }, Some(6));
```
- `then` => **Only valid for not `try` macros.** Will be executed in any case, act as `handler(result0, result1, ..)`

```rust
assert_eq!(join! { Some(1), Some(2), Some(3), then => |a: Option<u8>, b: Option<u8>, c: Option<u8>| Some(a.unwrap() + b.unwrap() + c.unwrap()) }, Some(6));
```

or not specified - then `Result<(result0, result1, ..), Error>` or `Option<(result0, result1, ..)>` will be returned.

## Custom futures crate path

You can specify custom path (`futures_crate_path`) at the beginning of macro call

```rust
use join::try_join_async;
use futures::future::ok;

#[tokio::main]
async fn main() {
    let value = try_join_async! {
        futures_crate_path(::futures)
        ok::<_,u8>(2u16)
    }.await.unwrap();
    
    println!("{}", value);
}
```

## Let pattern

You can specify `let` pattern for each branch in order to share result with other branches, or in case if you need to have `mut` value between steps.

```rust
assert_eq!(try_join! {
    let mut branch_0 = Ok::<_,u8>(1) ~|> |v| v + 1,
    let branch_1 = Ok::<_,u8>(2) ~|> { let value_0 = branch_0.as_ref().unwrap(); move |v| v + value_0 },
    map => |b_0, b_1| b_0 * b_1
}.unwrap(), 6);
```

## Block captures

In order to capture variables (for ex. values of other branches in example above) you can pass block statements instead of functions:
```rust
let mut some_value = Some("capture me");
assert_eq!(try_join! {
    Some(0) |> |v| { 
        // assign `None` to some_value in step expr
        some_value = None; 
        v 
    } |> { 
        // capture value before step and get str len
        let captured_len = some_value.as_ref().unwrap().len(); 
        move |v| v + captured_len
    }
}.unwrap(), 10);
```
These blocks will be placed before actual step expressions.

## Demos

### Sync demo 

Using this macro you can write things like

```rust
#![recursion_limit = "256"]

use rand::prelude::*;
use std::sync::Arc;
use join::try_join_spawn;

// Problem: generate vecs filled by random numbers in parallel, make some operations on them in parallel,
// find max of each vec in parallel and find final max of 3 vecs

// Solution:
fn main() {
    // Branches will be executed in parallel, each in its own thread
    let max = try_join_spawn! {
        let branch_0 =
            generate_random_vec(1000, 10000000u64)
                .into_iter()
                // Multiply every element by itself
                |> power2
                // Filter even values
                ?> is_even
                // Collect values into `Vec<_>`
                =>[] Vec<_>
                // Use `Arc` to share data with branch 1
                -> Arc::new -> Some
                // Find max and clone its value
                ~=> >>> ..iter().max() |> Clone::clone,
        generate_random_vec(10000, 100000000000000f64)
            .into_iter()
            // Extract sqrt from every element
            |> get_sqrt
            -> Some
            ~=> >>> 
                // Add index in order to compare with the values of branch_0 (call `enumerate`)
                |n>
                |> {
                    // Get data from branch 0 by cloning arc
                    let branch_0 = branch_0.as_ref().unwrap().clone();
                    let len = branch_0.len();
                    // Compare every element of branch 1 with element of branch_0
                    // with the same index and take min
                    move |(index, value)|
                        if index < len && value as u64 > branch_0[index] {
                            branch_0[index]
                        } else {
                            value as u64
                        }
                }..max(),
        generate_random_vec(100000, 100000u32)
            .into_iter()
            -> Some
            ~=> >>> ..max(),
        and_then => |max0, max1, max2|
            // Find final max
            [max0, max1, max2 as u64].iter().max().map(Clone::clone)
    }
    .unwrap();
    println!("Max: {}", max);
}

fn generate_random_vec<T>(size: usize, max: T) -> Vec<T>
where
    T: From<u8>
        + rand::distributions::uniform::SampleUniform
        + rand::distributions::uniform::SampleBorrow<T>
        + Copy,
{
    let mut rng = rand::thread_rng();
    (0..size)
        .map(|_| rng.gen_range(T::from(0u8), max))
        .collect()
}

fn is_even<T>(value: &T) -> bool
where
    T: std::ops::Rem<Output = T> + std::cmp::PartialEq + From<u8> + Copy
{
    *value % 2u8.into() == 0u8.into()
}

fn get_sqrt<T>(value: T) -> T
where
    T: Into<f64>,
    f64: Into<T>,
{
    let value_f64: f64 = value.into();
    value_f64.sqrt().into()
}

fn power2<T>(value: T) -> T
where
    T: std::ops::Mul<Output = T> + Copy,
{
    value * value
}
```

### Futures demo

And like this

<details><summary>Cargo.toml</summary>
<p>

```toml
[dependencies]
futures = { version = "=0.3.0-alpha.19", package = "futures-preview", features=["async-await"] }
tokio = "0.2.0-alpha.6"
failure = "0.1.6"
futures-timer = "1.0.2"
reqwest = "0.10.0-alpha.1"
```

</p>
</details>

```rust no_run
#![recursion_limit="1024"]

use join::try_join_async;
use futures::stream::{iter, Stream};
use reqwest::Client;
use futures::future::{try_join_all, ok, ready};
use failure::{format_err, Error};

#[tokio::main]
async fn main() {
    println!(
        "{} {}\n{}",
        "Hello.\nThis's is the game where winner is player, which number is closest to",
        "the max count of links (starting with `https://`) found on one of random pages.",
        "You play against random generator (0-500)."
    );

    enum GameResult {
        Won,
        Lost,
        Draw
    }

    let client = Client::new();
    
    let game = try_join_async! {
        // Make requests to several sites
        // and calculate count of links starting from `https://`
        get_urls_to_calculate_link_count()
            |> {
                // If pass block statement instead of fn, it will be placed before current step,
                // so it will us allow to capture some variables from context
                let ref client = client;
                move |url|
                    // `try_join_async!` wraps its content into `Box::pin(async move { })`
                    try_join_async! {
                        client
                            .get(url).send()
                            => |value| value.text()
                            => |body| ok((url, body.matches("https://").collect::<Vec<_>>().len()))
                    }
            }
            // Collect values into `Vec<_>`
            =>[] Vec<_>
            |> Ok
            => try_join_all
            !> |err| format_err!("Error retrieving pages to calculate links: {:#?}", err)
            => >>>
                ..into_iter()
                .max_by_key(|(_, link_count)| link_count.clone())
                .ok_or(format_err!("Failed to find max link count"))
                -> ready
            // It waits for input in stdin before log max links count
            ~?? |result| {
                result
                    .as_ref()
                    .map(
                        |(url, count)| {
                            let split = url.to_owned().split('/').collect::<Vec<_>>();
                            let domain_name = split.get(2).unwrap_or(&url);
                            println!("Max `https://` link count found on `{}`: {}", domain_name, count)
                        }
                    )
                    .unwrap_or(());
            },
        // Concurrently it makes request to the site which generates random number
        get_url_to_get_random_number()
            -> ok
            => {
                // If pass block statement instead of fn, it will be placed before current step,
                // so it will allow us to capture some variables from context
                let ref client = client;
                let map_parse_error =
                    |value|
                        move |err|
                            format_err!("Failed to parse random number: {:#?}, value: {}", err, value);
                move |url|
                    try_join_async! {
                        client
                            .get(url)
                            .send()
                            => |value| value.text()
                            !> |err| format_err!("Error retrieving random number: {:#?}", err)
                            => |value| ok(value[..value.len() - 1].to_owned()) // remove \n from `154\n`
                            => |value|  
                                ready(
                                    value
                                        .parse::<u16>()
                                        .map_err(map_parse_error(value))
                                )
                    }
            }
            // It waits for input in stdin before log random value
            ~?? |random| {
                random
                    .as_ref()
                    .map(|number| println!("Random: {}", number))
                    .unwrap_or(());
            },
        // Concurrently it reads value from stdin
        read_number_from_stdin(),
        // Finally, when we will have all results, we can decide, who is winner
        map => |(_url, link_count), random_number, number_from_stdin| {
            let random_diff = (link_count as i32 - random_number as i32).abs();
            let stdin_diff = (link_count as i32 - number_from_stdin as i32).abs();
            match () {
                _ if random_diff > stdin_diff => GameResult::Won,
                _ if random_diff < stdin_diff => GameResult::Lost,
                _ => GameResult::Draw
            }
        }    
    };

    let _ = game.await.map(
        |result|
            println!(
                "You {}",
                match result {
                    GameResult::Won => "won!",
                    GameResult::Lost => "lose...",
                    _ => "have the same result as random generator!"
                }
            )
    ).unwrap_or_else(|error| eprintln!("Error: {:#?}", error));
}

fn get_urls_to_calculate_link_count() -> impl Stream<Item = &'static str> {
    iter(
        vec![
            "https://en.wikipedia.org/w/api.php?format=json&action=query&generator=random&grnnamespace=0&prop=revisions|images&rvprop=content&grnlimit=100",
            "https://github.com/explore",
            "https://twitter.com/search?f=tweets&vertical=news&q=%23news&src=unkn"
        ]
    )   
}

fn get_url_to_get_random_number() -> &'static str {
    "https://www.random.org/integers/?num=1&min=0&max=500&col=1&base=10&format=plain&rnd=new"
}

async fn read_number_from_stdin() -> Result<u16, Error> {
    use tokio::*;
    use futures::stream::StreamExt;
    
    let map_parse_error =
        |value|
            move |error|
                format_err!("Value from stdin isn't a correct `u16`: {:?}, input: {}", error, value);

    let mut reader = codec::FramedRead::new(io::BufReader::new(io::stdin()), codec::LinesCodec::new());

    loop {
        println!("Please, enter number (`u16`)");

        let next = reader.next();
    
        let result = try_join_async! {
            next
                |> >>> 
                    ..ok_or(format_err!("Unexpected end of input"))
                    => >>> !> |err| format_err!("Failed to apply codec: {:#?}", err)
                    <<<
                <<<
                => |value|
                    ready(
                        value
                            .parse()
                            .map_err(map_parse_error(value))
                    )
                !> |error| { eprintln!("Error: {:#?}", error); error}
        }.await;

        if result.is_ok() {
            break result
        }
    }
}
```

## Single thread combinations

### Sync branches

Converts input in series of chained results and joins them step by step.

```rust
use std::error::Error;
use join::try_join;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

fn action_1() -> Result<u16> {
    Ok(1)
}

fn action_2() -> Result<u8> {
    Ok(2)
}

fn main() {
    let sum = try_join! {
        // action_1(),
        action_1(),
        
        // action_2().map(|v| v as u16),
        action_2() |> |v| v as u16,
        
        // action_2().map(|v| v as u16 + 1).and_then(|v| Ok(v * 4)),
        action_2() |> |v| v as u16 + 1 => |v| Ok(v * 4),
        
        // action_1().and_then(|_| Err("5".into())).or(Ok(2)),
        action_1() => |_| Err("5".into()) <| Ok(2),
        
        map => |a, b, c, d| a + b + c + d
    }.expect("Failed to calculate sum");

    println!("Calculated: {}", sum);
}
```

### Futures

Each branch will represent future chain. All branches will be joined using `::futures::join!`/`::futures::try_join!` macro and `join_async!`/`try_join_async!` will return `unpolled` future.

```rust
#![recursion_limit="256"]

use std::error::Error;
use join::try_join_async;
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
    let sum = try_join_async! {
        // action_1(),
        action_1(),

        // action_2().and_then(|v| ok(v as u16)),
        action_2() => |v| ok(v as u16),

        // action_2().map(|v| v.map(|v| v as u16 + 1)).and_then(|v| ok(v * 4u16)),
        action_2() |> |v| v.map(|v| v as u16 + 1) => |v| ok(v * 4u16),

        // action_1().and_then(|_| err("5".into())).or_else(|_| ok(2u16)),
        action_1() => |_| err("5".into()) <= |_| ok(2u16),

        and_then => |a, b, c, d| ok(a + b + c + d)
    }.await.expect("Failed to calculate sum");

    println!("Calculated: {}", sum);
}
```

## Multi thread combinations

To execute several tasks in parallel you could use `join_spawn!` (`spawn!`) for sync tasks
and `join_async_spawn!` (`async_spawn!`) for futures. Since `join_async` already provides concurrent futures execution in one thread, `join_async_spawn!` spawns every branch into `tokio` executor, so they will be evaluated in multi threaded executor.

### Sync threads

`join_spawn` spawns one `::std::thread` per each step of each branch (number of branches is the max thread count at the time).

```rust

use std::error::Error;
use join::try_join_spawn;

type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;

fn action_1() -> Result<usize> {
    Ok(1)
}

fn action_2() -> Result<u16> {
    Ok(2)
}

fn main() {
    // Branches will be executed in parallel
    let sum = try_join_spawn! {
        // thread::spawn(move || action_1()),
        action_1(),
        
        // thread::spawn(move || action_2().map(|v| v as usize)),
        action_2() |> |v| v as usize,
        
        // thread::spawn(move || action_2().map(|v| v as usize + 1).and_then(|v| Ok(v * 4))),
        action_2() |> |v| v as usize + 1 => |v| Ok(v * 4),
        
        // thread::spawn(move || action_1().and_then(|_| Err("5".into())).or(Ok(2))),
        action_1() => |_| Err("5".into()) <| Ok(2),
        
        map => |a, b, c, d| a + b + c + d
    }.expect("Failed to calculate sum");

    println!("Calculated: {}", sum);
}
```

*Thread names* 

In runtime thread's name will be constructed from name of parent thread and join_%branch_index%.

Example code with many branches:

```rust
extern crate join;

use std::thread;

use join::try_join_spawn;

fn get_current_thread_name() -> String {
    thread::current().name().unwrap().to_owned()
}

fn print_branch_thread_name(index: &Result<usize, ()>) {
    println!("Branch: {}. Thread name: {}.", index.unwrap(), get_current_thread_name());
}

fn main() {
    let _ = try_join_spawn! {
        Ok(0) ?? print_branch_thread_name,
        Ok(1) ?? print_branch_thread_name,
        try_join_spawn! {
            Ok(2) ?? print_branch_thread_name,
            try_join_spawn! {
                Ok(3) ?? print_branch_thread_name,
            }
        }
    }.unwrap();
}

// Branch: 0. Thread name: main_join_0.
// Branch: 1. Thread name: main_join_1.
// Branch: 2. Thread name: main_join_2_join_0.
// Branch: 3. Thread name: main_join_2_join_1_join_0.
// Order could be different.
```

### Future tasks

`join_async_spawn!` uses `::tokio::spawn` function to spawn tasks so it should be done inside `tokio` runtime
(number of branches is the max count of `tokio` tasks at the time).

```rust
#![recursion_limit="256"]

use std::error::Error;
use join::try_join_async_spawn;
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
    let sum = try_join_async_spawn! {
        // tokio::spawn(Box::pin(action_1()))
        action_1(),

        // tokio::spawn(Box::pin(action_2().and_then(|v| ok(v as u16))))
        action_2() => |v| ok(v as u16),

        // tokio::spawn(Box::pin(action_2().map(|v| v.map(|v| v as u16 + 1)).and_then(|v| ok(v * 4u16))))
        action_2() |> |v| v.map(|v| v as u16 + 1) => |v| ok(v * 4u16),

        // tokio::spawn(Box::pin(action_1().and_then(|_| err("5".into())).or_else(|_| ok(2u16))))
        action_1() => |_| err("5".into()) <= |_| ok(2u16),

        and_then => |a, b, c, d| ok(a + b + c + d)
    }.await.expect("Failed to calculate sum");

    println!("Calculated: {}", sum);
}
```

## Detailed steps example

By separating chain in actions, you will make actions wait for completion of all of them in current step before go to the next step.

```rust
#![recursion_limit="256"]

use std::error::Error;
use join::try_join;

type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;

fn action_1() -> Result<u16> {
    Ok(1)
}

fn action_2() -> Result<u8> {
    Ok(2)
}

fn main() {
    let sum = try_join! {
        action_1(),
        let result_1 = action_2() ~|> |v| v as u16 + 1,
        action_2() ~|> {
            // `result_1` now is the result of `action_2()` [Ok(1u8)]
            let result_1 = result_1.as_ref().ok().map(Clone::clone);
            move |v| {
                if result_1.is_some() {
                    v as u16 + 1
                } else {
                    unreachable!()
                }
            }
        } ~=> {
            // `result_1` now is the result of `|v| v as u16 + 1` [Ok(2u16)]
            let result_1 = result_1.as_ref().ok().map(Clone::clone);
            move |v| {
                if let Some(result_1) = result_1 {
                    Ok(v * 4 + result_1)
                } else {
                    unreachable!()
                }
            }
        },
        action_1() ~=> |_| Err("5".into()) <| Ok(2),
        map => |a, b, c, d| a + b + c + d
    }.expect("Failed to calculate sum");
    
    println!("Calculated: {}", sum);
}
```