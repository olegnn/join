# `join!`

**Macros** which provide useful shortcut combinators, combine sync/async chains, support single and multi thread (sync/async) step by step execution of branches, transform tuple of results to result of tuple.

- `join!` macros will just return final values. Use it if you are working with iterators/streams etc.
- `try_join!` macros will transpose tuple of `Option`s/`Result`s in `Option`/`Result` of tuple. Use it when you are dealing with options or results. If one of branches produces `None`/`Err` at the end of step, next steps execution will be aborted. In case of `async` macro you can only provide `Result`s because `::futures::try_join` doesn't support `Option`s.

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

- [Features](#features)
- [Macros](#macros)
- [Combinators](#combinators)
- [Nested combinators](#nested-combinators)
- [Handler](#handler)
- [Let pattern](#let-pattern)
- [Block captures](#block-captures)
- [Custom configuration](#custom-configuration)
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

## Features

- Performance. Macros produce well-optimized code (it doesn't use inactive branches during steps, doesn't clone results/options or any other values, doesn't allocate any memory on heap [except wrapping futures into `Box::pin`]) - you can check it with `cargo expand`.
- Steps allow to write code which depends on results of branches in previous iteration.
- One-line chains which can't be created using pure `Rust` without macros.
- Briefness. Less code to express the same flow. Shortcut combinators = less parentheses.
- `async` _macros_ produce futures, so they can be used in non-`async` functions.
- Configurability. There're many options which can be configured independently to fully change macro behaviour.

## Macros

- `try_join!` - combines `Result`s/`Option`s, transposes tuple of `Result`s/`Option`s into `Result`/`Option` of tuple.

```rust
assert_eq!(
    try_join!(Ok::<_,()>(1), Ok::<_,()>("2"), Ok::<_,()>(3.0)),
    Ok::<_,()>((1, "2", 3.0))
);
```

- `try_join_async!` - combines futures, transposes tuple of `Result`s into `Result` of tuple.

```rust
assert_eq!(
    try_join_async!(ok::<_,()>(1), ok::<_,()>("2"), ok::<_,()>(3.0)).await,
    Ok::<_,()>((1, "2", 3.0))
);
```

- `try_join_spawn!` - spawns `std::thread` per each branch and joins results, transposes tuple of `Result`s/`Option`s into `Result`/`Option` of tuple.

```rust
assert_eq!(
    try_join_spawn!(Ok::<_,()>(1), Ok::<_,()>("2"), Ok::<_,()>(3.0)),
    Ok::<_,()>((1, "2", 3.0))
);
```

- `try_spawn!` - alias for `try_join_spawn!`.
- `try_join_async_spawn!` - spawns tokio task using `tokio::spawn` per each branch, transposes tuple of `Result`s into `Result` of tuple.

```rust
assert_eq!(
    try_join_async_spawn!(ok::<_,()>(1), ok::<_,()>("2"), ok::<_,()>(3.0)).await,
    Ok::<_,()>((1, "2", 3.0))
);
```

- `try_async_spawn!` - alias for `try_join_async_spawn!`.
- `join!` - combines values.

```rust
assert_eq!(
    join!(1, "2", 3.0), (1, "2", 3.0)
);
```

- `join_async!` - combines futures.

```rust
assert_eq!(
    join_async!(ready(1), ready("2"), ready(3.0)).await, (1, "2", 3.0)
);
```

- `join_spawn!` - spawns `std::thread` per each branch.

```rust
assert_eq!(
    join_spawn!(1, "2", 3.0), (1, "2", 3.0)
);
```

- `spawn!` - alias for `join_spawn!`.
- `join_async_spawn!` - spawns tokio task using `tokio::spawn` per each branch.

```rust
assert_eq!(
    join_async_spawn!(ready(1), ready("2"), ready(3.0)).await, (1, "2", 3.0)
);
```

- `async_spawn!` - alias for `join_async_spawn!`.

## Combinators

- Then: **`->`**

```rust no_run
join! { value -> expr }; // => expr(value)
```

- Map: **`|>`**

```rust no_run
join! { value |> expr }; // => value.map(expr)
```

- AndThen: **`=>`**

```rust no_run
join! { value => expr }; // => value.and_then(expr)
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
assert_eq!(
    try_join! {
        Some(1),
        Some(2),
        Some(3),
        map => |a, b, c| a + b + c
    },
    Some(6)
);
```

- `and_then` => **Only valid for `try` macros.** Will act as `results.and_then(|(result0, result1, ..)| handler(result0, result1, ..))`

```rust
assert_eq!(
    try_join! {
        Some(1),
        Some(2),
        Some(3),
        and_then => |a, b, c| Some(a + b + c)
    },
    Some(6)
);
```

- `then` => **Only valid for not `try` macros.** Will be executed in any case, act as `handler(result0, result1, ..)`

```rust
assert_eq!(
    join! {
        Some(1),
        Some(2),
        Some(3),
        then => |a: Option<u8>, b: Option<u8>, c: Option<u8>|
            Some(a.unwrap() + b.unwrap() + c.unwrap())
    },
    Some(6)
);
```

or not specified - then `Result<(result0, result1, ..), Error>` or `Option<(result0, result1, ..)>` will be returned for `try` macros and `(result0, result1, ..)` for not `try` macros.

## Custom configuration

You can specify any params at the beginning of macro call.

- `futures_crate_path` - specifies custom crate path for `futures` crate, which will be used for all `futures`-related items, used by `async` `join!` macros. Only valid for `async` macros.
- `custom_joiner` - specifies custom joiner _function_ or _macro_, which will join active branches in step if their count is greater than 1.
- `transpose_results` - specifies should macro transpose tuple of `Result`s/`Option`s into `Result`/`Option` of tuple or not. Useful when provided joiner already returns `Result` of tuple and there's no need to transpose it.
- `lazy_branches` - wrap every branch into `move || {}` when pass values to joiner. By default `true` for `try_join_spawn!`, `try_spawn!` and `join_spawn!` , `spawn!` macros because they use `thread::spawn` call. Only if active branch count > 1.

```rust
use join::try_join_async;
use futures::future::ok;

macro_rules! custom_futures_joiner {
    ($($futures: expr),+) => {
        ::futures::try_join!($($futures),*);
    }
}

#[tokio::main]
async fn main() {
    let value = try_join_async! {
        futures_crate_path(::futures)
        custom_joiner(custom_futures_joiner!)
        transpose_results(false)
        ok::<_,()>(2u16), ok::<_,()>(3u16),
        map => |a, b| a + b
    }.await.unwrap();

    assert_eq!(value, 5);
}
```

_Rayon demo_

```rust
use join::{try_join, join};

fn fib(num: u8) -> usize {
    let mut prev = 0;
    let mut cur = if num > 0 { 1 } else { 0 };
    for _ in 1..num as usize {
        let tmp = cur;
        cur = prev + cur;
        prev = tmp;
    }
    cur
}

fn main() {
    let pool = rayon::ThreadPoolBuilder::new().build().unwrap();
    let calculated = pool.install(||
        try_join! {
            custom_joiner(rayon::join)
            || Some(fib(50)),
            || Some(
                join! {
                    custom_joiner(rayon::join)
                    lazy_branches(true)
                    fib(20) -> |v| v + 25,
                    fib(30) -> |v| vec![v; 10].into_iter() |n> |> |(index, value)| value + index ..sum::<usize>(),
                    then => |a, b| a + b
                }
            ),
            map => |a, b| a * b
        }
    );
    assert_eq!(calculated.unwrap(), 104808819944395875);
}
```

## Let pattern

You can specify `let` pattern for each branch in order to share result with other branches, or in case if you need to have `mut` value between steps.

```rust
assert_eq!(
    try_join! {
        let mut branch_0 = Ok::<_,()>(1) ~|> |v| v + 1,
        let branch_1 = Ok::<_,()>(2) ~|> { let value_0 = branch_0.as_ref().unwrap(); move |v| v + value_0 },
        map => |b_0, b_1| b_0 * b_1
    }.unwrap(),
    6
);
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
                // .map(power2) (Multiply every element by itself)
                |> power2
                // .filter(is_even) (Filter even values)
                ?> is_even
                // .collect::<Vec<_>>() (Collect values into `Vec<_>`)
                =>[] Vec<_>
                // Arc::new(Some(...))
                // Use `Arc` to share data with branch 1
                -> Arc::new -> Some
                // Find max and clone its value
                // .and_then(|v| v.iter().max().cloned())
                ~=> >>> ..iter().max() |> Clone::clone,
        generate_random_vec(10000, 100000000000000f64)
            .into_iter()
            // .map(sqrt) (Extract sqrt from every element)
            |> sqrt
            // Some(...)
            -> Some
            // .and_then(|v| v...)
            ~=> >>>
                // .enumerate() (Add index in order to compare with the values of branch_0)
                |n>
                // .map(...)
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
            // .and_then(|v| v.max())
            ~=> >>> ..max(),
        and_then => |max0, max1, max2|
            // Find final max
            [max0, max1, max2 as u64].iter().max().cloned()
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

fn sqrt<T>(value: T) -> T
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

```rust
extern crate rand;
extern crate join;

use rand::prelude::*;
use join::try_join;

fn main() {
    let mut rng = rand::thread_rng();

    let result = try_join! {
        (0..10)
            // .map(|index| { let value ... })
            |> |index| { let value = rng.gen_range(0, index + 5); if rng.gen_range(0f32, 2.0) > 1.0 { Ok(value) } else { Err(value) }}
            // .filter(|result| ...)
            ?> |result| match result { Ok(_) => true, Err(value) => *value > 2 }
            // .map(|v| v.map(|value| value + 1))
            |> >>> |> |value| value + 1
            <<<
            // .try_fold(0i32, |acc, cur| {...})
            ?^@ 0i32, |acc, cur| {
                cur.map(|cur| acc + cur).or_else(|cur| Ok(acc - cur))
            }
            // .and_then(|value| if ...)
            => |value| if value > 0 { Ok(value as u8) } else { Err(0) }
            // Wait for all branches to be successful and then calculate fib
            ~|> fib,
        (0..6)
            // .map(|index| { let value ... })
            |> |index| { let value = rng.gen_range(0, index + 5); if rng.gen_range(0f32, 2.0) > 1.0 { Some(value) } else { None }}
            // .filter_map(|v| v)
            ?|> >>>
            <<<
            ..sum::<u16>()
            // Return `Ok` only if value is less than 20
            -> |value| if value < 20 { Ok(value as u8) } else { Err(0) }
            // Wait for all branches to be successful and then calculate fib
            ~|> fib,
        // In case of success, multilpy fibs
        map => |v_1, v_2| v_1 * v_2
    };

    result.map(|value| println!("Result: {}", value)).unwrap_or_else(|err| println!("Error: {:#?}", err));
}

fn fib(num: u8) -> usize {
    println!("CALLED FIB!");
    let mut prev = 0;
    let mut cur = if num > 0 { 1 } else { 0 };
    for _ in 1..num as usize {
        let tmp = cur;
        cur = prev + cur;
        prev = tmp;
    }
    cur
}
```

### Futures demo

```rust
use join::try_join_async;
use futures::stream::{iter, Stream};
use reqwest::Client;
use futures::future::{try_join_all, ok, ready};
use failure::{format_err, Error};
#[tokio::main]
async fn main() {
    println!("Hello.\nThis's is the game where winner is player, which number is closest to the max count of links (starting with `https://`) found on one of random pages.\nYou play against random generator (0-500).");

    enum GameResult {
        Won,
        Lost,
        Draw
    }
    let client = Client::new();

    let game = try_join_async! {
        // Make requests to several sites
        // and calculate count of links starting from `https://`
        urls_to_calculate_link_count()
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
                .max_by_key(|(_, link_count)| *link_count)
                .ok_or(format_err!("Failed to find max link count"))
                -> ready
            // It waits for input in stdin before log max links count
            ~?? >>>
                ..as_ref()
                |> |(url, count)| {
                    let split = url.to_owned().split('/').collect::<Vec<_>>();
                    let domain_name = split.get(2).unwrap_or(&url);
                    println!("Max `https://` link count found on `{}`: {}", domain_name, count)
                }
                ..unwrap_or(()),
        // Concurrently it makes request to the site which generates random number
        url_to_random_number()
            -> ok
            => {
                // If pass block statement instead of fn, it will be placed before current step,
                // so it will allow us to capture some variables from context
                let ref client = client;
                let map_parse_error = |error, value| format_err!("Failed to parse random number: {:#?}, value: {}", error, value);
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
                                        .map_err(|err| map_parse_error(err, value))
                                )
                    }
            }
            // It waits for input in stdin before log random value
            ~?? >>>
                ..as_ref()
                |> |number| println!("Random: {}", number)
                ..unwrap_or(()),
        // Concurrently it reads value from stdin
        read_number_from_stdin() |> Ok,
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
fn urls_to_calculate_link_count() -> impl Stream<Item = &'static str> {
    iter(
        vec![
            "https://en.wikipedia.org/w/api.php?format=json&action=query&generator=random&grnnamespace=0&prop=revisions|images&rvprop=content&grnlimit=100",
            "https://github.com/explore",
            "https://twitter.com/search?f=tweets&vertical=news&q=%23news&src=unkn"
        ]
    )
}
fn url_to_random_number() -> &'static str {
    "https://www.random.org/integers/?num=1&min=0&max=500&col=1&base=10&format=plain&rnd=new"
}
async fn read_number_from_stdin() -> u16 {
    use tokio::io::{stdin, BufReader, Error, ErrorKind, AsyncBufReadExt};
    let mut reader = BufReader::new(stdin()).lines();

    loop {
        println!("Please, enter number (`u16`)");
        let next = reader.next_line();

        let result = try_join_async! {
            next
                => >>>
                   ..ok_or(Error::new(ErrorKind::Other, "Failed to read value from stdin"))
                   => >>>
                       ..parse()
                       !> |err| Error::new(ErrorKind::Other, format!("Value from stdin isn't a correct `u16`: {:?}", err))
                   <<<
                   -> ready
        }.await;
        if let Ok(value) = result {
            break value
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

_Thread names_

In runtime thread's name will be constructed from name of parent thread and join\_%branch_index%.

Example with several branches:

```rust
extern crate join;

use std::thread;

use join::try_join_spawn;

fn current_thread_name() -> String {
    thread::current().name().unwrap().to_owned()
}

fn print_branch_thread_name(index: &Result<usize, ()>) {
    println!("Branch: {}. Thread name: {}.", index.unwrap(), current_thread_name());
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
            let result_1 = result_1.as_ref().ok().cloned();
            move |v| {
                if result_1.is_some() {
                    v as u16 + 1
                } else {
                    unreachable!()
                }
            }
        } ~=> {
            // `result_1` now is the result of `|v| v as u16 + 1` [Ok(2u16)]
            let result_1 = result_1.as_ref().ok().cloned();
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
