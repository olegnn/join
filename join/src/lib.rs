//! # `join!`
//!
//! **Macro** which provides useful shortcut combinators, combines sync/async chains, supports single and multi thread (sync/async) step by step execution of branches, transforms tuple of results in result of tuple.
//!
//! - `join` macros will just return final values. Use it if you are working with iterators/streams etc.
//! - `try_join` macros will transpose tuple of `Option`s/`Result`s in `Option`/`Result` of tuple. Use it when you are dealing with results or options.
//!
//! [![Docs][docs-badge]][docs-url]
//! [![Crates.io][crates-badge]][crates-url]
//! [![MIT licensed][mit-badge]][mit-url]
//! [![Build Status][travis-badge]][travis-url]
//!
//! [docs-badge]: https://docs.rs/join/badge.svg
//! [docs-url]: https://docs.rs/join
//! [crates-badge]: https://img.shields.io/crates/v/join.svg
//! [crates-url]: https://crates.io/crates/join
//! [mit-badge]: https://img.shields.io/badge/license-MIT-blue.svg
//! [mit-url]: LICENSE
//! [travis-badge]: https://travis-ci.org/olegnn/join.svg?branch=master
//! [travis-url]: https://travis-ci.org/olegnn/join
//!
//! - [Demos](#demos)
//! - [Combinators](#combinators)
//! - [Nested combinators](#nested-combinators)
//! - [Handler](#handler)
//! - [Single thread examples](#single-thread-combinations)
//!     - [Sync](#sync-branches)
//!     - [Async](#futures)
//! - [Multi thread examples](#multi-thread-combinations)
//!     - [Sync](#sync-threads)
//!     - [Async](#future-tasks)
//! - [Detailed steps example](#detailed-steps-example)
//!
//! ## Combinators
//!
//! - Map: **`|>`**
//! ```rust
//! # use join::try_join;
//! # fn main() {
//! # let value = Some(1u8);
//! # let expr = |v| v;
//! try_join! { value |> expr }; // => value.map(expr)
//! # }
//! ```
//!
//! - AndThen: **`=>`**
//! ```rust
//! # use join::try_join;
//! # fn main() {
//! # let value = Some(1u8);
//! # let expr = |v| Some(v + 1);
//! # let result =
//! try_join! { value => expr }; // => value.and_then(expr)
//! # assert_eq!(result, Some(2));
//! # }
//! ```
//!
//! - Then: **`->`**
//! ```rust
//! # use join::try_join;
//! # fn main() {
//! # let value = Some(1u8);
//! # let expr = |v: Option<u8>| Some(v.unwrap() + 1);
//! # let result =
//! try_join! { value -> expr }; // => expr(value)
//! # assert_eq!(result, Some(2));
//! # }
//! ```
//!
//! - Filter: **`?>`**
//! ```rust
//! # use join::join;
//! # fn expr(v: &u8) -> bool { *v == 3 }
//! # fn main() {
//! # let value = vec![1u8, 2, 3].into_iter();
//! # let result =
//! join! { value ?> expr }; // => value.filter(expr)
//! # assert_eq!(result.collect::<Vec<_>>(), vec![3]);
//! # }
//! ```
//!
//! - Dot: **`..`** or **`>.`**
//! ```rust
//! # use join::join;
//! # fn main() {
//! # let value = Some(1u8);
//! # let result =
//! join! { value .. is_some() }; // => value.is_some()
//! # assert_eq!(result, true);
//! # let result =
//! join! { value >. is_none() }; // => value.is_none()
//! # assert_eq!(result, false);
//! # }
//! ```
//!
//! - Or: **`<|`**
//! ```rust
//! # use join::try_join;
//! # fn main() {
//! # let value = Some(1u8);
//! # let expr = Some(2u8);
//! # let result =
//! try_join! { value <| expr }; // => value.or(expr)
//! # assert_eq!(result, Some(1));
//! # }
//! ```
//!
//! - OrElse: **`<=`**
//! ```rust
//! # use join::try_join;
//! # fn main() {
//! # let value = None;
//! # let expr = || Some(6);
//! # let result =
//! try_join! { value <= expr }; // => value.or_else(expr)  
//! # assert_eq!(result, Some(6));
//! # }
//! ```
//!
//! - MapErr: **`!>`**
//! ```rust
//! # use join::try_join;
//! # fn main() {
//! # let value = Err::<u8,_>(1u8);
//! # let expr = |err| err + 1;
//! # let result =
//! try_join! { value !> expr }; // => value.map_err(expr)
//! # assert_eq!(result, Err(2));
//! # }
//! ```
//!
//! - Collect: **`=>[]`** (type is optional)
//! ```rust
//! # use join::join;
//! # fn main() {
//! # let value = vec![1u8, 2, 3].into_iter();
//! # let result =
//! join! { value =>[] Vec<_> }; // => value.collect::<Vec<_>>()
//! # assert_eq!(result, vec![1u8, 2, 3]);
//! # let value = vec![1u8, 3, 4].into_iter();
//! let result: Vec<_> = join! { value =>[] }; // => value.collect()
//! # assert_eq!(result, vec![1u8, 3, 4]);
//! # }
//! ```
//!
//! - Chain: **`>@>`**
//! ```rust
//! # use join::join;
//! # fn main() {
//! # let value = vec![1u8].into_iter();
//! # let expr = vec![1u8].into_iter();
//! # let result =
//! join! { value >@> expr }; // => value.chain(expr)
//! # assert_eq!(result.collect::<Vec<_>>(), vec![1, 1]);
//! # }
//! ```
//!
//! - FindMap: **`?|>@`**
//! ```rust
//! # use join::join;
//! # fn main() {
//! # let mut value = vec![1u8, 2, 3, 4].into_iter();
//! # let expr = |v| if v == 4 { Some(v) } else { None };
//! # let result =
//! join! { value ?|>@ expr }; // => value.find_map(expr)
//! # assert_eq!(result, Some(4));
//! # }
//! ```
//!
//! - FilterMap: **`?|>`**
//! ```rust
//! # use join::join;
//! # fn main() {
//! # let value = vec![1u8, 2, 3, 4, 5].into_iter();
//! # let expr = |v| Some(v);
//! # let result =
//! join! { value ?|> expr }; // => value.filter_map(expr)
//! # assert_eq!(result.collect::<Vec<_>>(), vec![1u8, 2, 3, 4, 5]);
//! # }
//! ```
//!
//! - Enumerate: **`|n>`**
//! ```rust
//! # use join::join;
//! # fn main() {
//! # let value = vec![1u8].into_iter();
//! # let result =
//! join! { value |n> }; // => value.enumerate()
//! # assert_eq!(result.collect::<Vec<_>>(), vec![(0usize, 1u8)]);
//! # }
//! ```
//!
//! - Partition: **`?&!>`**
//! ```rust
//! # use join::join;
//! # fn filter<'a>(v: &'a u8) -> bool { v % 2 == 0 }
//! # fn main() {
//! # let mut value = vec![1u8, 2, 3, 4, 5].into_iter();
//! # let expr = filter;
//! # let result =
//! join! { value ?&!> expr }; // => value.partition(expr)
//! # assert_eq!(result, (vec![2, 4], vec![1, 3, 5]));
//! # }
//! ```
//!
//! - Flatten: **`^^>`**
//! ```rust
//! # use join::join;
//! # fn main() {
//! # let mut value = vec![vec![1u8]].into_iter();
//! # let result =
//! join! { value ^^> }; // => value.flatten()
//! # assert_eq!(result.collect::<Vec<_>>(), vec![1u8]);
//! # }
//! ```
//!
//! - Fold: **`^@`**
//! ```rust
//! # use join::join;
//! # fn main() {
//! # let mut value = vec![1u8, 2u8].into_iter();
//! # let init_expr = 0;
//! # let fn_expr = |a, b| a + b;
//! # let result =
//! join! { value ^@ init_expr, fn_expr }; // => value.fold(init_expr, fn_expr)
//! # assert_eq!(result, 3);
//! # }
//! ```
//!
//! - TryFold: **`?^@`**
//! ```rust
//! # use join::join;
//! # fn main() {
//! # let mut value = vec![1u8, 2u8].into_iter();
//! # let init_expr = 0;
//! # let fn_expr = |a, b| Ok::<_,u8>(a + b);
//! # let result =
//! join! { value ?^@ init_expr, fn_expr }; // => value.try_fold(init_expr, fn_expr)
//! # assert_eq!(result, Ok(3));
//! # }
//! ```
//!
//! - Find: **`?@`**
//! ```rust
//! # use join::try_join;
//! # fn filter(v: &u8) -> bool { true }
//! # fn main() {
//! # let mut value = vec![1u8, 2u8].into_iter();
//! # let expr = filter;
//! # let result =
//! try_join! { value ?@ expr }; // => value.find(expr)
//! # assert_eq!(result, Some(1));
//! # }
//! ```
//!
//! - Zip: **`>^>`**
//! ```rust
//! # use join::join;
//! # fn main() {
//! # let value = vec![1u8, 2u8].into_iter();
//! # let expr = vec![1u8, 2u8].into_iter();
//! # let result =
//! join! { value >^> expr }; // => value.zip(expr)
//! # assert_eq!(result.collect::<Vec<_>>(), vec![(1, 1), (2, 2)]);
//! # }
//! ```
//!
//! - Unzip: **`<->`** (types are optional)
//! ```rust
//! # use join::join;
//! # fn main() {
//! # let value = vec![(1u8, 2u8)].into_iter();
//! # let result =
//! join! { value <-> _, _, Vec<_>, Vec<_> }; // => value.unzip::<_, _, Vec<_>, Vec<_>>()
//! # assert_eq!(result, (vec![1], vec![2]));
//! # let value = vec![(1u8, 2u8)].into_iter();
//! let result: (Vec<_>, Vec<_>) = join! { value <-> }; // => value.unzip()
//! # assert_eq!(result, (vec![1], vec![2]));
//! # }
//! ```
//! - Inspect: **`??`**
//! ```rust
//! # use join::{try_join, try_join_async};
//! # use futures::executor::block_on;
//! # fn expr<T: std::fmt::Debug>(v: &T) { println!("{:?}", v); }
//! # fn main() {
//! # let value = Ok::<_,u8>(1u8);
//! # let result =
//! try_join! { value ?? expr }; // => (|value| { (expr)(&value); value })(value) // for sync
//! # assert_eq!(result, Ok::<_,u8>(1u8));
//! # let value = ::futures::future::ok::<_,u8>(1u8);
//! # let result =
//! try_join_async! { value ?? expr }; // => value.inspect(expr) for async
//! # block_on(async { assert_eq!(result.await, Ok::<_,u8>(1u8)); });
//! # }
//! ```
//!
//! where `value` is the previous value.
//!
//! **Every combinator prefixed by `~` will act as deferred action (all actions will wait until completion in every step and only after move to the next one).**
//! 
//! ## Nested combinators
//! 
//! Wrap: `combinator` **`>>>`** `combinator`(s)...
//! ```rust
//! # use join::try_join;
//! # fn main() {
//! # let value = Some(Some(5));
//! # let result =
//! try_join! { value => >>> |> |v| v + 2 }; // => value.and_then(|value| value.map(|v| v + 2))
//! # assert_eq!(result, Some(7));
//! # }
//! ```
//! Use to create nested constructions like 
//! ```rust
//! # fn main() {
//! # let a = Ok::<_,u8>(Ok::<_,u8>(Ok::<_,u8>(4)));
//! # let value = 
//! a.and_then(
//!     // >>>
//!     |b| b.and_then(
//!         // >>>
//!         |c| c.and_then(
//!             |v| Ok(v + 2)
//!         )
//!     )
//! )
//! # ;
//! # assert_eq!(value, Ok(6));
//! # }
//! ```
//! 
//! Unwrap: **`<<<`**
//! ```rust
//! # use join::try_join;
//! # fn main() {
//! # let value = Some(Some(5));
//! # let result =
//! try_join! { 
//!     value 
//!     => >>> 
//!         |> |v| v + 2 
//!     <<<
//!     |> |v| Some(v + 4)  
//! }; // => value.and_then(|value| value.map(|v| v + 2)).map(|v| Some(v + 4))
//! # assert_eq!(result, Some(Some(11)));
//! # }
//! ```
//! Use to move out of nested constructions
//! ```rust
//! # fn main() {
//! # let a = Ok::<_,u8>(Ok::<_,u8>(Ok::<_,u8>(4)));
//! # let value = 
//! a.and_then(
//!     // >>>
//!     |b| b.and_then(
//!         // >>>
//!         |c| c.and_then(
//!             |v| Ok(v + 2)
//!         )
//!         // <<<
//!     )
//!     // <<<
//! ).map(
//!     |v| v + 1
//! )
//! # ;
//! # assert_eq!(value, Ok(7));
//! # }
//! ```
//!
//!
//! ## Handler
//!
//! **Only valid in `try` form.**
//!
//! might be one of
//!
//! - `map` => will act as `results.map(|(result0, result1, ..)| handler(result0, result1, ..))`
//! ```rust
//! # use join::try_join;
//! # fn main() {
//! # let value = Some(1u8);
//! assert_eq!(try_join! { Some(1), Some(2), Some(3), map => |a, b, c| a + b + c }, Some(6));
//! # }
//! ```
//! - `and_then` => will act as `results.and_then(|(result0, result1, ..)| handler(result0, result1, ..))`
//! ```rust
//! # use join::try_join;
//! # fn main() {
//! # let value = Some(1u8);
//! assert_eq!(try_join! { Some(1), Some(2), Some(3), and_then => |a, b, c| Some(a + b + c) }, Some(6));
//! # }
//! ```
//! - `then` => will act as `handler(result0, result1, ..)`
//! ```rust
//! # use join::try_join;
//! # fn main() {
//! # let value = Some(1u8);
//! assert_eq!(try_join! { Some(1), Some(2), Some(3), then => |a: Option<u8>, b: Option<u8>, c: Option<u8>| Some(a.unwrap() + b.unwrap() + c.unwrap()) }, Some(6));
//! # }
//! ```
//! or not specified - then `Result<(result0, result1, ..), Error>` or `Option<(result0, result1, ..)>` will be returned.
//!
//! ## Custom futures crate path
//!
//! You can specify custom path (`futures_crate_path`) at the beginning of macro call
//!
//! ```rust
//! use join::try_join_async;
//! use futures::future::ok;
//!
//! #[tokio::main]
//! async fn main() {
//!     let value = try_join_async! {
//!         futures_crate_path(::futures)
//!         ok::<_,u8>(2u16)
//!     }.await.unwrap();
//!     
//!     println!("{}", value);
//! }
//! ```
//!
//! ## Demos
//!
//! Using this macro you can write things like
//!
//! ```rust
//! #![recursion_limit = "256"]
//!
//! use rand::prelude::*;
//! use std::sync::Arc;
//! use join::try_join_spawn;
//!
//! // Problem: generate vecs filled by random numbers in parallel, make some operations on them in parallel,
//! // find max of each vec in parallel and find final max of 3 vecs
//!
//! // Solution:
//! fn main() {
//!     // Branches will be executed in parallel, each in its own thread
//!     let max = try_join_spawn! {
//!         let branch_0 =
//!             generate_random_vec(1000, 10000000u64)
//!                 .into_iter()
//!                 // Multiply every element by itself
//!                 |> power2
//!                 // Filter even values
//!                 ?> is_even
//!                 // Collect values into `Vec<_>`
//!                 =>[] Vec<_>
//!                 // Use `Arc` to share data with branch 1
//!                 -> Arc::new
//!                 // Find max and clone its value
//!                 ~..iter().max()
//!                 |> Clone::clone,
//!         generate_random_vec(10000, 100000000000000f64)
//!             .into_iter()
//!             // Extract sqrt from every element
//!             |> get_sqrt
//!             // Add index in order to compare with the values of branch 0 (call `enumerate`)
//!             |n>
//!             ~|> {
//!                 // Get data from branch 0 by cloning arc
//!                 let branch_0 = branch_0.clone();
//!                 let len = branch_0.len();
//!                 // Compare every element of branch 1 with element of branch 0
//!                 // with the same index and take min
//!                 move |(index, value)|
//!                     if index < len && value as u64 > branch_0[index] {
//!                         branch_0[index]
//!                     } else {
//!                         value as u64
//!                     }
//!             }
//!             ..max(),
//!         generate_random_vec(100000, 100000u32)
//!             .into_iter()
//!             ~..max(),
//!         map => |max0, max1, max2|
//!             // Find final max
//!             *[max0, max1, max2 as u64].into_iter().max().unwrap()
//!     }
//!     .unwrap();
//!     println!("Max: {}", max);
//! }
//!
//! fn generate_random_vec<T>(size: usize, max: T) -> Vec<T>
//! where
//!     T: From<u8>
//!         + rand::distributions::uniform::SampleUniform
//!         + rand::distributions::uniform::SampleBorrow<T>
//!         + Copy,
//! {
//!     let mut rng = rand::thread_rng();
//!     (0..size)
//!         .map(|_| rng.gen_range(T::from(0u8), max))
//!         .collect()
//! }
//!
//! fn is_even<T>(value: &T) -> bool
//! where
//!     T: std::ops::Rem<Output = T> + std::cmp::PartialEq + From<u8> + Copy
//! {
//!     *value % 2u8.into() == 0u8.into()
//! }
//!
//! fn get_sqrt<T>(value: T) -> T
//! where
//!     T: Into<f64>,
//!     f64: Into<T>,
//! {
//!     let value_f64: f64 = value.into();
//!     value_f64.sqrt().into()
//! }
//!
//! fn power2<T>(value: T) -> T
//! where
//!     T: std::ops::Mul<Output = T> + Copy,
//! {
//!     value * value
//! }
//! ```
//!
//! And like this
//!
//! ```rust no_run
//! #![recursion_limit="1024"]
//!
//! use join::try_join_async;
//! use futures::stream::{iter, Stream};
//! use reqwest::Client;
//! use futures::future::{try_join_all, ok, ready};
//! use failure::{format_err, Error};
//!
//! #[tokio::main]
//! async fn main() {
//!     println!(
//!         "{} {}\n{}",
//!         "Hello.\nThis's is the game where winner is player, which number is closest to",
//!         "the max count of links (starting with `https://`) found on one of random pages.",
//!         "You play against random generator (0-500)."
//!     );
//!
//!     enum GameResult {
//!         Won,
//!         Lost,
//!         Draw
//!     }
//!
//!     let client = Client::new();
//!     
//!     let game = try_join_async! {
//!         // Make requests to several sites
//!         // and calculate count of links starting from `https://`
//!         get_urls_to_calculate_link_count()
//!             |> {
//!                 // If pass block statement instead of fn, it will be placed before current step,
//!                 // so it will us allow to capture some variables from context
//!                 let ref client = client;
//!                 move |url|
//!                     // `try_join_async!` wraps its content into `Box::pin(async move { })`
//!                     try_join_async! {
//!                         client
//!                             .get(url).send()
//!                             => |value| value.text()
//!                             => |body| ok((url, body.matches("https://").collect::<Vec<_>>().len()))
//!                     }
//!             }
//!             // Collect values into `Vec<_>`
//!             =>[] Vec<_>
//!             |> Ok
//!             => try_join_all
//!             !> |err| format_err!("Error retrieving pages to calculate links: {:#?}", err)
//!             => |results|
//!                 ok(
//!                     results
//!                         .into_iter()
//!                         .max_by_key(|(_, link_count)| link_count.clone())
//!                         .unwrap()
//!                 )
//!             // It waits for input in stdin before log max links count
//!             ~?? |result| {
//!                 result
//!                     .as_ref()
//!                     .map(
//!                         |(url, count)| {
//!                             let split = url.to_owned().split('/').collect::<Vec<_>>();
//!                             let domain_name = split.get(2).unwrap_or(&url);
//!                             println!("Max `https://` link count found on `{}`: {}", domain_name, count)
//!                         }
//!                     )
//!                     .unwrap_or(());
//!             },
//!         // Concurrently it makes request to the site which generates random number
//!         get_url_to_get_random_number()
//!             -> ok
//!             => {
//!                 // If pass block statement instead of fn, it will be placed before current step,
//!                 // so it will allow us to capture some variables from context
//!                 let ref client = client;
//!                 let map_parse_error =
//!                     |value|
//!                         move |err|
//!                             format_err!("Failed to parse random number: {:#?}, value: {}", err, value);
//!                 move |url|
//!                     try_join_async! {
//!                         client
//!                             .get(url)
//!                             .send()
//!                             => |value| value.text()
//!                             !> |err| format_err!("Error retrieving random number: {:#?}", err)
//!                             => |value| ok(value[..value.len() - 1].to_owned()) // remove \n from `154\n`
//!                             => |value|  
//!                                 ready(
//!                                     value
//!                                         .parse::<u16>()
//!                                         .map_err(map_parse_error(value))
//!                                 )
//!                     }
//!             }
//!             // It waits for input in stdin before log random value
//!             ~?? |random| {
//!                 random
//!                     .as_ref()
//!                     .map(|number| println!("Random: {}", number))
//!                     .unwrap_or(());
//!             },
//!         // Concurrently it reads value from stdin
//!         read_number_from_stdin(),
//!         // Finally, when we will have all results, we can decide, who is winner
//!         map => |(_url, link_count), random_number, number_from_stdin| {
//!             let random_diff = (link_count as i32 - random_number as i32).abs();
//!             let stdin_diff = (link_count as i32 - number_from_stdin as i32).abs();
//!             match () {
//!                 _ if random_diff > stdin_diff => GameResult::Won,
//!                 _ if random_diff < stdin_diff => GameResult::Lost,
//!                 _ => GameResult::Draw
//!             }
//!         }    
//!     };
//!
//!     let _ = game.await.map(
//!         |result|
//!             println!(
//!                 "You {}",
//!                 match result {
//!                     GameResult::Won => "won!",
//!                     GameResult::Lost => "lose...",
//!                     _ => "have the same result as random generator!"
//!                 }
//!             )
//!     ).unwrap();  
//! }
//!
//! fn get_urls_to_calculate_link_count() -> impl Stream<Item = &'static str> {
//!     iter(
//!         vec![
//!             "https://en.wikipedia.org/w/api.php?format=json&action=query&generator=random&grnnamespace=0&prop=revisions|images&rvprop=content&grnlimit=100",
//!             "https://github.com/explore",
//!             "https://twitter.com/search?f=tweets&vertical=news&q=%23news&src=unkn"
//!         ]
//!     )   
//! }
//!
//! fn get_url_to_get_random_number() -> &'static str {
//!     "https://www.random.org/integers/?num=1&min=0&max=500&col=1&base=10&format=plain&rnd=new"
//! }
//!
//! async fn read_number_from_stdin() -> Result<u16, Error> {
//!     use tokio::*;
//!     use futures::stream::StreamExt;
//!     
//!     let map_parse_error =
//!         |value|
//!             move |error|
//!                 format_err!("Value from stdin isn't a correct `u16`: {:?}, input: {}", error, value);
//!
//!     let mut result;
//!     let mut reader = codec::FramedRead::new(io::BufReader::new(io::stdin()), codec::LinesCodec::new());
//!
//!     while {
//!         println!("Please, enter number (`u16`)");
//!
//!         let next = reader.next();
//!     
//!         result = try_join_async! {
//!             next
//!                 |> |value| value.ok_or(format_err!("Unexpected end of input"))
//!                 => >>> 
//!                     !> |err| format_err!("Failed to apply codec: {:?}", err) -> ready
//!                 <<<
//!                 => |value|
//!                     ready(
//!                         value
//!                             .parse()
//!                             .map_err(map_parse_error(value))
//!                     )
//!                 !> |error| { eprintln!("Error: {:#?}", error); error}
//!         }.await;
//!
//!         result.is_err()
//!     } {}
//!
//!     result
//! }
//! ```
//!
//! ## Single thread combinations
//!
//! ### Sync branches
//!
//! Converts input in series of chained results and joins them step by step.
//!
//! ```rust
//! use std::error::Error;
//! use join::try_join;
//!
//! type Result<T> = std::result::Result<T, Box<dyn Error>>;
//!
//! fn action_1() -> Result<u16> {
//!     Ok(1)
//! }
//!
//! fn action_2() -> Result<u8> {
//!     Ok(2)
//! }
//!
//! fn main() {
//!     let sum = try_join! {
//!         // action_1(),
//!         action_1(),
//!         
//!         // action_2().map(|v| v as u16),
//!         action_2() |> |v| v as u16,
//!         
//!         // action_2().map(|v| v as u16 + 1).and_then(|v| Ok(v * 4)),
//!         action_2() |> |v| v as u16 + 1 => |v| Ok(v * 4),
//!         
//!         // action_1().and_then(|_| Err("5".into())).or(Ok(2)),
//!         action_1() => |_| Err("5".into()) <| Ok(2),
//!         
//!         map => |a, b, c, d| a + b + c + d
//!     }.expect("Failed to calculate sum");
//!
//!     println!("Calculated: {}", sum);
//! }
//! ```
//!
//! ### Futures
//!
//! Each branch will represent future chain. All branches will be joined using `::futures::join!` macro and `join_async!` will return `unpolled` future.
//!
//! ```rust
//! #![recursion_limit="256"]
//!
//! use std::error::Error;
//! use join::try_join_async;
//! use futures::future::{ok, err};
//!
//! type Result<T> = std::result::Result<T, Box<dyn Error>>;
//!
//! async fn action_1() -> Result<u16> {
//!     Ok(1)
//! }
//! async fn action_2() -> Result<u8> {
//!     Ok(2)
//! }
//!
//! #[tokio::main]
//! async fn main() {
//!     // Branches will be executed concurrently
//!     let sum = try_join_async! {
//!         // action_1(),
//!         action_1(),
//!
//!         // action_2().and_then(|v| ok(v as u16)),
//!         action_2() => |v| ok(v as u16),
//!
//!         // action_2().map(|v| v.map(|v| v as u16 + 1)).and_then(|v| ok(v * 4u16)),
//!         action_2() |> |v| v.map(|v| v as u16 + 1) => |v| ok(v * 4u16),
//!
//!         // action_1().and_then(|_| err("5".into())).or_else(|_| ok(2u16)),
//!         action_1() => |_| err("5".into()) <= |_| ok(2u16),
//!
//!         and_then => |a, b, c, d| ok(a + b + c + d)
//!     }.await.expect("Failed to calculate sum");
//!
//!     println!("Calculated: {}", sum);
//! }
//! ```
//!
//! ## Multi thread combinations
//!
//! To execute several tasks in parallel you could use `join_spawn!` (`spawn!`) for sync tasks
//! and `join_async_spawn!` (`async_spawn!`) for futures. Since `join_async`already provides concurrent futures execution in one thread, `join_async_spawn!` spawns every branch into `tokio` executor, so they will be evaluated in multi threaded executor.
//!
//! ### Sync threads
//!
//! `join_spawn` spawns one `::std::thread` per each step of each branch (number of branches is the max thread count at the time).
//!
//! ```rust
//!
//! use std::error::Error;
//! use join::try_join_spawn;
//!
//! type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;
//!
//! fn action_1() -> Result<usize> {
//!     Ok(1)
//! }
//!
//! fn action_2() -> Result<u16> {
//!     Ok(2)
//! }
//!
//! fn main() {
//!     // Branches will be executed in parallel
//!     let sum = try_join_spawn! {
//!         // thread::spawn(move || action_1()),
//!         action_1(),
//!         
//!         // thread::spawn(move || action_2().map(|v| v as usize)),
//!         action_2() |> |v| v as usize,
//!         
//!         // thread::spawn(move || action_2().map(|v| v as usize + 1).and_then(|v| Ok(v * 4))),
//!         action_2() |> |v| v as usize + 1 => |v| Ok(v * 4),
//!         
//!         // thread::spawn(move || action_1().and_then(|_| Err("5".into())).or(Ok(2))),
//!         action_1() => |_| Err("5".into()) <| Ok(2),
//!         
//!         map => |a, b, c, d| a + b + c + d
//!     }.expect("Failed to calculate sum");
//!
//!     println!("Calculated: {}", sum);
//! }
//! ```
//!
//! ### Future tasks
//!
//! `join_async_spawn!` uses `::tokio::spawn` function to spawn tasks so it should be done inside `tokio` runtime
//! (number of branches is the max count of `tokio` tasks at the time).
//!
//! ```rust
//! #![recursion_limit="256"]
//!
//! use std::error::Error;
//! use join::try_join_async_spawn;
//! use futures::future::{ok, err};
//!
//! type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;
//!
//! async fn action_1() -> Result<u16> {
//!     Ok(1)
//! }
//!
//! async fn action_2() -> Result<u8> {
//!     Ok(2)
//! }
//!
//! #[tokio::main]
//! async fn main() {
//!     // Branches will be executed concurrently, in multi thread executor
//!     let sum = try_join_async_spawn! {
//!         // tokio::spawn(Box::pin(action_1()))
//!         action_1(),
//!
//!         // tokio::spawn(Box::pin(action_2().and_then(|v| ok(v as u16))))
//!         action_2() => |v| ok(v as u16),
//!
//!         // tokio::spawn(Box::pin(action_2().map(|v| v.map(|v| v as u16 + 1)).and_then(|v| ok(v * 4u16))))
//!         action_2() |> |v| v.map(|v| v as u16 + 1) => |v| ok(v * 4u16),
//!
//!         // tokio::spawn(Box::pin(action_1().and_then(|_| err("5".into())).or_else(|_| ok(2u16))))
//!         action_1() => |_| err("5".into()) <= |_| ok(2u16),
//!
//!         and_then => |a, b, c, d| ok(a + b + c + d)
//!     }.await.expect("Failed to calculate sum");
//!
//!     println!("Calculated: {}", sum);
//! }
//! ```
//!
//! ## Detailed steps example
//!
//! By separating chain in actions, you will make actions wait for completion of all of them in current step before go to the next step.
//!
//! ```rust
//! #![recursion_limit="256"]
//!
//! use std::error::Error;
//! use join::try_join;
//!
//! type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>;
//!
//! fn action_1() -> Result<u16> {
//!     Ok(1)
//! }
//!
//! fn action_2() -> Result<u8> {
//!     Ok(2)
//! }
//!
//! fn main() {
//!     let sum = try_join! {
//!         action_1(),
//!         let result_1 = action_2() ~|> |v| v as u16 + 1,
//!         action_2() ~|> {
//!             // `result_1` now is the result of `action_2()` [Ok(1u8)]
//!             let result_1 = result_1.as_ref().ok().map(Clone::clone);
//!             move |v| {
//!                 if result_1.is_some() {
//!                     v as u16 + 1
//!                 } else {
//!                     unreachable!()
//!                 }
//!             }
//!         } ~=> {
//!             // `result_1` now is the result of `|v| v as u16 + 1` [Ok(2u16)]
//!             let result_1 = result_1.as_ref().ok().map(Clone::clone);
//!             move |v| {
//!                 if let Some(result_1) = result_1 {
//!                     Ok(v * 4 + result_1)
//!                 } else {
//!                     unreachable!()
//!                 }
//!             }
//!         },
//!         action_1() ~=> |_| Err("5".into()) <| Ok(2),
//!         map => |a, b, c, d| a + b + c + d
//!     }.expect("Failed to calculate sum");
//!     println!("Calculated: {}", sum);
//! }
//! ```
#![recursion_limit = "256"]
extern crate join_export;
extern crate proc_macro_hack;
extern crate proc_macro_nested;

use proc_macro_hack::proc_macro_hack;

///
/// Use to combine sync results.
///
/// ```rust
/// extern crate join;
///
/// use join::try_join;
///
/// fn main() {
///     let product = try_join! {
///         Ok::<_,u8>(2) |> |v| v + 2,
///         Ok::<_,u8>(3),
///         Ok::<_,u8>(4),
///         map => |a, b, c| a * b * c
///     }.unwrap();
///
///     assert_eq!(product, 48);
/// }
/// ```
///
#[proc_macro_hack(support_nested)]
pub use join_export::try_join;

///
/// Use to combine futures.
///
/// ```rust
/// #![recursion_limit="256"]
///
/// extern crate join;
/// extern crate futures;
///
/// use join::try_join_async;
/// use futures::future::ok;
///
/// #[tokio::main]
/// async fn main() {
///     let product = try_join_async! {
///         ok::<_,u8>(2u16) => |v| ok::<_,u8>(v + 2u16),
///         ok::<_,u8>(3u16),
///         ok::<_,u8>(4u16),
///         map => |a, b, c| a * b * c
///     }.await.unwrap();
///
///     assert_eq!(product, 48);
/// }
/// ```
///
#[proc_macro_hack(support_nested, internal_macro_calls = 20)]
pub use join_export::try_join_async;

///
/// Use to spawn `::std::thread` per each step of each branch.
///
/// ```rust
/// extern crate join;
///
/// use join::try_join_spawn;
///
/// fn main() {
///     let product = try_join_spawn! {
///         Ok::<_,u8>(2) |> |v| v + 2 ?? |_| {
///             println!("Hello from parallel world!");
///             ::std::thread::sleep(::std::time::Duration::from_secs(1));
///             println!("I'm done.");
///         },
///         Ok::<_,u8>(3) ?? |_| {
///             println!("Hello from parallel world again!");
///             ::std::thread::sleep(::std::time::Duration::from_secs(2));
///             println!("Me too.");
///         },
///         Ok::<_,u8>(4),
///         map => |a, b, c| a * b * c
///     }.unwrap();
///
///     assert_eq!(product, 48);
/// }
///```
#[proc_macro_hack(support_nested)]
pub use join_export::try_join_spawn;

///
/// Alias for `try_join_spawn!`.
///
#[proc_macro_hack(support_nested)]
pub use join_export::try_spawn;

///
/// Use to spawn `::tokio::spawn` per each step of each branch.
/// ```rust
/// #![recursion_limit="512"]
///
/// extern crate join;
/// extern crate futures;
/// extern crate tokio;
/// extern crate futures_timer;
///
/// use join::try_join_async_spawn;
/// use futures::future::ok;
/// use futures_timer::Delay;
/// use std::time::Duration;
///
/// #[tokio::main]
/// async fn main() {
///     let product = try_join_async_spawn! {
///         ok::<_,u8>(2u16) |> |v| Ok::<_,u8>(v.unwrap() + 2u16) => |v| async move {
///             println!("Hello from parallel world!");
///             Delay::new(Duration::from_secs(1)).await;
///             println!("I'm done.");
///             Ok(v)
///         },
///         ok::<_,u8>(3u16) => |v| async move {
///             println!("Hello from parallel world again!");
///             Delay::new(Duration::from_secs(2)).await;
///             println!("Me too.");
///             Ok(v)
///         },
///         ok::<_,u8>(4u16),
///         map => |a, b, c| a * b * c
///     }.await.unwrap();
///
///     assert_eq!(product, 48);
/// }
///```
#[proc_macro_hack(support_nested, internal_macro_calls = 20)]
pub use join_export::try_join_async_spawn;

///
/// Alias for `try_join_async_spawn!`.
///
#[proc_macro_hack(support_nested, internal_macro_calls = 20)]
pub use join_export::try_async_spawn;

///
/// Use to combine sync values. It produces tuple of values or single value in case of 1 branch.
///
/// ```rust
/// extern crate join;
///
/// use join::join;
///
/// fn main() {
///     let filtered: Vec<_> = join! { vec![1,2,3].into_iter() ?> |v| v % 2 == 0 =>[] };
///     assert_eq!(filtered, vec![2]);
/// }
/// ```
///
#[proc_macro_hack(support_nested)]
pub use join_export::join;

///
/// Use to combine futures. It produces tuple of values or single value in case of 1 branch.
///
/// ```rust
/// #![recursion_limit="256"]
///
/// extern crate join;
/// extern crate futures;
///
/// use join::join_async;
/// use futures::future::{ready};
/// use futures::stream::iter;
///
/// #[tokio::main]
/// async fn main() {
///     let filtered: Vec<_> = join_async! { iter(vec![1u8,2,3]) ?> |v| ready(v % 2 == 0) =>[] }.await;
///     assert_eq!(filtered, vec![2]);
/// }
/// ```
///
#[proc_macro_hack(support_nested, internal_macro_calls = 20)]
pub use join_export::join_async;

///
/// Use to spawn `::std::thread` per each step of each branch.
///
/// ```rust
/// extern crate join;
///
/// use join::join_spawn;
///
/// fn main() {
///     let filtered: Vec<_> = join_spawn! { vec![1,2,3].into_iter() ?> |v| v % 2 == 0 =>[] };
///     assert_eq!(filtered, vec![2]);
/// }
///```
#[proc_macro_hack(support_nested)]
pub use join_export::join_spawn;

///
/// Alias for `join_spawn!`.
///
#[proc_macro_hack(support_nested)]
pub use join_export::spawn;

///
/// Use to spawn `::tokio::spawn` per each step of each branch.
/// ```rust
/// #![recursion_limit="256"]
///
/// extern crate join;
/// extern crate futures;
///
/// use join::join_async_spawn;
/// use futures::future::ready;
/// use futures::stream::{iter, StreamExt};
///
/// #[tokio::main]
/// async fn main() {
///     let filtered: Vec<_> = join_async_spawn! { iter(vec![1u8,2,3]) ?> |v| ready(v % 2 == 0) =>[] }.await;
///     assert_eq!(filtered, vec![2]);
/// }
///```
#[proc_macro_hack(support_nested, internal_macro_calls = 20)]
pub use join_export::join_async_spawn;

///
/// Alias for `join_async_spawn!`.
///
#[proc_macro_hack(support_nested, internal_macro_calls = 20)]
pub use join_export::async_spawn;