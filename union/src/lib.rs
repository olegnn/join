//! # `union!`
//! 
//! `union!` - one macro to rule them all. Provides useful shortcut combinators, combines sync/async chains, transforms tuple of results in result of tuple, supports single and multi thread (sync/async) step by step execution of branches.
//! 
//! Using this macro you can write things like
//! 
//! ```rust no_run
//! #![recursion_limit="1024"]
//! 
//! use union::union_async;
//! use futures::stream::{iter, Stream};
//! use reqwest::Client;
//! use futures::future::{try_join_all, ok, ready};
//! use failure::{format_err, Error};
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
//! fn get_url_to_get_random_number() -> String {
//!     "https://www.random.org/integers/?num=1&min=0&max=500&col=1&base=10&format=plain&rnd=new".to_owned()
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
//!         result = union_async! {
//!             next
//!                 |> |value| value.ok_or(format_err!("Unexpected end of input"))
//!                 => |result| ready(result.map_err(|err| format_err!("Failed to apply codec: {:?}", err)))
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
//! 
//! #[tokio::main]
//! async fn main() {
//!     println!(
//!         "{} {}\n{}",
//!         "Hello.\nThis's is the game where winner is player, which abs(value) is closest to",
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
//!     let game = union_async! {
//!         // Make requests to several sites
//!         // and calculate count of links starting from `https://`
//!         get_urls_to_calculate_link_count()
//!             |> {
//!                 // If pass block statement instead of fn, it will be placed before current step,
//!                 // so it will us allow to capture some variables from context
//!                 let ref client = client;
//!                 move |url| {
//!                     let client = client.clone();
//!                     // `union_async!` wraps its content into `async move { }` 
//!                     union_async! {
//!                         client
//!                             .get(url).send()
//!                             => |value| value.text()
//!                             => |body| ok((url, body))
//!                     }
//!                 }
//!             }
//!             >.collect::<Vec<_>>()
//!             |> Ok
//!             => try_join_all
//!             !> |err| format_err!("Error retrieving pages to calculate links: {:#?}", err)
//!             => |results|
//!                 ok(
//!                     results
//!                         .into_iter()
//!                         .map(|(url, body)| (url, body.matches("https://").collect::<Vec<_>>().len()))
//!                         .max_by_key(|(_, link_count)| link_count.clone())
//!                         .unwrap()
//!                 )
//!             // It waits for input in stdin before log max links count
//!             ~?> |result| {
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
//!         // In parallel it makes request to the site which generates random number
//!         get_url_to_get_random_number()
//!             -> ok
//!             => {
//!                 // If pass block statement instead of fn, it will be placed before current step,
//!                 // so it will allow us to capture some variables from context
//!                 let client = client.clone();
//!                 let map_parse_error =
//!                     |value|
//!                         move |err|
//!                             format_err!("Failed to parse random number: {:#?}, value: {}", err, value);
//!                 move |url| {
//!                     union_async! {
//!                         client
//!                             .get(&url)
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
//!                 }
//!             }
//!             // It waits for input in stdin before log random value
//!             ~?> |random| {
//!                 random
//!                     .as_ref()
//!                     .map(|number| println!("Random: {}", number))
//!                     .unwrap_or(());
//!             },
//!         // In parallel it reads value from stdin
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
//! ```
//! 
//! ## Combinators
//! 
//! - Map: `|>` expr - `value`.map(`expr`)
//! 
//! - AndThen: `=>` expr - `value`.and_then(`expr`),
//! 
//! - Then: `->` expr - `expr`(`value`)
//! 
//! - Dot: `>.` expr - `value`.`expr`
//! 
//! - Or: `<|` expr - `value`.or(`expr`)
//! 
//! - OrElse: `<=` expr - `value`.or_else(`expr`)  
//! 
//! - MapErr: `!>` expr - `value`.map_err(`expr`)
//! 
//! - Inspect: `?>` expr - (|`value`| { `expr`(&`value`); `value` })(`value`) for sync chains and (|`value`| `value`.inspect(`expr`))(`value`) for futures
//! 
//! where `value` is the previous value.
//! 
//! Every combinator prefixed by `~` will act as deferred action (all actions will wait until completion in every step and only after move to the next one).
//! 
//! ## Handler
//! 
//! might be one of
//! 
//! - `map` => will act as results.map(|(result0, result1, ..)| handler(result0, result1, ..))
//! 
//! - `and_then` => will act as results.and_then(|(result0, result1, ..)| handler(result0, result1, ..))
//! 
//! - `then` => will act as handler(result0, result1, ..)
//! 
//! or not specified - then Result<(result0, result1, ..), Error> or Option<(result0, result1, ..)> will be returned.
//! 
//! ## Custom futures crate path
//! 
//! You can specify custom path (`futures_crate_path`) at the beginning of macro call
//! 
//! ```rust
//! use union::union_async;
//! use futures::future::ok;
//! 
//! #[tokio::main]
//! async fn main() {
//!     let value = union_async! {
//!         futures_crate_path(::futures)
//!         ok::<_,u8>(2u16)
//!     }.await.unwrap();
//!     
//!     println!("{}", value);
//! }
//! ```
//! 
//! ## Single thread combinations
//! 
//! ### Simple results combination
//! 
//! Converts input in series of chained results and joins them step by step.
//! 
//! ```rust
//! 
//! use std::error::Error;
//! use union::union;
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
//!     let sum = union! {
//!         action_1(),
//!         action_2().map(|v| v as u16),
//!         action_2().map(|v| v as u16 + 1).and_then(|v| Ok(v * 4)),
//!         action_1().and_then(|_| Err("5".into())).or(Ok(2)),
//!         map => |a, b, c, d| a + b + c + d
//!     }.expect("Failed to calculate sum");
//! 
//!     println!("Calculated: {}", sum);
//! }
//! ```
//! 
//! ### Futures combination
//! 
//! Each branch will represent chain of tasks. All branches will be joined using `join!` macro and macro will return `unpolled` future.
//! 
//! ```rust
//! #![recursion_limit="256"]
//!
//! use std::error::Error;
//! use union::union_async;
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
//!     let sum = union_async! {
//!         action_1(),
//!         action_2().and_then(|v| ok(v as u16)),
//!         action_2().map(|v| v.map(|v| v as u16 + 1)).and_then(|v| ok(v * 4u16)),
//!         action_1().and_then(|_| err("5".into())).or_else(|_| ok(2u16)),
//!         and_then => |a, b, c, d| ok(a + b + c + d)
//!     }.await.expect("Failed to calculate sum");
//! 
//!     println!("Calculated: {}", sum);
//! }
//! ```
//! 
//! ## Multi-thread combinations
//! 
//! To execute several tasks in parallel you could use `union_spawn!` (`spawnion!`) for sync tasks
//! and `union_async_spawn!` (`async_spawn!`) for futures. Since `union_async` already provides parallel futures execution in one thread, `union_async_spawn!` spawns every branch into `tokio` executor so they will be evaluated in multi-threaded executor.
//! 
//! ### Multi-thread sync branches
//! 
//! `union_spawn` spawns one `::std::thread` per each step of each branch (number of branches is the max thread count at the time).
//! 
//! ```rust
//! 
//! use std::error::Error;
//! use union::union_spawn;
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
//!     let sum = union_spawn! {
//!         action_1(),
//!         action_2().map(|v| v as usize),
//!         action_2().map(|v| v as usize + 1).and_then(|v| Ok(v * 4)),
//!         action_1().and_then(|_| Err("5".into())).or(Ok(2)),
//!         map => |a, b, c, d| a + b + c + d
//!     }.expect("Failed to calculate sum");
//! 
//!     println!("Calculated: {}", sum);
//! }
//! ```
//! 
//! `union_async_spawn!` uses `::tokio::spawn` function to spawn tasks so it should be done inside `tokio` runtime
//! (number of branches is the max count of `tokio` tasks at the time).
//! 
//! ### Multi-thread futures
//! 
//! ```rust
//! #![recursion_limit="256"]
//! 
//! use std::error::Error;
//! use union::union_async_spawn;
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
//!     let sum = union_async_spawn! {
//!         action_1(),
//!         action_2().and_then(|v| ok(v as u16)),
//!         action_2().map(|v| v.map(|v| v as u16 + 1)).and_then(|v| ok(v * 4u16)),
//!         action_1().and_then(|_| err("5".into())).or_else(|_| ok(2u16)),
//!         and_then => |a, b, c, d| ok(a + b + c + d)
//!     }.await.expect("Failed to calculate sum");
//! 
//!     println!("Calculated: {}", sum);
//! }
//! ```
//! 
//! Using combinators we can rewrite first sync example like
//! 
//! ```rust
//! 
//! use std::error::Error;
//! use union::union;
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
//!     let sum = union! {
//!         action_1(),
//!         action_2() |> |v| v as u16,
//!         action_2() |> |v| v as u16 + 1 => |v| Ok(v * 4),
//!         action_1() => |_| Err("5".into()) <| Ok(2),
//!         map => |a, b, c, d| a + b + c + d
//!     }.expect("Failed to calculate sum");
//! 
//!     println!("Calculated: {}", sum);
//! }
//! ```
//! 
//! By separating chain in actions, you will make actions wait for completion of all of them in current step before go to the next step.
//! 
//! ```rust
//! #![recursion_limit="256"]
//!
//! use std::error::Error;
//! use union::union;
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
//!     let sum = union! {
//!         action_1(),
//!         let result_1 = action_2() ~|> |v| v as u16 + 1,
//!         action_2() ~|> {
//!             let result_1 = result_1.as_ref().ok().map(Clone::clone);
//!             move |v| {
//!                 // `result_1` now is the result of `action_2()` [Ok(1u8)]
//!                 if result_1.is_some() {
//!                     v as u16 + 1
//!                 } else {
//!                     unreachable!()
//!                 }
//!             }
//!         } ~=> {
//!             let result_1 = result_1.as_ref().ok().map(Clone::clone);
//!             move |v| {
//!                 // `result_1` now is the result of `|v| v as u16 + 1` [Ok(2u16)]
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


extern crate proc_macro_hack;
extern crate proc_macro_nested;
extern crate union_export;

use proc_macro_hack::proc_macro_hack;

///
/// Use to combine sync results.
///
/// ```rust
/// extern crate union;
///
/// use union::union;
///
/// fn main() {
///     let product = union! {
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
pub use union_export::union;

///
/// Use to combine futures.
///
/// ```rust
/// extern crate union;
/// extern crate futures;
///
/// use union::union_async;
/// use futures::future::ok;
///
/// #[tokio::main]
/// async fn main() {
///     let product = union_async! {
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
pub use union_export::union_async;

///
/// Alias for `union_async!`.
///
#[proc_macro_hack(support_nested, internal_macro_calls = 20)]
pub use union_export::asyncion;

///
/// Use to spawn `::std::thread` per each step of each branch.
///
/// ```rust
/// extern crate union;
///
/// use union::union_spawn;
///
/// fn main() {
///     let product = union_spawn! {
///         Ok::<_,u8>(2) |> |v| v + 2 ?> |_| { 
///             println!("Hello from parallel world!"); 
///             ::std::thread::sleep(::std::time::Duration::from_secs(1));
///             println!("I'm done.");
///         },
///         Ok::<_,u8>(3) ?> |_| { 
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
pub use union_export::union_spawn;

///
/// Alias for `union_spawn!`.
/// 
#[proc_macro_hack(support_nested)]
pub use union_export::spawn;

///
/// Use to spawn `::tokio::spawn` per each step of each branch.
/// ```rust
/// #![recursion_limit="512"]
/// 
/// extern crate union;
/// extern crate futures;
/// extern crate tokio;
///
/// use union::union_async_spawn;
/// use futures::future::ok;
/// 
/// #[tokio::main]
/// async fn main() {
///     let product = union_async_spawn! {
///         ok::<_,u8>(2u16) |> |v| Ok::<_,u8>(v.unwrap() + 2u16) ?> |_| { 
///             println!("Hello from parallel world!"); 
///             // !!! Don't use std::thread::sleep to wait inside future because it will block executor thread !!!
///             // It's used here only to show that futures are executed on multi thread executor.
///             ::std::thread::sleep(::std::time::Duration::from_secs(1));
///             println!("I'm done.");
///         },
///         ok::<_,u8>(3u16) ?> |_| { 
///             println!("Hello from parallel world again!"); 
///             // !!! Don't use std::thread::sleep to wait inside future because it will block executor thread !!!
///             // It's used here only to show that futures are executed on multi thread executor.
///             ::std::thread::sleep(::std::time::Duration::from_secs(2));
///             println!("Me too.");
///         },
///         ok::<_,u8>(4u16),
///         map => |a, b, c| a * b * c
///     }.await.unwrap();
///
///     assert_eq!(product, 48);
/// }
///```
#[proc_macro_hack(support_nested, internal_macro_calls = 20)]
pub use union_export::union_async_spawn;

///
/// Alias for `union_async_spawn!`.
/// 
#[proc_macro_hack(support_nested, internal_macro_calls = 20)]
pub use union_export::async_spawn;