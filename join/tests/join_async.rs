#![recursion_limit = "1024"]

#[cfg(test)]
mod join_async_tests {
    use futures::executor::block_on;
    use futures::future::{err, ok, ready};
    use futures_timer::Delay;
    use join::join_async;
    use std::error::Error;

    type BoxedError = Box<dyn Error>;

    type Result<T> = std::result::Result<T, BoxedError>;

    type _Result<T, E> = std::result::Result<T, E>;

    async fn get_three() -> u16 {
        3
    }

    async fn get_ok_four() -> Result<u16> {
        Ok(4)
    }

    async fn get_some_five() -> Option<u16> {
        Some(5)
    }

    async fn get_err() -> Result<u16> {
        Err("error".into())
    }

    async fn get_none() -> Option<u16> {
        None
    }

    fn add_one_sync(v: u16) -> u16 {
        v + 1
    }

    async fn add_one(v: u16) -> u16 {
        v + 1
    }

    async fn add_one_ok(v: u16) -> Result<u16> {
        Ok(add_one(v).await)
    }

    async fn to_err(v: u16) -> Result<u16> {
        Err(v.to_string().into())
    }

    async fn to_some(v: u16) -> Option<u16> {
        Some(v)
    }

    fn add_one_to_ok(v: Result<u16>) -> Result<u16> {
        v.map(add_one_sync)
    }

    #[test]
    fn it_produces_n_branches_with_length_1() {
        block_on(async {
            let product = join_async! {
                ok(2u16),
                ok(get_three().await),
                get_ok_four(),
                ok(get_some_five().await.unwrap()),
                and_then => |a, b, c, d| ok(a * b * c * d)
            };

            assert_eq!(product.await.unwrap(), 120u16);

            let err = join_async! {
                ok(2u16),
                ok(get_three().await),
                get_ok_four(),
                get_err(),
                and_then => |a, b, c, d| ok(a * b * c * d)
            };

            assert_eq!(
                format!("{:?}", err.await.unwrap_err()),
                format!("{:?}", get_err().await.unwrap_err())
            );
        });
    }

    #[test]
    fn it_produces_n_branches_with_any_length() {
        block_on(async {
            let product = join_async! {
                ok(2u16).map(add_one_to_ok).and_then(add_one_ok), //4
                ok(get_three().await).and_then(add_one_ok).map(add_one_to_ok).map(add_one_to_ok).map(add_one_to_ok), //7
                get_ok_four().map(add_one_to_ok), //5
                get_some_five().map(|v| v.ok_or("unreachable".into())).and_then(to_err).and_then(add_one_ok).or_else(|_| ok(5)), // 5
                and_then => |a, b, c, d| ok(a * b * c * d)
            };

            assert_eq!(product.await.unwrap(), 700u16);

            let err = join_async! {
                ok(2).map(add_one_to_ok),
                ok(get_three().await).and_then(to_err),
                get_ok_four(),
                get_err(),
                map => |a, b, c, d| a * b * c * d
            };

            assert_eq!(
                format!("{:?}", err.await.unwrap_err()),
                format!("{:?}", to_err(get_three().await).await.unwrap_err())
            );
        });
    }
    #[test]
    fn it_produces_n_branches_with_any_length_using_combinators() {
        block_on(async {
            let product = join_async! {
                ok(2u16) |> add_one_to_ok => add_one_ok, //4
                ok(get_three().await) => add_one_ok |> add_one_to_ok |> add_one_to_ok |> add_one_to_ok, //7
                get_ok_four() |> add_one_to_ok, //5
                get_some_five() |> |v| v.ok_or("hello".into()) |> add_one_to_ok => to_err => add_one_ok <= |_| ok(5), // 5
                map => |a, b, c, d| a * b * c * d
            };

            assert_eq!(product.await.unwrap(), 700);

            let sum = join_async! {
                2u16 -> ok |> add_one_to_ok => add_one_ok, //4
                get_three().await -> ok => add_one_ok |> add_one_to_ok |> add_one_to_ok |> add_one_to_ok, //7
                get_ok_four() |> add_one_to_ok, //5
                get_some_five() |> |v| v.ok_or("hello".into()) |> add_one_to_ok => to_err => add_one_ok <= |_| ok(5), // 5
                and_then => |a, b, c, d| ok(a + b + c + d)
            };

            assert_eq!(sum.await.unwrap(), 21);

            let err: Result<u16> = join_async! {
                ok(2) |> add_one_to_ok,
                ok(get_three().await) => to_err,
                get_ok_four() => |_| err("some error".into()),
                get_err(),
                map => |a: u16, b: u16, c: u16, d: u16| a * b * c * d
            }
            .await;

            assert_eq!(
                format!("{:?}", err.unwrap_err()),
                format!("{:?}", to_err(get_three().await).await.unwrap_err())
            );

            let none = join_async! {
                2 -> to_some,
                get_some_five().await -> ready |> |_| None,
                get_ok_four() |> |v| v.ok(),
                get_none(),
                map => |a: u16, b: u16, c: u16, d: u16| a * b * c * d
            };

            assert_eq!(none.await, None);
        });
    }

    #[test]
    fn it_tests_handler_behaviour() {
        block_on(async {
            let ok_value = join_async! {
                ok(2u16),
                ok(3u16),
                get_ok_four(),
                and_then => |a, b, c| ok::<Option<u16>, _>(None)
            };

            assert_eq!(ok_value.await.unwrap(), None);

            let err_value = join_async! {
                ok(2u16),
                ok(3u16),
                get_ok_four(),
                and_then => |a, b, c| to_err(a)
            };

            assert_eq!(
                format!("{:?}", err_value.await.unwrap_err()),
                format!("{:?}", to_err(2).await.unwrap_err())
            );

            let some = join_async! {
                ready(Some(2u16)),
                ready(Some(3u16)),
                get_some_five(),
                map => |a, b, c| a
            };

            assert_eq!(some.await, Some(2u16));

            let none = join_async! {
                ready(Some(2u16)),
                ready(Some(3u16)),
                get_some_five(),
                then => |a, b, c| ready(None::<u16>)
            };

            assert_eq!(none.await, None);

            let some = join_async! {
                ready(Some(2u16)),
                ready(Some(3u16)),
                get_some_five(),
                map => |a, b, c| a + b + c
            };

            assert_eq!(some.await, Some(10));

            let none = join_async! {
                ready(Some(2u16)),
                None -> ready,
                get_some_five(),
                map => |a: u16, b: u16, c: u16| a + b + c
            };

            assert_eq!(none.await, None);

            let ok_value = join_async! {
                ok(2u16),
                ok(3u16),
                get_ok_four(),
                then => |a: Result<u16>, b: Result<u16>, c: Result<u16>| ok::<_, u8>(a.unwrap() + b.unwrap() + c.unwrap())
            };

            assert_eq!(ok_value.await.unwrap(), 9u16);

            let err_value = join_async! {
                ok(2u16),
                ok(3u16),
                get_ok_four(),
                then => |a: Result<u16>, b: Result<u16>, c: Result<u16>| err::<u16, _>(a.unwrap() + b.unwrap() + c.unwrap())
            };

            assert_eq!(err_value.await, Err(9));
        });
    }

    #[test]
    fn it_tests_steps() {
        block_on(async {
            let product = join_async! {
                let branch_0 = ok(2u16) ~|> {
                    let branch_0 = branch_0.as_ref().ok().map(Clone::clone);
                    let branch_1 = branch_1.as_ref().ok().map(Clone::clone);
                    let branch_2 = branch_2.as_ref().ok().map(Clone::clone);
                    let branch_3 = branch_3.as_ref().map(Clone::clone);
                    move |value: Result<u16>| {
                        assert_eq!(branch_0, value.as_ref().ok().map(Clone::clone));
                        assert_eq!(branch_1, Some(3));
                        assert_eq!(branch_2, Some(4));
                        assert_eq!(branch_3, Some(5));
                        value.map(add_one_sync)
                    }
                } ~=> add_one_ok, //4
                let branch_1 = get_three().await -> ok ~=> add_one_ok ~|> |value| value |> {
                    let branch_0 = branch_0.as_ref().ok().map(Clone::clone);
                    let branch_1 = branch_1.as_ref().ok().map(Clone::clone);
                    let branch_2 = branch_2.as_ref().ok().map(Clone::clone);
                    let branch_3 = branch_3.as_ref().ok().map(Clone::clone);
                    move |value: Result<u16>| {
                        assert_eq!(branch_0, Some(3));
                        assert_eq!(branch_1, value.as_ref().ok().map(Clone::clone));
                        assert_eq!(branch_2, Some(5));
                        assert_eq!(branch_3, Some(5));
                        value.map(add_one_sync)
                    }
                } ~|> add_one_to_ok ~|> add_one_to_ok, //7
                let branch_2 = get_ok_four() ~|> add_one_to_ok, //5
                let branch_3 = get_some_five() ~|> |v| v.ok_or("unreachable".into()) ~|> add_one_to_ok ~=> to_err <= |_| ok(5) ~=> add_one_ok, // 6
                map => |a, b, c, d| a * b * c * d
            };

            assert_eq!(product.await.unwrap(), 840);
        });
    }
    #[test]
    fn it_checks_mutli_threading() {
        block_on(async {
            use futures::lock::Mutex;
            use std::sync::Arc;
            use std::time::Duration;

            let values = Arc::new(Mutex::new(Vec::new()));

            let (values0, values1, values2) = (values.clone(), values.clone(), values.clone());

            let _ = join_async! {
                ok((values0, 1u16)) => |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(1)).await;
                    let mut values = values.lock().await;
                    values.sort();
                    assert_eq!(values[..], [1, 2, 3]);
                    values.pop();
                    Ok::<_, BoxedError>(())
                },
                ok((values1, 2u16)) => |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(2)).await;
                    let mut values = values.lock().await;
                    values.sort();
                    assert_eq!(values[..], [1, 2]);
                    values.pop();
                    Ok::<_, BoxedError>(())
                },
                ok((values2, 3u16)) => |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(3)).await;
                    let mut values = values.lock().await;
                    values.sort();
                    assert_eq!(values[..], [1]);
                    values.pop();
                    Ok::<_, BoxedError>(())
                },
                then => move |_, _, _| async move {
                    assert_eq!(values.lock().await.len(), 0);
                    Ok::<_, BoxedError>(())
                }
            }
            .await
            .unwrap();
        });
    }

    #[test]
    fn it_produces_tuple() {
        block_on(async {
            let values = join_async! {
                futures_crate_path(::futures)
                ok::<_,u8>(2), ok::<_,u8>(3)
            }
            .await;
            assert_eq!(values.unwrap(), (2, 3));
        });
    }

    #[test]
    fn it_produces_single_value() {
        block_on(async {
            let value = join_async! { ready(Some(1)) }.await;
            assert_eq!(value.unwrap(), 1);
        });
    }

    #[allow(unreachable_code)]
    #[test]
    fn it_creates_unpolled_future() {
        block_on(async {
            let _fut = join_async! {
                ready(panic!()),
                ready(unreachable!()),
                then => |a: Result<u8>, b: Result<u8>| ready(a)
            };
        });
    }

    #[test]
    fn it_tests_readme_demo_async_behaviour_and_requires_internet_connection() {
        use ::failure::{format_err, Error};
        use ::futures::future::{ok, ready, try_join_all};
        use ::futures::stream::{iter, Stream};
        use ::reqwest::Client;
        use join::{join, join_async};

        type Result<T, E> = std::result::Result<T, E>;

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

        async fn read_number_from_stdin() -> Result<usize, Error> {
            let mut input = iter(vec!["100"]);
            let result = join_async! {
                input
                    .next()
                    |> |value| value.ok_or(format_err!("Failed to read from input"))
                    => |value|
                        ready(
                            value
                                .parse()
                                .map_err(|e| format_err!("Failed to parse input from stdin: {:?}, input: {}", e, value))
                        )
            };
            result.await
        }

        let rt = ::tokio::runtime::Runtime::new().unwrap();

        rt.block_on(async {

            println!(
                "Hello.\nThis's is the game where winner is the player, which value is the closest to {}",
                "the maximum link count found on one of random pages.\nYou play against random generator (0-500)."
            );

            enum GameResult {
                Won,
                Lost,
                Draw
            }

            let client = Client::new();
            
            let task = join_async! {
                // This programs makes requests to several sites 
                // and calculates count of links starting from `https://`
                get_urls_to_calculate_link_count() 
                    |> {
                        // If pass block statement instead of fn, it will be placed before current step, 
                        // so it will us allow to capture some variables from context
                        let ref client = client;
                        move |url| {
                            let client = client.clone();
                            async move { 
                                client
                                    .get(url)
                                    .send()
                                    .and_then(|value| value.text())
                                    .await
                                    .and_then(|body| Ok((url, body))) 
                            }
                        }
                    }
                    >.collect::<Vec<_>>()
                    |> Ok
                    => try_join_all
                    !> |err| format_err!("Error retrieving pages to calculate links: {:#?}", err)
                    => |results| 
                        ok(
                            results
                                .into_iter()
                                .map(|(url, body)| (url.to_owned(), body.matches("https://").collect::<Vec<_>>().len()))
                                .max_by_key(|(_, link_count)| link_count.clone())
                                .unwrap()
                        )
                    // It waits for input in stdin before log max links count
                    ~?> |result| { 
                        result
                            .as_ref()
                            .map(
                                |(url, count)| 
                                    println!("Max link count found on `{}`: {}", url, count)
                            )
                            .unwrap_or(()); 
                    },
                // In parallel it makes request to the site which generates random number
                get_url_to_get_random_number()
                    -> ok
                    => {
                        // If pass block statement instead of fn, it will be placed before current step,
                        // so it will allow us to capture some variables from context
                        let client = client.clone();
                        let map_parsing_error = 
                            |value| 
                                move |err| 
                                    format_err!("Failed to parse random number: {:#?}, value: {}", err, value);
                        move |url| {
                            join_async! {
                                client
                                    .get(url)
                                    .send()
                                    => |value| value.text()
                                    !> |err| format_err!("Error retrieving random number: {:#?}", err)
                                    => |value| ok(value[..value.len() - 1].to_owned()) // remove \n from `154\n`
                                    => |value|  
                                        ready(
                                            value
                                                .parse::<usize>()
                                                .map_err(map_parsing_error(value))
                                        )

                                
                            }
                        }
                    }
                    // It waits for input in stdin before log random value
                    ~?> |random| {
                        random
                            .as_ref()
                            .map(|number| println!("Random: {}", number))
                            .unwrap_or(()); 
                    },
                // In parallel it reads value from stdin
                println!("Please, enter number") -> |_| read_number_from_stdin(),
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

            // Use sync join because we don't need parallel execution and sync map
            // is more convenient
            let _ = join! {
                task 
                    .await
                    |> |result| 
                        println!(
                            "You {}", 
                            match result { 
                                GameResult::Won => "won!", 
                                GameResult::Lost => "lose...", 
                                _ => "have the same result as random generator!"
                            }
                        )
            }.unwrap();   
        });
    }
}
