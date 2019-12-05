#![recursion_limit = "1024"]

#[cfg(test)]
mod join_async_tests {
    use futures::executor::block_on;
    use futures::future::{err, ok, ready};
    use futures_timer::Delay;
    use join::{join_async, try_join_async};
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

    async fn get_ok_five() -> Result<u16> {
        Ok(5)
    }

    async fn get_err() -> Result<u16> {
        Err("error".into())
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

    fn add_one_to_ok(v: Result<u16>) -> Result<u16> {
        v.map(add_one_sync)
    }

    #[test]
    fn it_produces_n_branches_with_length_1() {
        block_on(async {
            let product = try_join_async! {
                ok(2u16),
                ok(get_three().await),
                get_ok_four(),
                ok(get_ok_five().await.unwrap()),
                and_then => |a, b, c, d| ok(a * b * c * d)
            };

            assert_eq!(product.await.unwrap(), 120u16);

            let err = try_join_async! {
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
            let product = try_join_async! {
                ok(2u16).map(add_one_to_ok).and_then(add_one_ok), //4
                ok(get_three().await).and_then(add_one_ok).map(add_one_to_ok).map(add_one_to_ok).map(add_one_to_ok), //7
                get_ok_four().map(add_one_to_ok), //5
                get_ok_five().map(|v| v).and_then(to_err).and_then(add_one_ok).or_else(|_| ok(5)), // 5
                and_then => |a, b, c, d| ok(a * b * c * d)
            };

            assert_eq!(product.await.unwrap(), 700u16);

            let err = try_join_async! {
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
            let product = try_join_async! {
                ok(2u16) |> add_one_to_ok => add_one_ok, //4
                ok(get_three().await) => add_one_ok |> add_one_to_ok |> add_one_to_ok |> add_one_to_ok, //7
                get_ok_four() |> add_one_to_ok, //5
                get_ok_five() |> |v| v |> add_one_to_ok => to_err => add_one_ok <= |_| ok(5), // 5
                map => |a, b, c, d| a * b * c * d
            };

            assert_eq!(product.await.unwrap(), 700);

            let sum = try_join_async! {
                2u16 -> ok |> add_one_to_ok => add_one_ok, //4
                get_three().await -> ok => add_one_ok |> add_one_to_ok |> add_one_to_ok |> add_one_to_ok, //7
                get_ok_four() |> add_one_to_ok, //5
                get_ok_five() |> |v| v |> add_one_to_ok => to_err => add_one_ok <= |_| ok(5), // 5
                and_then => |a, b, c, d| ok(a + b + c + d)
            };

            assert_eq!(sum.await.unwrap(), 21);

            let err: Result<u16> = try_join_async! {
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
        });
    }

    #[test]
    fn it_tests_handler_behaviour() {
        block_on(async {
            let ok_value = try_join_async! {
                ok(2u16),
                ok(3u16),
                get_ok_four(),
                and_then => |a, b, c| ok::<Option<u16>, _>(None)
            };

            assert_eq!(ok_value.await.unwrap(), None);

            let err_value = try_join_async! {
                ok(2u16),
                ok(3u16),
                get_ok_four(),
                and_then => |a, b, c| to_err(a)
            };

            assert_eq!(
                format!("{:?}", err_value.await.unwrap_err()),
                format!("{:?}", to_err(2).await.unwrap_err())
            );

            let okay = try_join_async! {
                ready(Ok(2u16)),
                ready(Ok(3u16)),
                get_ok_five(),
                map => |a, b, c| a
            };

            assert_eq!(okay.await.unwrap(), 2u16);

            let error: Result<u16> = try_join_async! {
                ready(Ok(2u16)),
                ready(Ok(3u16)),
                get_ok_five(),
                and_then => |a, b, c| ready(Err("error".into()))
            }
            .await;

            assert_eq!(error.unwrap_err().to_string(), "error".to_string());

            let okay = try_join_async! {
                ready(Ok(2u16)),
                ready(Ok(3u16)),
                get_ok_five(),
                map => |a, b, c| a + b + c
            };

            assert_eq!(okay.await.unwrap(), 10);

            let okay: Result<u16> = join_async! {
                ready(Ok::<_,BoxedError>(2u16)),
                ready(Ok::<_,BoxedError>(3u16)),
                err::<u16,BoxedError>("25".into()),
                then => |a, b, c| ready(a)
            }
            .await;

            assert!(okay.is_ok());

            let error = try_join_async! {
                ready(Ok(2u16)),
                Err("hey".into()) -> ready,
                get_ok_five(),
                map => |a: u16, b: u16, c: u16| a + b + c
            };

            assert_eq!(error.await.unwrap_err().to_string(), "hey".to_string());

            let ok_value = join_async! {
                ok(2u16),
                ok(3u16),
                get_ok_four(),
                then => |a: Result<u16>, b: Result<u16>, c: Result<u16>| ok::<_,()>(a.unwrap() + b.unwrap() + c.unwrap())
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

    #[allow(unreachable_code)]
    #[test]
    fn it_checks_evalutation_in_case_of_error() {
        block_on(async {
            let error = try_join_async! { err::<u8,u8>(2u8) ~=> |_| { unreachable!(); ok(2u8) }, ok::<u8,u8>(3u8) };
            assert_eq!(error.await, Err(2u8));
        });
    }

    #[test]
    fn it_tests_steps() {
        block_on(async {
            let product = try_join_async! {
                let branch_0 = ok(2u16) ~|> {
                    let branch_0 = branch_0.as_ref().ok().map(Clone::clone);
                    let branch_1 = branch_1.as_ref().ok().map(Clone::clone);
                    let branch_2 = branch_2.as_ref().ok().map(Clone::clone);
                    let branch_3 = branch_3.as_ref().ok().map(Clone::clone);
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
                let branch_3 = get_ok_five() ~|> |v| v ~|> add_one_to_ok ~=> to_err <= |_| ok(5) ~=> add_one_ok, // 6
                map => |a, b, c, d| a * b * c * d
            };

            assert_eq!(product.await.unwrap(), 840);
        });
    }

    #[test]
    fn it_checks_concurrent_branches_execution() {
        block_on(async {
            use futures::lock::Mutex;
            use std::sync::Arc;
            use std::time::Duration;

            let values = Arc::new(Mutex::new(Vec::new()));

            let _ = join_async! {
                ok((values.clone(), 1u16)) => |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(1)).await.unwrap();
                    {
                        let mut values = values.lock().await;
                        values.sort();
                        assert_eq!(values[..], [1, 2, 3]);
                        values.pop();
                    }
                    Ok::<_, BoxedError>((values, value + 1))
                } ~=> |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(1)).await.unwrap();
                    let mut values = values.lock().await;
                    values.sort();
                    assert_eq!(values[..], [2, 3, 4]);
                    Ok::<_, BoxedError>(())
                },
                ok((values.clone(), 2u16)) => |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(2)).await.unwrap();
                    {
                        let mut values = values.lock().await;
                        values.sort();
                        assert_eq!(values[..], [1, 2]);
                        values.pop();
                    }
                    Ok::<_, BoxedError>((values, value + 1))
                } ~=> |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(2)).await.unwrap();
                    let mut values = values.lock().await;
                    values.sort();
                    assert_eq!(values[..], [2, 3, 4]);
                    Ok::<_, BoxedError>(())
                },
                ok((values.clone(), 3u16)) => |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(3)).await.unwrap();
                    {
                        let mut values = values.lock().await;
                        values.sort();
                        assert_eq!(values[..], [1]);
                        values.pop();
                    }
                    Ok::<_, BoxedError>((values, value + 1))
                } ~=> |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(3)).await.unwrap();
                    let mut values = values.lock().await;
                    values.sort();
                    assert_eq!(values[..], [2, 3, 4]);
                    Ok::<_, BoxedError>(())
                },
                then => |_, _, _| async {
                    assert_eq!(values.clone().lock().await[..], [2, 3, 4]);
                    Ok::<_, BoxedError>(())
                }
            }
            .await
            .unwrap();
        });
    }

    #[test]
    fn it_works_with_custom_joiner() {
        block_on(async {
            let values = join_async! {
                ready(2), ready(3)
            }
            .await;
            assert_eq!(values, (2, 3));
        });
    }

    #[test]
    fn it_produces_tuple() {
        block_on(async {
            let values = try_join_async! {
                futures_crate_path(::futures)
                ok::<_,()>(2), ok::<_,()>(3)
            }
            .await;
            assert_eq!(values.unwrap(), (2, 3));
        });
    }

    #[test]
    fn it_produces_single_value() {
        block_on(async {
            let value = try_join_async! { ready(Ok::<_,()>(1)) }.await;
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
    fn it_works_with_references_correcly() {
        let mut arr = [1u8, 2, 3, 4, 5];
        let refer = &mut arr;
        block_on(Box::pin(async move {
            let _ = try_join_async! {
                ok::<_,()>(refer) ~=> >>> ..into_iter().for_each(|v| { *v += 1; }) -> ok,
            }
            .await;
        }));
        assert_eq!(arr, [2, 3, 4, 5, 6]);
    }

    #[test]
    fn it_tests_multi_step_single_branch() {
        block_on(async {
            let values = try_join_async! { vec![1u8,2,3,4,5,6,7,8,9].into_iter() -> ok ~=> >>> ?> |v| v % 3 != 0 =>[] Vec<_> -> ok::<_,()> ~|> |v| v ~=> |v| ok(v) }.await.unwrap();
            assert_eq!(values, vec![1u8, 2, 4, 5, 7, 8]);
        });
    }

    #[test]
    fn it_tests_iter_combinators() {
        block_on(async {
            let mut some_vec = Ok(vec![0u8]);

            let values: (Vec<u8>, Vec<u8>) = join_async! {
                vec![2u8, 3, 4, 5, 6, 7, 8, 9, 10, 11].into_iter() |> |v| { some_vec = Err(5); v + 1 } ?|> |v| if v % 2 == 0 { Some(v) } else { None } |n> ^@ { some_vec.clone() }, |mut acc, (index, v)| { acc.as_mut().unwrap().push(v + (index as u8)); acc } ..unwrap().into_iter() =>[] Vec<_> ..into_iter() ?&!> |&n| (n as f64).cos().abs() > ::std::f64::consts::PI / 3f64 -> ready
            }.await;

            assert_eq!(values, (vec![], vec![0u8, 4, 7, 10, 13, 16]));

            let values = vec![0, 1u8, 2, 3, 4, 5, 6];
            let other_values = vec![4u8, 5, 6, 7, 8, 9, 10];

            assert_eq!(
                join_async! {
                    { let values = values.clone(); values.into_iter() } >^> { let other_values = other_values.clone(); other_values.into_iter() } ?&!> |(v, v1)| v % 2 == 0 && v1 % 2 == 0 -> ready,
                }.await,
                (
                    vec![(0u8, 4u8), (2, 6), (4, 8), (6, 10)],
                    vec![(1u8, 5u8), (3, 7), (5, 9)]
                )
            );

            let values = vec![0, 1u8, 2, 3, 4, 5, 6];
            let other_values = vec![4u8, 5, 6, 7, 8, 9, 10];

            assert_eq!(
                join_async! {
                    { let values = values.clone(); values.into_iter() } >^> { let other_values = other_values.clone(); other_values.into_iter() } <-> u8, u8, Vec<_>, Vec<_> -> ready
                }.await,
                {  
                    let values = vec![0, 1u8, 2, 3, 4, 5, 6];
                    let other_values = vec![4u8, 5, 6, 7, 8, 9, 10]; 
                    (values, other_values)
                }
            );

            assert_eq!(
                join_async! { vec![1u8, 2, 3, 4, 5].into_iter() ?> |v| v % 2 != 0 =>[] Vec<_> -> ready }.await,
                vec![1u8, 3, 5]
            );

            assert_eq!(
                try_join_async! { let mut v = vec![1u8, 2, 3, 4, 5] ..into_iter() ?|>@ |v: u8| if v % 2 == 0 { Some(v) } else { None } -> |v: Option<u8>| ready(v.ok_or(Err::<u8,&'static str>("e"))) }.await,
                Ok(2u8)
            );

            assert_eq!(
                join_async! { vec![vec![1u8, 2, 3], vec![2]].into_iter() ^^> =>[] Vec<_> -> ready }
                    .await,
                vec![1u8, 2, 3, 2]
            );

            assert!(
                try_join_async! { vec![Ok(5u8), Err(4u8)].into_iter() ?^@ 0u8, |acc, v| v.map(|v| acc + v) -> ready }.await.is_err()
            );
        });
    }

    #[test]
    fn it_tests_nested_macro_combinations() {
        use futures::executor::block_on;
        use futures::future::*;
        use join::*;

        block_on(async {
            let value = try_join_async! {
                try_join_async! {
                    ok::<_,()>(2u32),
                    ok::<_,()>(3u32),
                    ok::<_,()>(4u32),
                    try_join_async! {
                        ok::<_,()>(6u32),
                        join_async! {
                            ok::<_,()>(8u32),
                            ok::<_,()>(9) ~=> |v| ok(v + 1)
                        } |> |v| v.1,
                        map => |a, b| b - a // 4
                    },
                    map => |a, b, c, d| a + b + c + d // 13
                },
                try_join_async!{
                    try_join_async! {
                        ok::<_,()>(21u32),
                        ok::<_,()>(22u32),
                        ok::<_,()>(23u32),
                        map => |a, b, c| a * b * c // 10626
                    },
                    ok(2u32),
                    and_then => |a, b| ok(a * b) // 21252
                },
                map => |a, b| a + b // 21265
            }
            .await
            .unwrap();

            assert_eq!(value, 21265);
        });
    }

    #[test]
    fn it_tests_readme_demo_async_behaviour_and_requires_internet_connection() {
        use ::failure::format_err;
        use ::futures::future::try_join_all;
        use ::futures::stream::{iter, Stream};
        use ::reqwest::Client;
        use join::try_join;

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

        async fn read_number_from_stdin() -> u16 {
            let mut input = iter(vec!["100"]);
            let result = try_join_async! {
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
            result.await.unwrap()
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
            
            let task = try_join_async! {
                // This programs makes requests to several sites 
                // and calculates count of links starting from `https://`
                get_urls_to_calculate_link_count() 
                    |> {
                        // If pass block statement instead of fn, it will be placed before current step, 
                        // so it will us allow to capture some variables from context
                        let ref client = client;
                        move |url| {
                            try_join_async! {
                                client
                                    .get(url)
                                    .send()
                                    => |value| value.text()
                                    => |body| ok((url, body.matches("https://").collect::<Vec<_>>().len()))
                            }
                        }
                    }
                    // Collect values into `Vec<_>`
                    =>[] Vec<_>
                    |> Ok
                    => try_join_all
                    !> |err| format_err!("Error retrieving pages to calculate links: {:#?}", err)
                    => |results| 
                        ok(
                            results
                                .into_iter()
                                .max_by_key(|(_, link_count)| link_count.clone())
                                .unwrap()
                        )
                    // It waits for input in stdin before log max links count
                    ~?? |result| { 
                        result
                            .as_ref()
                            .map(
                                |(url, count)| 
                                    println!("Max link count found on `{}`: {}", url, count)
                            )
                            .unwrap_or(()); 
                    },
                // Concurrently it makes request to the site which generates random number
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
                                                .map_err(map_parsing_error(value))
                                        )

                                
                            }
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
                println!("Please, enter number") -> |_| read_number_from_stdin() |> Ok,
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

            // Use sync join because we don't need concurrent execution
            // and sync `map` is more convenient
            let _ = try_join! {
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

    #[test]
    fn it_tests_initial_block_capture() {
        use std::sync::Arc;

        block_on(async {
            let out = Arc::new(5);
            let value = try_join_async! {
                let pat_1 = { let out = out.clone(); ok::<_,()>(*out) },
                let pat_2 = { ok::<_,()>(*out) },
                map => |a, _| a
            }
            .await
            .unwrap();

            assert_eq!(value, 5);
        });
    }
}
