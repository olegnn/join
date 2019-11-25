#![recursion_limit = "1024"]

#[cfg(test)]
mod join_async_spawn_tests {
    use futures::future::{err, ok, ready};
    use futures_timer::Delay;
    use join::{join_async_spawn, try_join_async_spawn};
    use std::error::Error;
    use std::time::Duration;
    use tokio::runtime::Runtime;

    type BoxedError = Box<dyn Error + Send + Sync>;

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
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let product = try_join_async_spawn! {
                ok(2u16),
                ok(get_three().await),
                get_ok_four(),
                ok(get_ok_five().await.unwrap()),
                and_then => |a, b, c, d| ok(a * b * c * d)
            };

            assert_eq!(product.await.unwrap(), 120u16);

            let err = try_join_async_spawn! {
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
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let product = try_join_async_spawn! {
                ok(2u16).map(add_one_to_ok).and_then(add_one_ok), //4
                ok(get_three().await).and_then(add_one_ok).map(add_one_to_ok).map(add_one_to_ok).map(add_one_to_ok), //7
                get_ok_four().map(add_one_to_ok), //5
                get_ok_five().and_then(to_err).and_then(add_one_ok).or_else(|_| ok(5)), // 5
                and_then => |a, b, c, d| ok(a * b * c * d)
            };

            assert_eq!(product.await.unwrap(), 700u16);

            let err = try_join_async_spawn! {
                ok(2).map(add_one_to_ok),
                ok(get_three().await).and_then(to_err),
                get_ok_four(),
                ok(2) ~=> |_| get_err(),
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
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let product = try_join_async_spawn! {
                ok(2u16) |> add_one_to_ok => add_one_ok, //4
                ok(get_three().await) => add_one_ok |> add_one_to_ok |> add_one_to_ok |> add_one_to_ok, //7
                get_ok_four() |> add_one_to_ok, //5
                get_ok_five() |> add_one_to_ok => to_err => add_one_ok <= |_| ok(5), // 5
                map => |a, b, c, d| a * b * c * d
            };

            assert_eq!(product.await.unwrap(), 700);

            let sum = try_join_async_spawn! {
                2u16 -> ok |> add_one_to_ok => add_one_ok, //4
                get_three().await -> ok => add_one_ok |> add_one_to_ok |> add_one_to_ok |> add_one_to_ok, //7
                get_ok_four() |> add_one_to_ok, //5
                get_ok_five() |> add_one_to_ok => to_err => add_one_ok <= |_| ok(5), // 5
                and_then => |a, b, c, d| ok(a + b + c + d)
            };

            assert_eq!(sum.await.unwrap(), 21);

            let error: Result<u16> = try_join_async_spawn! {
                ok(2) |> add_one_to_ok,
                ok(get_three().await) => to_err,
                get_ok_four() => |_| err("some error".into()),
                get_err(),
                map => |a: u16, b: u16, c: u16, d: u16| a * b * c * d
            }
            .await;

            assert!(error.is_err());

            let error = try_join_async_spawn! {
                2 -> ok,
                get_ok_five() ~=> |_| err("some error".into()),
                get_ok_four(),
                get_err(),
                map => |a: u16, b: u16, c: u16, d: u16| a * b * c * d
            };

            assert_eq!(
                format!("{:#?}", error.await.unwrap_err()),
                format!("{:#?}", Box::new("error"))
            );
        });
    }

    #[allow(unreachable_code)]
    #[test]
    fn it_checks_evalutation_in_case_of_error() {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let error =
                try_join_async_spawn! { err::<u8,u8>(2u8) ~=> |_| { unreachable!(); ok(2u8) }, ok::<u8,u8>(3u8) };
            assert_eq!(error.await, Err(2u8));
        });
    }

    #[test]
    fn it_tests_handler_behaviour() {
        let rt = Runtime::new().unwrap();
        rt.block_on(
            async {
                let ok_value = try_join_async_spawn! {
                    ok(2u16),
                    ok(3u16),
                    get_ok_four(),
                    and_then => |a, b, c| ok::<Option<u16>, _>(None)
                };

                assert_eq!(ok_value.await.unwrap(), None);

                let err_value = try_join_async_spawn! {
                    ok(2u16),
                    ok(3u16),
                    get_ok_four(),
                    and_then => |a, b, c| to_err(a)
                };

                assert_eq!(format!("{:?}", err_value.await.unwrap_err()), format!("{:?}", to_err(2).await.unwrap_err()));

                let some = try_join_async_spawn! {
                    ready(Ok(2u16)),
                    ready(Ok(3u16)),
                    get_ok_five(),
                    map => |a, b, c| a
                };

                assert_eq!(some.await.unwrap(), 2u16);

                let okay: Result<u16> = join_async_spawn! {
                    ready(Ok::<_,BoxedError>(2u16)),
                    ready(Ok::<_,BoxedError>(3u16)),
                    err::<u16,BoxedError>("25".into()),
                    then => |a, b, c| ready(a)
                }.await;

                assert!(okay.is_ok());

                let okay = try_join_async_spawn! {
                    ready(Ok(2u16)),
                    ready(Ok(3u16)),
                    get_ok_five(),
                    map => |a, b, c| a + b + c
                };

                assert_eq!(okay.await.unwrap(), 10);

                let ok_value= join_async_spawn! {
                    ok(2u16),
                    ok(3u16),
                    get_ok_four(),
                    then => |a: Result<u16>, b: Result<u16>, c: Result<u16>| ok::<_, u8>(a.unwrap() + b.unwrap() + c.unwrap())
                };

                assert_eq!(ok_value.await.unwrap(), 9u16);

                let err_value = join_async_spawn! {
                    ok(2u16),
                    ok(3u16),
                    get_ok_four(),
                    then => |a: Result<u16>, b: Result<u16>, c: Result<u16>| err::<u16, _>(a.unwrap() + b.unwrap() + c.unwrap())
                };

                assert_eq!(err_value.await, Err(9));
            }
        );
    }

    #[test]
    fn it_tests_steps() {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let product = try_join_async_spawn! {
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
                        assert_eq!(branch_3, Some(6));
                        value.map(add_one_sync)
                    }
                } ~|> add_one_to_ok ~|> add_one_to_ok, //7
                let branch_2 = get_ok_four() ~|> add_one_to_ok, //5
                let branch_3 = get_ok_five() ~|> add_one_to_ok ~=> to_err <= |_| ok(5) ~=> add_one_ok, // 6
                map => |a, b, c, d| a * b * c * d
            };

            assert_eq!(product.await.unwrap(), 840);
        });
    }

    #[test]
    fn it_tests_nested_macro_combinations() {
        use join::*;
        let rt = tokio::runtime::Builder::new()
            .core_threads(4)
            .build()
            .unwrap();

        rt.block_on(async {
            let value = try_join_async! {
                try_join_async_spawn! {
                    ok::<_,u8>(2u32),
                    ok::<_,u8>(3u32),
                    ok::<_,u8>(4u32),
                    ok::<_,u8>(4u32),
                    map => |a, b, c, d| a + b + c + d // 13
                },
                try_join_async!{
                    try_join_async! {
                        ok::<_,u8>(21u32),
                        ok::<_,u8>(22u32),
                        ok::<_,u8>(23u32),
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
    fn it_checks_concurrent_branches_execution() {
        let rt = tokio::runtime::Builder::new()
            .core_threads(4)
            .build()
            .unwrap();
        rt.block_on(async {
            use futures::lock::Mutex;
            use std::sync::Arc;

            let values = Arc::new(Mutex::new(Vec::new()));

            let _ = join_async_spawn! {
                ok((values.clone(), 1u16)) => |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(1)).await;
                    {
                        let mut values = values.lock().await;
                        values.sort();
                        assert_eq!(values[..], [1, 2, 3]);
                        values.pop();
                    }
                    Ok::<_, BoxedError>((values, value + 1))
                } ~=> |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(1)).await;
                    let mut values = values.lock().await;
                    values.sort();
                    assert_eq!(values[..], [2, 3, 4]);
                    Ok::<_, BoxedError>(())
                },
                ok((values.clone(), 2u16)) => |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(2)).await;
                    {
                        let mut values = values.lock().await;
                        values.sort();
                        assert_eq!(values[..], [1, 2]);
                        values.pop();
                    }
                    Ok::<_, BoxedError>((values, value + 1))
                } ~=> |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(2)).await;
                    let mut values = values.lock().await;
                    values.sort();
                    assert_eq!(values[..], [2, 3, 4]);
                    Ok::<_, BoxedError>(())
                },
                ok((values.clone(), 3u16)) => |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(3)).await;
                    {
                        let mut values = values.lock().await;
                        values.sort();
                        assert_eq!(values[..], [1]);
                        values.pop();
                    }
                    Ok::<_, BoxedError>((values, value + 1))
                } ~=> |(values, value)| async move {
                    values.lock().await.push(value);
                    Delay::new(Duration::from_secs(3)).await;
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
    fn it_produces_tuple() {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let values = try_join_async_spawn! { ok::<_,u8>(2), ok::<_,u8>(3) }.await;
            assert_eq!(values.unwrap(), (2, 3));
        });
    }

    #[test]
    fn it_produces_single_value() {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let value = try_join_async_spawn! { ready(Ok::<_,u8>(1)) }.await;
            assert_eq!(value.unwrap(), 1);
        });
    }

    #[allow(unreachable_code)]
    #[test]
    fn it_creates_unpolled_future() {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let _fut = join_async_spawn! {
                ready(panic!()),
                ready(unreachable!()),
                then => |a: Result<u8>, b: Result<u8>| ready(a)
            };
        });
    }

    #[test]
    fn it_cant_work_without_tokio() {
        assert!(
            ::std::panic::catch_unwind(|| ::futures::executor::block_on(async {
                let failure = try_join_async_spawn! { ::futures::future::ready(Ok::<_,u8>(2u16)) };
                failure.await
            }))
            .is_err()
        );
    }

    #[test]
    fn it_tests_multi_step_single_branch() {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let values = try_join_async_spawn! { vec![1u8,2,3,4,5,6,7,8,9].into_iter() -> ok ~=> >>> ?> |v| v % 3 != 0 =>[] Vec<_> -> ok::<_,u8> ~|> |v| v ~=> |v| ok(v) }.await.unwrap();
            assert_eq!(values, vec![1u8, 2, 4, 5, 7, 8]);
        });
    }

    #[test]
    fn it_tests_iter_combinators() {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let mut some_vec = Some(vec![0u8]);

            let values: (Vec<u8>, Vec<u8>) = join_async_spawn! {
                vec![2u8, 3, 4, 5, 6, 7, 8, 9, 10, 11].into_iter() |> |v| { some_vec = None; v + 1 } ?|> |v| if v % 2 == 0 { Some(v) } else { None } |n> ^@ { some_vec.clone() }, |mut acc, (index, v)| { acc.as_mut().unwrap().push(v + (index as u8)); acc } ..unwrap().into_iter() =>[] Vec<_> ..into_iter() ?&!> |&n| (n as f64).cos().abs() > ::std::f64::consts::PI / 3f64 -> ready
            }.await;

            assert_eq!(values, (vec![], vec![0u8, 4, 7, 10, 13, 16]));

            let values = vec![0, 1u8, 2, 3, 4, 5, 6];
            let other_values = vec![4u8, 5, 6, 7, 8, 9, 10];

             assert_eq!(
                join_async_spawn! {
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
                join_async_spawn! {
                    { let values = values.clone(); values.into_iter() } >^> { let other_values = other_values.clone(); other_values.into_iter() } <-> u8, u8, Vec<_>, Vec<_> -> ready
                }.await,
                {  
                    let values = vec![0, 1u8, 2, 3, 4, 5, 6];
                    let other_values = vec![4u8, 5, 6, 7, 8, 9, 10]; 
                    (values, other_values)
                }
            );


            assert_eq!(
                join_async_spawn! { vec![1u8, 2, 3, 4, 5].into_iter() ?> |v| v % 2 != 0 =>[] Vec<_> -> ready }.await,
                vec![1u8, 3, 5]
            );

            assert_eq!(
                try_join_async_spawn! { let mut v = vec![1u8, 2, 3, 4, 5] ..into_iter() ?|>@ |v: u8| if v % 2 == 0 { Some(v) } else { None } -> |v: Option<u8>| ready(v.ok_or(Err::<u8,&'static str>("e"))) }.await,
                Ok(2u8)
            );

            assert_eq!(
                join_async_spawn! { vec![vec![1u8, 2, 3], vec![2]].into_iter() ^^> =>[] Vec<_> -> ready }.await,
                vec![1u8, 2, 3, 2]
            );

            assert!(
                try_join_async_spawn! { vec![Ok(5u8), Err(4u8)].into_iter() ?^@ 0u8, |acc, v| v.map(|v| acc + v) -> ready }.await.is_err()
            );
        });
    }

    #[test]
    fn it_tests_initial_block_capture() {
        use std::sync::Arc;

        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let out = Arc::new(5);
            let value = try_join_async_spawn! {
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
