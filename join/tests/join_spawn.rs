#![recursion_limit = "512"]

#[cfg(test)]
mod join_spawn_tests {
    use join::join_spawn;

    type Result<T> = std::result::Result<T, u8>;

    type _Result<T, E> = std::result::Result<T, E>;

    fn get_three() -> u16 {
        3
    }

    fn get_ok_four() -> Result<u16> {
        Ok(4)
    }

    fn get_some_five() -> Option<u16> {
        Some(5)
    }

    fn get_err() -> Result<u16> {
        Err(6)
    }

    fn get_none() -> Option<u16> {
        None
    }

    fn add_one(v: u16) -> u16 {
        v + 1
    }

    fn add_one_ok(v: u16) -> Result<u16> {
        Ok(add_one(v))
    }

    fn to_err(v: u16) -> Result<u16> {
        Err(v as u8)
    }

    fn to_none(_: u16) -> Option<u16> {
        None
    }

    #[test]
    fn it_produces_n_branches_with_length_1() {
        let product = join_spawn! {
            Ok(2u16),
            Ok(get_three()),
            get_ok_four(),
            get_some_five().ok_or(2),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product, Ok(120));

        let err = join_spawn! {
            Ok(2),
            Ok(get_three()),
            get_ok_four(),
            get_err(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(err, get_err());

        let product = join_spawn! {
            Some(2),
            Some(get_three()),
            get_some_five(),
            get_ok_four().ok(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product, Some(120));

        let none = join_spawn! {
            Some(2),
            Some(get_three()),
            get_some_five(),
            get_none(),
            get_ok_four().ok(),
            map => |a, b, c, d, e| a * b * c * d * e
        };

        assert_eq!(none, None);
    }

    #[test]
    fn it_produces_n_branches_with_any_length() {
        let product = join_spawn! {
            Ok(2u16).map(add_one).and_then(add_one_ok), //4
            Ok(get_three()).and_then(add_one_ok).map(add_one).map(add_one).map(add_one), //7
            get_ok_four().map(add_one), //5
            get_some_five().map(add_one).ok_or(2).and_then(to_err).and_then(add_one_ok).or(Ok(5)), // 5
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product, Ok(700));

        let err = join_spawn! {
            Ok(2).map(add_one),
            Ok(get_three()).and_then(to_err),
            get_ok_four().and_then(|_| -> Result<u16> { Err(10) }),
            get_err(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(err, to_err(get_three()));
    }

    #[test]
    fn it_produces_n_branches_with_any_length_using_combinators() {
        let product = join_spawn! {
            Ok(2u16) |> add_one => add_one_ok, //4
            Ok(get_three()) => add_one_ok |> add_one |> add_one |> add_one, //7
            get_ok_four() |> add_one, //5
            get_some_five() |> add_one >.ok_or(2) => to_err => add_one_ok <| Ok(5), // 5
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product, Ok(700));

        let sum = join_spawn! {
            2u16 -> Ok |> add_one => add_one_ok, //4
            get_three() -> Ok => add_one_ok |> add_one |> add_one |> add_one, //7
            get_ok_four() |> add_one, //5
            get_some_five() |> add_one >.ok_or(2) => to_err => add_one_ok <| Ok(5), // 5
            and_then => |a, b, c, d| Ok(a + b + c + d)
        };

        assert_eq!(sum, Ok(21));

        let err = join_spawn! {
            Ok(2) |> add_one,
            Ok(get_three()) => to_err,
            get_ok_four() => |_| -> Result<u16> { Err(10) },
            get_err(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(err, to_err(get_three()));

        let none = join_spawn! {
            2 -> Some |> add_one,
            Some(get_three()) => to_none,
            get_ok_four() => |_| -> Result<u16> { Ok(10) } >.ok(),
            get_none(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(none, None);
    }

    #[test]
    fn it_tests_handler_behaviour() {
        let ok = join_spawn! {
            Ok(2),
            Ok(3),
            get_ok_four(),
            and_then => |a, b, c| Ok::<Option<u16>, _>(None)
        };

        assert_eq!(ok, Ok(None));

        let err = join_spawn! {
            Ok(2),
            Ok(3),
            get_ok_four(),
            and_then => |a, b, c| Err::<Option<u16>, _>(a)
        };

        assert_eq!(err, Err(2));

        let some = join_spawn! {
            Some(2),
            Some(3),
            get_some_five(),
            and_then => |a, b, c| Some(a)
        };

        assert_eq!(some, Some(2));

        let none = join_spawn! {
            Some(2),
            Some(3),
            get_some_five(),
            and_then => |a, b, c| None::<u16>
        };

        assert_eq!(none, None);

        let some = join_spawn! {
            Some(2u16),
            Some(3u16),
            get_some_five(),
            map => |a, b, c| a + b + c
        };

        assert_eq!(some, Some(10));

        let none: Option<u16> = join_spawn! {
            Some(2u16),
            None,
            get_some_five(),
            and_then => |a: u16, b: u16, c: u16| -> Option<u16> { Some(a + b + c) }
        };

        assert_eq!(none, None);

        let ok = join_spawn! {
            Ok(2),
            Ok(3),
            get_ok_four(),
            then => |a: Result<u16>, b: Result<u16>, c: Result<u16>| Ok::<_, u8>(a.unwrap() + b.unwrap() + c.unwrap())
        };

        assert_eq!(ok, Ok(9));

        let err = join_spawn! {
            Ok(2),
            Ok(3),
            get_ok_four(),
            then => |a: Result<u16>, b: Result<u16>, c: Result<u16>| Err::<u16, _>(a.unwrap() + b.unwrap() + c.unwrap())
        };

        assert_eq!(err, Err(9));
    }

    #[test]
    fn it_tests_steps() {
        let product = join_spawn! {
            let branch_0 = Ok(2u16) ~|> move |value| {
                assert_eq!(branch_0, Ok(value));
                assert_eq!(branch_1, Ok(3));
                assert_eq!(branch_2, Ok(4));
                assert_eq!(branch_3, Some(5));
                add_one(value)
            } ~=> add_one_ok, //4
            let branch_1 = Ok(get_three()) ~=> add_one_ok ~|> move |value| {
                assert_eq!(branch_0, Ok(3));
                assert_eq!(branch_1, Ok(value));
                assert_eq!(branch_2, Ok(5));
                assert_eq!(branch_3, Ok(6));
                add_one(value)
            } ~|> add_one ~|> add_one, //7
            let branch_2 = get_ok_four() ~|> add_one, //5
            let branch_3 = get_some_five() ~|> add_one >.ok_or(2) ~=> to_err <| Ok(5) ~=> add_one_ok, // 6
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product, Ok(840));
    }

    #[test]
    fn it_checks_mutli_threading() {
        use std::sync::{Arc, Mutex};
        use std::thread;
        use std::time::Duration;

        let values = Arc::new(Mutex::new(Vec::new()));

        let (values0, values1, values2) = (values.clone(), values.clone(), values.clone());

        let _ = join_spawn! {
            Ok::<_,u8>((values0, 1)) |> |(values, value)| {
                values.lock().unwrap().push(value);
                thread::sleep(Duration::from_secs(1));
                let mut values = values.lock().unwrap();
                values.sort();
                assert_eq!(values[..], [1, 2, 3]);
                values.pop();
            },
            Ok::<_,u8>((values1, 2)) |> |(values, value)| {
                values.lock().unwrap().push(value);
                thread::sleep(Duration::from_secs(2));
                let mut values = values.lock().unwrap();
                values.sort();
                assert_eq!(values[..], [1, 2]);
                values.pop();
            },
            Ok::<_,u8>((values2, 3)) |> |(values, value)| {
                values.lock().unwrap().push(value);
                thread::sleep(Duration::from_secs(3));
                let mut values = values.lock().unwrap();
                values.sort();
                assert_eq!(values[..], [1]);
                values.pop();
            },
            then => move |_, _, _| {
                assert_eq!(values.lock().unwrap().len(), 0);
                Ok::<_,u8>(())
            }
        }
        .unwrap();
    }

    #[test]
    fn it_checks_multi_threading_with_non_copiable_results() {
        use std::error::Error;

        type BoxedError = Box<dyn Error + Send + Sync>;

        fn add_one_ok(v: u16) -> std::result::Result<u16, BoxedError> {
            Ok(add_one(v))
        }

        let product = join_spawn! {
            let branch_0 = Ok::<_, BoxedError>(2u16) ~|> {
                let branch_0 = branch_0.as_ref().ok().map(Clone::clone);
                let branch_1 = branch_1.as_ref().ok().map(Clone::clone);
                let branch_2 = branch_2.as_ref().ok().map(Clone::clone);
                let branch_3 = branch_3.as_ref().map(Clone::clone);
                move |value| {
                    assert_eq!(branch_0, Some(value));
                    assert_eq!(branch_1, Some(3));
                    assert_eq!(branch_2, Some(4));
                    assert_eq!(branch_3, Some(5));
                    add_one(value)
                }
            } ~=> add_one_ok, //4
            let branch_1 = Ok::<_, BoxedError>(get_three()) ~=> add_one_ok ~|> |value| value |> {
                let branch_0 = branch_0.as_ref().ok().map(Clone::clone);
                let branch_1 = branch_1.as_ref().ok().map(Clone::clone);
                let branch_2 = branch_2.as_ref().ok().map(Clone::clone);
                let branch_3 = branch_3.as_ref().ok().map(Clone::clone);
                move |value| {
                    assert_eq!(branch_0, Some(3));
                    assert_eq!(branch_1, Some(value));
                    assert_eq!(branch_2, Some(5));
                    assert_eq!(branch_3, Some(6));
                    add_one(value)
                }
            }  ~|> add_one ~|> add_one, //7
            let branch_2 = Ok::<_, BoxedError>(4u16) ~|> add_one, //5
            let branch_3 = get_some_five() ~|> add_one >.ok_or(2) ~=> to_err <| Ok(5) ~=> add_one_ok, // 6
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product.unwrap(), 840);
    }

    #[test]
    fn it_produces_tuple() {
        let values = join_spawn! { Ok::<_,u8>(2), Ok::<_,u8>(3) };
        assert_eq!(values.unwrap(), (2, 3));
    }

    #[test]
    fn it_produces_single_value() {
        let value = join_spawn! { Some(1) };
        assert_eq!(value.unwrap(), 1);
    }
}
