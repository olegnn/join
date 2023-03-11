#[cfg(test)]
#[allow(clippy::unused_unit)]
mod join_tests {
    use join::{join, try_join};
    use std::{convert::identity, error::Error};

    type BoxedError = Box<dyn Error + Send + Sync>;

    type Result<T> = std::result::Result<T, BoxedError>;

    type _Result<T, E> = std::result::Result<T, E>;

    fn three() -> u16 {
        3
    }

    #[allow(clippy::unnecessary_wraps)]
    fn ok_four() -> Result<u16> {
        Ok(4)
    }

    #[allow(clippy::unnecessary_wraps)]
    fn some_five() -> Option<u16> {
        Some(5)
    }

    fn make_err() -> Result<u16> {
        Err("error".to_string().into())
    }

    fn none() -> Option<u16> {
        None
    }

    fn add_one(v: u16) -> u16 {
        v + 1
    }

    #[allow(clippy::unnecessary_wraps)]
    fn add_one_ok(v: u16) -> Result<u16> {
        Ok(add_one(v))
    }

    fn to_err(v: u16) -> Result<u16> {
        Err(v.to_string().into())
    }

    fn to_none(_: u16) -> Option<u16> {
        None
    }

    #[test]
    fn it_creates_nested_chains() {
        let value = try_join! {
            Ok::<_,u8>(Ok::<_,u8>(Some(2u16)))
            => >>>
                => >>>
                    |> |v| v + 2
                    ..ok_or(4)
                <<<
                |> |v| Ok::<_,u8>(v + 5)
            <<<
        };

        assert_eq!(value.unwrap(), Ok(9));

        let value = join! {
            Some(Some(Some(2u16)))
            |> >>>
                |> >>>
                    |> |v| v + 2
                    ..ok_or(4)
                    .unwrap()
                <<<
                |> |v| Some(v + 5)
            <<<
        };

        assert_eq!(value, Some(Some(Some(9))));

        let value = try_join! {
            Some(Some(Some(2u16)))
            => >>>
                => >>>
                    |> >>>
        };

        assert_eq!(value, Some(2));

        let value = try_join! {
            Some(Some(Some(2u16)))
            => >>>
                => >>>
                    |> >>>
                    <<<
                <<<
            <<<
        };

        assert_eq!(value, Some(2));

        let value = try_join! {
            Some(Some(Some(2u16)))
            => >>>
                => >>>
                    |> >>>
            ~=> |v| Some(v * 2)
        };

        assert_eq!(value, Some(4));

        let mut captured = Some(2);

        let value = try_join! {
            Ok::<_,u8>(Ok::<_,u8>(Some(2u16)))
            => >>>
                |> |v| { captured = None; v }
                => >>>
                    |> { let captured = *captured.as_ref().unwrap(); move |v| v + captured }
                    ..ok_or(4)
                <<<
                |> |v| Ok::<_,u8>(v + 5)
            <<<
        };

        assert_eq!(value.unwrap(), Ok(9));
    }

    #[test]
    fn it_produces_n_branches_with_length_1() {
        let product = try_join! {
            Ok(2u16),
            Ok(three()),
            ok_four(),
            some_five().ok_or_else(|| "error".into()),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product.unwrap(), 120);

        let err = try_join! {
            Ok(2),
            Ok(three()),
            ok_four(),
            make_err(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(
            err.unwrap_err().to_string(),
            make_err().unwrap_err().to_string()
        );

        let product = try_join! {
            Some(2),
            Some(three()),
            some_five(),
            ok_four().ok(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product, Some(120));

        let none = try_join! {
            Some(2),
            Some(three()),
            some_five(),
            none(),
            ok_four().ok(),
            map => |a, b, c, d, e| a * b * c * d * e
        };

        assert_eq!(none, None);
    }

    #[test]
    fn it_produces_n_branches_with_any_length() {
        let product = try_join! {
            Ok(2u16).map(add_one).and_then(add_one_ok), //4
            Ok(three()).and_then(add_one_ok).map(add_one).map(add_one).map(add_one), //7
            ok_four().map(add_one), //5
            some_five().map(add_one).ok_or_else(|| "error".into()).and_then(to_err).and_then(add_one_ok).or(Ok(5)), // 5
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product.unwrap(), 700);

        let err = try_join! {
            Ok(2).map(add_one),
            Ok(three()).and_then(to_err),
            ok_four().and_then(|_| -> Result<u16> { Err("ERROR".into()) }),
            make_err(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(
            err.unwrap_err().to_string(),
            to_err(three()).unwrap_err().to_string()
        );
    }

    #[test]
    fn it_produces_n_branches_with_any_length_using_combinators() {
        let product = try_join! {
            Ok(2u16) |> add_one => add_one_ok, //4
            Ok(three()) => add_one_ok |> add_one |> add_one |> add_one, //7
            ok_four() |> add_one, //5
            some_five() |> add_one ..ok_or_else(|| "error".into()) => to_err => add_one_ok <| Ok(5), // 5
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product.unwrap(), 700);

        let sum = try_join! {
            2u16 -> Ok |> add_one => add_one_ok, //4
            three() -> Ok => add_one_ok |> add_one |> add_one |> add_one, //7
            ok_four() |> add_one, //5
            some_five() |> add_one ..ok_or_else(|| "error".into()) => to_err => add_one_ok <| Ok(5), // 5
            and_then => |a, b, c, d| Ok(a + b + c + d)
        };

        assert_eq!(sum.unwrap(), 21);

        let err = try_join! {
            Ok(2) |> add_one,
            Ok(three()) => to_err,
            ok_four() => |_| -> Result<u16> { Err("ERROR!".into()) },
            make_err(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(
            err.unwrap_err().to_string(),
            to_err(three()).unwrap_err().to_string()
        );

        let none = try_join! {
            2 -> Some |> add_one,
            Some(three()) => to_none,
            ok_four() => |_| -> Result<u16> { Ok(10) } ..ok(),
            none(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(none, None);
    }

    #[test]
    fn it_tests_handler_behaviour() {
        let ok = try_join! {
            Ok(2),
            Ok(3),
            ok_four(),
            and_then => |_a, _b, _c| Ok::<Option<u16>, _>(None)
        };

        assert_eq!(ok.unwrap(), None);

        let err = try_join! {
            Ok(2),
            Ok(3),
            ok_four(),
            and_then => |a: u8, _b, _c| Err::<Option<u16>, _>(a.to_string().into())
        };

        assert_eq!(
            err.unwrap_err().to_string(),
            Err::<u8, BoxedError>("2".into()).unwrap_err().to_string()
        );

        let some = try_join! {
            Some(2),
            Some(3),
            some_five(),
            and_then => |a, _b, _c| Some(a)
        };

        assert_eq!(some, Some(2));

        let none = try_join! {
            Some(2),
            Some(3),
            some_five(),
            and_then => |_a, _b, _c| None::<u16>
        };

        assert_eq!(none, None);

        let some = try_join! {
            Some(2u16),
            Some(3u16),
            some_five(),
            map => |a, b, c| a + b + c
        };

        assert_eq!(some, Some(10));

        let none: Option<u16> = try_join! {
            Some(2u16),
            None,
            some_five(),
            and_then => |a: u16, b: u16, c: u16| -> Option<u16> { Some(a + b + c) }
        };

        assert_eq!(none, None);

        let then: Result<u16> = Ok(2);
        let map: Result<u16> = Ok(3);
        let and_then = ok_four;

        let ok = join! {
            then,
            map,
            and_then(),
            then => |a: Result<u16>, b: Result<u16>, c: Result<u16>| Ok::<_, u8>(a.unwrap() + b.unwrap() + c.unwrap())
        };

        assert_eq!(ok.unwrap(), 9);

        let err = join! {
            Ok(2),
            Ok(3),
            ok_four(),
            then => |a: Result<u16>, b: Result<u16>, c: Result<u16>| Err::<u16, _>(a.unwrap() + b.unwrap() + c.unwrap())
        };

        assert_eq!(err, Err(9));
    }

    #[test]
    fn it_tests_steps() {
        let product = try_join! {
            let branch_0 = Ok(2u16) ~|> {
                let branch_0 = branch_0.as_ref().ok().cloned();
                let branch_1 = branch_1.as_ref().ok().cloned();
                let branch_2 = branch_2.as_ref().ok().cloned();
                let branch_3 = branch_3.as_ref().ok().cloned();

                move |value| {
                    assert_eq!(branch_0.unwrap(), value);
                    assert_eq!(branch_1.unwrap(), 3);
                    assert_eq!(branch_2.unwrap(), 4);
                    assert_eq!(branch_3.unwrap(), 6);
                    add_one(value)
                }
            } ~=> add_one_ok, //4
            let branch_1 = Ok(three()) ~=> add_one_ok ~|> |value| value |> {
                let branch_0 = branch_0.as_ref().ok().cloned();
                let branch_1 = branch_1.as_ref().ok().cloned();
                let branch_2 = branch_2.as_ref().ok().cloned();
                let branch_3 = branch_3.as_ref().ok().cloned();

                move |value| {
                    assert_eq!(branch_0.unwrap(), 3);
                    assert_eq!(branch_1.unwrap(), value);
                    assert_eq!(branch_2.unwrap(), 5);
                    assert_eq!(branch_3.unwrap(), 5);
                    add_one(value)
                }
            } ~|> add_one ~|> add_one, //7
            let branch_2 = ok_four() ~|> add_one, //5
            let branch_3 = some_five() |> add_one ..ok_or_else(|| "error".into()) ~=> to_err <| Ok(5) ~=> add_one_ok, // 6
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product.unwrap(), 840);
    }

    #[test]
    fn it_produces_tuple() {
        let values = try_join! { Ok::<_,u8>(2), Ok::<_,u8>(3) };
        assert_eq!(values.unwrap(), (2, 3));
    }

    #[test]
    fn it_tests_multi_step_single_branch() {
        let values =
            try_join! { vec![1,2,3,4,5,6,7,8,9].into_iter() -> Some ~|> >>> ?> |v| v % 3 != 0 =>[] Vec<_> ~|> |v| v }
                .unwrap();
        assert_eq!(values, vec![1, 2, 4, 5, 7, 8]);
    }

    #[test]
    fn it_checks_evalutation_in_case_of_error() {
        let error = try_join! { Err::<u8,_>(2) ~|> |_| unreachable!(), Ok::<_,u8>(3) };
        assert_eq!(error, Err(2));
    }

    #[test]
    fn it_tests_iter_combinators() {
        let mut some_vec = Some(vec![0u8]);

        let values: (Vec<_>, Vec<_>) = join! {
            vec![2u8, 3, 4, 5, 6, 7, 8, 9, 10, 11].into_iter() |> |v| { some_vec = None; v + 1 } ?|> |v| if v % 2 == 0 { Some(v) } else { None } |n> ^@ { some_vec.clone() }, |mut acc, (index, v)| { acc.as_mut().unwrap().push(v + (index as u8)); acc } ..unwrap().into_iter() =>[] Vec<_> ~..into_iter() ?&!> |&n| (n as f64).cos().abs() > ::std::f64::consts::PI / 3f64
        };

        assert_eq!(values, (vec![], vec![0, 4, 7, 10, 13, 16]));

        let values = vec![0, 1u8, 2, 3, 4, 5, 6];
        let other_values = vec![4u8, 5, 6, 7, 8, 9, 10];

        assert_eq!(
            join! {
                values.iter() >^> other_values.iter() ?&!> |&(&v, &v1)| v % 2 == 0 && v1 % 2 == 0
            },
            (
                vec![(&0u8, &4u8), (&2, &6), (&4, &8), (&6, &10)],
                vec![(&1u8, &5u8), (&3, &7), (&5, &9)]
            )
        );

        assert_eq!(
            join! {
                values.iter() >^> other_values.iter() <-> &u8, &u8, Vec<_>, Vec<_>
            },
            (values.iter().collect(), other_values.iter().collect())
        );

        assert_eq!(
            join! { vec![1u8, 2, 3, 4, 5].into_iter() ?> |v| v % 2 != 0 =>[] Vec<_> },
            vec![1u8, 3, 5]
        );

        assert_eq!(
            try_join! { let v = vec![1, 2, 3, 4, 5] -> Some ~=> >>> ..into_iter() ?|>@ |v| if v % 2 == 0 { Some(v) } else { None } },
            Some(2)
        );

        assert_eq!(
            join! { vec![vec![1, 2, 3], vec![2]].into_iter() ^^> =>[] Vec<_> },
            vec![1, 2, 3, 2]
        );

        assert!(
            try_join! { vec![Ok(5), Err(4)].into_iter() ?^@ 0, |acc, v| v.map(|v| acc + v) }
                .is_err()
        );
    }

    #[test]
    fn it_produces_single_value() {
        let value = try_join! { Some(1) };
        assert_eq!(value.unwrap(), 1);
    }

    #[test]
    fn it_tests_step_expr_with_large_indices() {
        let value = try_join! {
            Some(1),
            Some(2i32) |> identity |> identity |> identity |> identity |> identity |> identity |> identity |> identity |> identity |> identity |> {
                let b = 5;

                move |opt| opt - b
            },
            Some(3),
            Some(4),
            Some(5),
            Some(6),
            Some(7),
            Some(8),
            Some(9),
            Some(10),
            Some(11),
            Some(12) |> {
                let a = 1;

                move |opt| opt + a
            },
            map => |_, f, _, _, _, _, _, _, _, _, _, val| f + val
        };
        assert_eq!(value.unwrap(), 10);
    }

    #[test]
    fn it_tests_filter() {
        let value = try_join! { vec![1,2,3,4].into_iter() ?> |&value| value % 2 == 0 -> Some };
        assert_eq!(value.unwrap().collect::<Vec<_>>(), vec![2, 4]);
    }

    #[test]
    fn it_tests_nested_macro_combinations() {
        use futures::executor::block_on;
        use futures::future::*;
        use join::*;

        block_on(async {
            let value = try_join! {
                try_join! {
                    Ok::<_,u8>(2u32),
                    Ok::<_,u8>(3u32),
                    Ok::<_,u8>(4u32),
                    try_join! {
                        Ok::<_,u8>(6u32),
                        join! {
                            Ok::<_,u8>(8u32),
                            Ok::<_,u8>(9) ~|> |v| v + 1
                        }.1,
                        map => |a, b| b - a // 4
                    },
                    map => |a, b, c, d| a + b + c + d // 13
                },
                try_join!{
                    try_join_async! {
                        ok::<_,u8>(21u32),
                        ok::<_,u8>(22u32),
                        ok::<_,u8>(23u32),
                        map => |a, b, c| a * b * c // 10626
                    }.await,
                    Ok(2u32),
                    and_then => |a, b| Ok(a * b) // 21252
                },
                map => |a, b| a + b // 21265
            }
            .unwrap();

            assert_eq!(value, 21265);
        });
    }

    #[test]
    fn it_tests_initial_block_capture() {
        use std::sync::Arc;
        let out = Arc::new(5);
        let value = try_join! {
            let pat_1 = { let out = out.clone(); Some(*out) },
            let pat_2 = { Some(*out) },
            map => |a, _| a
        }
        .unwrap();

        assert_eq!(value, 5);
    }
}
