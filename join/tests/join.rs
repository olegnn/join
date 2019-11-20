#![recursion_limit = "256"]

#[cfg(test)]
mod join_tests {
    use join::{join, try_join};

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

        assert_eq!(value, Ok(Ok(9)));

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
                    |> { let captured = captured.as_ref().unwrap().clone(); move |v| v + captured }
                    ..ok_or(4)
                <<<
                |> |v| Ok::<_,u8>(v + 5)
            <<<
        };

        assert_eq!(value, Ok(Ok(9)));
    }

    #[test]
    fn it_produces_n_branches_with_length_1() {
        let product = try_join! {
            Ok(2u16),
            Ok(get_three()),
            get_ok_four(),
            get_some_five().ok_or(2),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product, Ok(120));

        let err = try_join! {
            Ok(2),
            Ok(get_three()),
            get_ok_four(),
            get_err(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(err, get_err());

        let product = try_join! {
            Some(2),
            Some(get_three()),
            get_some_five(),
            get_ok_four().ok(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product, Some(120));

        let none = try_join! {
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
        let product = try_join! {
            Ok(2u16).map(add_one).and_then(add_one_ok), //4
            Ok(get_three()).and_then(add_one_ok).map(add_one).map(add_one).map(add_one), //7
            get_ok_four().map(add_one), //5
            get_some_five().map(add_one).ok_or(2).and_then(to_err).and_then(add_one_ok).or(Ok(5)), // 5
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product, Ok(700));

        let err = try_join! {
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
        let product = try_join! {
            Ok(2u16) |> add_one => add_one_ok, //4
            Ok(get_three()) => add_one_ok |> add_one |> add_one |> add_one, //7
            get_ok_four() |> add_one, //5
            get_some_five() |> add_one ..ok_or(2) => to_err => add_one_ok <| Ok(5), // 5
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product, Ok(700));

        let sum = try_join! {
            2u16 -> Ok |> add_one => add_one_ok, //4
            get_three() -> Ok => add_one_ok |> add_one |> add_one |> add_one, //7
            get_ok_four() |> add_one, //5
            get_some_five() |> add_one ..ok_or(2) => to_err => add_one_ok <| Ok(5), // 5
            and_then => |a, b, c, d| Ok(a + b + c + d)
        };

        assert_eq!(sum, Ok(21));

        let err = try_join! {
            Ok(2) |> add_one,
            Ok(get_three()) => to_err,
            get_ok_four() => |_| -> Result<u16> { Err(10) },
            get_err(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(err, to_err(get_three()));

        let none = try_join! {
            2 -> Some |> add_one,
            Some(get_three()) => to_none,
            get_ok_four() => |_| -> Result<u16> { Ok(10) } ..ok(),
            get_none(),
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(none, None);
    }

    #[test]
    fn it_tests_handler_behaviour() {
        let ok = try_join! {
            Ok(2),
            Ok(3),
            get_ok_four(),
            and_then => |a, b, c| Ok::<Option<u16>, _>(None)
        };

        assert_eq!(ok, Ok(None));

        let err = try_join! {
            Ok(2),
            Ok(3),
            get_ok_four(),
            and_then => |a, b, c| Err::<Option<u16>, _>(a)
        };

        assert_eq!(err, Err(2));

        let some = try_join! {
            Some(2),
            Some(3),
            get_some_five(),
            and_then => |a, b, c| Some(a)
        };

        assert_eq!(some, Some(2));

        let none = try_join! {
            Some(2),
            Some(3),
            get_some_five(),
            and_then => |a, b, c| None::<u16>
        };

        assert_eq!(none, None);

        let some = try_join! {
            Some(2u16),
            Some(3u16),
            get_some_five(),
            map => |a, b, c| a + b + c
        };

        assert_eq!(some, Some(10));

        let none: Option<u16> = try_join! {
            Some(2u16),
            None,
            get_some_five(),
            and_then => |a: u16, b: u16, c: u16| -> Option<u16> { Some(a + b + c) }
        };

        assert_eq!(none, None);

        let then: Result<u16> = Ok(2);
        let map: Result<u16> = Ok(3);
        let and_then = get_ok_four;

        let ok = join! {
            then,
            map,
            and_then(),
            then => |a: Result<u16>, b: Result<u16>, c: Result<u16>| Ok::<_, u8>(a.unwrap() + b.unwrap() + c.unwrap())
        };

        assert_eq!(ok, Ok(9));

        let err = join! {
            Ok(2),
            Ok(3),
            get_ok_four(),
            then => |a: Result<u16>, b: Result<u16>, c: Result<u16>| Err::<u16, _>(a.unwrap() + b.unwrap() + c.unwrap())
        };

        assert_eq!(err, Err(9));
    }

    #[test]
    fn it_tests_steps() {
        let product = try_join! {
            let branch_0 = Ok(2u16) ~|> |value| {
                assert_eq!(branch_0, Ok(value));
                assert_eq!(branch_1, Ok(3));
                assert_eq!(branch_2, Ok(4));
                assert_eq!(branch_3, Ok(6));
                add_one(value)
            } ~=> add_one_ok, //4
            let branch_1 = Ok(get_three()) ~=> add_one_ok ~|> |value| value |> |value| {
                assert_eq!(branch_0, Ok(3));
                assert_eq!(branch_1, Ok(value));
                assert_eq!(branch_2, Ok(5));
                assert_eq!(branch_3, Ok(5));
                add_one(value)
            } ~|> add_one ~|> add_one, //7
            let branch_2 = get_ok_four() ~|> add_one, //5
            let branch_3 = get_some_five() |> add_one ..ok_or(2) ~=> to_err <| Ok(5) ~=> add_one_ok, // 6
            map => |a, b, c, d| a * b * c * d
        };

        assert_eq!(product, Ok(840));
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
            vec![2u8, 3, 4, 5, 6, 7, 8, 9, 10, 11].into_iter() |> |v| { some_vec = None; v + 1 } ?|> |v| if v % 2 == 0 { Some(v) } else { None } |n> ^@ { some_vec.clone() }, |mut acc, (index, v)| { acc.as_mut().unwrap().push(v + (index as u8)); acc } ..unwrap().into_iter() =>[] Vec<_> ..into_iter() ?&!> |&n| (n as f64).cos().abs() > ::std::f64::consts::PI / 3f64
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
            try_join! { let mut v = vec![1, 2, 3, 4, 5] -> Some ~=> >>> ..into_iter() ?|>@ |v| if v % 2 == 0 { Some(v) } else { None } },
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
    fn it_tests_filter() {
        let value = try_join! { vec![1,2,3,4].into_iter() ?> |&value| value % 2 == 0 -> Some };
        assert_eq!(value.unwrap().collect::<Vec<_>>(), vec![2, 4]);
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
