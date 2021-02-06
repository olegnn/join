#[macro_export]
macro_rules! parse_n_or_empty_unit_fn_signature {
    (0, true, $fn_name: ident) => {
        pub fn $fn_name<
            ResultExpr: Into<$crate::chain::expr::ActionExpr> + Clone + std::fmt::Debug,
        >(
            result_expr: ResultExpr,
            unit_parser: &impl $crate::parse::unit::ParseUnit<$crate::chain::group::ActionGroup>,
            action_group: &$crate::chain::group::ActionGroup,
            input: $crate::syn::parse::ParseStream<'_>,
        ) -> $crate::parse::unit::UnitResult<
            $crate::chain::group::ExprGroup<$crate::chain::expr::ActionExpr>,
            $crate::chain::group::ActionGroup,
        > {
            $crate::parse_n_or_empty_unit_fn_body!(
                0,
                true,
                $crate::parse::Empty,
                ResultExpr,
                |_| result_expr,
                unit_parser,
                action_group,
                input
            )
        }
    };
    ($unit_count: tt, false, $fn_name: ident) => {
        pub fn $fn_name<
            P: $crate::syn::parse::Parse + Clone + std::fmt::Debug,
            ResultExpr: Into<$crate::chain::expr::ActionExpr> + Clone + std::fmt::Debug,
        >(
            into_expr: impl FnOnce([P; $unit_count]) -> ResultExpr,
            unit_parser: &impl $crate::parse::unit::ParseUnit<$crate::chain::group::ActionGroup>,
            action_group: &$crate::chain::group::ActionGroup,
            input: $crate::syn::parse::ParseStream<'_>,
        ) -> $crate::parse::unit::UnitResult<
            $crate::chain::group::ExprGroup<$crate::chain::expr::ActionExpr>,
            $crate::chain::group::ActionGroup,
        > {
            use std::convert::TryInto;
            $crate::parse_n_or_empty_unit_fn_body!(
                $unit_count,
                false,
                P,
                ResultExpr,
                |val: Option<Vec<P>>| into_expr(
                    val.and_then(|v| v.try_into().ok())
                        .expect("join: Invalid parsed unit count. This's a bug, please report it.")
                ),
                unit_parser,
                action_group,
                input
            )
        }
    };
    ($unit_count: tt, true, $fn_name: ident) => {
        pub fn $fn_name<
            P: $crate::syn::parse::Parse + Clone + std::fmt::Debug,
            ResultExpr: Into<$crate::chain::expr::ActionExpr> + Clone + std::fmt::Debug,
        >(
            into_expr: impl FnOnce(Option<[P; $unit_count]>) -> ResultExpr,
            unit_parser: &impl $crate::parse::unit::ParseUnit<$crate::chain::group::ActionGroup>,
            action_group: &$crate::chain::group::ActionGroup,
            input: $crate::syn::parse::ParseStream<'_>,
        ) -> $crate::parse::unit::UnitResult<
            $crate::chain::group::ExprGroup<$crate::chain::expr::ActionExpr>,
            $crate::chain::group::ActionGroup,
        > {
            use std::convert::TryInto;
            $crate::parse_n_or_empty_unit_fn_body!(
                $unit_count,
                true,
                P,
                ResultExpr,
                |val: Option<Vec<P>>| into_expr(val.and_then(|v| v.try_into().ok())),
                unit_parser,
                action_group,
                input
            )
        }
    };
}

#[macro_export]
macro_rules! parse_n_or_empty_unit_fn_body {
    ($unit_count: tt, $allow_empty: tt, $parse_type: path, $out_type: path, $into_expr: expr, $unit_parser: expr, $action_group: expr, $input: expr) => {{
    use $crate::syn::Token;
    use $crate::parse::Empty;
    use $crate::chain::group::ExprGroup;
    use $crate::parse::unit::{MapParsed, Unit};

    fn to_none<T, V>(_: T) -> Option<V> {
        None
    }

    let unit_count = $unit_count;
    let into_expr = $into_expr;
    let unit_parser = $unit_parser;
    let action_group = $action_group;
    let input = $input;
    if $allow_empty {
        unit_parser
            .parse_unit::<Empty>(&input.fork(), true)
            .and_then(|_| unit_parser.parse_unit::<Empty>(&input, true))
            .map_parsed(to_none::<_, Vec<$parse_type>>)
    } else {
        Err::<Unit<Option<Vec<$parse_type>>, $crate::chain::group::ActionGroup>, _>($crate::syn::Error::new(input.span(), "Can't parse empty unit"))
    }.or_else(|_| {
        (0..unit_count)
            .map(|index| {
                unit_parser
                    .parse_unit::<$parse_type>(input, false)
                    .and_then(|unit| {
                        if index + 1 < unit_count {
                            input.parse::<Token![,]>().and_then(|_| {
                                if unit.next.is_none() {
                                    Ok(unit)
                                } else {
                                    Err(input.error(&format!(
                                        "Expected {} units, found group identifier!",
                                        unit_count
                                    )))
                                }
                            })
                        } else {
                            Ok(unit)
                        }
                    })
            })
            .try_fold(
                Unit {
                    parsed: Some(Vec::with_capacity(unit_count)),
                    next: None,
                },
                |mut acc, unit| {
                    unit.map(|unit| {
                        acc.parsed.as_mut().unwrap().push(unit.parsed);
                        acc.next = unit.next;
                        acc
                    })
                },
            )
    })
    .map_parsed(into_expr)
    .map_parsed(|parsed| {
        ExprGroup::new(
            parsed.into(),
            *action_group
        )
    })
}}}

#[macro_export]
macro_rules! parse_n_or_empty_unit_fn {
    ($($fn_name: ident => [$unit_count: tt, $allow_empty: tt]),+) => {$(
        $crate::parse_n_or_empty_unit_fn_signature!($unit_count, $allow_empty, $fn_name);
    )+};
}
