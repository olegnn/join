//!
//! Contains `ProcessExpr` definition.
//!

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Expr, Type};

use super::InnerExpr;

///
/// `ProcessExpr` used to define type of expressions in process position.
///
#[cfg(not(feature = "full"))]
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ProcessExpr {
    ///
    /// .map(Expr)
    ///
    Map([Expr; 1]),
    ///
    /// Expr()
    ///
    Then([Expr; 1]),
    ///
    /// .and_then(Expr)
    ///
    AndThen([Expr; 1]),
    ///
    /// .filter(Expr)
    ///
    Filter([Expr; 1]),
    ///
    /// .find_map(Expr)
    ///
    FindMap([Expr; 1]),
    ///
    /// .flatten()
    ///
    Flatten,
    ///
    /// Sync: |value| { Expr(&value); value }
    /// Async: .inspect(Expr)
    ///
    Inspect([Expr; 1]),
    ///
    /// .Expr
    ///
    Dot([Expr; 1]),
    ///
    /// .chain(Expr)
    ///
    Chain([Expr; 1]),
    ///
    /// .collect::<Type>()
    ///
    Collect(Option<Type>),
    ///
    /// .enumerate()
    ///
    Enumerate,
    ///
    /// .filter_map(Expr)
    ///
    FilterMap([Expr; 1]),
    ///
    /// .find(Expr)
    ///
    Find([Expr; 1]),
    ///
    /// .fold(Expr, Expr)
    ///
    Fold([Expr; 2]),
    ///
    /// .partition(Expr)
    ///
    Partition([Expr; 1]),
    ///
    /// .try_fold(Expr, Expr)
    ///
    TryFold([Expr; 2]),
    ///
    /// .unzip::<A, B, FromA, FromB>()
    ///
    Unzip(Option<(Type, Type, Type, Type)>),
    ///
    /// .zip(Expr)
    ///
    Zip([Expr; 1]),
    ///
    /// Special process expr used to define next group nested position [which will be #value.and_then(#previous_expr).#next_expr]
    ///
    UNWRAP,
}

#[cfg(not(feature = "full"))]
impl InnerExpr for ProcessExpr {
    fn extract_inner(&self) -> Option<&[Expr]> {
        match self {
            Self::Map(expr) => Some(&expr[..]),
            Self::Dot(expr) => Some(&expr[..]),
            Self::Filter(expr) => Some(&expr[..]),
            Self::AndThen(expr) => Some(&expr[..]),
            Self::Then(expr) => Some(&expr[..]),
            Self::Inspect(expr) => Some(&expr[..]),
            Self::Chain(expr) => Some(&expr[..]),
            Self::FilterMap(expr) => Some(&expr[..]),
            Self::Find(expr) => Some(&expr[..]),
            Self::FindMap(expr) => Some(&expr[..]),
            Self::Fold(expr) => Some(&expr[..]),
            Self::Partition(expr) => Some(&expr[..]),
            Self::TryFold(expr) => Some(&expr[..]),
            Self::Zip(expr) => Some(&expr[..]),
            _ => None,
        }
    }

    fn replace_inner(self, exprs: &[Expr]) -> Option<Self> {
        exprs.last().map(Clone::clone).and_then(|expr| match self {
            Self::Fold(_) => exprs
                .first()
                .map(Clone::clone)
                .map(|first_expr| Self::Fold([first_expr, expr])),
            Self::TryFold(_) => exprs
                .first()
                .map(Clone::clone)
                .map(|first_expr| Self::TryFold([first_expr, expr])),
            other if exprs.len() == 1 => match other {
                Self::Map(_) => Some(Self::Map([expr])),
                Self::Filter(_) => Some(Self::Filter([expr])),
                Self::AndThen(_) => Some(Self::AndThen([expr])),
                Self::Then(_) => Some(Self::Then([expr])),
                Self::Inspect(_) => Some(Self::Inspect([expr])),
                Self::Chain(_) => Some(Self::Chain([expr])),
                Self::FilterMap(_) => Some(Self::FilterMap([expr])),
                Self::FindMap(_) => Some(Self::FindMap([expr])),
                Self::Find(_) => Some(Self::Find([expr])),
                Self::Partition(_) => Some(Self::Partition([expr])),
                Self::Zip(_) => Some(Self::Zip([expr])),
                _ => None,
            },
            _ => None,
        })
    }

    fn is_replaceable(&self) -> bool {
        !matches!(
            self,
            Self::Dot(_) | Self::Collect(_) | Self::Unzip(_) | Self::Flatten | Self::Enumerate
        )
    }
}

#[cfg(feature = "full")]
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ProcessExpr {
    ///
    /// .map(Expr)
    ///
    Map([Expr; 1]),
    ///
    /// Expr()
    ///
    Then([Expr; 1]),
    ///
    /// .and_then(Expr)
    ///
    AndThen([Expr; 1]),
    ///
    /// .filter(Expr)
    ///
    Filter([Expr; 1]),
    ///
    /// Sync: |value| { Expr(&value); value }
    /// Async: .inspect(Expr)
    ///
    Inspect([Expr; 1]),
    ///
    /// .Expr
    ///
    Dot([Expr; 1]),
    ///
    /// .all(Expr)
    ///
    All([Expr; 1]),
    ///
    /// .any(Expr)
    ///
    Any([Expr; 1]),
    ///
    /// .by_ref()
    ///
    ByRef,
    ///
    /// .chain(Expr)
    ///
    Chain([Expr; 1]),
    ///
    /// .cloned(Expr)
    ///
    Cloned,
    ///
    /// .cmp(Expr)
    ///
    Cmp([Expr; 1]),
    ///
    /// .collect::<Type>()
    ///
    Collect(Option<Type>),
    ///
    /// .copied(Expr)
    ///
    Copied,
    ///
    /// .count()
    ///
    Count,
    ///
    /// .cycle()
    ///
    Cycle,
    ///
    /// .enumerate()
    ///
    Enumerate,
    ///
    /// .eq(Expr)
    ///
    Eq([Expr; 1]),
    ///
    /// .filter_map(Expr)
    ///
    FilterMap([Expr; 1]),
    ///
    /// .find(Expr)
    ///
    Find([Expr; 1]),
    ///
    /// .find_map(Expr)
    ///
    FindMap([Expr; 1]),
    ///
    /// .flat_map(Expr)
    ///
    FlatMap([Expr; 1]),
    ///
    /// .flatten()
    ///
    Flatten,
    ///
    /// .fold(Expr, Expr)
    ///
    Fold([Expr; 2]),
    ///
    /// .for_each(Expr)
    ///
    ForEach([Expr; 1]),
    ///
    /// .fuse()
    ///
    Fuse,
    ///
    /// .ge(Expr)
    ///
    Ge([Expr; 1]),
    ///
    /// .gt(Expr)
    ///
    Gt([Expr; 1]),
    ///
    /// .is_sorted()
    ///
    IsSorted,
    ///
    /// .is_sorted_by(Expr)
    ///
    IsSortedBy([Expr; 1]),
    ///
    /// .is_sorted_by_key(Expr)
    ///
    IsSortedByKey([Expr; 1]),
    ///
    /// .is_partitioned()
    ///
    IsPartitioned,
    ///
    /// .last()
    ///
    Last,
    ///
    /// .le(Expr)
    ///
    Le([Expr; 1]),
    ///
    /// .lt(Expr)
    ///
    Lt([Expr; 1]),
    ///
    /// .max()
    ///
    Max,
    ///
    /// .max_by(Expr)
    ///
    MaxBy([Expr; 1]),
    ///
    /// .max_by_key(Expr)
    ///
    MaxByKey([Expr; 1]),
    ///
    /// .min()
    ///
    Min,
    ///
    /// .min_by(Expr)
    ///
    MinBy([Expr; 1]),
    ///
    /// .min_by_key(Expr)
    ///
    MinByKey([Expr; 1]),
    ///
    /// .ne(Expr)
    ///
    Ne([Expr; 1]),
    ///
    /// .nth(Expr)
    ///
    Nth([Expr; 1]),
    ///
    /// .partial_cmp(Expr)
    ///
    PartialCmp([Expr; 1]),
    ///
    /// .partition(Expr)
    ///
    Partition([Expr; 1]),
    ///
    /// .partition_in_place(Expr)
    ///
    PartitionInPlace([Expr; 1]),
    ///
    /// .peekable()
    ///
    Peekable,
    ///
    /// .position(Expr)
    ///
    Position([Expr; 1]),
    ///
    /// .product()
    ///
    Product,
    ///
    /// .rev()
    ///
    Rev,
    ///
    /// .rposition(Expr)
    ///
    Rposition([Expr; 1]),
    ///
    /// .scan(Expr)
    ///
    Scan([Expr; 1]),
    ///
    /// .size_hint()
    ///
    SizeHint,
    ///
    /// .skip(Expr)
    ///
    Skip([Expr; 1]),
    ///
    /// .skip_while(Expr)
    ///
    SkipWhile([Expr; 1]),
    ///
    /// .step_by(Expr)
    ///
    StepBy([Expr; 1]),
    ///
    /// .sum()
    ///
    Sum,
    ///
    /// .take(Expr)
    ///
    Take([Expr; 1]),
    ///
    /// .take_while(Expr)
    ///
    TakeWhile([Expr; 1]),
    ///
    /// .try_fold(Expr, Expr)
    ///
    TryFold([Expr; 2]),
    ///
    /// .try_for_each(Expr)
    ///
    TryForEach([Expr; 1]),
    ///
    /// .unzip::<A, B, FromA, FromB>()
    ///
    Unzip(Option<(Type, Type, Type, Type)>),
    ///
    /// .zip(Expr)
    ///
    Zip([Expr; 1]),
    ///
    /// Special process expr used to define next group nested position [which will be #value.and_then(#previous_expr).#next_expr]
    ///
    UNWRAP,
}

impl ToTokens for ProcessExpr {
    #[cfg(not(feature = "full"))]
    fn to_tokens(&self, output: &mut TokenStream) {
        let tokens = match self {
            Self::AndThen([expr]) => {
                quote! { .and_then(#expr) }
            }
            Self::Map([expr]) => {
                quote! { .map(#expr) }
            }
            Self::Dot([expr]) => {
                quote! { .#expr }
            }
            Self::Filter([expr]) => {
                quote! { .filter(#expr) }
            }
            Self::Then([expr]) => {
                quote! {{ let __handler = #expr; __handler }}
            }
            //
            // Not used for now because returning closure requires bound lifetimes
            //
            Self::Inspect(_) => unimplemented!(),
            Self::Chain([expr]) => {
                quote! { .chain(#expr) }
            }
            Self::Collect(type_spec) => {
                quote! { .collect::<#type_spec>() }
            }
            Self::Enumerate => {
                quote! { .enumerate() }
            }
            Self::FilterMap([expr]) => {
                quote! { .filter_map(#expr) }
            }
            Self::Find([expr]) => {
                quote! { .find(#expr) }
            }
            Self::FindMap([expr]) => {
                quote! { .find_map(#expr) }
            }
            Self::Flatten => {
                quote! { .flatten() }
            }
            Self::Fold([init, expr]) => {
                quote! { .fold(#init, #expr) }
            }
            Self::Partition([expr]) => {
                quote! { .partition(#expr) }
            }
            Self::TryFold([init, expr]) => {
                quote! { .try_fold(#init, #expr) }
            }
            Self::Unzip(type_spec) => type_spec
                .as_ref()
                .map(|(a, b, c, d)| quote! { .unzip::<#a, #b, #c, #d>() })
                .unwrap_or_else(|| quote! { .unzip() }),
            Self::Zip([expr]) => {
                quote! { .zip(#expr) }
            }
            Self::UNWRAP => {
                panic!("S")
            }
        };
        output.extend(tokens);
    }

    #[cfg(feature = "full")]
    fn to_tokens(&self, output: &mut TokenStream) {
        let tokens = match self {
            Self::AndThen([expr]) => {
                quote! { .and_then(#expr) }
            }
            Self::Map([expr]) => {
                quote! { .map(#expr) }
            }
            Self::Dot([expr]) => {
                quote! { .#expr }
            }
            Self::Filter([expr]) => {
                quote! { .filter(#expr) }
            }
            Self::Then([expr]) => {
                quote! {{ let __handler = #expr; __handler }}
            }
            //
            // Not used for now because returning closure requires bound lifetimes
            //
            Self::Inspect(_) => unimplemented!(),
            Self::All([expr]) => {
                quote! { .all(#expr) }
            }
            Self::Any([expr]) => {
                quote! { .any(#expr) }
            }
            Self::ByRef => {
                quote! { .by_ref() }
            }
            Self::Chain([expr]) => {
                quote! { .chain(#expr) }
            }
            Self::Cloned => {
                quote! { .cloned() }
            }
            Self::Cmp([expr]) => {
                quote! { .cmp(#expr) }
            }
            Self::Collect(type_spec) => {
                quote! { .collect::<#type_spec>() }
            }
            Self::Copied => {
                quote! { .copied() }
            }
            Self::Count => {
                quote! { .count() }
            }
            Self::Cycle => {
                quote! { .cycle() }
            }
            Self::Enumerate => {
                quote! { .enumerate() }
            }
            Self::Eq([expr]) => {
                quote! { .eq(#expr) }
            }
            Self::FilterMap([expr]) => {
                quote! { .filter_map(#expr) }
            }
            Self::Find([expr]) => {
                quote! { .find(#expr) }
            }
            Self::FindMap([expr]) => {
                quote! { .find_map(#expr) }
            }
            Self::FlatMap([expr]) => {
                quote! { .flat_map(#expr) }
            }
            Self::Flatten => {
                quote! { .flatten() }
            }
            Self::Fold([init, expr]) => {
                quote! { .fold(#init, #expr) }
            }
            Self::ForEach([expr]) => {
                quote! { .for_each(#expr) }
            }
            Self::Fuse => {
                quote! { .fuse() }
            }
            Self::Ge([expr]) => {
                quote! { .ge(#expr) }
            }
            Self::Gt([expr]) => {
                quote! { .gt(#expr) }
            }
            Self::IsSorted => {
                quote! { .is_sorted() }
            }
            Self::IsSortedBy([expr]) => {
                quote! { .is_sorted_by(#expr) }
            }
            Self::IsSortedByKey([expr]) => {
                quote! { .is_sorted_by_key(#expr) }
            }
            Self::IsPartitioned => {
                quote! { .is_partitioned() }
            }
            Self::Last => {
                quote! { .last() }
            }
            Self::Le([expr]) => {
                quote! { .le(#expr) }
            }
            Self::Lt([expr]) => {
                quote! { .lt(#expr) }
            }
            Self::Max => {
                quote! { .max() }
            }
            Self::MaxBy([expr]) => {
                quote! { .max_by(#expr) }
            }
            Self::MaxByKey([expr]) => {
                quote! { .max_by_key(#expr) }
            }
            Self::Min => {
                quote! { .min() }
            }
            Self::MinBy([expr]) => {
                quote! { .min_by(#expr) }
            }
            Self::MinByKey([expr]) => {
                quote! { .min_by_key(#expr) }
            }
            Self::Ne([expr]) => {
                quote! { .ne(#expr) }
            }
            Self::Nth([expr]) => {
                quote! { .nth(#expr) }
            }
            Self::PartialCmp([expr]) => {
                quote! { .partial_cmp(#expr) }
            }
            Self::Partition([expr]) => {
                quote! { .partition(#expr) }
            }
            Self::PartitionInPlace([expr]) => {
                quote! { .partition_in_place(#expr) }
            }
            Self::Peekable => {
                quote! { .peekable() }
            }
            Self::Position([expr]) => {
                quote! { .position(#expr) }
            }
            Self::Product => {
                quote! { .product() }
            }
            Self::Rev => {
                quote! { .rev() }
            }
            Self::Rposition([expr]) => {
                quote! { .rposition(#expr) }
            }
            Self::Scan([expr]) => {
                quote! { .scan(#expr) }
            }
            Self::SizeHint => {
                quote! { .size_hint() }
            }
            Self::Skip([expr]) => {
                quote! { .skip(#expr) }
            }
            Self::SkipWhile([expr]) => {
                quote! { .skip_while(#expr) }
            }
            Self::StepBy([expr]) => {
                quote! { .step_by(#expr) }
            }
            Self::Sum => {
                quote! { .sum() }
            }
            Self::Take([expr]) => {
                quote! { .take(#expr) }
            }
            Self::TakeWhile([expr]) => {
                quote! { .take_while(#expr) }
            }
            Self::TryFold([init, expr]) => {
                quote! { .try_fold(#init, #expr) }
            }
            Self::TryForEach([expr]) => {
                quote! { .try_for_each(#expr) }
            }
            Self::Unzip(type_spec) => type_spec
                .as_ref()
                .map(|(a, b, c, d)| quote! { .unzip::<#a, #b, #c, #d>() })
                .unwrap_or_else(|| quote! { .unzip() }),
            Self::Zip([expr]) => {
                quote! { .zip(#expr) }
            }
            Self::UNWRAP => {
                quote! {}
            }
        };
        output.extend(tokens);
    }

    fn into_token_stream(self) -> TokenStream {
        let mut output = TokenStream::new();
        self.to_tokens(&mut output);
        output
    }
}

#[cfg(feature = "full")]
impl InnerExpr for ProcessExpr {
    fn extract_inner(&self) -> Option<&[Expr]> {
        match self {
            Self::Map(expr) => Some(expr),
            Self::Dot(expr) => Some(expr),
            Self::Filter(expr) => Some(expr),
            Self::AndThen(expr) => Some(expr),
            Self::Then(expr) => Some(expr),
            Self::Inspect(expr) => Some(expr),
            Self::All(expr) => Some(expr),
            Self::Any(expr) => Some(expr),
            Self::Chain(expr) => Some(expr),
            Self::Cmp(expr) => Some(expr),
            Self::Eq(expr) => Some(expr),
            Self::FilterMap(expr) => Some(expr),
            Self::Find(expr) => Some(expr),
            Self::FindMap(expr) => Some(expr),
            Self::FlatMap(expr) => Some(expr),
            Self::Fold(expr) => Some(expr),
            Self::ForEach(expr) => Some(expr),
            Self::Ge(expr) => Some(expr),
            Self::Gt(expr) => Some(expr),
            Self::IsSortedBy(expr) => Some(expr),
            Self::IsSortedByKey(expr) => Some(expr),
            Self::Le(expr) => Some(expr),
            Self::Lt(expr) => Some(expr),
            Self::MaxBy(expr) => Some(expr),
            Self::MaxByKey(expr) => Some(expr),
            Self::MinBy(expr) => Some(expr),
            Self::MinByKey(expr) => Some(expr),
            Self::Ne(expr) => Some(expr),
            Self::Nth(expr) => Some(expr),
            Self::PartialCmp(expr) => Some(expr),
            Self::Partition(expr) => Some(expr),
            Self::PartitionInPlace(expr) => Some(expr),
            Self::Position(expr) => Some(expr),
            Self::Rposition(expr) => Some(expr),
            Self::Scan(expr) => Some(expr),
            Self::Skip(expr) => Some(expr),
            Self::SkipWhile(expr) => Some(expr),
            Self::StepBy(expr) => Some(expr),
            Self::Take(expr) => Some(expr),
            Self::TakeWhile(expr) => Some(expr),
            Self::TryFold(exprs) => Some(exprs),
            Self::TryForEach(expr) => Some(expr),
            Self::Zip(expr) => Some(expr),
            _ => None,
        }
    }

    fn replace_inner(self, exprs: &[Expr]) -> Option<Self> {
        exprs.last().map(Clone::clone).and_then(|expr| match self {
            Self::Fold(_) => exprs
                .first()
                .map(Clone::clone)
                .map(|first_expr| Self::Fold([first_expr, expr])),
            Self::TryFold(_) => exprs
                .first()
                .map(Clone::clone)
                .map(|first_expr| Self::TryFold([first_expr, expr])),
            other if exprs.len() == 1 => match other {
                Self::Map(_) => Some(Self::Map([expr])),
                Self::Filter(_) => Some(Self::Filter([expr])),
                Self::AndThen(_) => Some(Self::AndThen([expr])),
                Self::Then(_) => Some(Self::Then([expr])),
                Self::Inspect(_) => Some(Self::Inspect([expr])),
                Self::All(_) => Some(Self::All([expr])),
                Self::Any(_) => Some(Self::Any([expr])),
                Self::Chain(_) => Some(Self::Chain([expr])),
                Self::Cmp(_) => Some(Self::Cmp([expr])),
                Self::Eq(_) => Some(Self::Eq([expr])),
                Self::FilterMap(_) => Some(Self::FilterMap([expr])),
                Self::Find(_) => Some(Self::Find([expr])),
                Self::FindMap(_) => Some(Self::FindMap([expr])),
                Self::FlatMap(_) => Some(Self::FlatMap([expr])),
                Self::ForEach(_) => Some(Self::ForEach([expr])),
                Self::Ge(_) => Some(Self::Ge([expr])),
                Self::Gt(_) => Some(Self::Gt([expr])),
                Self::IsSortedBy(_) => Some(Self::IsSortedBy([expr])),
                Self::IsSortedByKey(_) => Some(Self::IsSortedByKey([expr])),
                Self::Le(_) => Some(Self::Le([expr])),
                Self::Lt(_) => Some(Self::Lt([expr])),
                Self::MaxBy(_) => Some(Self::MaxBy([expr])),
                Self::MaxByKey(_) => Some(Self::MaxByKey([expr])),
                Self::MinBy(_) => Some(Self::MinBy([expr])),
                Self::MinByKey(_) => Some(Self::MinByKey([expr])),
                Self::Ne(_) => Some(Self::Ne([expr])),
                Self::Nth(_) => Some(Self::Nth([expr])),
                Self::PartialCmp(_) => Some(Self::PartialCmp([expr])),
                Self::Partition(_) => Some(Self::Partition([expr])),
                Self::PartitionInPlace(_) => Some(Self::PartitionInPlace([expr])),
                Self::Position(_) => Some(Self::Position([expr])),
                Self::Rposition(_) => Some(Self::Rposition([expr])),
                Self::Scan(_) => Some(Self::Scan([expr])),
                Self::Skip(_) => Some(Self::Skip([expr])),
                Self::SkipWhile(_) => Some(Self::SkipWhile([expr])),
                Self::StepBy(_) => Some(Self::StepBy([expr])),
                Self::Take(_) => Some(Self::Take([expr])),
                Self::TakeWhile(_) => Some(Self::TakeWhile([expr])),
                Self::TryForEach(_) => Some(Self::TryForEach([expr])),
                Self::Zip(_) => Some(Self::Zip([expr])),
                _ => None,
            },
            _ => None,
        })
    }

    fn is_replaceable(&self) -> bool {
        !matches!(self,
            Self::Dot(_)
            | Self::Collect(_)
            | Self::Max
            | Self::Min
            | Self::Unzip(_)
            | Self::IsSorted
            | Self::IsPartitioned
            | Self::ByRef
            | Self::Sum
            | Self::SizeHint
            | Self::Product
            | Self::Cloned
            | Self::Copied
            | Self::Flatten
            | Self::Count
            | Self::Cycle
            | Self::Enumerate
            | Self::Rev
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::{parse_quote, Expr};

    fn are_streams_equal(a: TokenStream, b: TokenStream) -> bool {
        format!("{:#?}", a) == format!("{:#?}", b)
    }

    #[test]
    fn it_tests_inner_expr_trait_impl_for_process_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };

        for process_expr in vec![
            ProcessExpr::Map([expr.clone()]),
            ProcessExpr::Filter([expr.clone()]),
            ProcessExpr::Dot([expr.clone()]),
            ProcessExpr::AndThen([expr.clone()]),
            ProcessExpr::Then([expr.clone()]),
            ProcessExpr::Chain([expr.clone()]),
            ProcessExpr::Inspect([expr.clone()]),
            ProcessExpr::FilterMap([expr.clone()]),
            ProcessExpr::Partition([expr.clone()]),
            ProcessExpr::Find([expr.clone()]),
            ProcessExpr::FindMap([expr.clone()]),
            ProcessExpr::Zip([expr.clone()]),
        ]
        .into_iter()
        {
            assert_eq!(
                process_expr.extract_inner().clone(),
                Some(&[expr.clone()][..])
            );
        }

        assert_eq!(
            ProcessExpr::Fold([expr.clone(), expr.clone()])
                .extract_inner()
                .clone(),
            Some(&[expr.clone(), expr.clone()][..])
        );

        assert_eq!(
            ProcessExpr::TryFold([expr.clone(), expr.clone()])
                .extract_inner()
                .clone(),
            Some(&[expr.clone(), expr][..])
        );
    }

    #[test]
    fn it_tests_inner_expr_trait_impl_replace_inner_for_process_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };
        let replace_inner: Expr = parse_quote! { |v| 3 + v };

        for process_expr in vec![
            ProcessExpr::Map([expr.clone()]),
            ProcessExpr::Filter([expr.clone()]),
            ProcessExpr::AndThen([expr.clone()]),
            ProcessExpr::Then([expr.clone()]),
            ProcessExpr::Chain([expr.clone()]),
            ProcessExpr::Inspect([expr.clone()]),
            ProcessExpr::FilterMap([expr.clone()]),
            ProcessExpr::Partition([expr.clone()]),
            ProcessExpr::Find([expr.clone()]),
            ProcessExpr::FindMap([expr.clone()]),
            ProcessExpr::Zip([expr.clone()]),
        ]
        .into_iter()
        {
            assert_eq!(
                process_expr
                    .replace_inner(&[replace_inner.clone()])
                    .unwrap()
                    .extract_inner()
                    .clone(),
                Some(&[replace_inner.clone()][..])
            );
        }

        assert_eq!(
            ProcessExpr::Dot([expr.clone()]).replace_inner(&[replace_inner.clone()]),
            None
        );

        assert_eq!(
            ProcessExpr::Fold([expr.clone(), expr.clone()])
                .replace_inner(&[replace_inner.clone(), replace_inner.clone()])
                .unwrap()
                .extract_inner()
                .clone(),
            Some(&[replace_inner.clone(), replace_inner.clone()][..])
        );

        assert_eq!(
            ProcessExpr::TryFold([expr.clone(), expr])
                .replace_inner(&[replace_inner.clone(), replace_inner.clone()])
                .unwrap()
                .extract_inner()
                .clone(),
            Some(&[replace_inner.clone(), replace_inner][..])
        );
    }

    #[test]
    fn it_tests_to_tokens_trait_impl_for_process_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };

        for process_expr in vec![
            ProcessExpr::Map([expr.clone()]),
            ProcessExpr::Filter([expr.clone()]),
            ProcessExpr::Dot([expr.clone()]),
            ProcessExpr::AndThen([expr.clone()]),
            ProcessExpr::Then([expr.clone()]),
            ProcessExpr::Chain([expr.clone()]),
            ProcessExpr::FilterMap([expr.clone()]),
            ProcessExpr::Partition([expr.clone()]),
            ProcessExpr::Fold([expr.clone(), expr.clone()]),
            ProcessExpr::TryFold([expr.clone(), expr.clone()]),
            ProcessExpr::Find([expr.clone()]),
            ProcessExpr::FindMap([expr.clone()]),
            ProcessExpr::Zip([expr.clone()]),
        ]
        .into_iter()
        {
            let mut token_stream = TokenStream::new();
            process_expr.to_tokens(&mut token_stream);
            assert!(are_streams_equal(
                token_stream.clone(),
                process_expr.clone().into_token_stream()
            ));
            assert!(are_streams_equal(
                token_stream,
                #[cfg(not(feature = "full"))]
                match process_expr {
                    ProcessExpr::AndThen([expr]) => {
                        quote! { .and_then(#expr) }
                    }
                    ProcessExpr::Map([expr]) => {
                        quote! { .map(#expr) }
                    }
                    ProcessExpr::Dot([expr]) => {
                        quote! { .#expr }
                    }
                    ProcessExpr::Filter([expr]) => {
                        quote! { .filter(#expr) }
                    }
                    ProcessExpr::Then([expr]) => {
                        quote! {{ let __handler = #expr; __handler }}
                    }
                    //
                    // Not used for now because returning closure requires bound lifetimes
                    //
                    ProcessExpr::Inspect(_) => unimplemented!(),
                    ProcessExpr::Chain([expr]) => {
                        quote! { .chain(#expr) }
                    }
                    ProcessExpr::Collect(type_spec) => {
                        quote! { .collect::<#type_spec>() }
                    }
                    ProcessExpr::Enumerate => {
                        quote! { .enumerate() }
                    }
                    ProcessExpr::FilterMap([expr]) => {
                        quote! { .filter_map(#expr) }
                    }
                    ProcessExpr::Find([expr]) => {
                        quote! { .find(#expr) }
                    }
                    ProcessExpr::FindMap([expr]) => {
                        quote! { .find_map(#expr) }
                    }
                    ProcessExpr::Flatten => {
                        quote! { .flatten() }
                    }
                    ProcessExpr::Fold([expr1, expr2]) => {
                        quote! { .fold(#expr1, #expr2) }
                    }
                    ProcessExpr::Partition([expr]) => {
                        quote! { .partition(#expr) }
                    }
                    ProcessExpr::TryFold([expr1, expr2]) => {
                        quote! { .try_fold(#expr1, #expr2) }
                    }
                    ProcessExpr::Unzip(type_spec) => type_spec
                        .as_ref()
                        .map(|(a, b, c, d)| quote! { .unzip::<#a, #b, #c, #d>() })
                        .unwrap_or_else(|| quote! { .unzip() }),
                    ProcessExpr::Zip([expr]) => {
                        quote! { .zip(#expr) }
                    }
                    ProcessExpr::UNWRAP => {
                        quote! {}
                    }
                },
                #[cfg(feature = "full")]
                match process_expr {
                    ProcessExpr::AndThen([expr]) => {
                        quote! { .and_then(#expr) }
                    }
                    ProcessExpr::Map([expr]) => {
                        quote! { .map(#expr) }
                    }
                    ProcessExpr::Dot([expr]) => {
                        quote! { .#expr }
                    }
                    ProcessExpr::Filter([expr]) => {
                        quote! { .filter(#expr) }
                    }
                    ProcessExpr::Then([expr]) => {
                        quote! {{ let __handler = #expr; __handler }}
                    }
                    //
                    // Not used for now because returning closure requires bound lifetimes
                    //
                    ProcessExpr::Inspect(_) => unimplemented!(),
                    ProcessExpr::All([expr]) => {
                        quote! { .all(#expr) }
                    }
                    ProcessExpr::Any([expr]) => {
                        quote! { .any(#expr) }
                    }
                    ProcessExpr::ByRef => {
                        quote! { .by_ref() }
                    }
                    ProcessExpr::Chain([expr]) => {
                        quote! { .chain(#expr) }
                    }
                    ProcessExpr::Cloned => {
                        quote! { .cloned() }
                    }
                    ProcessExpr::Cmp([expr]) => {
                        quote! { .cmp(#expr) }
                    }
                    ProcessExpr::Collect(type_spec) => {
                        quote! { .collect::<#type_spec>() }
                    }
                    ProcessExpr::Copied => {
                        quote! { .copied() }
                    }
                    ProcessExpr::Count => {
                        quote! { .count() }
                    }
                    ProcessExpr::Cycle => {
                        quote! { .cycle() }
                    }
                    ProcessExpr::Enumerate => {
                        quote! { .enumerate() }
                    }
                    ProcessExpr::Eq([expr]) => {
                        quote! { .eq(#expr) }
                    }
                    ProcessExpr::FilterMap([expr]) => {
                        quote! { .filter_map(#expr) }
                    }
                    ProcessExpr::Find([expr]) => {
                        quote! { .find(#expr) }
                    }
                    ProcessExpr::FindMap([expr]) => {
                        quote! { .find_map(#expr) }
                    }
                    ProcessExpr::FlatMap([expr]) => {
                        quote! { .flat_map(#expr) }
                    }
                    ProcessExpr::Flatten => {
                        quote! { .flatten() }
                    }
                    ProcessExpr::Fold([expr1, expr2]) => {
                        quote! { .fold(#expr1, #expr2) }
                    }
                    ProcessExpr::ForEach([expr]) => {
                        quote! { .for_each(#expr) }
                    }
                    ProcessExpr::Fuse => {
                        quote! { .fuse() }
                    }
                    ProcessExpr::Ge([expr]) => {
                        quote! { .ge(#expr) }
                    }
                    ProcessExpr::Gt([expr]) => {
                        quote! { .gt(#expr) }
                    }
                    ProcessExpr::IsSorted => {
                        quote! { .is_sorted() }
                    }
                    ProcessExpr::IsSortedBy([expr]) => {
                        quote! { .is_sorted_by(#expr) }
                    }
                    ProcessExpr::IsSortedByKey([expr]) => {
                        quote! { .is_sorted_by_key(#expr) }
                    }
                    ProcessExpr::IsPartitioned => {
                        quote! { .is_partitioned() }
                    }
                    ProcessExpr::Last => {
                        quote! { .last() }
                    }
                    ProcessExpr::Le([expr]) => {
                        quote! { .le(#expr) }
                    }
                    ProcessExpr::Lt([expr]) => {
                        quote! { .lt(#expr) }
                    }
                    ProcessExpr::Max => {
                        quote! { .max() }
                    }
                    ProcessExpr::MaxBy([expr]) => {
                        quote! { .max_by(#expr) }
                    }
                    ProcessExpr::MaxByKey([expr]) => {
                        quote! { .max_by_key(#expr) }
                    }
                    ProcessExpr::Min => {
                        quote! { .min() }
                    }
                    ProcessExpr::MinBy([expr]) => {
                        quote! { .min_by(#expr) }
                    }
                    ProcessExpr::MinByKey([expr]) => {
                        quote! { .min_by_key(#expr) }
                    }
                    ProcessExpr::Ne([expr]) => {
                        quote! { .ne(#expr) }
                    }
                    ProcessExpr::Nth([expr]) => {
                        quote! { .nth(#expr) }
                    }
                    ProcessExpr::PartialCmp([expr]) => {
                        quote! { .partial_cmp(#expr) }
                    }
                    ProcessExpr::Partition([expr]) => {
                        quote! { .partition(#expr) }
                    }
                    ProcessExpr::PartitionInPlace([expr]) => {
                        quote! { .partition_in_place(#expr) }
                    }
                    ProcessExpr::Peekable => {
                        quote! { .peekable() }
                    }
                    ProcessExpr::Position([expr]) => {
                        quote! { .position(#expr) }
                    }
                    ProcessExpr::Product => {
                        quote! { .product() }
                    }
                    ProcessExpr::Rev => {
                        quote! { .rev() }
                    }
                    ProcessExpr::Rposition([expr]) => {
                        quote! { .rposition(#expr) }
                    }
                    ProcessExpr::Scan([expr]) => {
                        quote! { .scan(#expr) }
                    }
                    ProcessExpr::SizeHint => {
                        quote! { .size_hint() }
                    }
                    ProcessExpr::Skip([expr]) => {
                        quote! { .skip(#expr) }
                    }
                    ProcessExpr::SkipWhile([expr]) => {
                        quote! { .skip_while(#expr) }
                    }
                    ProcessExpr::StepBy([expr]) => {
                        quote! { .step_by(#expr) }
                    }
                    ProcessExpr::Sum => {
                        quote! { .sum() }
                    }
                    ProcessExpr::Take([expr]) => {
                        quote! { .take(#expr) }
                    }
                    ProcessExpr::TakeWhile([expr]) => {
                        quote! { .take_while(#expr) }
                    }
                    ProcessExpr::TryFold([expr1, expr2]) => {
                        quote! { .try_fold(#expr1, #expr2) }
                    }
                    ProcessExpr::TryForEach([expr]) => {
                        quote! { .try_for_each(#expr) }
                    }
                    ProcessExpr::Unzip(type_spec) => type_spec
                        .as_ref()
                        .map(|(a, b, c, d)| quote! { .unzip::<#a, #b, #c, #d>() })
                        .unwrap_or_else(|| quote! { .unzip() }),
                    ProcessExpr::Zip([expr]) => {
                        quote! { .zip(#expr) }
                    }
                    ProcessExpr::UNWRAP => {
                        quote! {}
                    }
                }
            ));
        }

        assert!(::std::panic::catch_unwind(
            move || ProcessExpr::Inspect([expr]).into_token_stream()
        )
        .is_err());
    }
}
