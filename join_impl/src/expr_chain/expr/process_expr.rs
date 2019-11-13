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
    Map(Expr),
    ///
    /// Expr()
    ///
    Then(Expr),
    ///
    /// .and_then(Expr)
    ///
    AndThen(Expr),
    ///
    /// .filter(Expr)
    ///
    Filter(Expr),
    ///
    ///	.find_map(Expr)
    ///
    FindMap(Expr),
    ///
    ///	.flatten()
    ///
    Flatten,
    ///
    /// Sync: |value| { Expr(&value); value }
    /// Async: .inspect(Expr)
    ///
    Inspect(Expr),
    ///
    /// .Expr
    ///
    Dot(Expr),
    ///
    ///	.chain(Expr)
    ///
    Chain(Expr),
    ///
    ///	.collect::<Type>()
    ///
    Collect(Option<Type>),
    ///
    ///	.enumerate()
    ///
    Enumerate,
    ///
    ///	.filter_map(Expr)
    ///
    FilterMap(Expr),
    ///
    ///	.find(Expr)
    ///
    Find(Expr),
    ///
    ///	.fold(Expr, Expr)
    ///
    Fold((Expr, Expr)),
    ///
    ///	.partition(Expr)
    ///
    Partition(Expr),
    ///
    ///	.try_fold(Expr, Expr)
    ///
    TryFold((Expr, Expr)),
    ///
    ///	.unzip()
    ///
    Unzip(Option<Vec<Type>>),
    ///
    ///	.zip(Expr)
    ///
    Zip(Expr),
}

#[cfg(not(feature = "full"))]
impl InnerExpr for ProcessExpr {
    fn extract_inner(&self) -> Option<Vec<&Expr>> {
        match self {
            Self::Map(expr) => Some(vec![expr]),
            Self::Dot(expr) => Some(vec![expr]),
            Self::Filter(expr) => Some(vec![expr]),
            Self::AndThen(expr) => Some(vec![expr]),
            Self::Then(expr) => Some(vec![expr]),
            Self::Inspect(expr) => Some(vec![expr]),
            Self::Chain(expr) => Some(vec![expr]),
            Self::FilterMap(expr) => Some(vec![expr]),
            Self::Find(expr) => Some(vec![expr]),
            Self::FindMap(expr) => Some(vec![expr]),
            Self::Fold((init, expr)) => Some(vec![init, expr]),
            Self::Partition(expr) => Some(vec![expr]),
            Self::TryFold((init, expr)) => Some(vec![init, expr]),
            Self::Zip(expr) => Some(vec![expr]),
            _ => None,
        }
    }

    fn replace_inner(&self, exprs: &mut Vec<Expr>) -> Option<Self> {
        exprs.pop().and_then(|expr| match self {
            Self::Fold(_) => {
                if exprs.len() != 1 {
                    None
                } else {
                    Some(Self::Fold((exprs.pop().unwrap(), expr)))
                }
            }
            Self::TryFold(_) => {
                if exprs.len() != 1 {
                    None
                } else {
                    Some(Self::TryFold((exprs.pop().unwrap(), expr)))
                }
            }
            other if exprs.is_empty() => match other {
                Self::Map(_) => Some(Self::Map(expr)),
                Self::Filter(_) => Some(Self::Filter(expr)),
                Self::AndThen(_) => Some(Self::AndThen(expr)),
                Self::Then(_) => Some(Self::Then(expr)),
                Self::Inspect(_) => Some(Self::Inspect(expr)),
                Self::Chain(_) => Some(Self::Chain(expr)),
                Self::FilterMap(_) => Some(Self::FilterMap(expr)),
                Self::FindMap(_) => Some(Self::FindMap(expr)),
                Self::Find(_) => Some(Self::Find(expr)),
                Self::Partition(_) => Some(Self::Partition(expr)),
                Self::Zip(_) => Some(Self::Zip(expr)),
                _ => None,
            },
            _ => None,
        })
    }

    fn is_replaceable(&self) -> bool {
        match self {
            Self::Dot(_) | Self::Collect(_) | Self::Unzip(_) | Self::Flatten | Self::Enumerate => {
                false
            }
            _ => true,
        }
    }
}

#[cfg(feature = "full")]
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ProcessExpr {
    ///
    /// .map(Expr)
    ///
    Map(Expr),
    ///
    /// Expr()
    ///
    Then(Expr),
    ///
    /// .and_then(Expr)
    ///
    AndThen(Expr),
    ///
    /// .filter(Expr)
    ///
    Filter(Expr),
    ///
    /// Sync: |value| { Expr(&value); value }
    /// Async: .inspect(Expr)
    ///
    Inspect(Expr),
    ///
    /// .Expr
    ///
    Dot(Expr),
    ///
    ///	.all(Expr)
    ///
    All(Expr),
    ///
    ///	.any(Expr)
    ///
    Any(Expr),
    ///
    ///	.by_ref()
    ///
    ByRef,
    ///
    ///	.chain(Expr)
    ///
    Chain(Expr),
    ///
    ///	.cloned(Expr)
    ///
    Cloned,
    ///
    ///	.cmp(Expr)
    ///
    Cmp(Expr),
    ///
    ///	.collect::<Type>()
    ///
    Collect(Option<Type>),
    ///
    ///	.copied(Expr)
    ///
    Copied,
    ///
    ///	.count()
    ///
    Count,
    ///
    ///	.cycle()
    ///
    Cycle,
    ///
    ///	.enumerate()
    ///
    Enumerate,
    ///
    ///	.eq(Expr)
    ///
    Eq(Expr),
    ///
    ///	.filter_map(Expr)
    ///
    FilterMap(Expr),
    ///
    ///	.find(Expr)
    ///
    Find(Expr),
    ///
    ///	.find_map(Expr)
    ///
    FindMap(Expr),
    ///
    ///	.flat_map(Expr)
    ///
    FlatMap(Expr),
    ///
    ///	.flatten()
    ///
    Flatten,
    ///
    ///	.fold(Expr, Expr)
    ///
    Fold((Expr, Expr)),
    ///
    ///	.for_each(Expr)
    ///
    ForEach(Expr),
    ///
    ///	.fuse()
    ///
    Fuse,
    ///
    ///	.ge(Expr)
    ///
    Ge(Expr),
    ///
    ///	.gt(Expr)
    ///
    Gt(Expr),
    ///
    /// .is_sorted()
    ///
    IsSorted,
    ///
    ///	.is_sorted_by(Expr)
    ///
    IsSortedBy(Expr),
    ///
    ///	.is_sorted_by_key(Expr)
    ///
    IsSortedByKey(Expr),
    ///
    /// .is_partitioned()
    ///
    IsPartitioned,
    ///
    ///	.last()
    ///
    Last,
    ///
    ///	.le(Expr)
    ///
    Le(Expr),
    ///
    ///	.lt(Expr)
    ///
    Lt(Expr),
    ///
    ///	.max()
    ///
    Max,
    ///
    ///	.max_by(Expr)
    ///
    MaxBy(Expr),
    ///
    ///	.max_by_key(Expr)
    ///
    MaxByKey(Expr),
    ///
    ///	.min()
    ///
    Min,
    ///
    ///	.min_by(Expr)
    ///
    MinBy(Expr),
    ///
    ///	.min_by_key(Expr)
    ///
    MinByKey(Expr),
    ///
    ///	.ne(Expr)
    ///
    Ne(Expr),
    ///
    ///	.nth(Expr)
    ///
    Nth(Expr),
    ///
    ///	.partial_cmp(Expr)
    ///
    PartialCmp(Expr),
    ///
    ///	.partition(Expr)
    ///
    Partition(Expr),
    ///
    ///	.partition_in_place(Expr)
    ///
    PartitionInPlace(Expr),
    ///
    ///	.peekable()
    ///
    Peekable,
    ///
    ///	.position(Expr)
    ///
    Position(Expr),
    ///
    ///	.product()
    ///
    Product,
    ///
    ///	.rev()
    ///
    Rev,
    ///
    ///	.rposition(Expr)
    ///
    Rposition(Expr),
    ///
    ///	.scan(Expr)
    ///
    Scan(Expr),
    ///
    ///	.size_hint()
    ///
    SizeHint,
    ///
    ///	.skip(Expr)
    ///
    Skip(Expr),
    ///
    ///	.skip_while(Expr)
    ///
    SkipWhile(Expr),
    ///
    ///	.step_by(Expr)
    ///
    StepBy(Expr),
    ///
    ///	.sum()
    ///
    Sum,
    ///
    ///	.take(Expr)
    ///
    Take(Expr),
    ///
    ///	.take_while(Expr)
    ///
    TakeWhile(Expr),
    ///
    ///	.try_fold(Expr, Expr)
    ///
    TryFold((Expr, Expr)),
    ///
    ///	.try_for_each(Expr)
    ///
    TryForEach(Expr),
    ///
    ///	.unzip()
    ///
    Unzip(Option<Vec<Type>>),
    ///
    ///	.zip(Expr)
    ///
    Zip(Expr),
}

impl ToTokens for ProcessExpr {
    #[cfg(not(feature = "full"))]
    fn to_tokens(&self, output: &mut TokenStream) {
        let tokens = match self {
            ProcessExpr::AndThen(expr) => {
                quote! { .and_then(#expr) }
            }
            ProcessExpr::Map(expr) => {
                quote! { .map(#expr) }
            }
            ProcessExpr::Dot(expr) => {
                quote! { .#expr }
            }
            ProcessExpr::Filter(expr) => {
                quote! { .filter(#expr) }
            }
            ProcessExpr::Then(expr) => {
                quote! {{ let __handler = #expr; __handler }}
            }
            //
            // Not used for now because returning closure requires bound lifetimes
            //
            ProcessExpr::Inspect(_) => unimplemented!(),
            ProcessExpr::Chain(expr) => {
                quote! { .chain(#expr) }
            }
            ProcessExpr::Collect(type_spec) => {
                quote! { .collect::<#type_spec>() }
            }
            ProcessExpr::Enumerate => {
                quote! { .enumerate() }
            }
            ProcessExpr::FilterMap(expr) => {
                quote! { .filter_map(#expr) }
            }
            ProcessExpr::Find(expr) => {
                quote! { .find(#expr) }
            }
            ProcessExpr::FindMap(expr) => {
                quote! { .find_map(#expr) }
            }
            ProcessExpr::Flatten => {
                quote! { .flatten() }
            }
            ProcessExpr::Fold((init, expr)) => {
                quote! { .fold(#init, #expr) }
            }
            ProcessExpr::Partition(expr) => {
                quote! { .partition(#expr) }
            }
            ProcessExpr::TryFold((init, expr)) => {
                quote! { .try_fold(#init, #expr) }
            }
            ProcessExpr::Unzip(type_spec) => type_spec
                .as_ref()
                .map(|type_spec| quote! { .unzip::<#( #type_spec ),*>() })
                .unwrap_or_else(|| quote! { .unzip() }),
            ProcessExpr::Zip(expr) => {
                quote! { .zip(#expr) }
            }
        };
        output.extend(tokens);
    }

    #[cfg(feature = "full")]
    fn to_tokens(&self, output: &mut TokenStream) {
        let tokens = match self {
            ProcessExpr::AndThen(expr) => {
                quote! { .and_then(#expr) }
            }
            ProcessExpr::Map(expr) => {
                quote! { .map(#expr) }
            }
            ProcessExpr::Dot(expr) => {
                quote! { .#expr }
            }
            ProcessExpr::Filter(expr) => {
                quote! { .filter(#expr) }
            }
            ProcessExpr::Then(expr) => {
                quote! {{ let __handler = #expr; __handler }}
            }
            //
            // Not used for now because returning closure requires bound lifetimes
            //
            ProcessExpr::Inspect(_) => unimplemented!(),
            ProcessExpr::All(expr) => {
                quote! { .all(#expr) }
            }
            ProcessExpr::Any(expr) => {
                quote! { .any(#expr) }
            }
            ProcessExpr::ByRef => {
                quote! { .by_ref() }
            }
            ProcessExpr::Chain(expr) => {
                quote! { .chain(#expr) }
            }
            ProcessExpr::Cloned => {
                quote! { .cloned() }
            }
            ProcessExpr::Cmp(expr) => {
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
            ProcessExpr::Eq(expr) => {
                quote! { .eq(#expr) }
            }
            ProcessExpr::FilterMap(expr) => {
                quote! { .filter_map(#expr) }
            }
            ProcessExpr::Find(expr) => {
                quote! { .find(#expr) }
            }
            ProcessExpr::FindMap(expr) => {
                quote! { .find_map(#expr) }
            }
            ProcessExpr::FlatMap(expr) => {
                quote! { .flat_map(#expr) }
            }
            ProcessExpr::Flatten => {
                quote! { .flatten() }
            }
            ProcessExpr::Fold((init, expr)) => {
                quote! { .fold(#init, #expr) }
            }
            ProcessExpr::ForEach(expr) => {
                quote! { .for_each(#expr) }
            }
            ProcessExpr::Fuse => {
                quote! { .fuse() }
            }
            ProcessExpr::Ge(expr) => {
                quote! { .ge(#expr) }
            }
            ProcessExpr::Gt(expr) => {
                quote! { .gt(#expr) }
            }
            ProcessExpr::IsSorted => {
                quote! { .is_sorted() }
            }
            ProcessExpr::IsSortedBy(expr) => {
                quote! { .is_sorted_by(#expr) }
            }
            ProcessExpr::IsSortedByKey(expr) => {
                quote! { .is_sorted_by_key(#expr) }
            }
            ProcessExpr::IsPartitioned => {
                quote! { .is_partitioned() }
            }
            ProcessExpr::Last => {
                quote! { .last() }
            }
            ProcessExpr::Le(expr) => {
                quote! { .le(#expr) }
            }
            ProcessExpr::Lt(expr) => {
                quote! { .lt(#expr) }
            }
            ProcessExpr::Max => {
                quote! { .max() }
            }
            ProcessExpr::MaxBy(expr) => {
                quote! { .max_by(#expr) }
            }
            ProcessExpr::MaxByKey(expr) => {
                quote! { .max_by_key(#expr) }
            }
            ProcessExpr::Min => {
                quote! { .min() }
            }
            ProcessExpr::MinBy(expr) => {
                quote! { .min_by(#expr) }
            }
            ProcessExpr::MinByKey(expr) => {
                quote! { .min_by_key(#expr) }
            }
            ProcessExpr::Ne(expr) => {
                quote! { .ne(#expr) }
            }
            ProcessExpr::Nth(expr) => {
                quote! { .nth(#expr) }
            }
            ProcessExpr::PartialCmp(expr) => {
                quote! { .partial_cmp(#expr) }
            }
            ProcessExpr::Partition(expr) => {
                quote! { .partition(#expr) }
            }
            ProcessExpr::PartitionInPlace(expr) => {
                quote! { .partition_in_place(#expr) }
            }
            ProcessExpr::Peekable => {
                quote! { .peekable() }
            }
            ProcessExpr::Position(expr) => {
                quote! { .position(#expr) }
            }
            ProcessExpr::Product => {
                quote! { .product() }
            }
            ProcessExpr::Rev => {
                quote! { .rev() }
            }
            ProcessExpr::Rposition(expr) => {
                quote! { .rposition(#expr) }
            }
            ProcessExpr::Scan(expr) => {
                quote! { .scan(#expr) }
            }
            ProcessExpr::SizeHint => {
                quote! { .size_hint() }
            }
            ProcessExpr::Skip(expr) => {
                quote! { .skip(#expr) }
            }
            ProcessExpr::SkipWhile(expr) => {
                quote! { .skip_while(#expr) }
            }
            ProcessExpr::StepBy(expr) => {
                quote! { .step_by(#expr) }
            }
            ProcessExpr::Sum => {
                quote! { .sum() }
            }
            ProcessExpr::Take(expr) => {
                quote! { .take(#expr) }
            }
            ProcessExpr::TakeWhile(expr) => {
                quote! { .take_while(#expr) }
            }
            ProcessExpr::TryFold((init, expr)) => {
                quote! { .try_fold(#init, #expr) }
            }
            ProcessExpr::TryForEach(expr) => {
                quote! { .try_for_each(#expr) }
            }
            ProcessExpr::Unzip(type_spec) => type_spec
                .as_ref()
                .map(|type_spec| quote! { .unzip::<#( #type_spec ),*>() })
                .unwrap_or_else(|| quote! { .unzip() }),
            ProcessExpr::Zip(expr) => {
                quote! { .zip(#expr) }
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
    fn extract_inner(&self) -> Option<Vec<&Expr>> {
        match self {
            Self::Map(expr) => Some(vec![expr]),
            Self::Dot(expr) => Some(vec![expr]),
            Self::Filter(expr) => Some(vec![expr]),
            Self::AndThen(expr) => Some(vec![expr]),
            Self::Then(expr) => Some(vec![expr]),
            Self::Inspect(expr) => Some(vec![expr]),
            Self::All(expr) => Some(vec![expr]),
            Self::Any(expr) => Some(vec![expr]),
            Self::Chain(expr) => Some(vec![expr]),
            Self::Cmp(expr) => Some(vec![expr]),
            Self::Eq(expr) => Some(vec![expr]),
            Self::FilterMap(expr) => Some(vec![expr]),
            Self::Find(expr) => Some(vec![expr]),
            Self::FindMap(expr) => Some(vec![expr]),
            Self::FlatMap(expr) => Some(vec![expr]),
            Self::Fold((init, expr)) => Some(vec![init, expr]),
            Self::ForEach(expr) => Some(vec![expr]),
            Self::Ge(expr) => Some(vec![expr]),
            Self::Gt(expr) => Some(vec![expr]),
            Self::IsSortedBy(expr) => Some(vec![expr]),
            Self::IsSortedByKey(expr) => Some(vec![expr]),
            Self::Le(expr) => Some(vec![expr]),
            Self::Lt(expr) => Some(vec![expr]),
            Self::MaxBy(expr) => Some(vec![expr]),
            Self::MaxByKey(expr) => Some(vec![expr]),
            Self::MinBy(expr) => Some(vec![expr]),
            Self::MinByKey(expr) => Some(vec![expr]),
            Self::Ne(expr) => Some(vec![expr]),
            Self::Nth(expr) => Some(vec![expr]),
            Self::PartialCmp(expr) => Some(vec![expr]),
            Self::Partition(expr) => Some(vec![expr]),
            Self::PartitionInPlace(expr) => Some(vec![expr]),
            Self::Position(expr) => Some(vec![expr]),
            Self::Rposition(expr) => Some(vec![expr]),
            Self::Scan(expr) => Some(vec![expr]),
            Self::Skip(expr) => Some(vec![expr]),
            Self::SkipWhile(expr) => Some(vec![expr]),
            Self::StepBy(expr) => Some(vec![expr]),
            Self::Take(expr) => Some(vec![expr]),
            Self::TakeWhile(expr) => Some(vec![expr]),
            Self::TryFold((init, expr)) => Some(vec![init, expr]),
            Self::TryForEach(expr) => Some(vec![expr]),
            Self::Zip(expr) => Some(vec![expr]),
            _ => None,
        }
    }

    fn replace_inner(&self, exprs: &mut Vec<Expr>) -> Option<Self> {
        exprs.pop().and_then(|expr| match self {
            Self::Fold(_) => {
                if exprs.len() != 1 {
                    None
                } else {
                    Some(Self::Fold((exprs.pop().unwrap(), expr)))
                }
            }
            Self::TryFold(_) => {
                if exprs.len() != 1 {
                    None
                } else {
                    Some(Self::TryFold((exprs.pop().unwrap(), expr)))
                }
            }
            other if exprs.is_empty() => match other {
                Self::Map(_) => Some(Self::Map(expr)),
                Self::Filter(_) => Some(Self::Filter(expr)),
                Self::AndThen(_) => Some(Self::AndThen(expr)),
                Self::Then(_) => Some(Self::Then(expr)),
                Self::Inspect(_) => Some(Self::Inspect(expr)),
                Self::All(_) => Some(Self::All(expr)),
                Self::Any(_) => Some(Self::Any(expr)),
                Self::Chain(_) => Some(Self::Chain(expr)),
                Self::Cmp(_) => Some(Self::Cmp(expr)),
                Self::Eq(_) => Some(Self::Eq(expr)),
                Self::FilterMap(_) => Some(Self::FilterMap(expr)),
                Self::Find(_) => Some(Self::Find(expr)),
                Self::FindMap(_) => Some(Self::FindMap(expr)),
                Self::FlatMap(_) => Some(Self::FlatMap(expr)),
                Self::ForEach(_) => Some(Self::ForEach(expr)),
                Self::Ge(_) => Some(Self::Ge(expr)),
                Self::Gt(_) => Some(Self::Gt(expr)),
                Self::IsSortedBy(_) => Some(Self::IsSortedBy(expr)),
                Self::IsSortedByKey(_) => Some(Self::IsSortedByKey(expr)),
                Self::Le(_) => Some(Self::Le(expr)),
                Self::Lt(_) => Some(Self::Lt(expr)),
                Self::MaxBy(_) => Some(Self::MaxBy(expr)),
                Self::MaxByKey(_) => Some(Self::MaxByKey(expr)),
                Self::MinBy(_) => Some(Self::MinBy(expr)),
                Self::MinByKey(_) => Some(Self::MinByKey(expr)),
                Self::Ne(_) => Some(Self::Ne(expr)),
                Self::Nth(_) => Some(Self::Nth(expr)),
                Self::PartialCmp(_) => Some(Self::PartialCmp(expr)),
                Self::Partition(_) => Some(Self::Partition(expr)),
                Self::PartitionInPlace(_) => Some(Self::PartitionInPlace(expr)),
                Self::Position(_) => Some(Self::Position(expr)),
                Self::Rposition(_) => Some(Self::Rposition(expr)),
                Self::Scan(_) => Some(Self::Scan(expr)),
                Self::Skip(_) => Some(Self::Skip(expr)),
                Self::SkipWhile(_) => Some(Self::SkipWhile(expr)),
                Self::StepBy(_) => Some(Self::StepBy(expr)),
                Self::Take(_) => Some(Self::Take(expr)),
                Self::TakeWhile(_) => Some(Self::TakeWhile(expr)),
                Self::TryForEach(_) => Some(Self::TryForEach(expr)),
                Self::Zip(_) => Some(Self::Zip(expr)),
                _ => None,
            },
            _ => None,
        })
    }

    fn is_replaceable(&self) -> bool {
        match self {
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
            | Self::Rev => false,
            _ => true,
        }
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
    fn it_tests_extract_inner_trait_impl_for_process_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };

        for process_expr in vec![
            ProcessExpr::Map(expr.clone()),
            ProcessExpr::Filter(expr.clone()),
            ProcessExpr::Dot(expr.clone()),
            ProcessExpr::AndThen(expr.clone()),
            ProcessExpr::Then(expr.clone()),
            ProcessExpr::Chain(expr.clone()),
            ProcessExpr::Inspect(expr.clone()),
            ProcessExpr::FilterMap(expr.clone()),
            ProcessExpr::Partition(expr.clone()),
            ProcessExpr::Find(expr.clone()),
            ProcessExpr::FindMap(expr.clone()),
            ProcessExpr::Zip(expr.clone()),
        ]
        .into_iter()
        {
            assert_eq!(process_expr.extract_inner().clone(), Some(vec![&expr]));
        }

        assert_eq!(
            ProcessExpr::Fold((expr.clone(), expr.clone()))
                .extract_inner()
                .clone(),
            Some(vec![&expr, &expr])
        );

        assert_eq!(
            ProcessExpr::TryFold((expr.clone(), expr.clone()))
                .extract_inner()
                .clone(),
            Some(vec![&expr, &expr])
        );
    }

    #[test]
    fn it_tests_replace_inner_trait_impl_for_process_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };
        let replace_inner: Expr = parse_quote! { |v| 3 + v };

        for process_expr in vec![
            ProcessExpr::Map(expr.clone()),
            ProcessExpr::Filter(expr.clone()),
            ProcessExpr::AndThen(expr.clone()),
            ProcessExpr::Then(expr.clone()),
            ProcessExpr::Chain(expr.clone()),
            ProcessExpr::Inspect(expr.clone()),
            ProcessExpr::FilterMap(expr.clone()),
            ProcessExpr::Partition(expr.clone()),
            ProcessExpr::Find(expr.clone()),
            ProcessExpr::FindMap(expr.clone()),
            ProcessExpr::Zip(expr.clone()),
        ]
        .into_iter()
        {
            assert_eq!(
                process_expr
                    .replace_inner(&mut vec![replace_inner.clone()])
                    .unwrap()
                    .extract_inner()
                    .clone(),
                Some(vec![&replace_inner])
            );
        }

        assert_eq!(
            ProcessExpr::Dot(expr.clone()).replace_inner(&mut vec![replace_inner.clone()]),
            None
        );

        assert_eq!(
            ProcessExpr::Fold((expr.clone(), expr.clone()))
                .replace_inner(&mut vec![replace_inner.clone(), replace_inner.clone()])
                .unwrap()
                .extract_inner()
                .clone(),
            Some(vec![&replace_inner, &replace_inner])
        );

        assert_eq!(
            ProcessExpr::TryFold((expr.clone(), expr.clone()))
                .replace_inner(&mut vec![replace_inner.clone(), replace_inner.clone()])
                .unwrap()
                .extract_inner()
                .clone(),
            Some(vec![&replace_inner, &replace_inner])
        );
    }

    #[test]
    fn it_tests_to_tokens_trait_impl_for_process_expr() {
        let expr: Expr = parse_quote! { |v| v + 1 };

        for process_expr in vec![
            ProcessExpr::Map(expr.clone()),
            ProcessExpr::Filter(expr.clone()),
            ProcessExpr::Dot(expr.clone()),
            ProcessExpr::AndThen(expr.clone()),
            ProcessExpr::Then(expr.clone()),
            ProcessExpr::Chain(expr.clone()),
            ProcessExpr::FilterMap(expr.clone()),
            ProcessExpr::Partition(expr.clone()),
            ProcessExpr::Fold((expr.clone(), expr.clone())),
            ProcessExpr::TryFold((expr.clone(), expr.clone())),
            ProcessExpr::Find(expr.clone()),
            ProcessExpr::FindMap(expr.clone()),
            ProcessExpr::Zip(expr.clone()),
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
                match process_expr {
                    ProcessExpr::AndThen(expr) => {
                        quote! { .and_then(#expr) }
                    }
                    ProcessExpr::Map(expr) => {
                        quote! { .map(#expr) }
                    }
                    ProcessExpr::Dot(expr) => {
                        quote! { .#expr }
                    }
                    ProcessExpr::Filter(expr) => {
                        quote! { .filter(#expr) }
                    }
                    ProcessExpr::Then(expr) => {
                        quote! {{ let __handler = #expr; __handler }}
                    }
                    //
                    // Not used for now because returning closure requires bound lifetimes
                    //
                    ProcessExpr::Inspect(_) => unimplemented!(),
                    ProcessExpr::Chain(expr) => {
                        quote! { .chain(#expr) }
                    }
                    ProcessExpr::Collect(type_spec) => {
                        quote! { .collect::<#type_spec>() }
                    }
                    ProcessExpr::Enumerate => {
                        quote! { .enumerate() }
                    }
                    ProcessExpr::FilterMap(expr) => {
                        quote! { .filter_map(#expr) }
                    }
                    ProcessExpr::Find(expr) => {
                        quote! { .find(#expr) }
                    }
                    ProcessExpr::FindMap(expr) => {
                        quote! { .find_map(#expr) }
                    }
                    ProcessExpr::Flatten => {
                        quote! { .flatten() }
                    }
                    ProcessExpr::Fold((init, expr)) => {
                        quote! { .fold(#init, #expr) }
                    }
                    ProcessExpr::Partition(expr) => {
                        quote! { .partition(#expr) }
                    }
                    ProcessExpr::TryFold((init, expr)) => {
                        quote! { .try_fold(#init, #expr) }
                    }
                    ProcessExpr::Unzip(type_spec) => type_spec
                        .as_ref()
                        .map(|type_spec| quote! { .unzip::<#( #type_spec ),*>() })
                        .unwrap_or_else(|| quote! { .unzip() }),
                    ProcessExpr::Zip(expr) => {
                        quote! { .zip(#expr) }
                    }
                }
            ));
        }

        assert!(::std::panic::catch_unwind(
            move || ProcessExpr::Inspect(expr.clone()).into_token_stream()
        )
        .is_err());
    }
}
