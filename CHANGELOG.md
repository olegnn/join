# [0.1.0-alpha.8]

Reassigned filter combinator `?>`, reassigned inspect combinator (`??`). Dot combinator now has two possible forms: (`>.`) and (`..`). Added new combinators:

- Collect
- Chain
- FindMap
- FilterMap
- Enumerate
- Partition
- Flatten
- Fold
- TryFold
- Find
- Zip
- Unzip

Defined `full` and `static` features for `join_impl`. More generic implementation. `DefaultExpr` now is `ErrExpr`. Updated docs.

# [0.1.0-alpha.7]

Refactored `ActionExprChain`

# [0.1.0-alpha.6]

`MapErr` is `DefaultExpr`, updated docs and internal macros

# [0.1.0-alpha.5]

Added more tests, fixed docs + added travis

# [0.1.0-alpha.4]

Wrap future into `Box::pin(...)` + fixed join_async with one branch, small documentation fixes

# [0.1.0-alpha.3]

Docs hotfix

# [0.1.0-alpha.2] 

Added filter (`@>`)

# [0.1.0-alpha.1]

Initial