# [0.1.1]

Misc docs fixes.

# [0.1.0]

Initial release. Internal implementation updates.

# [0.1.0-beta.8]

Fixed features implementations. Enabled all-features tests. Implementation changes. Cosmetic fixes.

# [0.1.0-beta.7]

Fixed incorrect docs link.

# [0.1.0-beta.6]

Updated Cargo.toml of `join` package.

# [0.1.0-beta.5]

Updated docs + internal improvements.

# [0.1.0-beta.4]

Added `custom_joiner`, `transpose_results`, `lazy_branches` custom configuration options.

# [0.1.0-beta.3]

Use combinators instead of `async` fn to spawn tokio tasks and send value. Updated docs.

# [0.1.0-beta.2]

Fixed docs. Restructured implementation.

# [0.1.0-beta.1]

Updated docs. Small refactoring.
s
# [0.1.0-alpha.17]

Optimized steps: branches which are not active during step are excluded from code. Fixed `join_spawn!` last step behaviour. 

# [0.1.0-alpha.16]

Handler definition moved to the beginning of generated code. `try` macros will abort execution in case of `None`/`Err`  value at the end of any step. Use `::futures::try_join` in `try` `async` macros. Updated docs. Small refactoring.

# [0.1.0-alpha.15]

Fixed possible wrapper list.

# [0.1.0-alpha.14]

Corrected readme.

# [0.1.0-alpha.13]

Added nested combinators `>>>` and `<<<`. Chain now has `>@>` group identifier.

# [0.1.0-alpha.12]

Documentation fixes + small refactoring.

# [0.1.0-alpha.11]

Added `try_join!`, `try_join_async!`, `try_join_spawn!`, `try_join_async_spawn!`, `try_async_spawn!` which act as previously unprefixed macros. 

`join!`, `join_async!`, `join_spawn!`, `join_async_spawn!`, `async_spawn!` now don't have `handler` and **don't** transpose final tuple values.

# [0.1.0-alpha.10]

Bugfix + updated docs. Assert docs tests.

# [0.1.0-alpha.9]

Bug hotfix + minor docs fixes. More tests.

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

Refactored `ActionExprChain`.

# [0.1.0-alpha.6]

`MapErr` is `DefaultExpr`, updated docs and internal macros.

# [0.1.0-alpha.5]

Added more tests, fixed docs + added travis.

# [0.1.0-alpha.4]

Wrap future into `Box::pin(...)` + fixed join_async with one branch, small documentation fixes.

# [0.1.0-alpha.3]

Docs hotfix.

# [0.1.0-alpha.2] 

Added filter (`@>`).

# [0.1.0-alpha.1]

Initial.