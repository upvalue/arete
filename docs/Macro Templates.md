# Macro templates

Arete's native expander gives `#'name` and identifiers inside `` #` `` templates
the lexical scope where the template is written. A helper defined in module A
keeps A's bindings when a transformer in module B calls it. Unquoted expressions
supply their own values and identifiers.

The reader emits `(%syntax-rename name)`. The expander captures its environment
and generates a call to `%make-syntax-rename`. That function finds the binding's
environment and allocates a fresh rename on each evaluation. Using the binding's
environment lets templates in nested scopes share identifiers when no intervening
binding shadows them. Unbound names retain their module for forward references.

Explicit `(rename name)` calls still use the running transformer's environment,
as does the rename procedure passed to a three-argument transformer. Code that
intentionally relies on that dynamic scope should use these explicit operations.

`tests/expander/template-scope.scm` covers helper scope, lexical shadowing, fresh
renames, forward references, and the explicit renaming interfaces. The bootstrap
test loads the same cases after recompiling the core.

Rebuild `heap.boot` with `make heap.boot` after changing the reader's internal
forms or global symbol layout. See [[Psyntax]] for the separate `syntax-case`
bootstrap experiment.
