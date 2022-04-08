# Macro Tower

A macro expander based on the system presented in Macroexpansion Reflective Tower.

Difference to the Macroexpansion Reflective Tower:
- `install-macro-form!` uses an a-list for the macro environments instead of functions for easier debugging.
- `pure-meaning` is not used for extracting defined symbols before expansion to preallocate boxes for them. Instead, the expansion environment is updated on each level right after evaluation of the expanded code in that level.

Caveats:
- `really-expand` is using the same immutable environment for all subexpressions of an expression, meaning that no macro definitions are applied in the other subexpressions at the same level.
