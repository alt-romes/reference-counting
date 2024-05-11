# Notes

Typically you'll want to export a type synonym for `Alias` over the monad you
can free resources in, import `Data.Linear.Alias` qualified (most functions,
such as `share` and `get` are short and are arguably better to be qualified in
general), such that there are no conflicts between the type synonym `Alias` and the
library defined `Alias.Alias` name
```haskell
import qualified Data.Linear.Alias as Alias
type Alias = Alias.Alias Linear.IO -- Choose the monad you can allocate and free this
                                   -- reference counted resource in
```

