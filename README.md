# Usage

The only application to date using `reference-counting` (this package) is
`ghengin`.  Ghengin is a (very) experimental engine where both CPU and GPU
resources required to render games are tracked linearly throughout the engine
(using linear IO).

When developing such a large program using linear types, a way to share
linear resources and interleave their usage in non-trivial ways throughout the
renderer turned out to be critical. That's where `reference-counting` comes in.
At that point in time, I was fortunately reading TAPL2 which has a chapter on
reference counting with linear types which also inspired me to write a draft
version of this library.

`reference-counting` allows a linear resource to be shared:
```
share :: Shareable m a => a %1 -> m (a, a)
```
How is this safe?

First, note that the package is still in an experimental/candidate state where
there are potential unsoundness bugs in the API design.
The point of this candidate release is to receive feedback on the design and its
flaws -- I'm very interested if you discover it is unsound -- it has been a
while since I wrote this first, and some things look suspicious to me now. OTOH
the paged-in-me was mostly sure of them at the time.

Despite my uneasiness, I have to say, as a pratical statement of reliability,
that it has been working wonderfully as a central point in resource management
within the engine[1].

If you are looking for a considerably complex application using
`reference-counting`, you can try to take a look at the engine source code and
hope that your eyes don't bleed out.

If you are looking for a more palpable example, there are two trivial ones in `test/`.

[1] Seriously, after having fully rewritten the Core of the renderer in the
linear IO monad with reference counting I haven't yet encountered resource bugs,
whereas previously there were so many that I decided to rewrite the engine with
linear types...

# Design

## General idea

You can create an alias to a resource, and then `share` that alias
multiple times. You can `forget` aliases of resources. When the last alias is
forgotten, the resource is freed with the given finaliser.

Linearity in the reference counting API guarantees that an aliased resource will
be freed exactly once because, in turn, all aliases of a resource need to be
forgotten exactly once. Moreover, linearity exacts an explicit operation to
share aliases which means we can trivially increment the reference count when
`share` is called.

## More in-depth

The key datatype is `Alias` (opaque, tracks references dynamically under the
hood). Any `a` that satisfies `Aliasable a` can be turned into an `Alias m a` in
a linear `MonadIO`:
```haskell
newAlias :: (a ⊸ μ ()) ⊸ a ⊸ m (Alias μ a)
```
The first argument is a function to free the resource when the last reference to
it is freed, and the second argument is the resource you're creating an alias
for. `Aliasable` should be derived via `Generic` with `deriving anyclass Aliasable`.

When you have an alias of a resource (`Alias m a`, where the `m` is the context
in which the resource can be freed), you can `share` the alias. Sharing an alias
to a resource means there are now more aliases to the resource:
```haskell
share :: Alias μ a %1 -> m (Alias μ a, Alias μ a)
```
In fact, you can `share` anything which is `Shareable`. `Alias`es are trivially
shareable, but other types can be as well, with a little bit of effort. For example,
we can `share` a list of aliases `[Alias Renderer Vk.Buffer]` to get two lists with the same
aliases (`([Alias Renderer Vk.Buffer], [Alias Renderer Vk.Buffer])`).

When you no longer need an alias you can `forget` it. If this is the last alias
to the resource, the finalizer action (the first argument to `newAlias`) will be
called on the aliased value `a`.
```haskell
forget :: Alias μ a %1 -> μ ()
```
Similarly to `share` and `Shareable`, `forget` also works for any `Forgettable` type.
Note that for `Alias`, the context in which `forget` is called has to be the
same where the finalizer action is defined (the `μ` parameter to `Alias`), since
the finalizer will be called if this was the last alias.

### The tricky bits

This becomes way less simple when we allow aliased values to be actually used.
That is, we only care about aliases if we can refer to the original values using
them. Here be dragons.

The two key ways of using the stored value are `get` and `use/useM`.
`useM` follows a more common Haskell pattern:
```haskell
useM :: Alias μ a %1 -> (a %1 -> IO (a, b)) %1 -> IO (Alias μ a, b)
```
Operate on the aliased `a` and keep it aliased plus return the `b` byproduct.
There is also a misnamed `modifyM` function which is `useM` without the `b`...

Then, there's the arguably most difficult to reason about primitive, `get`:
```haskell
get :: Alias μ a %1 -> μ (a, a %1 -> μ ())
```
The `get` primitive will un-alias a resource and get its value.
By case analysis, either this was the last alias to this resource, or it was
not. If it was the last alias, we need to make sure the resource is linearly
freed. If it was not, we need to make sure it *isn't* freed since there are
still aliases to it.

How do we ensure that the resource is freed if this was the last alias, and that
it is not if it wasn't? The trick is to return both the resource and a finalizer
function linearly -- where the finalizer is actually a no-op if there are more
aliases to that same resource.

The returned function needs to be called exactly once on the resource since it
is returned in a linear tuple -- it will either make sure the resource isn't
consumed (because it is fake-consumed by the finaliser), or make sure it is
consumed (because the finaliser will really consume the resource if this was the
last alias).

This sounds like an awful idea. Maybe we could even just get by with `useM`
which seems less troublesome? One thing that worries me is to what extent could
you call the weird-finaliser on something which wasn't related to the resource
returned alongside it? ...

# Notes

> [!CAUTION]
> It seems that Haddock is rendering incorrectly the lollipop symbol (`⊸`) which
> stands for linear arrow (`%1 ->`) in some places. This is quite unfortunate.
> You may have to double check the source to see if an argument is in fact
> linear...

### type alias for alias

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
### opaqueness for soundness?

Perhaps this is very important to explain somehow earlier on or enforce: The
`Aliasable` things should be opaque. That is, you should not be able to easily
modify the value which is aliased into something else. So, good aliasable values
would be e.g.  pointers, handles, buffers, devices, GPU textures, `IORef`s,
etc...

The problem here is the underlying representation of `Alias m a` which will not use
a mutable reference like `IORef` for the `a` across the aliases (I've wondered
before if it should, but it never made sense for my case where the things I was
aliasing were already mutable like `Vk.Buffer`; leaving it as `a` doesn't
preclude the user from stuffing an `IORef` to alias).

So, if the `a` value is changeable, you could end up in a situation where you
have two aliases that should hold the same value, but are different. In effect,
whichever of the two aliases is forgotten last will call the finalizer action on
the `a` value. If these values are different, the finalizer action will depend
on the last forgotten resource... which is very unsound.

### thread safety

We are using `atomic-counter` for counting, but what happens if there two
actions on the same alias at the same time? It may just be that the
responsibility of thread safety when two aliased resources are used at the same
time is of the resource type (it seems that way).

For example, if the aliased resource was an `IORef`, it'd be the caller's
responsibility to use `atomicModifyIORef` if a multi-threaded program were using
multiple aliases to that same ref at once.

