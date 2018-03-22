# Classy Miso

[Miso](https://hackage.haskell.org/package/miso) is a tasty web framework for Haskell. It was originally inspired by
[Elm](http://elm-lang.org/) (or, at least, [The Elm Architecture (TEA)](https://guide.elm-lang.org/architecture/)).
It also emphasizes isomorphic (ie: server-side) rendering, which is pretty awesome if you actually want to do SEO and
page caching. Miso is really quite nice...but it wasn't quite Haskell-y enough for me!

Unfortunately for Elm devs, Elm doesn't have typeclasses or type families. Fortunately for Haskell devs, we do!
We also have the `IO` monad, JavaScript FFI, and a whole slew of other nifty tricks, which makes Haskell a much
nicer place to implement TEA (IMHO).

This project contains an typeclassification of TEA. It is a work in progress, but the intention is
for it to be accessible to beginners and to make the easy things easy, but with enough hooks and bells and whistles to
make the hard things possible.

The primary typeclass is `Component`, which is your basic participation in Miso's messaging and view systems. The
methods on the typeclass correlate strongly with TEA, and the correlaries should be obvious to anyone who has read the
TEA architecture guide.

Once you have your components defined, then you can actually drive them through `MisoApp`. That puts together the
routing at the topmost level and gives you a couple of functions to actually start the app up.
