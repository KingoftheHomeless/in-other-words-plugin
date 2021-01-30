# in-other-words-plugin

## Overview

A typechecker plugin that can disambiguate "obvious" uses of effects when using
[`in-other-words`](https://hackage.haskell.org/package/in-other-words).


## Example

Consider the following program:

```haskell
foo :: Eff (State Int) m => m ()
foo = put 10
```

What does this program do? Any human will tell you that it changes the state of
the `Int` to 10, which is clearly what's meant.

Unfortunately, `in-other-words` can't work this out on its own. Its reasoning is
"maybe you wanted to change some other `State` effect which is *also* a `Num`,
but you just forgot to add some `Eff`/`s` constraints for it."

This is obviously insane, but it's the way the cookie crumbles.
`in-other-words-plugin` is a typechecker plugin which will disambiguate the above
program (and others) so the compiler will do what you want.


## Usage

Add the following line to your package configuration:

```
ghc-options: -fplugin=Control.Effect.Plugin
```


## Limitations

`in-other-words-plugin` will only disambiguate effects if there is exactly one
relevant constraint in scope. For example, it will *not* disambiguate the
following program:

```haskell
bar :: Effs '[ State Int
             , State Double
             ] m
    => m ()
bar = put 10
```

because it is now unclear whether you're attempting to set the `Int` or the
`Double`. Instead, you can manually write a type application in this case.

```haskell
bar :: Effs '[ State Int
             , State Double
             ] m
    => m ()
bar = put @Int 10
```

## Acknowledgments

This plugin and this README is copied almost verbatim from
[`polysemy-plugin`](https://hackage.haskell.org/package/polysemy-plugin),
which itself is copied almost verbatim from [`simple-effects`](https://hackage.haskell.org/package/simple-effects).

