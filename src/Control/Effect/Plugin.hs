{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

------------------------------------------------------------------------------
-- | A typechecker plugin that can disambiguate "obvious" uses of effects in
-- @in-other-words@.
--
-- __Example:__
--
-- Consider the following program:
--
-- @
-- foo :: 'Control.Effect.Eff' ('Control.Effect.State.State' Int) m => m ()
-- foo = 'Control.Effect.State.put' 10
-- @
--
-- What does this program do? Any human will tell you that it changes the state
-- of the 'Int' to 10, which is clearly what's meant.
--
-- Unfortunately, @in-other-words@ can't work this out on its own. Its reasoning is
-- "maybe you wanted to change some other 'Control.Effect.State.State' effect which
-- is /also/ a 'Num', but you just forgot to add a 'Eff' constraint
-- for it."
--
-- This is obviously insane, but it's the way the cookie crumbles.
-- 'Control.Effect.Plugin' is a typechecker plugin which will disambiguate the above
-- program (and others) so the compiler will do what you want.
--
-- __Usage:__
--
-- Add the following line to your package configuration:
--
-- @
-- ghc-options: -fplugin=Control.Effect.Plugin
-- @
--
-- __Limitations:__
--
-- The 'Control.Effect.Plugin' will only disambiguate effects if there is exactly one
-- relevant constraint in scope. For example, it will /not/ disambiguate the
-- following program:
--
-- @
-- bar :: 'Control.Effect.Effs' \'[ 'Control.Effect.State.State' Int, 'Control.Effect.State.State' Double ] m => m ()
-- bar = 'Control.Effect.State.put' 10
-- @
--
-- because it is now unclear whether you're attempting to set the 'Int' or the
-- 'Double'. Instead, you can manually write a type application in this case.
--
-- @
-- bar :: 'Control.Effect.Effs' \'[ 'Control.Effect.State.State' Int, 'Control.Effect.State.State' Double ] m => m ()
-- bar = 'Control.Effect.State.put' @Int 10
-- @
--
module Control.Effect.Plugin
  ( plugin
  ) where

import Control.Effect.Plugin.Fundep

import GhcPlugins

------------------------------------------------------------------------------
plugin :: Plugin
plugin = defaultPlugin
    { tcPlugin = const $ Just fundepPlugin
#if __GLASGOW_HASKELL__ >= 806
    , pluginRecompile  = purePlugin
#endif
    }
