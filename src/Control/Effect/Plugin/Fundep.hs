{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE CPP                        #-}

------------------------------------------------------------------------------
-- The MIT License (MIT)
--
-- Copyright (c) 2017 Luka Horvat, 2019 Sandy Maguire, 2020 Love Waern
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
-- sell copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
-- IN THE SOFTWARE.
--
------------------------------------------------------------------------------
--
-- This module is an adaptation of 'Polysemy.Plugin' from the 'polysemy-plugin' package,
-- by Sandy Maguire.
-- That module was, in turn, originally based on 'Control.Effects.Plugin' from the
-- 'simple-effects' package, by Luka Horvat.
--
-- https://gitlab.com/LukaHorvat/simple-effects/commit/966ce80b8b5777a4bd8f87ffd443f5fa80cc8845#f51c1641c95dfaa4827f641013f8017e8cd02aab

module Control.Effect.Plugin.Fundep (fundepPlugin) where

import           Control.Monad
import           Data.Bifunctor
import           Data.IORef
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Control.Effect.Plugin.Fundep.Unification
import           Control.Effect.Plugin.Fundep.Utils
import           TcEvidence
import           TcPluginM (tcPluginIO, tcLookupClass)
import           TcRnTypes
#if __GLASGOW_HASKELL__ >= 810
import           Constraint
#endif
import           TcSMonad hiding (tcLookupClass)
import           Type

import GHC (Class, mkModuleName)
import GHC.TcPluginM.Extra (lookupName)
import OccName (mkTcOcc)
import Packages (lookupModuleWithSuggestions, LookupResult (..))
import Outputable (pprPanic, text, (<+>), ($$))

getMemberClass :: TcPluginM Class
getMemberClass = do
  dflags <- unsafeTcPluginTcM getDynFlags

  let error_msg = pprPanic "in-other-words-plugin"
          $ text ""
         $$ text "--------------------------------------------------------------------------------"
         $$ text "`in-other-words-plugin` is loaded, but"
        <+> text "`in-other-words` isn't available as a package."
         $$ text "Probable fix: add `in-other-words` to your cabal `build-depends`"
         $$ text "--------------------------------------------------------------------------------"
         $$ text ""
  let lookupRes = lookupModuleWithSuggestions
                    dflags
                    (mkModuleName "Control.Effect.Internal.Membership")
                    (Just "in-other-words")
  case lookupRes of
    LookupFound md _ -> do
      nm <- lookupName md (mkTcOcc "Member")
      tcLookupClass nm
    _                -> error_msg

fundepPlugin :: TcPlugin
fundepPlugin = TcPlugin
  { tcPluginInit =
      (,) <$> tcPluginIO (newIORef S.empty)
          <*> getMemberClass
  , tcPluginSolve = solveFundep
  , tcPluginStop = const $ pure ()
  }


------------------------------------------------------------------------------
-- | Corresponds to a 'Control.Effect.Internal.Membership.Member' constraint. For example,
-- given @Member (State s) r@, we would get:
data MemberConstraint = MemberConstraint
  { mcLoc        :: CtLoc
  , mcEffectName :: Type  -- ^ @State@
  , mcEffect     :: Type  -- ^ @State s@
  , mcRow        :: Type  -- ^ @r@
  }


------------------------------------------------------------------------------
-- | Given a list of constraints, filter out the 'MemberConstraint's.
getMemberConstraints :: Class -> [Ct] -> [MemberConstraint]
getMemberConstraints cls cts = do
  cd@CDictCan{cc_class = cls', cc_tyargs = [_, eff, r]} <- cts
  guard $ cls == cls'
  pure $ MemberConstraint
    { mcLoc = ctLoc cd
    , mcEffectName = getEffName eff
    , mcEffect = eff
    , mcRow = r
    }


------------------------------------------------------------------------------
-- | If there's only a single @Member@ in the same @r@ whose effect name
-- matches and could possibly unify, return its effect (including tyvars.)
findMatchingEffectIfSingular
    :: MemberConstraint
    -> [MemberConstraint]
    -> Maybe Type
findMatchingEffectIfSingular (MemberConstraint _ eff_name wanted r) ts =
  singleListToJust $ do
    MemberConstraint _ eff_name' eff' r' <- ts
    guard $ eqType eff_name eff_name'
    guard $ eqType r r'
    guard $ canUnifyRecursive FunctionDef wanted eff'
    pure eff'


------------------------------------------------------------------------------
-- | Given an effect, compute its effect name.
getEffName :: Type -> Type
getEffName t = fst $ splitAppTys t


------------------------------------------------------------------------------
-- | Generate a wanted unification for the effect described by the
-- 'MemberConstraint' and the given effect.
mkWantedForce
  :: MemberConstraint
  -> Type
  -> TcPluginM (Unification, Ct)
mkWantedForce mc given = do
  (ev, _) <- unsafeTcPluginTcM
           . runTcSDeriveds
           $ newWantedEq (mcLoc mc) Nominal wanted given
  pure ( Unification (OrdType wanted) (OrdType given)
       , CNonCanonical ev
       )
  where
    wanted = mcEffect mc

------------------------------------------------------------------------------
-- | Generate a wanted unification for the effect described by the
-- 'MemberConstraint' and the given effect --- if they can be unified in this
-- context.
mkWanted
    :: MemberConstraint
    -> SolveContext
    -> Type  -- ^ The given effect.
    -> TcPluginM (Maybe (Unification, Ct))
mkWanted mc solve_ctx given =
  whenA (not (mustUnify solve_ctx) || canUnifyRecursive solve_ctx wanted given) $
    mkWantedForce mc given
  where
    wanted = mcEffect mc


------------------------------------------------------------------------------
-- | Determine if there is exactly one wanted find for the @r@ in question.
exactlyOneWantedForR
    :: [MemberConstraint]  -- ^ Wanted finds
    -> Type              -- ^ Effect row
    -> Bool
exactlyOneWantedForR wanteds
    = fromMaybe False
    . flip M.lookup singular_r
    . OrdType
  where
    singular_r = M.fromList
               -- TODO(sandy)/(KingoftheHomeless):
               -- Nothing fails if this is just @second (const
               -- True)@. Why not? Incomplete test suite, or doing too much
               -- work?
               . fmap (second (/= 1))
               . countLength
               $ OrdType . mcRow <$> wanteds


solveFundep
    :: ( IORef (S.Set Unification)
       , Class
       )
    -> [Ct]
    -> [Ct]
    -> [Ct]
    -> TcPluginM TcPluginResult
solveFundep _ _ _ [] = pure $ TcPluginOk [] []
solveFundep (ref, cls) given _ wanted = do
  let wanted_finds = getMemberConstraints cls wanted
      given_finds  = getMemberConstraints cls given

  eqs <- forM wanted_finds $ \mc -> do
    let r  = mcRow mc
    case findMatchingEffectIfSingular mc given_finds of
      -- We found a real given, therefore we are in the context of a function
      -- with an explicit @Member e r@ constraint. We also know it can
      -- be unified (although it may generate unsatisfiable constraints).
      Just eff' -> Just <$> mkWantedForce mc eff'

      -- Otherwise, check to see if @r ~ (e ': r')@. If so, pretend we're
      -- trying to solve a given @Member e r@. But this can only happen in the
      -- context of an interpreter!
      Nothing ->
        case splitAppTys r of
          (_, [_, eff', _]) ->
            mkWanted mc
                     (InterpreterUse $ exactlyOneWantedForR wanted_finds r)
                     eff'
          _ -> pure Nothing

  -- We only want to emit a unification wanted once, otherwise a type error can
  -- force the type checker to loop forever.
  already_emitted <- tcPluginIO $ readIORef ref
  let (unifications, new_wanteds) = unzipNewWanteds already_emitted $ catMaybes eqs
  tcPluginIO $ modifyIORef ref $ S.union $ S.fromList unifications

  pure $ TcPluginOk [] new_wanteds
