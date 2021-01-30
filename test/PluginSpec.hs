{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module PluginSpec where

import Data.Functor.Identity
import GHC.Exts
import Control.Effect
import Control.Effect.Reader
import Control.Effect.Error
import Control.Effect.State
import Control.Effect.Writer
import Test.Hspec

data Accum s :: Effect where
  Look :: Accum s m s
  Accum :: s -> Accum s m ()

type AskListC i = CompositionC
 '[ ReinterpretSimpleC (Ask i) '[State [i], Throw ()]
  , StateC [i]
  , ThrowC ()
  ]

runAskList :: ( Carrier m
              , Threaders '[ReaderThreads, ErrorThreads, StateThreads] m p
              )
           => [i]
           -> AskListC i m a
           -> m (Maybe a)
runAskList l =
   fmap (either (const Nothing) Just)
 . runThrow
 . evalState l
 . reinterpretSimple (\case
     Ask -> get >>= \case
       (x:xs) -> put xs >> return x
       _      -> throw mempty
   )
 . runComposition

runAccum :: ( Carrier m
            , Monoid s
            , Threaders '[ReaderThreads, StateThreads] m p
            )
         => ReinterpretSimpleC (Accum s) '[State s] (StateC s m) a
         -> m (s, a)
runAccum =
   runState mempty
 . reinterpretSimple (\case
     Look -> get
     Accum s -> modify' (<> s)
   )

idState :: Eff (State s) m => m ()
idState = do
  s <- get
  put s

intState :: Eff (State Int) m => m ()
intState = put 10

numState :: Num a => Eff (State a) m => m ()
numState = put 10

strState :: Eff (State String) m => m ()
strState = put "hello"

oStrState :: IsString a => Eff (State a) m => m ()
oStrState = put "hello"


err :: Eff (Error e) m => m Bool
err =
  catch
    (throw undefined)
    (\_ -> pure True)


errState :: Num s => Effs '[Error e, State s] m => m Bool
errState = do
  numState
  err


lifted :: Monad b => Eff (Embed b) m => m ()
lifted = embed $ pure ()


newtype MyString = MyString String
  deriving (IsString, Eq, Show)




spec :: Spec
spec = do
  describe "State effect" $ do
    describe "get/put" $ do
      it "should work in simple cases" $ do
        flipShouldBe (True, ()) . run $ runState True idState

      it "should, when polymorphic, eliminate the first matching effect" $ do
        flipShouldBe (False, (True, ()))   . run $ runState False $ runState True idState

      it "should, when polymorphic, not eliminate unmatching effects" $ do
        flipShouldBe (True, Right @Int ()) . run $ runState True $ runError idState

    describe "numbers" $ do
      it "should interpret against concrete Int" $ do
        flipShouldBe (10, ()) . run $ runState 0 intState

      describe "polymorphic Num constraint" $ do
        it "should interpret against Int" $ do
          flipShouldBe (10 :: Int, ())     . run $ runState 0 numState

        it "should interpret against Float" $ do
          flipShouldBe (10 :: Float, ())   . run $ runState 0 numState

        it "should interpret against Double" $ do
          flipShouldBe (10 :: Double, ())  . run $ runState 0 numState

        it "should interpret against Integer" $ do
          flipShouldBe (10 :: Integer, ()) . run $ runState 0 numState

    describe "strings" $ do
      it "concrete interpret against concrete String" $ do
        flipShouldBe ("hello", ()) . run $ runState "nothing" strState

      describe "polymorphic IsString constraint" $ do
        it "should interpret against String" $ do
          flipShouldBe ("hello" :: String, ())   . run $ runState "nothing" oStrState

        it "should interpret against MyString" $ do
          flipShouldBe ("hello" :: MyString, ()) . run $ runState "nothing" oStrState


  describe "Error effect" $ do
    it "should interpret against Int" $ do
      flipShouldBe (Right @Int True)  . run $ runError err
    it "should interpret against Bool" $ do
      flipShouldBe (Right @Bool True) . run $ runError err


  describe "State/Error effect" $ do
    it "should interpret against Int/String" $ do
      flipShouldBe (10 :: Int, Right @String True)  . run $ runState 0 $ runError errState
    it "should interpret against Float/Bool" $ do
      flipShouldBe (10 :: Float, Right @Bool True)  . run $ runState 0 $ runError errState


  describe "Error/State effect" $ do
    it "should interpret against String/Int" $ do
      flipShouldBe (Right @String (10 :: Int, True))  . run $ runError $ runState 0 errState
    it "should interpret against Bool/Float" $ do
      flipShouldBe (Right @Bool (10 :: Float, True))  . run $ runError $ runState 0 errState

  describe "Tell effect" $ do
    it "should unify recursively with tyvars" $ do
      flipShouldBe 11 . sum . fst . run . runTell $ do
        tell [1]
        tell $ replicate 2 5


  describe "Embed effect" $ do
    it "should interpret against IO" $ do
      res <- runM lifted
      res `shouldBe` ()

    it "should interpret against Identity" $ do
      let res = runM lifted
      res `shouldBe` Identity ()


flipShouldBe :: (Show a, Eq a) => a -> a -> Expectation
flipShouldBe = flip shouldBe
