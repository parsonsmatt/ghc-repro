{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}

module Main where

import           Control.Monad.Fix
import           Data.String            (IsString (..))
import           Data.Typeable
import           GHC.Exts               (Constraint, coerce)
import           GHC.TypeLits

import Control.Monad.ST

theThing :: ST s ()
theThing = pure ()

weirdlyLocal :: ST s ()
weirdlyLocal = theThing

runSTIO :: (forall s. ST s a) -> IO a
runSTIO x = pure (runST x)

thisWorks :: IO ()
thisWorks = mdo
    let weirdlyLocal = theThing
    runSTIO weirdlyLocal
    runSTIO weirdlyLocal

thisBreaks :: IO ()
thisBreaks = mdo
    runSTIO weirdlyLocal
    let weirdlyLocal = theThing
    runSTIO weirdlyLocal

thisIsFine :: IO ()
thisIsFine = mdo
    runSTIO weirdlyLocal
    let asdf = theThing
    runSTIO asdf
