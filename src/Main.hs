{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Control.Monad.Fix
import           Data.String            (IsString (..))
import           Data.Typeable
import           GHC.Exts               (Constraint, coerce)
import           GHC.TypeLits

import Control.Monad.ST


weirdlyLocal :: ST s ()
weirdlyLocal = pure ()

runSTIO :: (forall s. ST s a) -> IO a
runSTIO x = pure (runST x)

largerFunction :: IO ()
largerFunction = do
    runSTIO $ do
        weirdlyLocal

    runSTIO $ do
        weirdlyLocal

    pure ()
