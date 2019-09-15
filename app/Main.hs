{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.Except         (runExceptT)

import qualified Data.Conduit                  as C
import           Data.Conduit                  ((.|))
import qualified Data.Conduit.Combinators      as CC
import qualified Data.Csv                      as CSV
import qualified Data.Csv.Conduit              as CSVC
import           Data.Char                     (ord)
import qualified Data.IntMap.Strict            as IM

import           Data.Teryt.Hierarchy
import           Data.Teryt.NTS

import           System.Environment            (getArgs)
import           System.Exit                   (exitFailure)

main :: IO ()
main = do
  [path] <- getArgs
  parseResult <- runResourceT $ runExceptT $ C.runConduit $ pipeline path
  sortedByLevel <- expectRight parseResult
  let rootData = NtsData { name     = "ROOT"
                         , type_    = NtRoot
                         , validOn  = "00-00-0000"
                         }
  hierarchy <- expectRight $ mkAmbiguousHierarchy rootData sortedByLevel
  printAmbiguousHierarchy printNtsData hierarchy
  where
    pipeline path =  CC.sourceFile path
                  .| parse
                  .| CC.foldl addToLevelBucket IM.empty
    parse = CSVC.fromCsv @(AmbiguousHierarchy NtsData) semicolonDelimiter CSV.HasHeader
    semicolonDelimiter = CSV.DecodeOptions { CSV.decDelimiter = toEnum $ ord ';' }
    printNtsData NtsData{ name, type_ } =
      name <> " (" <> ntsTypeToText type_ <> ")"

expectRight :: Show e => Either e a -> IO a
expectRight = \case
  Left e -> do
    print e
    exitFailure
  Right a ->
    pure a

conduitDrop :: Monad m => Int -> C.ConduitT a a m ()
conduitDrop n = go 0
  where
    go i = C.await >>= \case
      Nothing -> pure ()
      Just a  ->
        if i < n then
          go (i+1)
        else do
          C.yield a
          go i
