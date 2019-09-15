{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import           Control.Applicative          (empty)
import           Control.Monad                (guard, foldM)
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.Except         (runExceptT)

import qualified Data.Conduit                  as C
import           Data.Conduit                  ((.|))
import qualified Data.Conduit.Combinators      as CC
import qualified Data.Csv                      as P
import           Data.Csv                      ((.!))
import qualified Data.Csv.Conduit              as CP
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TI
import           Data.Text                     (Text)
import           Data.Char                     (ord)
import qualified Data.IntMap.Strict            as IM
import           Data.IntMap.Strict            (IntMap)
import           Data.Foldable                 (for_)
import qualified Data.NonEmpty                 as NE

import           System.Environment            (getArgs)
import           System.Exit                   (exitFailure)

data UnitType
  = UtRoot
  | UtRegion
  | UtWojewodztwo
  | UtPodregion
  | UtPowiat
  | UtMiastoNaPrawachPowiatu
  | UtGminaMiejska
  | UtGminaWiejska
  | UtGminaMiejskoWiejska
  | UtMiastoWGminieMiejskoWiejskiej
  | UtObszarWiejski
  | UtDzielnica
  | UtDelegatura
  deriving Show


data AdministrativeUnit
  = AdministrativeUnit
  { path     :: [Int]
  , name     :: Text
  , type_    :: UnitType
  , validOn  :: Text
  , children :: IntMap (NE.T [] AdministrativeUnit)
  } deriving Show

instance P.FromRecord AdministrativeUnit where
  parseRecord v =
    AdministrativeUnit <$> parsePath (v .! 0)
                                     [v .! 1, v .! 2, v .! 3, v .! 4, v .! 5]
                       <*> v .! 7
                       <*> parseType (v .! 8) (v .! 6)
                       <*> v .! 9
                       <*> pure IM.empty
    where
      parsePath levelP eltPs = do
        level <- levelP
        guard $ level > 0
        path <- parsePathElements eltPs
        guard $ length path == level
        pure path

      parsePathElements = go False []
        where
          go seenNothing pathSoFar = \case
            [] ->
              pure $ reverse pathSoFar
            eltP : eltPs -> do
              elt <- eltP
              case elt of
                Nothing                  -> go True pathSoFar eltPs
                Just i | not seenNothing -> go False (i : pathSoFar) eltPs
                       | otherwise       -> empty

      parseType :: P.Parser Text -> P.Parser (Maybe Int) -> P.Parser UnitType
      parseType typeNameP typeIdP = (,) <$> typeNameP <*> typeIdP >>= \case
        ("region",                            Nothing) -> pure UtRegion
        ("województwo",                       Nothing) -> pure UtWojewodztwo
        ("podregion",                         Nothing) -> pure UtPodregion
        ("powiat",                            Nothing) -> pure UtPowiat
        ("miasto na prawach powiatu",         Nothing) -> pure UtMiastoNaPrawachPowiatu
        ("gmina miejska",                     Just 1 ) -> pure UtGminaMiejska
        ("gmina wiejska",                     Just 2 ) -> pure UtGminaWiejska
        ("gmina miejsko-wiejska",             Just 3 ) -> pure UtGminaMiejskoWiejska
        ("miasto w gminie miejsko-wiejskiej", Just 4 ) -> pure UtMiastoWGminieMiejskoWiejskiej
        ("obszar wiejski",                    Just 5 ) -> pure UtObszarWiejski
        ("dzielnica",                         Just 8 ) -> pure UtDzielnica
        ("delegatura",                        Just 9 ) -> pure UtDelegatura
        _                                              -> empty

sortByLevel :: IntMap [AdministrativeUnit]
            -> AdministrativeUnit
            -> IntMap [AdministrativeUnit]
sortByLevel m unit@AdministrativeUnit{ path } =
  IM.alter addToBucket (length path) m
  where
    addToBucket = \case
      Nothing    -> Just [unit]
      Just units -> Just (unit : units)

data PathError
  = PeSplitRoot
  | PeMissingLink [Int]
  | PeConflict [Int]
  deriving Show

addToHierarchy :: AdministrativeUnit
               -> AdministrativeUnit
               -> Either PathError AdministrativeUnit
addToHierarchy root unit@AdministrativeUnit{ path } = do
  newRoots <- go [] path (Just $ NE.singleton root)
  case NE.viewL newRoots of
    (newRoot, []) ->
      pure newRoot
    _ ->
      Left PeSplitRoot
  where
    go :: [Int]
       -> [Int]
       -> Maybe (NE.T [] AdministrativeUnit)
       -> Either PathError (NE.T [] AdministrativeUnit)
    go pathTraversed pathAhead maybeCurrent =
      case pathAhead of
        [] ->
          case maybeCurrent of
            Nothing ->
              Right $ NE.singleton unit
            Just units ->
              Right $ NE.flatten units `NE.snoc` unit
        i : is ->
          case maybeCurrent of
            Nothing ->
              Left $ PeMissingLink $ reverse pathTraversed'
            Just units ->
              case NE.viewL units of
                (current@AdministrativeUnit{ children }, []) ->
                  NE.singleton . setChildren current <$> IM.alterF f i children
                _ ->
                  Left $ PeConflict $ reverse pathTraversed'
            where
              setChildren u c = u { children = c }
              f = fmap Just . go pathTraversed' is
              pathTraversed' = i : pathTraversed

printHierarchy :: AdministrativeUnit -> IO ()
printHierarchy = go 0
  where
    go level AdministrativeUnit{ name, type_, children } = do
      TI.putStrLn dsc
      for_ (concatMap NE.flatten $ IM.elems children) $ go (level + 1)
      where
        dsc =  T.replicate (2*level) " "
            <> "- " <> name
            <> " (" <> T.pack (show type_) <> ")"

main :: IO ()
main = do
  [path] <- getArgs
  parseResult <- runResourceT $ runExceptT $ C.runConduit $ pipeline path
  sortedByLevel <- expectRight parseResult
  let units = IM.foldr (++) [] sortedByLevel
  let emptyRoot = AdministrativeUnit { path     = []
                                     , name     = "root"
                                     , type_    = UtRoot
                                     , validOn  = "00-00-0000"
                                     , children = IM.empty
                                     }
  hierarchy <- expectRight $ foldM addToHierarchy emptyRoot units
  printHierarchy hierarchy
  where
    pipeline path =  CC.sourceFile path
                  .| parse
                  .| CC.foldl sortByLevel IM.empty
    parse = CP.fromCsv @AdministrativeUnit semicolonDelimiter P.HasHeader
    semicolonDelimiter = P.DecodeOptions { P.decDelimiter = toEnum $ ord ';' }

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
