{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.Teryt.Hierarchy
  ( Hierarchy(..), AmbiguousHierarchy(..)
  , parseHierarchy, parseAmbiguousHierarchy, parsePathWithLength, parsePath
  , addToLevelBucket
  , PathError(..)
  , mkHierarchy, mkAmbiguousHierarchy
  , traverseHierarchy, traverseAmbiguousHierarchy
  ) where

import           Control.Monad         (guard, mzero, foldM)

import qualified Data.Csv              as CSV
import           Data.Foldable         (for_)
import qualified Data.IntMap.Strict    as IM
import           Data.IntMap.Strict    (IntMap)
import qualified Data.NonEmpty         as NE

data Hierarchy a
  = Hierarchy
  { path     :: [Int]
  , payload  :: a
  , children :: IntMap (Hierarchy a)
  }

type NonEmptyList = NE.T []

data AmbiguousHierarchy a
  = AmbiguousHierarchy
  { path :: [Int]
  , payload :: a
  , children :: IntMap (NonEmptyList (AmbiguousHierarchy a))
  }

parseHierarchy :: CSV.Parser [Int]
               -> CSV.Parser a
               -> CSV.Parser (Hierarchy a)
parseHierarchy pathP payloadP =
  Hierarchy <$> pathP
            <*> payloadP
            <*> pure IM.empty

parseAmbiguousHierarchy :: CSV.Parser [Int]
                        -> CSV.Parser a
                        -> CSV.Parser (AmbiguousHierarchy a)
parseAmbiguousHierarchy pathP payloadP =
  AmbiguousHierarchy <$> pathP
                     <*> payloadP
                     <*> pure IM.empty

parsePathWithLength :: CSV.Parser Int
                    -> [CSV.Parser (Maybe a)]
                    -> CSV.Parser [a]
parsePathWithLength lenP eltPs = do
  len <- lenP
  guard $ len > 0
  path <- parsePath eltPs
  guard $ length path == len
  pure path

parsePath :: [CSV.Parser (Maybe a)] -> CSV.Parser [a]
parsePath = go True []
  where
    go nonEmptySoFar eltsSoFar = \case
      [] ->
        pure $ reverse eltsSoFar
      eltP : eltPs -> do
        maybeElt <- eltP
        case maybeElt of
          Nothing                  -> go False eltsSoFar eltPs
          Just elt | nonEmptySoFar -> go True (elt : eltsSoFar) eltPs
                   | otherwise     -> mzero

class HasPath a where
  getPath :: a -> [Int]

instance HasPath (Hierarchy a) where
  getPath = path

instance HasPath (AmbiguousHierarchy a) where
  getPath = path

addToLevelBucket :: HasPath a
                 => IntMap (NonEmptyList a)
                 -> a
                 -> IntMap (NonEmptyList a)
addToLevelBucket buckets unit =
  IM.alter addToBucket (length $ getPath unit) buckets
  where
    addToBucket = \case
      Nothing    -> Just $ NE.singleton unit
      Just units -> Just $ pushFront unit units

data PathError
  = PeMissingLink [Int]
  | PeConflict [Int]
  deriving Show

mkHierarchy :: a
            -> IntMap (NonEmptyList (Hierarchy a))
            -> Either PathError (Hierarchy a)
mkHierarchy rootPayload buckets = foldM addToHierarchy emptyRoot
                                  $ concatMap NE.flatten
                                  $ IM.elems buckets
  where
    emptyRoot = Hierarchy { path     = []
                          , payload  = rootPayload
                          , children = IM.empty
                          }

addToHierarchy :: Hierarchy a -> Hierarchy a -> Either PathError (Hierarchy a)
addToHierarchy root unit@Hierarchy{ path } = go [] path (Just root)
  where
    go pathTraversed pathAhead maybeCurrent =
      case pathAhead of
        [] ->
          case maybeCurrent of
            Nothing -> Right unit
            Just _  -> Left $ PeConflict $ reverse pathTraversed
        i : is ->
          case maybeCurrent of
            Nothing ->
              Left $ PeMissingLink $ reverse pathTraversed'
            Just current@Hierarchy{ children } ->
              setChildren current <$> IM.alterF f i children
            where
              setChildren :: Hierarchy a -> IntMap (Hierarchy a) -> Hierarchy a
              setChildren h c = h { children = c }
              f = fmap Just . go pathTraversed' is
              pathTraversed' = i : pathTraversed

mkAmbiguousHierarchy :: a
                     -> IntMap (NonEmptyList (AmbiguousHierarchy a))
                     -> Either PathError (AmbiguousHierarchy a)
mkAmbiguousHierarchy rootPayload buckets =
    foldM addToAmbiguousHierarchy emptyRoot
  $ concatMap NE.flatten
  $ IM.elems buckets
  where
    emptyRoot = AmbiguousHierarchy { path     = []
                                   , payload  = rootPayload
                                   , children = IM.empty
                                   }

addToAmbiguousHierarchy :: AmbiguousHierarchy a
                        -> AmbiguousHierarchy a
                        -> Either PathError (AmbiguousHierarchy a)
addToAmbiguousHierarchy root unit@AmbiguousHierarchy{ path } = do
  newRoots <- go [] path (Just $ NE.singleton root)
  case NE.viewL newRoots of
    (newRoot, []) ->
      pure newRoot
    _ ->
      Left $ PeConflict []
  where
    go pathTraversed pathAhead maybeCurrent =
      case pathAhead of
        [] ->
          case maybeCurrent of
            Nothing ->
              Right $ NE.singleton unit
            Just units ->
              Right $ pushBack unit units
        i : is ->
          case maybeCurrent of
            Nothing ->
              Left $ PeMissingLink $ reverse pathTraversed'
            Just units ->
              case NE.viewL units of
                (current@AmbiguousHierarchy{ children }, []) ->
                  NE.singleton . setChildren current <$> IM.alterF f i children
                _ ->
                  Left $ PeConflict $ reverse pathTraversed'
            where
              setChildren :: AmbiguousHierarchy a -> IntMap (NonEmptyList (AmbiguousHierarchy a)) -> AmbiguousHierarchy a
              setChildren ah c = ah { children = c }
              f = fmap Just . go pathTraversed' is
              pathTraversed' = i : pathTraversed

pushFront :: a -> NonEmptyList a -> NonEmptyList a
pushFront x xs = x `NE.cons` NE.flatten xs

pushBack :: a -> NonEmptyList a -> NonEmptyList a
pushBack x xs = NE.flatten xs `NE.snoc` x

traverseHierarchy :: Applicative f
                  => Hierarchy a
                  -> (Int -> [Int] -> a -> f b)
                  -> f ()
traverseHierarchy h f = go 0 h
  where
    go level Hierarchy{ path, payload , children } =
      f level path payload *> for_ children (go (level + 1))

traverseAmbiguousHierarchy :: Applicative f
                           => AmbiguousHierarchy a
                           -> (Int -> [Int] -> a -> f b)
                           -> f ()
traverseAmbiguousHierarchy ah f = go 0 ah
  where
    go level AmbiguousHierarchy{ path, payload , children } =
      f level path payload *>
      for_ children (\group -> for_ group (go (level + 1)))
