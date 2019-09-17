{-# LANGUAGE FlexibleInstances #-}

module Data.Teryt.TERC where

import qualified Data.Csv  as CSV
import           Data.Csv  ((.!))
import           Data.Text (Text)

import           Data.Teryt.Hierarchy (Hierarchy, AmbiguousHierarchy,
                                       parseHierarchy, parseAmbiguousHierarchy,
                                       parsePath)

data TercData
  = TercData
  { name     :: Text
  , typeId   :: Maybe  Int
  , typeName :: Text
  , validOn  :: Text
  }

instance CSV.FromRecord TercData where
  parseRecord v = TercData <$> v .! 4
                           <*> v .! 3
                           <*> v .! 5
                           <*> v .! 6

instance CSV.FromRecord (Hierarchy TercData) where
  parseRecord v = parseHierarchy pathP (CSV.parseRecord v)
    where
      pathP = parsePath [ v .! 0, v .! 1, v .! 2 ]

instance CSV.FromRecord (AmbiguousHierarchy TercData) where
  parseRecord v = parseAmbiguousHierarchy pathP (CSV.parseRecord v)
    where
      pathP = parsePath [ v .! 0, v .! 1, v .! 2 ]
