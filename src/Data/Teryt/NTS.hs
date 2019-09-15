{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Teryt.NTS
  ( NtsType(..), ntsTypeToText
  , NtsData(..)
  ) where

import qualified Data.Csv             as CSV
import           Data.Csv             ((.!))
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8)

import           Data.Teryt.Hierarchy (Hierarchy, AmbiguousHierarchy,
                                       parseHierarchy, parseAmbiguousHierarchy,
                                       parsePathWithLength)
import           Data.Teryt.Utils     (inverse, liftMaybe)

data NtsType
  = NtRoot
  | NtRegion
  | NtWojewodztwo
  | NtPodregion
  | NtPowiat
  | NtMiastoNaPrawachPowiatu
  | NtGminaMiejska
  | NtGminaWiejska
  | NtGminaMiejskoWiejska
  | NtMiastoWGminieMiejskoWiejskiej
  | NtObszarWiejski
  | NtDzielnica
  | NtDelegatura
  deriving (Enum, Bounded)

instance CSV.FromField NtsType where
  parseField = liftMaybe . inverse ntsTypeToText . decodeUtf8

ntsTypeToText :: NtsType -> Text
ntsTypeToText = \case
  NtRoot                          -> "root"
  NtRegion                        -> "region"
  NtWojewodztwo                   -> "województwo"
  NtPodregion                     -> "podregion"
  NtPowiat                        -> "powiat"
  NtMiastoNaPrawachPowiatu        -> "miasto na prawach powiatu"
  NtGminaMiejska                  -> "gmina miejska"
  NtGminaWiejska                  -> "gmina wiejska"
  NtGminaMiejskoWiejska           -> "gmina miejsko-wiejska"
  NtMiastoWGminieMiejskoWiejskiej -> "miasto w gminie miejsko-wiejskiej"
  NtObszarWiejski                 -> "obszar wiejski"
  NtDzielnica                     -> "dzielnica"
  NtDelegatura                    -> "delegatura"

data NtsData
  = NtsData
  { name    :: Text
  , type_   :: NtsType
  , validOn :: Text
  }

instance CSV.FromRecord NtsData where
  parseRecord v = NtsData <$> v .! 7 <*> v .! 8 <*> v .! 9

instance CSV.FromRecord (Hierarchy NtsData) where
  parseRecord v = parseHierarchy pathP (CSV.parseRecord v)
    where
      pathP = parsePathWithLength (v .! 0)
                                  [v .! 1, v .! 2, v .! 3, v .! 4, v .! 5]

instance CSV.FromRecord (AmbiguousHierarchy NtsData) where
  parseRecord v = parseAmbiguousHierarchy pathP (CSV.parseRecord v)
    where
      pathP = parsePathWithLength (v .! 0)
                                  [v .! 1, v .! 2, v .! 3, v .! 4, v .! 5]
