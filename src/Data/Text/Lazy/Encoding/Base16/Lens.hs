{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Safe #-}
-- |
-- Module       : Data.Text.Lazy.Encoding.Base16.Lens
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : non-portable
--
-- This module contains 'Prism''s and 'Iso''s Base16-encoding and
-- decoding lazy 'Text' values.
--
module Data.Text.Lazy.Encoding.Base16.Lens
( -- * Prisms
  _Base16
, _Hex
  -- * Isos
, _Base16Lenient
  -- * Patterns
, pattern Hex
, pattern Base16
, pattern Base16Lenient
) where

import Control.Lens

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding.Base16 as B16TL


-- $setup
--
-- >>> import Control.Lens
-- >>> import Data.Text.Lazy.Encoding.Base16.Lens
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications

-- -------------------------------------------------------------------------- --
-- Optics

-- | A 'Prism'' into the Base16 encoding of a lazy 'Text' value.
--
-- >>> _Base16 # "Sun"
-- "53756e"
--
-- >>> "53756e" ^? _Base16
-- Just "Sun"
--
_Base16 :: Prism' Text Text
_Base16 = prism' B16TL.encodeBase16 $ \s -> case B16TL.decodeBase16 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base16 #-}

-- | A 'Prism'' into the Base16 encoding of a lazy 'Text' value. This
-- function is an alias for '_Base16'.
--
-- >>> _Hex # "Sun"
-- "53756e"
--
-- >>> "53756e" ^? _Hex
-- Just "Sun"
--
_Hex :: Prism' Text Text
_Hex = prism' B16TL.encodeBase16 $ \s -> case B16TL.decodeBase16 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Hex #-}

-- | A 'Iso'' into the Base16 encoding of a leniently decoded
-- 'Text' value.
--
-- >>> _Base16Lenient # "Sun"
-- "53756e"
--
-- >>> "53756e" ^. _Base16Lenient
-- "Sun"
--
_Base16Lenient :: Iso' Text Text
_Base16Lenient = iso B16TL.decodeBase16Lenient B16TL.encodeBase16
{-# INLINE _Base16Lenient #-}

-- -------------------------------------------------------------------------- --
-- Patterns

-- | Bidirectional pattern synonym for Base16-encoded lazy 'Text' values.
--
pattern Hex :: Text -> Text
pattern Hex a <- (preview _Hex -> Just a) where
    Hex a = _Hex # a

-- | Bidirectional pattern synonym for Base16-encoded lazy 'Text' values.
--
pattern Base16 :: Text -> Text
pattern Base16 a <- (preview _Base16 -> Just a) where
    Base16 a = _Base16 # a

-- | Bidirectional pattern synonym for leniently decoded,
-- Base16-encoded 'Text' values.
--
pattern Base16Lenient :: Text -> Text
pattern Base16Lenient a <- (view _Base16Lenient -> a) where
    Base16Lenient a = _Base16Lenient # a
{-# COMPLETE Base16Lenient #-}
