{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module       : Data.Text.Encoding.Base16.Lens
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: non-portable
--
-- This module contains 'Prism's Base16-encoding and
-- decoding lazy 'Text' values.
--
module Data.Text.Lazy.Encoding.Base16.Lens
( -- * Prisms
  _Hex
, _Base16
  -- * Patterns
, pattern Hex
, pattern Base16
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

-- | A 'Prism'' into the Base16 encoding of a lazy 'Text' value
--
-- >>> _Hex # "Sun"
-- "53756e"
--
-- >>> "53756e" ^? _Hex
-- Just "Sun"
--
_Base16 :: Prism' Text Text
_Base16 = prism' B16TL.encodeBase16 $ \s -> case B16TL.decodeBase16 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base16 #-}

-- | A 'Prism'' into the Base16 encoding of a lazy 'Text' value. This
-- function is an alias for '_Base16'
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
