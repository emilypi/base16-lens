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
-- decoding 'Text' values.
--
module Data.Text.Encoding.Base16.Lens
( -- * Prisms
  _Hex
  -- * Patterns
, pattern Hex
) where

import Control.Lens

import Data.Text (Text)
import qualified Data.Text.Encoding.Base16 as B16T


-- $setup
--
-- >>> import Control.Lens
-- >>> import Data.Text.Encoding.Base16.Lens
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications

-- -------------------------------------------------------------------------- --
-- Optics

-- | A 'Prism'' into the Base16 encoding of a 'Text' value
--
-- >>> _Hex # "Sun"
-- "53756e"
--
-- >>> "53756e" ^? _Hex
-- Just "Sun"
--
_Hex :: Prism' Text Text
_Hex = prism' B16T.encodeBase16 $ \s -> case B16T.decodeBase16 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Hex #-}

-- -------------------------------------------------------------------------- --
-- Patterns

-- | Bidirectional pattern synonym for Base16-encoded 'Text' values.
--
pattern Hex :: Text -> Text
pattern Hex a <- (preview _Hex -> Just a) where
    Hex a = _Hex # a
