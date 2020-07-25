{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Short.Base16.Lens
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : non-portable
--
-- This module contains 'Prism''s and 'Iso''s for Base16-encoding and
-- decoding 'ShortByteString' values.
--
module Data.ByteString.Short.Base16.Lens
( -- * Prisms
  _Hex
, _Base16
  -- * Isos
, _Base16Lenient
  -- * Patterns
, pattern Hex
, pattern Base16
, pattern Base16Lenient
) where


import Control.Lens

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short.Base16 as B16S


-- $setup
--
-- >>> import Control.Lens
-- >>> import Data.ByteString.Short.Base16.Lens
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications


-- -------------------------------------------------------------------------- --
-- Optics

-- | A 'Prism'' into the Base16 encoding of a 'ShortByteString' value.
--
-- >>> _Base16 # "Sun"
-- "53756e"
--
-- >>> "53756e" ^? _Base16
-- Just "Sun"
--
_Base16 :: Prism' ShortByteString ShortByteString
_Base16 = prism' B16S.encodeBase16' $ \s -> case B16S.decodeBase16 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Base16 #-}

-- | A 'Prism'' into the Base16 encoding of a lazy 'ShortByteString' value. This function
-- is an alias of '_Base16'.
--
-- >>> _Hex # "Sun"
-- "53756e"
--
-- >>> "53756e" ^? _Hex
-- Just "Sun"
--
_Hex :: Prism' ShortByteString ShortByteString
_Hex = prism' B16S.encodeBase16' $ \s -> case B16S.decodeBase16 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Hex #-}

-- | A 'Iso'' into the Base16 encoding of a leniently decoded 'ShortByteString' value.
--
-- >>> _Base16Lenient # "Sun"
-- "53756e"
--
-- >>> "53756e" ^. _Base16
-- "Sun"
--
_Base16Lenient :: Iso' ShortByteString ShortByteString
_Base16Lenient = iso B16S.decodeBase16Lenient B16S.encodeBase16'
{-# INLINE _Base16Lenient #-}

-- -------------------------------------------------------------------------- --
-- Patterns

-- | Bidirectional pattern synonym for Base16-encoded lazy 'ShortByteString' values.
--
pattern Hex :: ShortByteString -> ShortByteString
pattern Hex a <- (preview _Hex -> Just a) where
    Hex a = _Hex # a

-- | Bidirectional pattern synonym for Base16-encoded lazy 'ShortByteString' values.
--
pattern Base16 :: ShortByteString -> ShortByteString
pattern Base16 a <- (preview _Base16 -> Just a) where
    Base16 a = _Base16 # a

-- | Bidirectional pattern synonym for Base16-encoded 'ShortByteString' values.
--
pattern Base16Lenient :: ShortByteString -> ShortByteString
pattern Base16Lenient a <- (view _Base16Lenient -> a) where
    Base16Lenient a = _Base16Lenient # a
{-# COMPLETE Base16Lenient #-}
