{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module       : Data.ByteString.Base16.Lazy.Lens
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: non-portable
--
-- This module contains 'Prism''s for Base16-encoding and
-- decoding lazy 'ByteString' values.
--
module Data.ByteString.Base16.Lazy.Lens
( -- * Prisms
  _Hex
  -- * Patterns
, pattern Hex
) where


import Control.Lens

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Base16.Lazy as B16L


-- $setup
--
-- >>> import Control.Lens
-- >>> import Data.ByteString.Base16.Lazy.Lens
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications


-- -------------------------------------------------------------------------- --
-- Optics

-- | A 'Prism'' into the Base16 encoding of a lazy 'ByteString' value
--
-- >>> _Hex # "Sun"
-- "53756e"
--
-- >>> "53756e" ^? _Hex
-- Just "Sun"
--
_Hex :: Prism' ByteString ByteString
_Hex = prism' B16L.encodeBase16' $ \s -> case B16L.decodeBase16 s of
    Left _ -> Nothing
    Right a -> Just a
{-# INLINE _Hex #-}

-- -------------------------------------------------------------------------- --
-- Patterns

-- | Bidirectional pattern synonym for Base16-encoded lazy 'ByteString' values.
--
pattern Hex :: ByteString -> ByteString
pattern Hex a <- (preview _Hex -> Just a) where
    Hex a = _Hex # a
