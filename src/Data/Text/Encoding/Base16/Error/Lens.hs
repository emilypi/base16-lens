{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
-- |
-- Module       : Data.Text.Encoding.Base16.Error.Lens
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : non-portable
--
-- This module contains 'Prism''s for the `Data.Text.Encoding.Base16.Error.Base16Error`
-- datatype.
--
module Data.Text.Encoding.Base16.Error.Lens
( -- * Prisms
  _DecodeError
, _ConversionError
) where


import Control.Lens

import Data.Text (Text)
import Data.Text.Encoding.Base16.Error (Base16Error(..))


-- | A 'Prism'' into the 'DecodeError' case of a 'Base16Error'
--
_DecodeError :: forall err. Prism' (Base16Error err) Text
_DecodeError = prism' DecodeError $ \e -> case e of
    DecodeError t -> Just t
    ConversionError{} -> Nothing

-- | A 'Prism'' into the 'ConversionError' case of a 'Base16Error'
--
_ConversionError :: forall err. Prism' (Base16Error err) err
_ConversionError = prism' ConversionError $ \e -> case e of
    ConversionError err -> Just err
    DecodeError{} -> Nothing
