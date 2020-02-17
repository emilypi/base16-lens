# Base16-lens

[![Build Status](https://travis-ci.com/emilypi/base16-lens.svg?branch=master)](https://travis-ci.com/emilypi/base16-lens)
[![Hackage](https://img.shields.io/hackage/v/base16-lens.svg)](https://hackage.haskell.org/package/base16-lens)

This package provides optics and convenient pattern synonyms for the [base16](https://hackage.haskell.org/package/base16) library.

### Patterns

The pattern synonyms provided in this library are:

```haskell
pattern Hex :: ByteString -> ByteString
-- and
pattern Hex :: Text -> Text
```

These provide a convenient high level interface for passing Base16 encoded values.


### Optics

`Prism`s for encoding and decoding `Text` and `ByteString` values are given as part of the library:


```haskell
_Hex :: Prism' ByteString ByteString

-- and

_Hex:: Prism' Text Text
```

If a particular structure has a `Lens` into some `Text` or `ByteString` value they might want to encode (or decode), then composing such a `Lens` with these `Prisms` yields an affine `Traversal`, resulting in a structure which has the focus of its `Lens` encoded as or decoded from Base16.
