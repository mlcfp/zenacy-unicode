# Zenacy Unicode

[![Linux build status](https://img.shields.io/travis/com/mlcfp/zenacy-unicode?label=Linux%20build)](https://travis-ci.org/mlcfp/zenacy-unicode)

Zenacy Unicode includes tools for checking byte order marks (BOM) and
cleaning data to remove invalid bytes.  These tools can help ensure that
data pulled from the web can be parsed and converted to text.

The following is an example of converting dubious data to a text.

```haskell
textDecode :: ByteString -> Text
textDecode b =
  case bomStrip b of
    (Nothing, s)           -> T.decodeUtf8 $ unicodeCleanUTF8 s -- Assume UTF8
    (Just BOM_UTF8, s)     -> T.decodeUtf8 $ unicodeCleanUTF8 s
    (Just BOM_UTF16_BE, s) -> T.decodeUtf16BE s
    (Just BOM_UTF16_LE, s) -> T.decodeUtf16LE s
    (Just BOM_UTF32_BE, s) -> T.decodeUtf32BE s
    (Just BOM_UTF32_LE, s) -> T.decodeUtf32LE s
```
