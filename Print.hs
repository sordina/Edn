{-# LANGUAGE OverloadedStrings #-}

module Print (fromValue, fromTagged, encode) where

import Data.Monoid (mappend)
import qualified Data.Text as T
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Data.EDN.Types as E
import Data.EDN.Types.Class (ToEDN, toEDN)

-- | Encode a Tagged EDN value to a 'Builder'.
fromTagged :: Int -> E.TaggedValue -> Builder
fromTagged n (E.NoTag v) = fromValue n v
fromTagged n (E.Tagged v "" t) = singleton '#' <> string t <> " " <> fromValue n v
fromTagged n (E.Tagged v ns t) = singleton '#' <> string ns <> singleton '/' <> string t <> " " <> fromValue n v

spaces :: Int -> Builder
spaces n = fromString $ replicate n '\t'

-- | Encode a raw EDN value to a 'Builder'.
fromValue :: Int -> E.Value -> Builder
fromValue _ E.Nil           = "nil"
fromValue _ (E.Boolean b)   = if b then "true" else "false"
fromValue _ (E.String t)    = "\"" <> quote t <> "\""
fromValue _ (E.Character c) = "\\" <> quoteChar c
fromValue _ (E.Symbol "" v) = string v
fromValue _ (E.Symbol ns v) = string ns <> "/" <> string v
fromValue _ (E.Keyword kw)  = ":" <> string kw
fromValue _ (E.Integer i)   = decimal i
fromValue _ (E.Floating f)  = realFloat f
fromValue n (E.List xs)     = "("  <> fromList n xs <> ")"
fromValue n (E.Vec xs)      = "["  <> fromList n (V.toList xs) <> "]"
fromValue n (E.Set xs)      = "#{" <> fromList n (S.toList xs) <> "}"
fromValue n (E.Map as)      = "{"  <> "\n" <> fromAssoc (n+1) (M.assocs as) <> spaces n <> "}"

string :: BS.ByteString -> Builder
string s = fromLazyText . decodeUtf8 . L.fromChunks $ [s]

quote :: T.Text -> Builder
quote q = case T.uncons t of
    Nothing -> fromText h
    Just (c, t') -> fromText h <> escape c <> quote t'
    where
        (h, t) = T.break isEscape q
        isEscape c = c == '\"' || c == '\\' || c < '\x20'
        escape '\"' = "\\\""
        escape '\\' = "\\\\"
        escape '\n' = "\\n"
        escape '\r' = "\\r"
        escape '\t' = "\\t"
        escape c = singleton c

quoteChar :: Char -> Builder
quoteChar c = case c of
    '\n' -> string "newline"
    '\r' -> string "return"
    '\t' -> string "tab"
    ' '  -> string "space"
    _    -> singleton c

fromList :: Int -> [E.TaggedValue] -> Builder
fromList _ [] = ""
fromList n (x:[]) = fromTagged n x
fromList n (x:xs) = fromTagged n x <> " " <> fromList n xs

fromAssoc :: Int -> [(E.Value, E.TaggedValue)] -> Builder
fromAssoc _ [] = ""
fromAssoc n ((k, v):[]) = spaces n <> fromValue n k <> " " <> fromTagged n v <> "\n"
fromAssoc n ((k, v):as) = spaces n <> fromValue n k <> " " <> fromTagged n v <> "\n" <> fromAssoc n as

-- | Serialize a value as a lazy 'L.ByteString'.
encode :: ToEDN a => a -> L.ByteString
encode = encodeUtf8 . toLazyText . fromTagged 0 . toEDN
{-# INLINE encode #-}

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
{-# INLINE (<>) #-}
infixr 6 <>
