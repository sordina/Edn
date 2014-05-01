
module Main where

import Print
import Data.EDN.Parser
import Data.EDN.Types
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Text.Internal.Builder as B
import qualified Data.Text.Lazy.IO          as T

removeTag :: TaggedValue -> Value
removeTag (NoTag v)      = v
removeTag (Tagged v _ _) = v

main :: IO ()
main = BSL.getContents >>= run

run :: BSL.ByteString -> IO ()
run = maybe (return ()) pp . parseMaybe

pp :: TaggedValue -> IO ()
pp = T.putStr . B.toLazyText . fromValue 0 . removeTag
