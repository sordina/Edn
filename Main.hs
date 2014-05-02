
module Main where

import Print
import System.IO
import System.Exit
import Data.EDN.Parser
import Data.EDN.Types
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Text.Internal.Builder as B
import qualified Data.Text.Lazy.IO          as T

removeTag :: TaggedValue -> Value
removeTag (NoTag v)      = v
removeTag (Tagged v _ _) = v

main :: IO ()
main = BSL.getContents >>= validate

validate :: BSL.ByteString -> IO ()
validate b | BSL.null b = help
           | otherwise  = run b

help :: IO ()
help = hPutStrLn stderr "Usage: edn < EDNFILE.edn"
    >> exitFailure

run :: BSL.ByteString -> IO ()
run = maybe errors pp . parseMaybe

errors :: IO ()
errors = hPutStrLn stderr "Error parsing edn input"
      >> exitFailure

pp :: TaggedValue -> IO ()
pp = T.putStrLn . B.toLazyText . fromValue 0 . removeTag
