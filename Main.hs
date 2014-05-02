
module Main where

import Print
import System.IO          (stderr, hPutStrLn)
import System.Exit        (exitFailure)
import System.Environment (getArgs)
import Data.EDN.Parser
import Data.EDN.Types
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Text.Internal.Builder as B
import qualified Data.Text.Lazy.IO          as T

removeTag :: TaggedValue -> Value
removeTag (NoTag v)      = v
removeTag (Tagged v _ _) = v

main :: IO ()
main = getArgs >>= options

options :: [String] -> IO ()
options ("-h"    :_) = help
options ("--help":_) = help
options (_ : _ : _ ) = help
options [file      ] = BSL.readFile file >>= validate
options [          ] = BSL.getContents   >>= validate

validate :: BSL.ByteString -> IO ()
validate b | BSL.null b = help
           | otherwise  = run b

help :: IO ()
help = hPutStrLn stderr "Usage:\n  edn FILE\n  edn < FILE"
    >> exitFailure

run :: BSL.ByteString -> IO ()
run = maybe errors pp . parseMaybe

errors :: IO ()
errors = hPutStrLn stderr "Error parsing edn input"
      >> exitFailure

pp :: TaggedValue -> IO ()
pp = T.putStrLn . B.toLazyText . fromValue 0 . removeTag
