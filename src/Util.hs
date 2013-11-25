module Util where

import Data.Either
--import Data.ByteString.Char8(unpack)
import qualified Data.ByteString.Lazy as L
import Control.Exception as X
import Data.Char(chr)
import Data.Word(Word8)
import Network.URI
import Data.Maybe(fromJust, fromMaybe)
import Debug.Trace
import Network.HTTP.Conduit


grabUrl :: String -> IO (Maybe String)
grabUrl url = do
                 body <- simpleHttp url `X.catch` exceptionHandler
                 return
                    (if body == L.empty then Nothing else
                        Just $ bytesToString $ L.unpack body)
                 where exceptionHandler ::  HttpException -> IO L.ByteString
                       exceptionHandler e = putStr "An error occured downloading article: " >> print e >> return L.empty
--for error handling Eithers - crap haskell stdlib again?
isLeft (Left _) = True
isLeft _        = False

bytesToString :: [Word8] -> String
bytesToString = map (chr . fromIntegral)

--haskell doesn't have basic triple-or-above manip funcs, wtf?
fst3::(a,b,c)->a
fst3 (a,b,c) = a

