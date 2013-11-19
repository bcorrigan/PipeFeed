module Util where

--import Network.Curl.Download
--import Network.Curl.Opts
import Network.HTTP
import Network.HTTP.Headers
import Data.Either
--import Data.ByteString.Char8(unpack)
--import qualified Data.ByteString.Lazy as L
--import Control.Exception as X
--import Data.Char(chr)
--import Data.Word(Word8)
import Network.URI
import Data.Maybe(fromJust, fromMaybe)
import Debug.Trace

--no can-do on arm
{-
grabUrl :: String -> IO (Maybe String)
grabUrl url = do
                 body <- simpleHttp url `X.catch` exceptionHandler
                 return
                    (if body == L.empty then Nothing else
                        Just $ bytesToString $ L.unpack body)
                 where exceptionHandler ::  HttpException -> IO L.ByteString
                       exceptionHandler e = putStr "An error occured downloading article: " >> print e >> return L.empty
-}


--curl fails on NYT
{- grabUrl url = do
                result <- openURIWithOpts [CurlFollowLocation True] url
                case result of
                    Left error -> do 
                        print $ "Error fetching " ++ url ++ " Error: " ++ error
                        return Nothing
                    Right body ->
                        return $ Just $ unpack body
-}

    
--for error handling Eithers - crap haskell stdlib again?
isLeft (Left _) = True
isLeft _        = False

--bytesToString :: [Word8] -> String
--bytesToString = map (chr . fromIntegral)

--haskell doesn't have basic triple-or-above manip funcs, wtf?
fst3::(a,b,c)->a
fst3 (a,b,c) = a

grabUrl url = grabUrlHack url Nothing

{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success. -}
grabUrlHack :: String -> Maybe [Header] -> IO (Maybe String)
grabUrlHack url headers =
    do resp <- simpleHTTP request
       print $ "Got cookies passed in?" ++ (show $ length reqCookies )
       print $ "Response:" ++ (show resp)
       case resp of
         Left x -> do
                    print x
                    return Nothing
         Right r -> 
             case rspCode r of
               (2,_,_) -> trace ("0" ++ rspBody r) $ return $ Just (rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return $ Just (show r)
                   Just url -> grabUrlHack url (Just (recurseCookies r))
               _ -> return $ Just (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = userAgent : reqCookies,
                             rqBody = ""}
          uri = fromJust $ parseURI $ replace "////" "//" url
          userAgent = mkHeader HdrUserAgent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.6; rv:25.0) Gecko/20100101 Firefox/25.0"
          respCookie r = trace ("POOP:" ++ show (filterCookies r)) filterCookies r
          reqCookies = fromMaybe [] headers
          recurseCookies r = reqCookies ++ map convertCookie (respCookie r)
          convertCookie header = mkHeader HdrCookie $ hdrValue header
          filterCookies = retrieveHeaders HdrSetCookie
          
          
--lifted from stackoverflow, ie a string replace function without depending on 50MB of stupid dependencies... bloody haskell
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace needle replacement haystack
  = case begins haystack needle of
      Just remains -> replacement ++ remains
      Nothing      -> case haystack of
                        []     -> []
                        x : xs -> x : replace needle replacement xs

begins :: Eq a => [a] -> [a] -> Maybe [a]
begins haystack []                = Just haystack
begins (x : xs) (y : ys) | x == y = begins xs ys
begins _        _                 = Nothing

