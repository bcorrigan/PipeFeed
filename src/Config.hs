module Config where

import Data.Configurator(load,require,Worth(Required))
import Data.Configurator.Types as C
import Data.Text(unpack,pack)
import Data.Maybe(fromJust,fromMaybe)
import Network.URI
import Transforms
import Types as T


configure::FilePath -> IO T.Config
configure path = do
                        cfg <- load[Required path]
                        cache <- require cfg "cache" :: IO FilePath
                        output <-require cfg "output" :: IO FilePath
                        
                        feedNames <- require cfg "feeds" :: IO [String]
                        
                        feeds <- mapM (getFeed cfg) feedNames
                                                
                        print feedNames
                        print feeds
                        print cache
                        
                        return T.Config{feeds = feeds, cache = cache, rssStore = output}

--get_feeds::[String] -> IO([Feeds])
--get_feeds feeds = map get_feed feeds 

getFeed::C.Config -> String -> IO Feed
getFeed cfg feed = do
                        url <- require cfg (pack (feed++".url")) :: IO String
                        let uri = fromMaybe (error (feed ++ ".url is not a valid url :-("))
                                                (parseURI url)

                        transformNames <- require cfg (pack $ feed++".transforms") :: IO [String]        
                        
                        transforms <- mapM (\t -> case t of 
                                   "fetchfull" -> return fetchfull
                                   "regexsnip" -> do
                                        start_re <- require cfg (pack $ feed++".regexsnip.start_re") :: IO String
                                        end_re <- require cfg (pack $ feed++".regexsnip.end_re") :: IO String
                                        return (regexsnip start_re end_re)
                                   _ -> error "Unknown transform?"
                                ) transformNames 
                                
                        return Feed{name = feed, items = [], feedurl = url, transforms = transforms}
                        

instance C.Configured [String] where
        convert (C.List vals) = Just $ map (fromJust . fmap unpack . convert) vals
        convert _ = Nothing
                        

