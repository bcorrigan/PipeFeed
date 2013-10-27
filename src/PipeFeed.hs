module Main where

import Config as Conf
import Types
import Transforms

import Text.Feed.Import
import Text.Feed.Query
import Network.URI
import Network.HTTP(simpleHTTP,getRequest,getResponseBody)
import Data.Maybe(fromMaybe,maybe)

--test

main::IO()
main = do 
        config <- Conf.configure "/home/bc/workspace/PipeFeed/feeds.config"
        done <- sequence $ map fetchFeed (feeds config)
        
        print done
        
        return ()

fetchFeed::Feed -> IO Feed 
fetchFeed feedcfg = do
                        let url = feedurl feedcfg
                        rsp <- simpleHTTP (getRequest url) -- >>= fmap (take 100) . getResponseBody
                        feedText <- getResponseBody rsp
                        print feedText
                        let feed = case (parseFeedString feedText) of
                                Just f -> f
                                Nothing -> error "Can't get feed" 
                        
                        let items = feedItems feed
                        let title = getFeedTitle feed
                        
                        let author = fromMaybe "unknown" (getFeedAuthor feed)
                        
                        print author
                        
                        print $ length items
                        print title
                        
                        let articles = map (\item ->
                                      Article{title=fromMaybe "Unknown title" (getItemTitle item)
                                        , body=maybe "nowt" fst3 (getItemEnclosure item)
                                        , author=fromMaybe "Unknown author" (getItemAuthor item)
                                        , itemurl=fromMaybe "Unknown url" (getItemLink item)
                                        , hash="TODO"} ) items
                        
                        return ( feedcfg{ items=articles } ) 

--haskell doesn't have basic triple-or-above manip funcs, wtf?
fst3::(a,b,c)->a
fst3 (a,b,c) = a
                     
--readConfig :: String -> Config 
--readConfig config = 
                     
--TODO: 1) readcfg :: IO Config - feeds populated with empty items
--      2) fetchFeeds :: IO Feed -> Feed - from Feed.feedurl. populates Feed.items
--      3) cacheSync :: IO Feed -> IO Feed , sync with items folder - Config.cache++Feed.name, returned Feed ha sonly new items
--      4) writeFeed :: IO Feed , writes out feed as a new rss.xml in Config.rss_store