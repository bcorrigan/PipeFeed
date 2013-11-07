module Main where

import Config as Conf
import Types as T
import Transforms

import Text.Feed.Import 
import Text.Feed.Query as Query
import Text.Atom.Feed as Atom
import Text.RSS.Syntax as RSS2
import Text.RSS1.Syntax as RSS1
import Text.Feed.Types as FT

import Network.URI
import Network.HTTP(simpleHTTP,getRequest,getResponseBody)
import Data.Maybe(fromMaybe,maybe)
import Data.Hashable(hash) 
import System.Directory
import Control.Monad(foldM)
import Data.List
import System.IO.Strict as S


--test

main::IO()
main = do 
        config <- Conf.configure "/home/bc/workspace/PipeFeed/feeds.config"
        feeds <- mapM fetchFeed (feeds config)
        feeds <- mapM (loadCache config . hashFeed) feeds
        feeds <- mapM transform feeds
        mapM_ (writeCache config) feeds
        mapM_ (deleteCache config) feeds
        mapM_ (write config) feeds

        
        print feeds
        
        return ()

fetchFeed::T.Feed -> IO T.Feed 
fetchFeed feedcfg = do
                        let url = feedurl feedcfg
                        rsp <- simpleHTTP (getRequest url) -- >>= fmap (take 100) . getResponseBody
                        feedText <- getResponseBody rsp
                        print feedText
                        let feed = fromMaybe (error "Can't get feed") (parseFeedString feedText) 
                        
                        let items = Query.feedItems feed
                        let title = getFeedTitle feed
                        
                        let author = fromMaybe "unknown" (getFeedAuthor feed)
                        
                        print author
                        
                        print $ length items
                        print title
                        
                        let articles = map (\item ->
                                      Article{title=fromMaybe "Unknown title" (getItemTitle item)
                                        , body=maybe "nowt" fst3 (getItemEnclosure item)
                                        , itemurl=fromMaybe "Unknown url" (getItemLink item)
                                        , transformed=False
                                        , cached=False
                                        , itemRec=item 
                                        , bodyhash=Nothing} ) items
                        
                        return ( feedcfg{ items=articles, feedRec=feed } ) 

--haskell doesn't have basic triple-or-above manip funcs, wtf?
fst3::(a,b,c)->a
fst3 (a,b,c) = a

--any on disk should be loaded
--this loads body into matching article, and marks that article as transformed
loadCache :: T.Config -> T.Feed -> IO T.Feed
loadCache cfg feed = do
                        createDirectoryIfMissing False  $ cache cfg

                        existing <- mapM (doesFileExist . mkPath cfg) 
                                                                    (items feed)                 
                        bodies <- mapM (\ (exists, item) -> if exists
                                                then S.readFile $ mkPath cfg item  
                                                else return $ body item )
                                                  (zip existing (items feed))
                                                
                        let newItems= map (\ (body, item) -> item{body=body}) (zip bodies (items feed))
                        
                        return feed{items = markTransformed newItems existing}

markTransformed :: [Article] -> [Bool] -> [Article]
markTransformed items transfms = map (\ (item, transformed) -> item{transformed=transformed}) (zip items transfms)

mkPath :: Config -> Article -> FilePath
mkPath cfg item = cache cfg ++ "/" ++ show (fromMaybe 0 (bodyhash item))

--writes any uncached
writeCache  :: Config -> T.Feed -> IO ()
writeCache cfg feed = mapM_ (\i -> writeFile (mkPath cfg i) (body i))    
                            (filter (not . cached) (items feed))

--zaps any articles on disk not in the passed feed
deleteCache :: Config -> T.Feed -> IO()
deleteCache cfg feed = do
                        savedHashes <- getDirectoryContents (cache cfg)
                        
                        let toDelete = filter (not . isPrefixOf ".") savedHashes \\ liveHashes
                        print toDelete
                        mapM_ (\hash -> removeFile $ cache cfg ++ "/" ++ hash) toDelete
                        
                        where liveHashes = map
                                            (show .
                                                fromMaybe
                                                    (error "It should be impossible for a Nothing to be here!")
                                                    . bodyhash)
                                                        (items feed)
                         
hashFeed :: T.Feed -> T.Feed
hashFeed feed = feed{items=map (\(a,h) -> a{bodyhash=Just h}) 
                               (zip (items feed) 
                                   (map (\i -> hash $ title i ++ body i) (items feed)))} 

--apply the transforms in order
--also marks transformed
transform :: T.Feed -> IO T.Feed
transform feed = do
                    articles<-mapM (\article -> 
                        if transformed article 
                        then return article{transformed=True}
                        else applyTransforms article
                      ) (items feed) 

                    return feed{items=articles}
                    
                    where applyTransforms :: Article -> IO Article
                          applyTransforms article = foldM (\acc f -> (f acc)) article
                                                             (transforms feed)
--write the resulting feed
--Best way to do this:
--Each feed & item can be cased into underlying type, see http://hackage.haskell.org/package/feed-0.3.9.1/docs/src/Text-Feed-Export.html#xmlFeed
--then simply need to use record syntax to make new body
--then Text.Feed.Export to chuck out the resulting feed
write :: Config -> T.Feed -> IO()
write cfg feed = do
                    
                    undefined


--a normal language would have .setBody() on polymorphic objects.. haskell has insanity
serialiseFeed:: T.Feed -> T.Feed 
serialiseFeed feed = case feedRec of
                        FT.RSSFeed f -> serialiseRSS2 f feed
                        FT.RSS1Feed f -> serialiseRSS1 f feed
                        FT.AtomFeed f -> serialiseAtom f feed
                        FT.XMLFeed f -> feed 
                     where feedRec=T.feedRec feed   

serialiseAtom:: Atom.Feed -> T.Feed -> T.Feed
serialiseAtom atom feed = undefined

serialiseRSS1 :: RSS1.Feed -> T.Feed -> T.Feed
serialiseRSS1 rss feed = undefined

serialiseRSS2 :: RSS2.RSS -> T.Feed -> T.Feed
serialiseRSS2 rss feed = undefined

 
--readConfig :: String -> Config 
--readConfig config = 
                     
--TODO: 1) readcfg :: IO Config - feeds populated with empty items
--      2) fetchFeeds :: IO Feed -> Feed - from Feed.feedurl. populates Feed.items
--      3) cacheSync :: IO Feed -> IO Feed , sync with items folder - Config.cache++Feed.name, returned Feed ha sonly new items
--      4) writeFeed :: IO Feed , writes out feed as a new rss.xml in Config.rss_store