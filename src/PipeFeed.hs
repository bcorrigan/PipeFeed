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
import Text.Feed.Export(xmlFeed)
import Text.XML.Light.Output(showTopElement)

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
        mapM_ (serialiseFeed config) feeds

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
                        createDirectoryIfMissing True  $ cacheDir cfg feed
                        
                        existing <- mapM (doesFileExist . mkPath (cacheDir cfg feed) ) 
                                                                    (items feed)                 
                        bodiesCached <- mapM (\ (exists, item) -> if exists
                                                then readFile item  
                                                else return (body item, False) )
                                                  (zip existing (items feed))
                                                
                        let newItems= map (\ (body, item) -> item{body = fst body, cached = snd body}) (zip bodiesCached (items feed))
                        
                        return feed{items = markTransformed newItems existing}
                        
                        where readFile item = do 
                                                body <- S.readFile $ mkPath (cacheDir cfg feed) item
                                                return (body,True)

cacheDir :: T.Config -> T.Feed -> String
cacheDir cfg feed = cache cfg ++ "/" ++ name feed

markTransformed :: [Article] -> [Bool] -> [Article]
markTransformed items transfms = map (\ (item, transformed) -> item{transformed=transformed}) (zip items transfms)

mkPath :: String -> Article -> FilePath
mkPath path item = path ++ "/" ++ show (fromMaybe 0 (bodyhash item))

--writes any uncached
writeCache  :: Config -> T.Feed -> IO ()
writeCache cfg feed = mapM_ (\i -> writeFile (mkPath (cacheDir cfg feed) i) (body i))    
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

                    return $ updateFeedItems feed articles
                    
                    where applyTransforms :: Article -> IO Article
                          applyTransforms article = foldM (\acc f -> (f acc)) article
                                                             (transforms feed)
--write the resulting feed
serialiseFeed:: T.Config -> T.Feed -> IO()
serialiseFeed cfg feed = do
                        createDirectoryIfMissing False  $ rssStore cfg
                        let output = showTopElement $ xmlFeed feedRec
                        writeFile (rssStore cfg ++ "/" ++ name feed ++ ".rss") output
                     where feedRec=T.feedRec feed   
 

 --cache should write to different dirs
 --error handling
 --command line argument should be path of config file