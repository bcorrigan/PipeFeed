module Types where

import Data.List(intercalate)
import Text.Feed.Types as F
import Text.RSS.Syntax as RSS2
import Text.RSS1.Syntax as RSS1
import Text.Atom.Feed as Atom
import Text.Feed.Types as FT

--probably needs timestamp and other things
data Article = Article { title :: String
                   , body :: String
                   , itemurl :: String
                   , bodyhash :: Maybe Int
                   , transformed :: Bool
                   , cached :: Bool
                   , itemRec :: F.Item
                   } deriving Show
                   
data Feed = Feed { name :: String
                 , items :: [Article]
                 , feedurl :: String
                 , feedRec :: F.Feed
                 , transforms :: [Article -> IO (Maybe Article)]
                 , extension :: String 
                 } 
                 
data Config = Config { feeds :: [Types.Feed]
                     , cache :: String
                     , rssStore :: String
                     } deriving Show
                     
instance Show Types.Feed where
        show f = "Feed. name: " ++ name f ++ " feedurl: " ++ show (feedurl f) ++
            " num items: " ++
            show (length $ items f) ++
            " Items:" ++
            intercalate ", " (map show (items f)) ++
            " num transforms: " ++ show (length $ transforms f)

--to set atom: fmap contentToStr $ Atom.entrySummary e
updateItem :: FT.Item -> String -> FT.Item
updateItem item body = case item of 
                        FT.RSSItem i -> FT.RSSItem i{rssItemDescription=Just body}
                        FT.RSS1Item i -> FT.RSS1Item i{itemDesc=Just body}
                        FT.AtomItem i -> FT.AtomItem i{entrySummary=Just $ HTMLString body}
                        FT.XMLItem i -> FT.XMLItem i
                        
updateTextFeedItems :: FT.Feed -> [FT.Item] -> FT.Feed
updateTextFeedItems (FT.AtomFeed f) items = FT.AtomFeed f{feedEntries=map toEntry items}
                                where toEntry (FT.AtomItem i) = i
updateTextFeedItems (FT.RSS1Feed f) items = FT.RSS1Feed f{feedItems=map toFeedItem items}
                                where toFeedItem (FT.RSS1Item i) = i
updateTextFeedItems (FT.RSSFeed f) items = FT.RSSFeed f{rssChannel=(rssChannel f){rssItems=map toRssItem items}}
                                where toRssItem (FT.RSSItem i) = i

updateFeedItems:: Types.Feed -> [Article] -> Types.Feed                                
updateFeedItems feed articles = feed{items=articles, feedRec=updateTextFeedItems (feedRec feed) $ map itemRec articles}                                
                                
updateArticle :: Article -> String -> Article
updateArticle article newbody = article { body=newbody, itemRec=updateItem (itemRec article) newbody }

