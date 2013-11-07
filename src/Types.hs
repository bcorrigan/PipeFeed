module Types where

import Network.URI
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
                 , transforms :: [Article -> IO Article]
                 } 
                 
data Config = Config { feeds :: [Types.Feed]
                     , cache :: String
                     , rssStore :: String
                     } deriving Show
                     
instance Show Types.Feed where
        show f = "Feed. name: " ++ name f ++ " feedurl: " ++ (show $ feedurl f) ++ " num items: " ++ (show $ length $ items f) ++ " Items:" ++ (intercalate ", " (map show (items f))) ++ " num transforms: " ++ (show $ length $ transforms f)

--newBody :: Article -> Article
--newBody a =

--to set atom: fmap contentToStr $ Atom.entrySummary e
updateItem :: FT.Item -> String -> FT.Item
updateItem item body = case item of 
                        FT.RSSItem i -> FT.RSSItem i{rssItemDescription=Just body}
                        FT.RSS1Item i -> FT.RSS1Item i{itemDesc=Just body}
                        FT.AtomItem i -> FT.AtomItem i{entrySummary=Just $ HTMLString body}
                        FT.XMLItem i -> FT.XMLItem i
                        
updateFeed :: FT.Feed -> [FT.Item] -> FT.Feed
updateFeed feed body = undefined
                        