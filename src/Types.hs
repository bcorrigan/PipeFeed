module Types where

import Network.URI
import Data.List(intercalate)
import Text.Feed.Types as F

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

