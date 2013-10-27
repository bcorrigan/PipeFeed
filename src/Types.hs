module Types where

import Network.URI
import Data.List(intercalate)

--probably needs timestamp and other things
data Article = Article { title :: String
                   , body :: String
                   , author :: String
                   , itemurl :: String
                   , hash :: String
                   } deriving Show
                   
data Feed = Feed { name :: String
                 , items :: [Article]
                 , feedurl :: String
                 , transforms :: [Article -> IO Article]
                 } 
                 
data Config = Config { feeds :: [Feed]
                     , cache :: String
                     , rss_store :: String
                     } deriving Show
                     
instance Show Feed where
        show f = "Feed. name: " ++ name f ++ " feedurl: " ++ (show $ feedurl f) ++ " num items: " ++ (show $ length $ items f) ++ " Items:" ++ (intercalate ", " (map show (items f))) ++ " num transforms: " ++ (show $ length $ transforms f)

--newBody :: Article -> Article
--newBody a = 