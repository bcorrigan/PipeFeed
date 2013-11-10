module Transforms where

import Text.Regex.TDFA ((=~))
--import Network.URI
--import Network.HTTP(simpleHTTP,getRequest,getResponseBody)
import Util
import Types
import Control.Concurrent

--fetches full length version of article and packs into body
fetchfull::Article -> IO Article
fetchfull article = do 
                        print $ "Fetching full article from " ++ itemurl article
                        newbody <- grabUrl $ itemurl article
                        threadDelay $ 10*1000*1000
                        return $ updateArticle article newbody
                        
--new body only has content between start_re and end_re                        
regexsnip::String -> String -> Article -> IO Article
regexsnip start_re end_re article = do
                        let (_,_,post) = body article =~ start_re :: (String, String, String)
                        let (newbody,_,_) = post =~ end_re :: (String,String,String)
                        return $ updateArticle article newbody

                        
