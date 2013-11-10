module Util where

import Network.Curl.Download
import Data.Either

grabUrl :: String -> IO (Maybe String)
grabUrl url = do
                result <- openURIString url
                case result of
                    Left error -> do 
                        print $ "Error fetching " ++ url ++ " Error: " ++ error
                        return Nothing
                    Right body ->
                        return $ Just body
    
--for error handling Eithers - crap haskell stdlib again?
isLeft (Left _) = True
isLeft _        = False

--haskell doesn't have basic triple-or-above manip funcs, wtf?
fst3::(a,b,c)->a
fst3 (a,b,c) = a