module Util where

import Network.Browser
import Network.HTTP.Base (rspBody)
import Network.HTTP (getRequest)

grabUrl :: String -> IO String
grabUrl url = fmap (rspBody . snd) . browse $ do
    -- Disable logging output
    setErrHandler $ const (return ())
    setOutHandler $ const (return ())

    setAllowRedirects True
    request $ getRequest url
    
    