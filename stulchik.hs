import System.IO
import Network.HTTP
import Text.HTML.TagSoup
import Data.List
import Data.Maybe
import Text.Regex.Base
import Text.Regex.Posix


main :: IO ()
main = do
    http <- simpleHTTP (getRequest "http://stulchik.net/main.shtml?ras") >>= getResponseBody
    
    putStrLn getAnchor (\x -> (x ~== TagOpen "a" []) && ( fromAttrib "href" x =~ "ras.shtml\\?kat")) $ parseTags(http)
    
	
getAnchor :: (Tag str -> Bool) -> [Tag str] -> [(str,Maybe str)]
getAnchor f [] = []
getAnchor f [t:ts] = case (f t) of
                           True  -> (fromAttrib "href" t, maybeTagText $ head ts) : (getAnchor f ts)
                           False ->  getAnchor f ts


--  filter (\x -> (x ~== TagOpen "a" []) && fromAttrib "href" x `matches` "ras.shtml\\?kat") $ parseTags(http)

--    let tags = dropWhile (~== TagOpen "a" []) (parseTags http)
--    done tags where
--        done xs = case xs of
--            [] -> putStrLn $ "\n"
--            _ -> do
--                putStrLn $ show $ head xs
--                done (tail xs)
