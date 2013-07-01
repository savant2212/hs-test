import System.IO
import Network.HTTP
import Text.HTML.TagSoup
import Data.List
import Data.Maybe
import Text.Regex.Base
import Text.Regex.Posix
import Control.Concurrent
import Data.Text.ICU.Convert 

main :: IO ()
main = do
	cnv <- open "CP1251" Nothing 
    body <- toUnicode cnv $ simpleHTTP (getRequest "http://stulchik.net/main.shtml?ras") >>= getResponseBody
    
    let categories = getAnchor (\x -> (x ~== TagOpen "a" []) && ( fromAttrib "href" x =~ "ras.shtml\\?kat")) $ parseTags(body)
	
	

