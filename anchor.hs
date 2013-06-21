import Text.HTML.TagSoup
import Data.List
import Data.Maybe
import Text.Regex.Base
import Text.Regex.Posix
import Codec.Text.IConv
import qualified Data.ByteString.Lazy.Char8 as C

getAnchor :: (Tag [Char] -> Bool) -> [Tag [Char]] -> [(String,C.ByteString)]
getAnchor f [] = []
getAnchor f (t:ts) = case (f t) of
                        True  -> (fromAttrib "href" t, convert "CP1251" "UTF-8" $ C.pack $ fromTagText $ head ts) : (getAnchor f ts)
                        False ->  getAnchor f ts
