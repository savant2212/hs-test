import Text.HTML.TagSoup
import Data.List
import Data.Maybe
import Text.Regex.Base
import Text.Regex.Posix
import Codec.Text.IConv
import Network.HTTP
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy.Char8 as C

data Tale = Tale {
					name 	:: String,
					author 	:: String,
					genre	:: [String],
					date	:: Maybe DateTime,
					id		:: Int,
					rate	:: Maybe Float,
					desc	:: String
				}

getAnchor :: (Tag [Char] -> Bool) -> [Tag [Char]] -> [(String,C.ByteString)]
getAnchor f [] = []
getAnchor f (t:ts) = case (f t) of
                        True  -> (fromAttrib "href" t, convert "CP1251" "UTF-8" $ C.pack $ fromTagText $ head ts) : (getAnchor f ts)
                        False ->  getAnchor f ts

						
parsePage :: String -> Int -> [Tale]
parsePage link index = do
				body <- simpleHTTP (getRequest link) >>= getResponseBody
				elems = filter (\x -> (x ~== TagOpen "p" [("align","justify")]) $ parseTags body
				--some kind of magic
				case ( elems == [] ) ->
					True -> []
					False -> elems : parsePage link (index + 1)
				
helper1 :: [Tag String] -> [[Tag String]]
helper1 [] 		= []
helper1 (x:xs)	= case ( x ~== TagOpen "p" [("align","justify")] ) of
					True	-> let res = span (\x -> x ~/= TagClose "p" [] ) >> [fst res] : helper1 snd res
					False	-> helper1 xs