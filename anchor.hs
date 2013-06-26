import Text.HTML.TagSoup
import Data.List
import Data.Maybe
import Text.Regex.Base
import Text.Regex.Posix
import Text.StringLike
import Codec.Text.IConv
import Network.HTTP
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy.Char8 as C

data Tale = Tale {
					name 	:: String,
					author 	:: String,
					genre	:: [String],
					date	:: Maybe String,
					id		:: Int,
					rate	:: Maybe Float,
					desc	:: String
				}

getAnchor :: (Tag [Char] -> Bool) -> [Tag [Char]] -> [(String,C.ByteString)]
getAnchor f [] = []
getAnchor f (t:ts) = case (f t) of
                        True  -> (fromAttrib "href" t, convert "CP1251" "UTF-8" $ C.pack $ fromTagText $ head ts) : (getAnchor f ts)
                        False ->  getAnchor f ts

						
--parsePage :: String -> Int -> [Tale]
--parsePage link index = do
--				body <- simpleHTTP (getRequest link) >>= getResponseBody
--				let elems = helper1 $ parseTags body
--				--some kind of magic
--				case ( elems == [] ) of
--					True -> []
--					False -> elems : parsePage link (index + 1)
				
helper1 :: (StringLike str) => [Tag str] -> [[Tag str]]
helper1 [] 		= []
helper1 xs = let res = (span (\x -> x ~/= TagClose "p" ) $ dropWhile ( ~/= TagOpen "p" [("align","justify")] ) xs ) in (drop 1 $ fst res) : (helper1 $ snd res)

