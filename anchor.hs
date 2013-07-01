import Network.HTTP
import System.IO
import Text.HTML.TagSoup
import Data.List
import Data.Maybe
import Text.Regex.Base
import Text.Regex.Posix
import Text.StringLike

import Text.HTML.TagSoup
import Data.Text.ICU.Convert
import Data.Char
import Data.List.Split

data Tale = Tale {
					name 	:: String,
					link	:: String,
					author 	:: String,
					genre	:: [String],
					date	:: String,
					num		:: Int,
					rate	:: Maybe Float,
					desc	:: String
				} deriving(Show)

getAnchor :: (Tag [Char] -> Bool) -> [Tag [Char]] -> [(String,String)]
getAnchor f [] = []
getAnchor f (t:ts) = case (f t) of
                        True  -> (fromAttrib "href" t, fromTagText $ head ts) : (getAnchor f ts)
                        False ->  getAnchor f ts

						
parsePage :: String -> Int -> [Tale]
parsePage link index = 				
				body <- getResponseBody =<< simpleHTTP (getRequest (link ++ (show index)))
				
				let elems = helper1 $ parseTags body
				--some kind of magic
				case ( elems == [] ) of
					True -> []
					--False -> map parseBlock elems : parsePage link (index + 1)
				
helper1 :: (StringLike str) => [Tag str] -> [[Tag str]]
helper1 [] 		= []
helper1 xs = let res = (span (\x -> x ~/= TagClose "p" ) $ dropWhile ( ~/= TagOpen "p" [("align","justify")] ) xs ) in (drop 1 $ fst res) : (helper1 $ snd res)

parseBlock ::[[Tag [Char]]] -> Tale
parseBlock x = Tale {
						name 	= fromTagText $ ( x !! 0 ) !! 5,
						link 	= fromAttrib "href" $ ( x !! 0 ) !! 4,
						author	= fromTagText $ ( x !! 1 ) !! 3,
						genre	= map (reverse . dropWhile isSpace ) $ splitOn "," $ fromTagText $ ( x !! 2 ) !! 3,
						date	= "", --map (reverse . dropWhile isSpace ) $ takeWhile (/= '(') $ fromTagText $ x !! 3 !! 3,
						num		= read (takeWhile (/=')') $ last $ splitOn "#" $ dropWhile (/= '(') $ fromTagText $ x !! 3 !! 3) :: Int,
						desc 	= fromTagText $ x !! 5 !! 1,
						rate 	= Nothing
					}