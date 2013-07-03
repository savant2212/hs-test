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
                        True  -> (takeWhile (/='_') $ fromAttrib "href" t, fromTagText $ head ts) : (getAnchor f ts)
                        False ->  getAnchor f ts

					
helper1 :: (StringLike str) => [Tag str] -> [[Tag str]]
helper1 [] 		= []
helper1 xs = let res = (span (\x -> x ~/= TagClose "p" ) $ dropWhile ( ~/= TagOpen "p" [("align","justify")] ) xs ) in (drop 1 $ fst res) : (helper1 $ snd res)

parseBlock ::[[Tag String]] -> Tale
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

genreMap :: String -> index -> IO [Tale]
genreMap link index = do
						cnv <- open "CP1251" Nothing
						stories <- fmap Data.Text.unpack $ fmap (toUnicode cnv) $ fmap C.pack  $ simpleHTTP (getRequest ("http://stulchik.net/" ++ link ++ "_" ++ (show index))) >>= getResponseBody
						return ( pageConvert stories )						
							
pageConvert :: String -> [Tale]
pageConvert stories = map parseBlock $ map (splitWhen (~== TagOpen "br" [])) $ reverse . tail . reverse $ helper1 $ parseTags stories
						
					
main :: IO ()
main = do
	cnv <- open "CP1251" Nothing 
    genres <- fmap Data.Text.unpack $ fmap (toUnicode cnv) $ fmap C.pack  $ simpleHTTP (getRequest "http://stulchik.net/main.shtml?ras") >>= getResponseBody
   
    let categories = getAnchor (\x -> (x ~== TagOpen "a" []) && ( fromAttrib "href" x =~ "ras.shtml\\?kat")) $ parseTags(genres)

    


