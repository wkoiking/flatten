{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Flatten where

-- xlsx
-- import Codec.Xlsx
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as L
   
-- container
import Data.Tree (Forest (..), Tree (..), drawTree)

-- parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim

-- base
import Control.Applicative (liftA2, liftA, (<*))
import System.IO (hSetEncoding, utf8, openFile, IOMode(..), hGetContents, hPutStrLn, hClose, mkTextEncoding)
import Data.Either (rights, lefts)
import Data.List (groupBy, sortBy, nubBy, isPrefixOf, foldl')
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.Char (isAlpha, isDigit, isUpper)

-- directory
import System.Directory

-- text
import Data.Text (pack)

-- lens
import Control.Lens hiding (noneOf)

-- old-time
import System.Time

-- file-embed
-- import Data.FileEmbed (embedFile)

-- myparser
-- import MyParser (integer, float)
import MyParser.Numeral
import MyParser.String
import MyParser.Text

data Diff a = D a | A a | S a

data Line = Line {
    _name :: String,
    _itemID :: String,
    _numOfItem :: Maybe Double,
    _revision :: String,
    _status :: String,
    _price :: Maybe Double
} deriving (Show, Ord, Eq)

makeLenses ''Line

priceList = [
    ("NY2979A-300-04", 1000),
    ("NW2145F-112-1", 100),
    ("NY3050A-300-1", 59454726),
    ("CSLT42-8020" , 241000),
    ("RAMD-1U05AWG", 4350),
    ("NY2979A-300-01", 1000),
    ("NY2979A-300-02", 1500),
    ("NY2979A-300-04", 2500),
    ("NY2979A-300-06", 3000),
    ("NY3050A-300-1", 59454726),
    ("P2214H",18979),
    ("POWEREDGER320(99033447)",340500),
    ("RAMD-1U10BWG",6000),
    ("RP34-5",0),
    ("RP87-VS1",3250),
    ("RPG15-64LSB",9200),
    ("RPG965-62",4100),
    ("RPHV81-10LT",23100),
    ("SFP-1GLHLC",40600),
    ("SMCK12-5060M",65500),
    ("SW-KVM2HDC",32184),
--     ("SX-FR-0.75SQ-ｱｶ",0),
--     ("SX-FR-0.75SQ-ｸﾛ",0),
    ("UL2127SBTEW2X18AWG(43/0.16)LFK",1000),
    ("VM0233-VM0089",918),
    ("WAC-2004(NS)",2350000),
--     ("WV0-1.25-ﾐﾄ",0),
    ("Z230",212976),
    ("2-LCF-LCF-SM-2-Y-1.1",1000),
    ("2-LCF-LCF-SM-2-Y-3.4",2000),
    ("216-204",0),
    ("CM-600-3SSC/1TX",149160),
    ("CM-600-4TX",24550),
    ("CPRM-2P-20A",1400),
    ("CSLT42-1020",284000),
    ("DR-4524",6620),
    ("E1715S",12980),
    ("EDS-408A",57780),
    ("EDS-510A-2SFP",117100),
    ("EDS-510A-1GT2SFP",117100),
    ("EDS-619", 260200),
    ("FV1.25-5",0),
    ("FV1.25-8",0),
    ("IKS-6726-2GTXSFP",201650),
    ("IKS-6726A-2GTXSFP-HV-HV-T",201650),
    ("IM-6700-8TX",18130),
    ("IM-6700A-8TX",18130),
    ("VM1296A-VM1708S-3M", 1000),
    ("CM-600-4MSC", 149160)]

-- xlsxTemplate :: L.ByteString
-- xlsxTemplate = L.pack $ B.unpack $(embedFile "品目・構成登録および出図連絡票.xlsx")

-- IO
showTreeIO :: FilePath -> IO ()
showTreeIO inFile = do
    h1 <- openFile inFile ReadMode
--     shiftJIS <- mkTextEncoding "CP932"
--     hSetEncoding h1 shiftJIS
    as <- hGetContents h1
    case parse parseEBOM "" as of
        Right [ta] -> do
            let subTrees = {- filter (not . null . snd) $ -} toSubTrees ta
            mapM_ (putStrLn . showSubTreeMD) subTrees
            hClose h1
        Right _ -> do
            putStrLn "The file contains forest"
            hClose h1
        Left err -> do
            putStrLn $ show err
            hClose h1

-- makeTransmittalsIO :: FilePath -> FilePath -> IO ()
-- makeTransmittalsIO inFile outDir = do
--     h1 <- openFile inFile ReadMode
--     shiftJIS <- mkTextEncoding "CP932"
--     hSetEncoding h1 shiftJIS
--     as <- hGetContents h1
--     ct <- getClockTime
--     case parse parseEBOM "" as of
--         Right [ta] -> do
--             createDirectory outDir
--             mapM_ (\ (file, xlsx) -> L.writeFile (outDir ++ "/" ++ file) $ fromXlsx ct xlsx)
--                 $ map makeTransmittal $ catMaybes $ map isTransmittalNeeded $ toSubTrees ta
--             hClose h1
--         Right _ -> do
--             putStrLn "The file contains forest"
--             hClose h1
--         Left err -> do
--             putStrLn $ show err
--             hClose h1

flattenIO :: FilePath -> FilePath -> IO ()
flattenIO inFile outFile = do
    h1 <- openFile inFile ReadMode
    h2 <- openFile outFile WriteMode
--     shiftJIS <- mkTextEncoding "CP932"
--     hSetEncoding h1 shiftJIS
--     hSetEncoding h2 shiftJIS
    as <- hGetContents h1
    case parse parseEBOM "" as of
        Right ts -> do
            mapM_ (putStrLn . drawTree . fmap show) ts
            hPutStrLn h2 "品目名称,型番,員数,レビジョン,ステータス"
            mapM_ (hPutStrLn h2 . showLine) $ flattenForest $ map toProperTree ts
            hClose h1
            hClose h2
        Left err -> do
            putStrLn $ show err
            hClose h1
            hClose h2

diffIO :: FilePath -> FilePath -> FilePath -> IO ()
diffIO inFile1 inFile2 outFile = do
    h1 <- openFile inFile1 ReadMode
    h2 <- openFile inFile2 ReadMode
    h3 <- openFile outFile WriteMode
--     shiftJIS <- mkTextEncoding "CP932"
--     hSetEncoding h1 shiftJIS
--     hSetEncoding h2 shiftJIS
--     hSetEncoding h3 shiftJIS
    as <- hGetContents h1
    bs <- hGetContents h2
    case liftA2 (,) (parse parseEBOM "" as)  (parse parseEBOM "" bs) of
        Right (tas, tbs) -> do
            let las = flattenForest $ map toProperTree tas
                lbs = flattenForest $ map toProperTree tbs
            hPutStrLn h3 "品目名称,型番,員数,レビジョン,ステータス"
            mapM_ (hPutStrLn h3 . showLine) $ diffLines las lbs
            hClose h1
            hClose h2
            hClose h3
        Left err -> do
            putStrLn $ show err
            hClose h1
            hClose h2
            hClose h3

diffTreeIO :: FilePath -> FilePath -> FilePath -> IO ()
diffTreeIO inFile1 inFile2 outDir = do
    h1 <- openFile inFile1 ReadMode
    h2 <- openFile inFile2 ReadMode
--     shiftJIS <- mkTextEncoding "CP932"
--     hSetEncoding h1 shiftJIS
--     hSetEncoding h2 shiftJIS
    as <- hGetContents h1
    bs <- hGetContents h2
    case liftA2 (,) (parse parseEBOM "" as)  (parse parseEBOM "" bs) of
        Right ([ta], [tb]) -> do
            createDirectory outDir
            mapM_ (outputSubTree outDir) $ diffTree ta tb
            hClose h1
            hClose h2
        Right _ -> do
            putStrLn "Either of the two files contains forest"
            hClose h1
            hClose h2
        Left err -> do
            putStrLn $ show err
            hClose h1
            hClose h2

outputSubTree :: FilePath -> (Line, [Line]) -> IO ()
outputSubTree outDir (l, ls) = do
    h <- openFile (outDir ++ "/" ++ _name l ++ "," ++ _itemID l ++ ".csv") WriteMode
    hPutStrLn h "変更差分"
    hPutStrLn h "変更者,藤永"
    hPutStrLn h "変更日時,2015-01-31"
    hPutStrLn h "内線番号,3703"
    hPutStrLn h $ "親品目型番" ++ "," ++ _itemID l
    hPutStrLn h $ "親品目名称" ++ "," ++ _name l
    hPutStrLn h "品目名称,型番,員数,レビジョン"
    mapM_ (hPutStrLn h . showLine) ls
    hClose h

calcPriceIO :: FilePath -> IO ()
calcPriceIO inFile = do
    h1 <- openFile inFile ReadMode
--     shiftJIS <- mkTextEncoding "CP932"
--     hSetEncoding h1 shiftJIS
    as <- hGetContents h1
    case parse parseEBOM "" as of
        Right [ta] -> do
            let subTrees = toSubTrees $ calcPrice priceList ta
            mapM_ (putStrLn . showSubTreeMD) subTrees
            hClose h1
        Right _ -> do
            putStrLn "The file contains forest"
            hClose h1
        Left err -> do
            putStrLn $ show err
            hClose h1

-- algorithm

calcPrice :: [(String, Double)] -> Tree Line -> Tree Line
calcPrice asoc (Node a []) = Node (price .~ lookup (_itemID a) asoc $ a) []
calcPrice asoc (Node a ns) = Node (price .~ parentPrice $ a) ns'
 where parentPrice = foldl (liftA2 (+)) (Just 0) $ map (lineToPrice . rootLabel) ns'
       ns' = map (calcPrice asoc) ns
       lineToPrice l = liftA2 (*) (_price l) (_numOfItem l)

groupLines :: [Line] -> [Line]
groupLines ls = map lineSum $ groupBy ((==) `on` _itemID) $ sortBy (comparing _itemID) ls

lineSum :: [Line] -> Line
lineSum ls@(l:_) = numOfItem .~ mn $ l 
 where mn = foldl1 (liftA2 (+)) $ map _numOfItem ls

flattenForest :: Forest Line -> [Line]
flattenForest [] = []
flattenForest ts = groupLines $ concatMap flattenTree ts

flattenTree :: Tree Line -> [Line]
flattenTree (Node x []) = [x]
flattenTree (Node x ts) = map (numOfItem %~ f) $ concatMap flattenTree ts
 where f = liftA2 (*) (_numOfItem x)

diffLines :: [Line] -> [Line] -> [Line]
diffLines xs ys = filter ((Just 0 /=) . _numOfItem) $ gDiffLines xs ys diffLine _itemID negLine

diffLine :: Line -> Line -> Line
diffLine x y = (numOfItem .~ liftA2 (flip subtract) (_numOfItem x) (_numOfItem y) $ x)

negLine :: Line -> Line
negLine y = numOfItem %~ (liftA negate) $ y

diffTree :: Tree Line -> Tree Line -> [(Line, [Line])]
diffTree xs ys = filter (not . null . snd) $ diffSubTrees xs' ys'
 where diffSubTrees :: [(Line, [Line])] -> [(Line, [Line])] -> [(Line, [Line])]
       diffSubTrees xs ys = gDiffLines xs ys diffSubTree (_itemID . fst) negSubTree
        where diffSubTree (l, ls) (r, rs) = (l, diffLines ls rs)
              negSubTree (x, xs) = (x, map negLine xs)
       xs' = toSubTrees xs
       ys' = toSubTrees ys

-- 新しいsubTreeが追加 -> +で中身も書く
-- subTreeが無くなった -> 何も書かない
-- makeDiffTree :: Tree Line -> Tree Line -> [(Line, [Diff Line])]
-- makeDiffTree xs ys = 
--  where xs' = sortBy (comparing idOf) $ toSubTrees xs
--        ys' = sortBy (comparing idOf) $ toSubTrees ys
--        makeDiffLines [] ys = map A ys
--        makeDiffLines xs [] = map D xs
--        makeDiffLines (x:xs) (y:ys)
--            | idOf x == idOf y = case liftM (-) (_numOfItem x) (_numOfItem y) of
--                Just 0 -> S x : makeDiffLines xs ys
--                _ -> D x : A y : makeDiffLines xs ys
--            | idOf x < idOf y  = D x : makeDiffLines xs (y:ys)
--            | otherwise        = A y : makeDiffLines (x:xs) ys

gDiffLines :: [a] -> [a] -> (a -> a -> a) -> (a -> String) -> (a -> a) -> [a]
gDiffLines xs ys diff idOf neg = gDiffLines' xs ys
 where xs' = sortBy (comparing idOf) xs
       ys' = sortBy (comparing idOf) ys
       gDiffLines' [] ys = map neg ys
       gDiffLines' xs [] = xs
       gDiffLines' (x:xs) (y:ys)
           | idOf x == idOf y = (x `diff` y) : gDiffLines' xs ys
           | idOf x < idOf y  = x : gDiffLines' xs (y:ys)
           | otherwise        = neg y : gDiffLines' (x:xs) ys

isNipponModelNo :: String -> Bool
isNipponModelNo [c1,c2,c3,c4,c5,c6,c7]
    = all (liftA2 (&&) isAlpha isUpper) [c1,c2,c7] && all isDigit [c3,c4,c5,c6]
isNipponModelNo _ = False
isTransmittalNeeded :: (Line, [Line]) -> Maybe (Line, [Line])
isTransmittalNeeded (l, ls)
    | (isNipponModelNo parentID && (parentStatus == "照査待")) || (not $ null children) = Just (l, children)
    | otherwise                                                                         = Nothing
 where parentID = _itemID l
       parentStatus = _status l
       children = filter (liftA2 (&&) ((parentID `isPrefixOf`) . _itemID)
                                      ((== "照査待") . _status))
                         ls

-- makeTransmittal :: (Line, [Line]) -> (FilePath, Xlsx)
-- makeTransmittal (l, ls) = (_itemID l ++ ".xlsx", xlsx')
--  where xlsx = toXlsx xlsxTemplate
--        Just sheet = xlsx ^? ixSheet "Sheet1"
--        sheet' = cellValueAt (8, 6) ?~ (CellText $ pack parentID)
--               $ cellValueAt (9, 6) ?~ (CellText $ pack parentName)
--               $ foldl' go sheet $ [(i, pack name, pack itemID) | (i, Line name itemID (Just d) revision status) <- zip [15..] ls]
--        xlsx' = xlsx & atSheet "Sheet1" ?~ sheet'
--        parentID = _itemID l
--        parentName = _name l
--        go acc (i, text1, text2) = cellValueAt (i,2) ?~ CellText text1
--                                 $ cellValueAt (i,8) ?~ CellText text2
--                                 $ acc 

-- writer
showLine :: Line -> String
showLine (Line name itemID md revision status mPrice)
    = name ++ "," ++ itemID ++ "," ++ showMaybeDouble md
   ++ "," ++ revision ++ "," ++ status ++ "," ++ showMaybeDouble mPrice

showMaybeDouble :: Maybe Double -> String
showMaybeDouble (Just d)
    | f == 0    = show n
    | otherwise = show d
 where (n, f) = properFraction d
showMaybeDouble _ = ""

showSubTree :: (Line, [Line]) -> String
showSubTree (l, ls) = unlines $
    [_itemID l ++ "," ++ _name l
    ,"品目名称,型番,員数,レビジョン"]
    ++ map showLine ls

showSubTreeMD :: (Line, [Line]) -> String
showSubTreeMD (l, ls) = unlines $
    ["* " ++ _name l ++ "(" ++ _itemID l ++ ")" ++ " @ " ++ showMaybeDouble (_price l)]
    ++ map showLineMD ls

showLineMD (Line name itemID md revision status mprice)
    = "    * " ++ name ++ "(" ++ itemID ++ ")" ++ " x " ++ showMaybeDouble md


-- parser
parseEBOM :: Parser (Forest Line)
parseEBOM = do
    fstLine <- parseCSVLine <* char '\n'
    ls <- parseCSVLine `endBy1` char '\n'
    iLines <- mapM (toLine . zip fstLine) ls
    return $ toForest iLines

lookupP :: (Eq a) => a -> [(a,b)] -> String -> Parser b
lookupP k ls msg = case lookup k ls of
    Nothing -> fail msg
    Just x -> return x

toLine :: [(String,String)] -> Parser (Int, Line)
toLine ls = do
    indentStr <- lookupP "レベル" ls "there is no level column"
    let indent = either (const 0) fromInteger (parse integer "" indentStr)
    name <- lookupP "品目名称" ls "there is no name column"
    id <- lookupP "品目番号／型番" ls "there is no ID column"
    nStr <- lookupP "員数（分子）" ls "there is no quantity column"
    let n = either (const Nothing) Just (parse float "" nStr)
    rev <- lookupP "変更経過表示" ls "there is no revision column"
    status <- lookupP "ステータス" ls "there is no status column"
    return $ (indent, Line name id n rev status Nothing)

parseCSVLine :: Parser [String]
parseCSVLine = do
    many (noneOf [',', '\n']) `sepBy1` char ','

toForest :: [(Int, Line)] -> Forest Line
toForest [] = []
toForest [(i,x)] = [Node x []]
toForest ((i,x):xs) =  [Node x (toForest ls)] ++ toForest rs
 where (ls,rs) = span ((> i). fst) xs

toSubTrees :: Tree Line -> [(Line, [Line])]
toSubTrees node@(Node x ts)
    = nubBy ((==) `on` (_itemID . fst)) . sortBy (comparing $ _itemID . fst) $ toSubTree node : concatMap toSubTrees ts
 where toSubTree :: Tree Line -> (Line, [Line])
       toSubTree (Node x ts) = (numOfItem .~ Nothing $ x, sortBy (comparing _itemID) $ map rootLabel ts)

toProperTree :: Tree Line -> Tree Line
toProperTree root@(Node l ts) = case _numOfItem l of
    Nothing -> Node (numOfItem .~ Just 1 $ l) ts
    Just _ -> root

