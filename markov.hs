{-# LANGUAGE OverloadedStrings #-}

import Data.List hiding (words)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import System.Random
import Data.Maybe

import Debug.Trace (trace)

type Text = T.Text
type Map = M.Map

type WordsMap = Map Text (Map Text Int)
type WordMap = Map Text Int
type FreqMap = (Int, Map Int [Text]) -- The first number is the sum of the frequency of each word.

readFileTx :: FilePath -> IO Text
readFileTx = fmap T.pack . readFile

dunwich :: IO Text
dunwich = fmap T.toLower $ readFileTx "dunwich.txt"

wordFrequencies :: Text -> FreqMap
wordFrequencies = invertMap . wordFrequencies'
  where wordFrequencies' :: Text -> WordMap
        wordFrequencies' = M.fromList . sort . map myCount . group . sort . T.words
        
        myCount :: [Text] -> (Text, Int)
        myCount s = ((s !! 0), length s)

-- Assumes the list is sorted in ascending order
stripInfrequent :: Int -> [(Int, Text)] -> [(Int, Text)]
stripInfrequent min = dropWhile (\(a, b) -> a <= min)

addAfterWord :: Text -> Text -> WordsMap -> WordsMap
addAfterWord base after m = let addWord :: WordMap -> WordMap
                                addWord wm = M.insertWith (+) after 1 wm
                            in M.insertWith (\_ m -> addWord m) base (M.singleton after 1) m

buildWordsMap :: Text -> WordsMap
buildWordsMap ss = buildWordsMap' M.empty $ T.words ss
  where buildWordsMap' :: WordsMap -> [Text] -> WordsMap
        buildWordsMap' m (base:after:[]) = m
        buildWordsMap' m (base:after:ss) = let newMap = addAfterWord base after m
                                               newWords = (after:ss)
                                           in buildWordsMap' newMap newWords

invertMap :: WordMap -> FreqMap
invertMap m = M.foldrWithKey genMap (0, M.empty) m
  where genMap :: Text -> Int -> FreqMap -> FreqMap
        genMap s i (total, fm) = (total + i, M.insertWith (++) i [s] fm)

getRWord :: RealFrac a => a -> FreqMap -> Text
getRWord r (size, m) | r < 0 || r >= 1 = error "Bad random number."
                     | otherwise       =
  let offset :: Int
      offset = floor (r * fromIntegral size)
      
      freqs :: [(Int, [Text])]
      freqs = M.toList m
  in getWord offset freqs

getWord :: Int -> [(Int, [Text])] -> Text
getWord offset ((i, ws):ts) | offset >= (i * length ws) = getWord (offset - (i * length ws)) ts
                            | otherwise                 = ws !! (offset `mod` length ws)

getSequence :: RealFrac a => WordsMap -> [a] -> Text -> [Text]
getSequence m xs word = word:(getSentence' m xs word)
  where getSentence' :: RealFrac a => WordsMap -> [a] -> Text -> [Text]
        getSentence' _ []     _    = error "Needs more random numbers!"
        getSentence' m (x:xs) word = let freqMap = invertMap $ m M.! word
                                         newWord = getRWord x freqMap
                                         rest = getSentence' m xs newWord
                                     in (newWord:rest)

getRandomCached :: WordsMap -> Text -> IO Text
getRandomCached wm start = do gen <- newStdGen
                              let rs = myRandoms gen
                                  sequence = getSequence wm rs start
                                  (s, ss) = break (\a -> last (T.unpack a) == '.') sequence
                                  sentence = s ++ [(head ss)]
                              return $ T.unwords $ sentence
myRandoms :: (RandomGen g) => g -> [Float]
myRandoms = randoms

getRandomSentence :: Text -> Text -> IO Text
getRandomSentence corpus = getRandomCached wm
  where wm :: WordsMap
        wm = buildWordsMap corpus

printRandomSequenceCached :: WordsMap -> Text -> IO ()
printRandomSequenceCached ws s = getRandomCached ws s >>= TIO.putStrLn

getRandomTextCached :: WordsMap -> [Text] -> Integer -> IO Text
getRandomTextCached _  _     0 = return T.empty
getRandomTextCached ws inits l = do gen <- newStdGen
                                    let (r:_) = myRandoms gen
                                        rInt = floor (r * fromIntegral (length inits))
                                        start = getWord rInt $ zip (repeat 1) $ fmap (\x -> [x]) inits
                                    s <- getRandomCached ws start
                                    next <- getRandomTextCached ws inits (l - 1)
                                    return $ T.concat [s, "  ", next]

{-
clean :: ((Char, Char) -> [Char]) -> Text -> Text
clean f text = fromMaybe text $
                 do (t1, ts') <- T.uncons text
                    (t2, ts)  <- T.uncons ts'
                    let (h, tails) = f (t1, t2)
                    return $ h `T.cons` (clean f ((T.pack tails) `T.append` ts))

alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
alphP :: Char -> Bool
alphP = flip elem alphabet

cleanF :: (Char, Char) -> (Char, [Char])
cleanF (' ', '\'') = (' ', [])
cleanF ('(', b)    = (b, 
                  
-}
