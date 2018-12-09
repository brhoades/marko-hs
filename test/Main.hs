module Main where

import qualified Marko as M
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString as D
import qualified Data.Attoparsec.ByteString as P
import Data.Hashable
import Data.Word
import Test.Hspec
import Test.QuickCheck

type File = D.ByteString
type PositiveInt = Int

instance Arbitrary (D.ByteString) where
  arbitrary = sized $ \n -> do
    str <- vectorOf n genChainableWord
    return $ D.pack str
  shrink x
    | D.length x == 0 = []
    | otherwise       = [D.take n x | n <- [0..D.length x]]

genPositiveInt :: Gen PositiveInt
genPositiveInt = choose (1, 100000)

genChainableFile :: Gen File
genChainableFile = sized $ \n -> do
  str <- vectorOf n genChainableWord
  return $ D.pack str

-- Generates ASCII characters with frequencies corresponding to an interesting parsable dataset.
genChainableWord :: Gen Word8
genChainableWord = do
  -- Excludes control characters below 32, space (32), ! (33), . (46), ? (63). Total: 219
  let base = elements [toWord8 x | x <- [0..255], x > 33, x < 255, x /= 63, x /= 46]
  let space = elements [toWord8 32]
  let punct = elements $ map toWord8 [33, 46, 63, 10]

  -- Average word is ~5 letters. Average sentence ~14 words. Average sentences per line is 1.
  -- Using weighted random, 219 Word8s are just letters. Targets: 1 in 5 space, 1 in 14 !/./?/\n.
  -- LCD of 14 5 and 219 = 15330. punct gets 15330/(14*3) (365), space 15330/5 (3066), newline 15330/14 (1095),
  frequency [(51, base), (14, space), (5, punct)]
  where
    toWord8 x = toEnum x :: Word8
    -- everything else 15330/219 (70)

genChains :: (Arbitrary a, Hashable a, Ord a) => Gen [[a]]
genChains = do
  a <- arbitrary
  elements [a]

getFlatChains :: [[a]] -> [(a, a)]
getFlatChains = concat . map (\x -> zip x $ tail x)

prop_pointsToNext :: (Hashable a, Ord a) => [[a]] -> Bool
prop_pointsToNext chains = do
  all (== True) $ map pointsToNextChain $ getFlatChains chains
  where 
    chainData = M.processChains chains
    pointsToNextChain chain = case HashMap.lookup (fst chain) chainData of
      Just v  -> HashMap.member (snd chain) v
      Nothing -> False

prop_nextCumFreq :: (Hashable a, Ord a) => [[a]] -> Bool
prop_nextCumFreq source = cumFreqs chainData
  where 
    chainData            = M.processChains source
    flatChains           = getFlatChains source
    getFreq pair         = length $ filter (== pair) flatChains
    cumFreqs             = HashMap.null . HashMap.filterWithKey cumFreqs'
    cumFreqs' first      = not . HashMap.null . (HashMap.filterWithKey $ \next freq -> getFreq (first, next) /= freq)
-- fn: line count, source text, parsed output. Return bool for success.
parseAssertion :: (Int -> File -> [D.ByteString] -> Bool) -> Int -> File -> Bool
parseAssertion fn nLines text = case P.parseOnly (M.lineParser nLines) text of
  Left _       -> False
  Right parsed -> fn nLines text parsed

countNewLines :: D.ByteString -> Int
countNewLines str = D.length $ D.filter (== 10) str

prop_atMostNLines :: PositiveInt -> File -> Bool
prop_atMostNLines = parseAssertion (\nLines _ parsed -> all ((<= nLines) . countNewLines) parsed)
  where

prop_hasAllText :: PositiveInt -> File -> Bool
prop_hasAllText = parseAssertion (\_ source chunks -> (numBytesOut chunks) == (expectedSize source chunks))
  where
    numBytesOut = foldr ((+) . D.length) 0
    sizeBeforeLastNewline text = D.length $ D.reverse $ (D.dropWhile (/= (fromIntegral $ fromEnum '\n'))) (D.reverse text)
    expectedSize source chunks = max (sizeBeforeLastNewline source - length chunks) 0

main :: IO ()
main = hspec $ do
  describe "processChains" $ do
    it "Each element refers to the next element from the source" $ do
      property (prop_pointsToNext :: [[Int]] -> Bool)
    it "Each element has the cumulative frequency of each next value" $ do
      property (prop_nextCumFreq :: [[Int]] -> Bool)
  describe "lineParser" $ do
    it "Parses passed text into chunks of at most n lines" $ do
      property prop_atMostNLines
    it "Has all passed text in chunks" $ do
      property $ prop_hasAllText
