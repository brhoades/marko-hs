module Marko where

import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.Either (fromLeft, either)
import Data.Traversable (mapAccumL)
import Data.Maybe (fromJust)
import System.Random (randomR, StdGen)
import qualified Data.ByteString as D
import qualified Data.Attoparsec.ByteString as P

 -- Word <-> (NextWord <-> Weight)
type WeightedNextChain a = HashMap.HashMap a Int
type NextChainData     a = HashMap.HashMap a (WeightedNextChain a)

-------------------------
-- Parsing and General IO
-------------------------

processChainsBackwards :: (Ord a, Hashable a) => [[a]] -> NextChainData a
processChainsBackwards = processChains . (map reverse)
 
processChains :: (Ord a, Hashable a) => [[a]] -> NextChainData a
processChains src = HashMap.map (either (flip (HashMap.singleton) 1) id) hmWithEither
  where
    -- chains :: [(a, a)]
    chains = (concat . map (\x -> zip x (tail x))) src
    chains' = map (\(x, y) -> (x, Left y)) chains
    hmWithEither = HashMap.fromListWith resolver chains'
    resolver :: (Eq a, Hashable a) => Either a (WeightedNextChain a) -> Either a (WeightedNextChain a) -> Either a (WeightedNextChain a) 
    resolver newVal oldVal = Right $ (HashMap.alter alterfn newVal' oldVal')
      where
        oldVal'   = either (\x -> HashMap.singleton x 1) id oldVal
        newVal' = fromLeft undefined newVal
        alterfn v = Just $ case v of
          Just x      -> x + 1
          Nothing -> 1

---------------------------------------------------
-- Parsing files. TODO This belongs somewhere else
---------------------------------------------------
-- Returns bytestrings with count or fewer newlines
lineParser :: Int -> P.Parser ([D.ByteString])
lineParser count = do
  lines' <- P.count count $ takeEol
  return $ filter ((/= 0) . D.length) lines'
  where
    takeEol = do
      P.skipWhile endline
      P.takeWhile (not . endline)
    endline = \x -> x == (toW '\n') || x == (toW '\r')
    toW = fromIntegral . fromEnum


-- Words are [a-zA-Z0-9] and are separated by [ ]+.
chainParser :: P.Parser ([[ D.ByteString]])
chainParser = P.many' parseLine
  where
    parseLine :: P.Parser ([D.ByteString])
    parseLine = do
      P.skipWhile (\x -> endline x || splitter x)
      P.manyTill parseWord (P.satisfy endline)
    parseWord :: P.Parser (D.ByteString)
    parseWord = do
      P.skipWhile splitter
      res <- P.choice [P.takeWhile1 (not . wordTerminator), P.takeWhile1 punct]
      P.skipWhile splitter
      return res
    splitter = (== toW ' ')
    endline = \x -> x == (toW '\n') || x == (toW '\r')
    punct = \x -> x == (toW '.') || x == (toW '!') || x == (toW '?')
    wordTerminator = \x -> (splitter x || endline x || punct x)
    
    toW = fromIntegral . fromEnum

readFromFile :: String -> IO [[D.ByteString]]
readFromFile filename = do
  putStrLn "Reading from file."
  contents <- D.readFile filename
  putStrLn $ "  Read " ++ (show $ D.length contents) ++ " bytes"
  case P.parseOnly chainParser contents of
    Left _ -> do
      putStrLn "Error parsing provided file. No chains returned."
      return []
    Right chains -> do
      putStrLn "Completed"
      putStrLn $ "Got " ++ (show $ length chains) ++ " lines"
      return chains

readFromFileChunked :: String -> Int -> IO ([D.ByteString])
readFromFileChunked filename numLines = do
  putStrLn "Reading from file."
  contents <- D.readFile filename
  putStrLn $ "  Read " ++ (show $ D.length contents) ++ " bytes"
  case P.parseOnly (lineParser numLines) contents of
    Left _ -> 
      -- putStrLn "Error parsing provided file. No chains returned."
      return []
    Right chains -> do
      putStrLn "Completed"
      putStrLn $ "Got " ++ (show $ length chains) ++ " chunks of " ++ (show numLines) ++ " lines."
      return chains

----------------------
-- More clever parsers
----------------------

-- parses all at once then transforms all at once.
parseString :: D.ByteString -> (HashMap.HashMap D.ByteString Int, [[Int]])
parseString string = (lookupMap, converted)
  where
    chains = case P.parseOnly chainParser string of
      Left _  -> error "Parser error"
      Right c -> c
    lookupMap = snd $ foldr (\e (idx, hm) -> atomsToNumbers e idx hm) (0, HashMap.empty) chains
    converted = (fmap . fmap) (\x -> fromJust (HashMap.lookup x lookupMap)) chains


-- incrementally parses and changes them to Ints
parseStrings :: [D.ByteString] -> (HashMap.HashMap D.ByteString Int, [[Int]])
parseStrings strings = (lookupMap, converted)
  where
    chains = getStrings (P.parse chainParser) (strings <> mempty) -- always at least one element
    lookupMap = snd $ foldr (\e (idx, hm) -> atomsToNumbers e idx hm) (0, HashMap.empty) chains
    converted = (fmap . fmap) (\x -> fromJust (HashMap.lookup x lookupMap)) chains
    getStrings fn (x:xs) = case fn x of
      P.Fail _ _ _ -> error "Parser error"
      P.Partial fn' -> getStrings fn' xs
      P.Done _ r   -> r
    -- Never occurs as parsinge terminates on empty string.
    getStrings _ []      = undefined

--------------------
-- Generating Chains
--------------------

-- Given a ChainRelationMap, generates a random atom to come next, weighted by occurrence in test data.
getRandomNext :: (Eq a, Hashable a) => StdGen -> NextChainData a -> a -> Maybe a
getRandomNext g n c = case HashMap.member c n of
    False -> Nothing
    True  -> case results of
      Just x  -> Just $ fst x
      Nothing -> Nothing
    where
      freqPerAtom = n HashMap.! c
      -- Generate an i that's between "0" and the maximum cumulative frequency of c
      -- We can then work backwards in freqPerand sum the frequencies until we find
      -- a atom with sum frequency < ours.
      i = fst $ randomR (0, cumFreq freqPerAtom) g
      summedFreqs = snd $ (mapAccumL (\x acc -> (x + acc, x + acc)) 0) freqPerAtom
      results = safeHead $ HashMap.toList $ HashMap.filter (>= i) summedFreqs

getRandomSentence :: (Eq a, Hashable a) => StdGen -> NextChainData a -> a -> [a]
getRandomSentence g nextData starter = getRandomSentence' starter (0 :: Integer)
  where
    -- getRandomSentence' :: (Eq b, Hashable b, Num c) => b -> c -> [b]
    getRandomSentence' l depth = case getRandomNext g nextData l of
      Just newWord -> [newWord] ++ (if depth < 15 then getRandomSentence' newWord (depth + 1) else []) -- terminate to avoid loops
      Nothing      -> [l]

----------------------
-- Statistic functions
----------------------

cumFreq :: WeightedNextChain a -> Int
cumFreq = HashMap.foldr (+) 0

totalRelations :: NextChainData a -> Int
totalRelations = HashMap.foldr (\x -> (+) (cumFreq x)) 0

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

------------------------------------------------
-- Clever stuff that was too clever to be useful
------------------------------------------------

-- Not Foldable.foldMap, but folds over a 2-d list, passing an accumulator from the last list.
foldrMap :: (Traversable t) => (a -> b -> b) -> b -> t (t a) -> b
foldrMap fn starter src = foldr (\list acc -> (foldr fn acc list)) starter src

-- Convert atoms to a numbers with a custom function + accumulator.
-- t: container type.
-- a: source type (a -> d).
-- b: a counter
-- c: some sort of lookup type to get from a -> d.
-- d: destination type (a -> d).
atomsToNumbersWith :: (Eq a, Traversable t, Enum b) => (a -> b -> c -> d -> (b, c, d)) -> t a -> b -> c -> d -> (b, c, d)
atomsToNumbersWith accfn from idx conversion starterlist = foldr (accfn') (idx, conversion, starterlist) from
  where
    accfn' e (idx', conversion', d') = accfn e idx' conversion' d'

-- Consumes the passed [a] and adds them to the initial lookup table starting with the index provided.
atomsToNumbers :: (Eq a, Hashable a, Enum b, Traversable t) => t a -> b -> HashMap.HashMap a b -> (b, HashMap.HashMap a b)
atomsToNumbers src idx lookupHM = headT $ atomsToNumbersWith (\e idx' hm _ -> lookupAndInc e idx' hm) src idx lookupHM ()
  where
    lookupAndInc e idx' hm = case HashMap.lookup e hm of
      Just _  -> (idx', hm, ())
      Nothing -> (succ idx', HashMap.insert e idx' hm, ())
    headT (x, y, _) = (x, y)

-- Walks over an array and replaces all a's with Integers, returning a lookup map, a reverse map, the new array, and the largest index..
atomsToNumbersForwardsBackwards :: (Eq a, Hashable a) => [a] -> HashMap.HashMap a Int -> [a] -> Int -> (HashMap.HashMap a Int, [a], [Int], Int)
atomsToNumbersForwardsBackwards  list hm reversehm idx = foldr accfn (hm, reversehm, [], idx) list
  where
    accfn :: (Eq a, Hashable a) => a -> (HashMap.HashMap a Int, [a], [Int], Int) -> (HashMap.HashMap a Int, [a], [Int], Int)
    accfn e (convertHM, reverseConvertHM, newlist, n) = case HashMap.lookup e convertHM of
      Just e' -> (convertHM, reverseConvertHM, e' : newlist, n)
      Nothing -> (HashMap.insert e n convertHM, reverseConvertHM ++ [e], n : newlist, n + 1)

