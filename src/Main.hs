module Main where
import qualified Network.Simple.TCP.TLS as Network
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as D
import Data.List (intercalate)
import Text.Printf
import qualified Marko
import Data.Maybe (isJust, fromJust)
import System.Environment (getArgs)
import System.Random (newStdGen, StdGen)
import System.IO (hFlush, stdout)
import IRC
 
server    :: [Char]
server    = "irc.wobscale.website"
port      :: [Char]
port      = "6697"
chan      :: [Char]
chan      = "##ircbottesting"
nick      :: [Char]
nick      = "marko2"
joinEvent :: Event -> Bool
joinEvent (ModeEvent user mode) = user == nick && mode == "+x"
joinEvent _                     = False

type ChainData = Marko.NextChainData D.ByteString
 
main :: IO [()]
main = do
  service <- Network.getDefaultClientSettings (server, pack port)
  Network.connect (service) server port passoff
  where
    passoff (h, _) = do
      write h "NICK" nick
      write h "USER" (nick ++ " 0 * :markovirc")

      args <- getArgs
      let filename = head args
      rawData <- Marko.readFromFile filename

      let term = D.pack $ map (fromIntegral . fromEnum) "\n"
      -- Eager load databases so that we don't timeout.
      let wordDB = Marko.processChains rawData $ Just term
      putStr "Loading forward word database... "
      hFlush stdout
      putStrLn $ "Done! " ++ (show $ length wordDB) ++ " words and " ++ (show $ Marko.totalRelations wordDB) ++ " chains loaded."

      let wordDBBackwards = Marko.processChainsBackwards rawData $ Just term
      putStr "Loading reverse word database... "
      hFlush stdout
      putStrLn $ "Done! " ++ (show $ length wordDBBackwards) ++ " words and " ++ (show $ Marko.totalRelations wordDBBackwards) ++ " chains loaded."


      listen wordDB wordDBBackwards h
 
write :: Network.Context -> String -> String -> IO ()
write h s t = do
  Network.send h $ pack $ concat [s, " ", t, "\n"]
  printf  "> %s %s\n" s t
 
listen :: ChainData -> ChainData -> Network.Context -> IO [()]
listen forwardsDB backwardsDB h = forever $ do
  g <- newStdGen
  maybeRawStr <- Network.recv h
  case maybeRawStr of
    Nothing  -> print "Connection to server closed gracefully."
    Just rawIRC -> do
      let parsedEvent = parseIncoming $ unpack rawIRC

      case parsedEvent of
        Just events -> do
          mapM_ (putStrLn . show) events
          let responses = (map (handleEvent g forwardsDB backwardsDB) events)
          mapM_ write' $ map fromJust $ filter (isJust) responses
        Nothing -> return ()
  where
    forever a = do _ <- a; forever a
    write' = uncurry (write h)

handleEvent :: StdGen -> ChainData -> ChainData -> Event -> Maybe (String, String)
handleEvent g forwardsDB backwardsDB event = do
  if joinEvent event then 
    Just ("JOIN", chan)
  else
    case event of
      Ping s -> do
        Just ("PONG ", s)
      MessageEvent (_, channel, Just user) m -> do
        let parts = words m
        case length parts of
          0 -> Nothing
          _ -> do
              -- since we used words, massage the [[Char]] back to a ByteString choice
              let choice = head parts
              let packedChoice = D.pack $ map (fromIntegral . fromEnum) choice
              let right = cleanup $ Marko.getRandomSentence g forwardsDB packedChoice
              let left = cleanup $ reverse $ Marko.getRandomSentence g backwardsDB packedChoice
              if user == nick then Nothing else
                  if channel == nick then
                    Just ("PRIVMSG " ++ user, intercalate " " $ concat [left, [choice], right])
                  else
                    Just ("PRIVMSG " ++ channel, intercalate " " $ concat [left, [choice], right])
      -- Just (NoticeEvent s) -> do
      _ -> Nothing
  where
    cleanup = filter (/= "\n") . (map unpack)
