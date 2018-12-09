module IRC where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Maybe (isJust, fromJust)

type Source  = (Server, Channel, Maybe User)
type User    = String
type Channel = String
type Message = String
type Server  = String

data Event = NoticeEvent Message | MessageEvent Source Message | JoinEvent Channel | ModeEvent User String | Ping Server

instance Show Event where
  show event = case event of
    Ping s -> do
      "< PONG " ++ s
    NoticeEvent m -> do
      "< Notice: " ++ m
    MessageEvent (_, channel, Just user) m -> do
      "< " ++ channel ++ " " ++ user ++ ": " ++ m
    MessageEvent (_, channel, Nothing) m -> do
      "< " ++ channel ++ " " ++ ": " ++ m
    ModeEvent user m -> do
      "< Mode '" ++ user ++ "' '" ++ m ++ "'"
    _ -> "< Unprintable Event"

-- TODO: I imagine this is some monad-enabled IRC state return... so JOINs update a state monad.
-- For now, we just return what the message represents.

-- parseIncoming is a thin veneer around parsing strings to irc Events.
parseIncoming :: String -> Maybe [Event]
parseIncoming str = do
  -- unreducedEvents :: [([Event], String)], per-iteration results from parsing IRC lines.
  let unreducedEvents = readP_to_S ircParser str

  if length unreducedEvents > 0 then
    Just $ fst . last $ unreducedEvents
  else
    Nothing
  where
    unreducedEvents = readP_to_S ircParser str

-- Parses a string of 0+ events.
ircParser :: ReadP [Event]
ircParser = do
  result <- (many $ choice [otherMessages, ping])
  return $ map fromJust $ filter isJust result
  where
    ping = do
      skipMany $ satisfy eolws
      _string "PING"
      _char ' '
      server <- consumeEOLWS
      return $ Just $ Ping server

    otherMessages = do
      skipMany $ satisfy eolws
      _char ':'
      server <- munch1 (not . eolws)
      _char ' '
      eventType <- munch1 (/= ' ')
      _char ' '
      getEvent server eventType

    getEvent server x
       | x == "PRIVMSG"  = handleMSG server
       | x == "NOTICE"   = do
          msg <- consumeEOL
          return $ Just $ NoticeEvent msg
       | x == "MODE"   = do
          umask <- munch1 (not . eolws)
          let user = parseUser umask
          _char ' '
          mode <- munch1 (not . eolws)
          return $ Just $ ModeEvent user mode
       | all (isDigit) x = do
          msg <- consumeEOL
          return $ Just $ NoticeEvent msg
       | otherwise       = do
          _ <- consumeEOL
          return Nothing

handleMSG :: String -> ReadP (Maybe Event)
handleMSG user = do
  let nick = parseUser user
  channel <- munch1 (/= ' ')
  _string " :"
  msg <- consumeEOL
  return $ Just $ MessageEvent ("", channel, Just $ nick) msg

parseUser :: String -> String
parseUser str = fst $ last $ readP_to_S user str
  where
    user = do
      -- get the nick for sure
      nick <- munch (\x -> x /= '!' && x /= '@' && (not $ eolws x))
      -- consume a umask, if present
      consumeEOLWS
      return nick

parseUserMask :: String -> (String, String, String, String)
parseUserMask str  = fst $ last $ readP_to_S userMask str
  where
    userMask = do
      nick <- munch (/= '!')
      _char '!'
      name <- munch (/= '@')
      _char '@'
      host <- consumeEOL
      let mask = nick ++ "!" ++ name ++ "@" ++ host
      return (nick, name, host, mask)

-- Parsing utility functions
_char :: Char -> ReadP ()
_char x = char x >> return ()
_string :: String -> ReadP ()
_string x = string x >> return ()

eol          :: Char -> Bool
eol          =  \x -> (x == '\n' || x == '\r')
eolws        :: Char -> Bool
eolws        =  \x -> (x == '\n' || x == '\r' || x == ' ')

consumeEOL   :: ReadP String
consumeEOL   = munch (not . eol)
consumeEOLWS :: ReadP String
consumeEOLWS = munch (not . eolws)
