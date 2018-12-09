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
-- 
-- parseIncoming takes the Maybe Events returned from the parser and returns just the events.
parseIncoming :: String -> [Event]
parseIncoming str = do
  let events = readP_to_S ircParser str

  if length events > 0 then 
    map (fromJust) $ filter (isJust) (fst . last $ events)
  else
    []

-- Parses a string of 0+ events.
ircParser :: ReadP [Maybe Event]
ircParser = do
    many $ choice [otherMessages, ping]
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
          let (user, _, _, _) = parseUser umask
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
  let (nick, _, _, _) = parseUser user
  channel <- munch1 (/= ' ')
  _string " :"
  msg <- consumeEOL
  return $ Just $ MessageEvent ("", channel, Just $ nick) msg

parseUser :: String -> (String, String, String, String)
parseUser x = fst $ last $ readP_to_S user x
  where
    -- user mask
    user = do
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
