# Marko (Haskell!)
## Installation
### Configuration
Start by configuring your desired IRC server and handle in Main.hs before compiling. Currently, it connects to irc.freenode.net.

Channel joining is based off of a specific mode being set to the bot. Currently, the default is "+x" stored in Main.hs `joinMode`. If more advanced joining events are required, consider tweaking `handleEvent` in Main.hs.

### Building
Get stack, clone the repository, and run `stack build`.

## Usage
Using the `marko` executable, pass a file with text to use as source material as the first parameter:

```
$ marko path/to/some_text.txt
```

The bot will read the file, parse it, and then join the server when ready:



## Input Format
Input text should be provided with each sentence or message being on a new line. If input text is all on one line, this bot will likely never finish a complete sentence.

This bot uses [ByteString|http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString.html#t:ByteString]s so it does not support unicode.


