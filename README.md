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

The bot will begin connecting to IRC, read the file, parse it, and authenticate:

```
> NICK marko2
> USER marko2 0 * :markovirc
Reading from file.
  Read 21640823 bytes
Completed
Got 499993 lines
Loading forward word database... Done! 213870 words and 3477257 chains loaded.
Loading reverse word database... Done! 224834 words and 3477257 chains loaded.
< Notice: Auth :*** Looking up your hostname...
```

## Input Format
Input text should be provided with each sentence or message being on a new line. If input text is all on one line, this bot will likely never finish a complete sentence.

This bot uses [ByteString](http://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString.html)s to represent text, so it does not support unicode.


