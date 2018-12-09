# Marko (Haskell!)
## Installation
### Configuration
Start by configuring your desired IRC server and handle in Main.hs before compiling. Currently, it connects to irc.wobscale.website.

Channel joining is determined by a function ran on every supported Event. By default, it's based off of the bot having "+x". This is configurable in Main.hs as `joinEvent`.

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

## Implementation
The basic idea behind this bot, without the Haskell bits, is described in a blog post [here](http://blog.brod.es/markov/chain/marko/ruby/bot/irc/2018/07/06/my-first-markov-chain-bot.html).

In this specific implemenation, I've represented the bot's memory as a double [HashMap](http://hackage.haskell.org/package/unordered-containers-0.2.9.0/docs/Data-HashMap-Strict.html). It's built by reading the passed filename and parsing it into words and sentences with [parser combinators](http://hackage.haskell.org/package/attoparsec-0.13.2.2/docs/Data-Attoparsec-ByteString.html#v:takeWhile1). Parsing, flattening, and converting into HashMaps yields the following itermediate types:

```haskell
-- Initial data from the file is digested into arrays of lines.
ByteString -> [ByteString]

-- Parse each line as a sentence. Breaking it into chains which have a "first" and a "second" word.
-- Think of this as zipping a sentence's words on itself, where the second element in the tuple is offset by one.
[ByteString] -> [[(ByteString, ByteString)]]

-- These are then flattened as each chain is now unique to its sentence.
[[(ByteString, ByteString)]] -> [(ByteString, ByteString)]

-- Begin parsing into HashMaps. The fastest and most memory efficient way is to break these chains into a list of
-- (key, value) pairs. We can now scan our list and begin folding the chains into a HashMap representation of
-- firstWord -> secondWord -> numOccurrences.
type WeightedNextChain a = HashMap.HashMap a Int
type NextChainData     a = HashMap.HashMap a (WeightedNextChain a)

[(ByteString, ByteString)] -> NextChainData ByteString
```

We've now ended up with `HashMap ByteString (HashMap ByteString Int)`, where the outermost HashMap is a ByteString of the first word in a chain. The inner HashMaps are the representation of all the possible next words, with their individual occurrences in the passed source text.

We can then use this to generate text that's derived from our original source. If we choose a word in our outer map, we can find out what word comes next. In order to get it randomly, we can sum up all the occurrence counts of next words and choose a number between 1 and the sum. We can then chain this over and over to generate the words to follow:

```
11:50:21 brodes | ->
11:50:21 marko2 | -> (a, Char) -> (a, Char) -> (a, Char) -> (a, Char) -> (a, Char) -> (a, Char) -> (a, Char) ->
                | f acc going around and i moved on the guy who want it in the guy who want it in the
```

Going backwards is not so easy. Since we did not store what the preceding word was, only the next, we will need a chain HashMap. I've chosen to generate one where the source text was reversed for simplicity. We can then apply the same method above and fill in the blanks:

```
11:50:49 brodes | do
11:50:49 marko2 | archlinux . archlinux . archlinux . archlinux . archlinux . archlinux . archlinux . archlinux
                | . If you're good idea, I'll do its cliche, but arm .. maybe it's in the files created a backup
                | your type IO because i think that probes
```
