Markov-chain Password Generator
===============================

Multiple-word passwords are highly entropic, but difficult to remember.  If
they sound a bit Englishy, they're easier...

```
ghci markov.hs 
GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( markov.hs, interpreted )
Ok, modules loaded: Main.
*Main> dunwich <- readFileTx "dunwich.txt" 
*Main> let wordsMap = buildWordsMap dunwich 
*Main> getRandomCached wordsMap $ T.pack "the"
"the visible along , and matting visible cattle ever have begotten on the rusty kitchen ; so vaguely threatening this long-distance sprayer of certain hours of the ruined Whateley farmhouse set about whom he dast ."
*Main> getRandomCached wordsMap $ T.pack "the"
"the telescope at what he up and the strange things , chinless face on the crowd with my Gawd , a matter into silence , close to a solid abaout ."
*Main> getRandomCached wordsMap $ T.pack "the"
"the first month overdue ."
*Main> getRandomCached wordsMap $ T.pack "the"
"the powder in odd , finally quiet dread and his firm denial on the Whateleys quarters or dimension outside will have begotten on which lay where the boy Luther Brown , yet can he feared the countryside had come up a force that put suddenly of the key at which many New England backwaters .
```

Disclaimer
----------

This is provided as-is.  I suggest you investigate the cryptographic security
of using Markov chains to generate passwords before you use it.  (Doing an
analysis of this is on my to-do list, but I haven't done it yet.)

This code is also a mess, and refactoring it is on my to-do list too.  Sorry
about that.