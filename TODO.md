# TODO

## Desired API

The end goal I have in mind is a track monad that automagically calculates
details like track byte length while providing significantly simplified
access to MIDI- and meta-events. Such a monad might take the following form:

    main :: IO ()
    main = midi $ do
      track 1 $ do
        setTempo $ bpm 120
        setTimeSignature 4 4
        setKeySignature A Minor
        playNote C4 8 $ do
          delay 4 >>= bendPitch -2 2
        playNote A5 16
