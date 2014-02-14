module Midi.Event.Meta
( MetaEvent (..)
, Key (..)
, Scale (..)
) where

import Midi.ByteStream

data Key = C
         | G
         | D
         | A
         | E
         | B
         | FSharp
         | CSharp
         | GSharp
         | DSharp
         | ASharp
         | F
         deriving (Enum, Eq, Read, Show)

data Scale = Major
           | Minor
           deriving (Bounded, Enum, Eq, Read, Show)

data MetaEvent = EndTrack
               | SetKeySignature Key Scale
               | SetTempo Int
               | SetTimeSignature Int Int
               deriving (Eq, Show)

instance Streaming MetaEvent where
  -- all meta-events are prepended by 0xff
  wordsFor e = 0xff : wordsFor' e

wordsFor' EndTrack =
  [ 0x2f                -- end of track
  , 0x00 ]              -- size

wordsFor' (SetKeySignature key scale) =
  [ 0x59                -- key signature event
  , 0x02                -- size of arguments
  , toWord8 key         -- key signature
  , toWord8 scale ]     -- scale (major/minor)

-- TODO: this needs more research
wordsFor' (SetTempo tempo) =
  [ 0x51                -- we're setting tempo
  , 0x03                -- size of arguments (3)
  , 0x07                -- tempo (in µs/quarter note)
  , 0xa1
  , 0x20 ]              -- TODO: 3-byte nums (500,000)

wordsFor' (SetTimeSignature numer denom) =
  [ 0x58                -- time signature event
  , 0x04                -- size of arguments
  , fromIntegral numer  -- numerator
  , denom'              -- denominator
  , 0x30                -- metronome (?)
  , 0x08 ]              -- num of 32nd notes to a beat
  -- denominator is input as a power of 2
  where denom' = floor $ logBase (fromIntegral 2) (fromIntegral denom)
