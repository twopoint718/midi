module Event.Midi
( Key(..)
, Note(..)
, Scale(..)
, noteOffEvent
, noteOnEvent
, setKeySignatureEvent
, setTimeSignatureEvent
) where

import Event.Midi.Note (Note(..), noteOffEvent, noteOnEvent)
import Event.Midi.KeySignature (Key(..), Scale(..), setKeySignatureEvent)
import Event.Midi.TimeSignature (setTimeSignatureEvent)
