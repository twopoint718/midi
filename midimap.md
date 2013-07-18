MIDI File format
================

Header chunk
------------

    [4d 54 68 64]  [00 00 00 06]  [00 00]  [00 01]  [01 e0]
     M  T  h  d       size (6)      fmt     #trk    time div

Track chunk
-----------

    [4d 54 72 6b]  [00 00 00 22]
     M  T  r  k      size (34)

### Set tempo

    [00]  [ff]  [51]  [03]  [07 a1 20]
    time  meta  tempo size  500k=120bpm

### Key signature

    [00]  [ff]  [59]  [02]  [00]  [00]
    time  meta  ksig  size  key   scal
                            (C)   (maj)

### Time signature

    [00]  [ff]  [58]  [04]  [04]  [02]  [30]  [08]
    time  meta  tsig  size  numer denom metro 32nds
                            (4)  (2^2=4) (48) (8/32 in q.note)

### Note on

    [01]  [90]  [3c]  [69]
    time  n.on  note  vely
                (60)  (105)

### Note off

    [81 6f]  [80]  [3c]  [00]
    time
    var len  n.off note  vely
    (239)

### End of track

    [1a]  [ff]  [2f]  [00]
    time  meta  eot   size
    (26)

Raw dump of above
-----------------

    4d 54 68 64 00 00 00 06  00 00 00 01 01 e0 4d 54  |MThd..........MT|
    72 6b 00 00 00 22 00 ff  51 03 07 a1 20 00 ff 59  |rk..."..Q... ..Y|
    02 00 00 00 ff 58 04 04  02 30 08 01 90 3c 69 81  |.....X...0...<i.|
    6f 80 3c 00 1a ff 2f 00                           |o.<.../.|
