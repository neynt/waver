# waver

Waver is an OCaml library for writing sounds as functions over time.

We build up abstractions from ground-level signal synthesis to music-theoretic
notions of melody and harmony to provide a comprehensive base for applications
from sound effect design to musical expression.

## abstraction stack

- A `Signal.t` is a function over time.
- A note is a MIDI note number. (69 = A4, 1 per semitone)
- A `Temperament.t` is a function from note to Hz.
- An `Instrument.t` is a function from Hz to `Signal.t`.
