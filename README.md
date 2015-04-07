Proof of Concept for a binary pickling library

- Uses Typed Arrays for high performance in JS
- Efficient encoding of Integers (1-5 bytes)
- Special encoding for UUID strings (1 + 16 bytes, instead of the normal 36)
- Encoded strings are cached and can be reused to speed up decoding (also saves space and memory)
