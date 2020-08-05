# prng

The `prng` plugin provides a set of pseudo-random number generators of varying quality ranging from laughable to solid. Some of the included generators are of historical significance, while others are implementations of noise generators found in various sound chips.


## API

### [procedure] `(prng::middle-square amount maxint #!optional (seed 12345678))`

A generator based on the Middle-Square Method, as devised by John von Neumann in 1946. It is statistically very poor and may break with unsuitable seeds. This implementation deviates from von Neumann's design by considering only the lower half of the 8 digits extracted from the 16-digit square. This is done to mitigate the effects of the convergence towards lower number sequences that is a common trait of the Middle Square method.


### [procedure] `(prng::middle-square-weyl-seq amount maxint #!optional (seed (rand maxint)) (magic #xb5ad4eceda1ce2a9))`

A variation of von Neumann's Middle Square Method that applies a Weyl sequence to the Middle Square generator. Developed by Bernard Widynski. Very good statistical quality. See [https://arxiv.org/abs/1704.00358v4](https://arxiv.org/abs/1704.00358v4).


### [procedure] `(prng::blum-blum-shub amount maxint #!optional (p 5651) (q 5623) (seed 31))`

[Blum Blum Shub Generator](https://en.wikipedia.org/wiki/Blum_Blum_Shub). SEED must be greater than 1, and neither p nor q may be a factor of SEED. The generator will replace invalid seeds with a random seed if SEED fails to satisfy these conditions.


### [procedure] `(prng::pcg amount maxint #!optional (seed (rand 64))`

Xorshift-Rotate-Right [Permuted Congruential Generator](https://www.pcg-random.org/).


### [procedure] `(prng::xorshift64 amount maxint #!optional (state (rand 64)))`

Classic non-scrambled 64-bit [Xorshift generator](https://en.wikipedia.org/wiki/Xorshift), as developed by George Marsaglia.


### [procedure] `(prng::randu amount maxint #!optional (seed (bitwise-ior 1 (rand 32))))`

A notoriously flawed LCG-type PRNG developed by IBM in the 1960s.


### [procedure] `(prng::el-cheapo-zx amount maxint #!optional (seed (rand 16) (magic #x2175))`

Very fast but extremely poor 8-bit PRNG used to generate noise in various ZX Spectrum beeper engines.


### [procedure] `(prng::dmg-noise amount maxint #!optional (seed (rand 15)))`

A PRNG based on the noise waveform generator of the Gameboy APU, which is a 15-bit LFSR with a tap at bit 1.


### [procedure] `(prng::sid-noise amount maxint #!optional (seed (rand 23)))`

A PRNG based on the noise waveform on the MOS 6581/8580 Sound Interface Device, which is a Fibonacci LFSR using the feedback polynomial x^22 + x^17 + 1. See http://www.sidmusic.org/sid/sidtech5.html. For added authenticity, initialize SEED to #x7ffff8.


### [procedure] `(prng::tia-noise amount maxint #!optional (seed (rand 9))`

A PRNG based on the noise waveform (AUDCx = 8) on the Atari VCS/2600,; which is a 9-bit LFSR with a tap at bit 4, resulting in a period of 511. For added authenticity, initialize SEED to #x1ff.


### [procedure] `(prng::pi-digits amount maxint #!optional (seed (rand 10)))`

Not an actual PRNG. Creates random-looking integer sequences from pi. Uses the Bailey–Borwein–Plouffe formula to calculate hexadecimal digits of pi. SEED is the initial fractional digit index, ie. SEED = 0 starts extracting digits from the first fractional digit.


### [procedure] `(prng::info . args)`

Retrieve information on the pseudo-random number generators available in this package. Call with no arguments to retrieve the complete list. Call with a symbol naming a procedure in this package to retrieve the documentation for that procedure.
