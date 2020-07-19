# prng

The `prng` plugin provides a set of pseudo-random number generators of varying quality ranging from laughable to solid. Some of the included generators are of historical significance, while others are implementations of noise generators found in various sound chips.

Note that several of the generators are currently broken and may not generate correct results.


## API

### [procedure] `(prng::middle-square amount bits #!optional (seed 12345678))`

A generator based on the Middle-Square Method, as devised by John von Neumann in 1946. It is statistically very poor and may break with unsuitable seeds. This implementation deviates from von Neumann's design by considering only the lower half of the 8 digits extracted from the 16-digit square. This is done to mitigate the effects of the convergence towards lower number sequences that is a common trait of the Middle Square method.


### [procedure] `(prng::middle-square-weyl-seq amount bits #!optional (seed (rand bits)) (magic #xb5ad4eceda1ce2a9))`

A variation of von Neumann's Middle Square Method that applies a Weyl sequence to the Middle Square generator. Developed by Bernard Widynski. Very good statistical quality. See [https://arxiv.org/abs/1704.00358v4](https://arxiv.org/abs/1704.00358v4).


### [procedure] `(prng::blum-blum-shub amount bits #!optional (p 5651) (q 5623) (seed 31))`

[Blum Blum Shub Generator](https://en.wikipedia.org/wiki/Blum_Blum_Shub).


### [procedure] `(prng::pcg amount bits #!optional (seed (rand 64)) (increment (rand 64)))`

[Permuted Congruential Generator](https://www.pcg-random.org/). SEED and INCREMENT must be a 64-bit integer seed. BITS must be an integer between 1 and 32.


### [procedure] `(prng::xorshift64 amount bits #!optional (state (rand 64)))`

Classic non-scrambled 64-bit [Xorshift generator](https://en.wikipedia.org/wiki/Xorshift), as developed by George Marsaglia.


### [procedure] `(prng::el-cheapo-zx amount bits #!optional (state 0) (seed #x2175))`

Very fast but extremely poor 8-bit PRNG used to generate noise in various ZX Spectrum beeper engines.


### [procedure] `(prng::sid-noise amount bits #!optional (seed (rand 23)))`

A PRNG based on the noise waveform on the MOS 6581/8580 Sound Interface Device, which is a Fibonacci LSFR using the feedback polynomial x^22 + x^17 + 1. See http://www.sidmusic.org/sid/sidtech5.html. For added authenticity, initialize SEED to #x7ffff8.


### [procedure] `(prng::tia-noise amount bits #!optional (seed #x1ff))`

A PRNG based on the noise waveform (AUDCx = 8) on the Atari VCS/2600,; which is a 9-bit LSFR with a tap at bit 4, resulting in a period of 511.


### [procedure] `(prng::info . args)`

Retrieve information on the pseudo-random number generators available in this package. Call with no arguments to retrieve the complete list. Call with a symbol naming a procedure in this package to retrieve the documentation for that procedure.
