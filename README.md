# Efficient clustered n-Body simulation in futhark

This project aims to implement the barnes-hut algorithm which runs in
_O(n log n)_ as compared to the naive version, which runs in _O(n²)_.


# Result

Should be a comparison between a collection of implementations:

* The existing
  [futhark implementation](https://github.com/diku-dk/futhark-benchmarks/tree/master/accelerate/nbody).
* A naive barnes-hut implementation.
* A flat barnes-hut implementation, ie. Where we apply all flattening and other
  rules we learned from the
  [Parallel Functional Programming](https://kurser.ku.dk/course/ndak14009u/2019-2020)-course

Of the implementations we would compare speed and draw conclusions
scaling/implementation wise.


## Barnes hut sources

* ["The barnes hut algorithm"](https://www.tp1.ruhr-uni-bochum.de/~grauer/lectures/compI_IIWS1819/pdfs/lec10.pdf),
  slideshows from Thomas Trost, The Institute for Theoretical Physics, Ruhr
  University Bochum.
* ["An efficient CUDA Implementation of the tree-based Barnes Hut n-Body Algorithm"](https://iss.oden.utexas.edu/Publications/Papers/burtscher11.pdf),
  CUDA implementation paper, M. Burtscher, K. Pingali.
  + [slideshow](https://www.cs.utexas.edu/~pingali/CS395T/2009fa/lectures/Barnes-Hut.pdf)
    (basically the same thing in a different format)
