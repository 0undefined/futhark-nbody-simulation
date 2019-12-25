# Efficient clustered n-Body simulation in futhark

This project aims to implement the barnes-hut algorithm which runs in
_O(n log n)_ as compared to the naive version, which runs in _O(n²)_.


# Result

Should be a comparison between a collection of implementations:

* The existing [futhark implementation](0).
* A naive barnes-hut implementation.
* A flat barnes-hut implementation, ie. Where we apply all flattening and other
  rules we learned from the [Parallel Functional Programming](1)-course

Of the implementations we would compare speed and draw conclusions
scaling/implementation wise.


## Barnes hut sources

* [_"The barnes hut algorithm"_](itp_slides), slideshows from Thomas Trost, The Institute for
  Theoretical Physics, Ruhr University Bochum.
* [_"An efficient CUDA Implementation of the tree-based Barnes Hut n-Body
  Algorithm"_](cuda_impl), CUDA implementation paper, M. Burtscher, K. Pingali.
  + [_slideshow_](burtscher_slides) (basically the same thing in a different
    format)

[0]: https://github.com/diku-dk/futhark-benchmarks/tree/master/accelerate/nbody
[1]: https://kurser.ku.dk/course/ndak14009u/2019-2020
[itp_slides]: https://www.tp1.ruhr-uni-bochum.de/~grauer/lectures/compI_IIWS1819/pdfs/lec10.pdf
[cuda_impl]: https://iss.oden.utexas.edu/Publications/Papers/burtscher11.pdf
[burtscher_slides]: https://www.cs.utexas.edu/~pingali/CS395T/2009fa/lectures/Barnes-Hut.pdf
