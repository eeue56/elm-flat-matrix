# elm-flat-matrix


Other Elm matrix solutions currently use a list of lists, or an array of arrays. This project uses a flat array, to increase internal speed. It mirrors Core.Array's API as closely as possible.

Matrix.Extra provides extra functions for working with matricies, such as mathematical operations.

The implementation is intended to be as fast as possible in pure Elm - there are no Native modules, and everything is done through using Elm functions.
