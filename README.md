# Geometric Algebra in Mathematica

This repository contains package files and example notebooks for using several types of projective geometric algebra in Mathematica. Packages are provided for both rigid and conformal geometric algebra over one, two, and three dimensional base spaces as well as projective spacetime geometric algebra.

These packages allow special symbols to be used as infix operators for the exterior products, geometric products, and inner products. The symbols for the wedge and antiwedge products have Unicode values U+2227 and U+2228.  The symbols for the geometric product and geometric antiproduct have Unicode values U+27D1 and U+27C7. The symbols for the dot and antidot products have Unicode values U+2022 and U+2218.

In order to use the symbols for the geometric and inner products, the text in the file `UnicodeCharactersAdditions.tr` must first be added to the Mathematica system file `UnicodeCharacters.tr`. On Windows, this file can be found in the `C:/Program Files/Wolfram Research/Mathematica/<version>/SystemFiles/FrontEnd/TextResources` directory.

A C++ implementation of the 2D and 3D rigid and conformal geometric algebras is available in the [Terathon Math Library](https://github.com/EricLengyel/Terathon-Math-Library/) repository.

See Eric Lengyel's [Projective Geometric Algebra website](https://projectivegeometricalgebra.org) for more information about projective geometric algebra in general.
