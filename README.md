# Abel

A computer algebra library design to support applications in Cryptography, Geometry, and other applications. 

Feel free to contribute if you like! 

## Current Features 
- Ring and Field Operations for: 
   - Rational Numbers 
   - Real Numbers 
   - Integers 
   - Complex Numbers
- Arithmetic Operations for Univariate Polynomials with arbitrary Coefficients from any Ring 
- Univariate Polynomial Factorization 
- Univariate Polynomial Root Finding 
- Large Integer Factorization 
- Large Integer Exponentiation 

## Requirements 
- scala 2.12.11 (or higher) 
- sbt 0.13.8 (or higher)

## Building and Testing the project 
Navigate to root directory and type: 
> sbt clean compile \
> sbt test

## References
Methods for root finding and factorization built on top of the 
[Rings](https://github.com/PoslavskySV/rings) Library by Stanislav Poslavsky

