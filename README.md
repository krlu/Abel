# MET-CS-789

An algebraic structures library (currently under development) that was originally a term project for MetCS 789 Cryptography Fall 2017 

Feel free to contribute if you like! 

## Currently Supports 
- Ring and Field Operations for: 
   - Rational Numbers 
   - Real Numbers 
   - Integers 
   - Complex Numbers
- Arithmetic Operations for Univariate Polynomials with arbitrary Coefficients from any Ring 
- Small Univariate Polynomial Factorization 
- Small Univariate Polynomial Root Finding 
- Large Integer Factorization 
- Large Integer Exponentiation 

## Requirements 
- scala 2.12.3 (or higher) 
- sbt 0.13.8 (or higher)

## Building and Testing the project 
Navigate to root directory and type: 
> sbt clean compile \
> sbt test
