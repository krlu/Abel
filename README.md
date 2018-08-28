# MET-CS-789

An algebraic structures library (currently under development) that was originally a term project for MetCS 789 Cryptography Fall 2017 

Feel free to contribute if you like! 

## Currently Supports 
- Arithmetic Operations for Univariate Polynomials with arbitrary Coefficients extending any ring 
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

Note that the tests may take up to 15 minutes to complete, due to testing against large prime numbers. 
If you wish to skip these tests, navigate to the [Test Suite](https://github.com/krlu/MET-CS-789/blob/master/src/test/scala/org/bu/metcs789/Tests.scala) and comment out the two tests at the bottom
