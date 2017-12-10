# MET-CS-789

Term Project for MetCS 789 Cryptography Fall 2017

## Requirements 
- scala 2.12.3 (or higher) 
- sbt 0.13.8 (or higher)

## Building and Testing the project 
Navigate to root directory and type: 
> sbt clean compile \
> sbt test

Note that the tests may take up to 15 minutes to complete, due to testing against large prime numbers. 
If you wish to skip these tests, navigate to the [Test Suite](https://github.com/krlu/MET-CS-789/blob/master/src/test/scala/Tests.scala) and comment out the two tests at the bottom

## Running the Experiment 
Go to the the [Main Experiment Class](https://github.com/krlu/MET-CS-789/blob/master/src/main/scala/org/bu/metcs789/FinalProjectExperiment.scala) and run the main method

