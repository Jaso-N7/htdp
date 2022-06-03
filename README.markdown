# Htdp - Learning How to Design Programs

## About

Practice and solutions to exercises from both edX's (UBCx HtC1x) How to Code: Simple Data and most of the exercises from the books HtDP/1e & HtDP/2e.

## Installation

`(ql:quickload '(:ptester :htdp))`

### Testing

`(asdf:test-system :htdp)`

Implementations tested:

* CCL (main implementation)
* ECL

If time permits, I would also like to implement Property-Based testing.

* NOTE: Some tests will fail due to floating point precision, which is implementation dependent.

## Author

* Jason Robinson

## Copyright

The copyright of the materials are held by the original authors of the course(s) and the books. My solutions are made for practice,
experience and for reference.
