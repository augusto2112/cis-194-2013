# CIS 194: Introduction to Haskell (Spring 2013)

[![Build Status](https://travis-ci.org/acamino/cis-194-2013.svg)](https://travis-ci.org/acamino/cis-194-2013)

Exploring functional programming for fun. If you want to know more please check
out this [great course](http://www.cis.upenn.edu/~cis194/spring13/lectures.html).

## Local Development

1. First clone this repository and `cd` into it.

   ```bash
   $ git clone git@github.com:acamino/cis-194-2013.git
   $ cd cis-194-2013
   ```

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/).

1. Get the appropriate GHC for the project.

   ```bash
   $ stack setup
   ```

1. Make sure the tests succeed.

   ```bash
   $ stack test
   ```

1. If you want to launch a REPL.

   ```bash
   $ stack ghci
   ```
