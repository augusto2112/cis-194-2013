<p align="center">
  <img src="https://dl.dropboxusercontent.com/s/1lj59ly4qxc8j7b/connecting-dots.png" width="400px" />
</p>

# Introduction to Haskell

[![Build Status](https://travis-ci.org/acamino/cis-194-2013.svg)](https://travis-ci.org/acamino/cis-194-2013)

Memories of the course **CIS 194 - Introduction to Haskell** offered for free by
the University of Pennsylvania. This course explore the fundamental concepts of
functional programming. I can't recommend this course enough.

If you want to know more about functional programming, please take a look at
[CIS 194 - Introduction to Haskell](http://www.cis.upenn.edu/~cis194/spring13/lectures.html).

## Local Development

1. Fork the project [on GitHub](https://github.com/acamino/cis-194-2013)
   and clone your fork locally.

   ```bash
   $ git clone git://github.com/username/cis-194-2013.git
   $ cd cis-194-2013
   $ git remote add upstream https://github.com/acamino/cis-194-2013.git
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
   $ stack repl
   ```

## Licence

The code in this repository is licensed under the terms of the
[MIT License](http://www.opensource.org/licenses/mit-license.html).  
Please see the [LICENSE](LICENSE) file for details.
