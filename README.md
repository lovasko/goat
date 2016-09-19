# GoatSwim

[![Build Status](https://travis-ci.org/lovasko/goat.svg?branch=master)](https://travis-ci.org/lovasko/goat)
[![Goat Status](https://img.shields.io/badge/goat-swimming-blue.svg)](https://github.com/lovasko/goat)

GoatSwim is a time-series database heavily inspired by the Gorilla paper
released by Facebook. It is intended to be used as a UNIX daemon that
awaits for data points and stores them in main memory. The project is
in its early state and no further implementation details and goals
are available.
![logo](http://smnd.sk/lovasko/goatswim.png)

## Dependencies
The GHC version used throughout the project is `8.0.1`. All packages
are sourced from the Stackage 7 LTS repository.

Currently the following Haskell packages are required:
 * `bifunctors`
 * `bytestring`
 * `floating-bits`
 * `safe`
 * `split`

This list is subject to change (e.g. `cereal` might be added) and should
not be taken as final.

## Supported platforms
GoatSwim can be successfully compiled on any platform that is supported
by the GHC compiler. Currently tested platforms are:
 * Mac OS X 10.8
 * FreeBSD 10.0

Platforms not included in this list (and supported by GHC) are supposed
to work but have not been tested. You are invited to do so and report
the results.

## License
GoatSwim is licensed under the terms of the 3-clause BSD license.
For more information please consult the [LICENSE](LICENSE.md) file.
In case that you need a different license, feel free to contact me.

## Author
Daniel Lovasko

