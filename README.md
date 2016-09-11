# GoatSwim
GoatSwim is a time-series database heavily inspired by the Gorilla paper
released by Facebook. It is intended to be used as a UNIX daemon that
awaits for data points and stores them in main memory. The project is
in it's early state and no further implementation details and goals
are available.

## Dependencies
Currently the following Haskell packages are required:
 * `bifunctors`
 * `bytestring`
 * `deepseq`
 * `floating-bits`
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

