# ECSStdLib
ECS Oberon-2 Compiler Standard Library Work in progress.

The intended use case is small personal utilities and
applications on the Windows and Linux platform.
For now only the Windows platform is tested and Linux
is partial supported. In addition baremetal platforms
arm32t are supported.

The API is a not intended to be compatibe with legacy Oberon-2
code and rather use comparable Python standard library API or POSIX
where it fits.

The unit tests can be inspected for basic usage.
For more complete usage the DataConfig module is
referenced as it uses many of the features of the library.

Currently a patched version of the ECS compiler is needed [Link](https://github.com/tenko/ECS)
With the next release of the ECS compiler these patches should be included.

Sphinx API Documentation [Link](https://tenko.github.io/ECSStdLib/)

## TODO

 * Support hex float literal.
 * Replace Regex with Lua type patterns.