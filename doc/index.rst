#####
Intro
#####

This library is developed for the *ECS Oberon-2 Compiler* to
support application development.

Currently the *Windows*, *Linux* & embedded *ARMv7M* targets are supported.

The :ref:`Const` module defines constants reused throughout the library.

The stream concept is used troughout the modules to support formatting,
reading and writing of data. The :ref:`Type` module defines the basic stream
type interface.

The unit tests can be inspected for basic usage.
For more complete usage the :ref:`DataConfig` module is
referenced as it uses many of the features of the library.

.. toctree::
    :maxdepth: 1
    :caption: Common:
    :hidden:

    src/Std.Config.mod
    src/Std.Const.mod
    src/Std.Type.mod

.. toctree::
    :maxdepth: 1
    :caption: Basic Data Types:
    :hidden:

    src/Std.ArrayOfByte.mod
    src/Std.ArrayOfChar.mod
    src/Std.ArrayOfSet.mod
    src/Std.Cardinal.mod
    src/Std.Char.mod
    src/Std.DateTime.mod
    src/Std.Integer.mod
    src/Std.Real.mod
    src/Std.String.mod

.. toctree::
    :maxdepth: 1
    :caption: Abstract Data Types:
    :hidden:

    src/Std.ADTBasicType.mod
    src/Std.ADTDictionary.mod
    src/Std.ADTList.mod
    src/Std.ADTSet.mod
    src/Std.ADTStream.mod
    src/Std.ADTTree.mod
    src/Std.ADTVector.mod

.. toctree::
    :maxdepth: 1
    :caption: Library:
    :hidden:

    src/Std.DataConfig.mod
    src/Std.DataLZ4.mod
    src/Std.O2Testing.mod
    src/Std.O2Timing.mod
    src/Std.OS.mod
    src/Std.OSDir.mod
    src/Std.OSFile.mod
    src/Std.OSHost.mod
    src/Std.OSPath.mod
    src/Std.OSStream.mod
    src/Std.StringPattern.mod


##################
Indices and tables
##################

* :ref:`genindex`
* :ref:`search`
