# ECSStdLib

The **ECS** standard library defines modules supporting application development  
with the **ECS Oberon** compiler, including:

 * Modules for basic types : [Char](https://tenko.github.io/ECSStdLib/src/Std.Char.mod.html), [Integer](https://tenko.github.io/ECSStdLib/src/Std.Integer.mod.html), [Cardinal](https://tenko.github.io/ECSStdLib/src/Std.Cardinal.mod.html), [Real](https://tenko.github.io/ECSStdLib/src/Std.Real.mod.html), [ArrayOfChar](https://tenko.github.io/ECSStdLib/src/Std.ArrayOfChar.mod.html), [ArrayOfByte](https://tenko.github.io/ECSStdLib/src/Std.ArrayOfByte.mod.html) & [ArrayOfSet](https://tenko.github.io/ECSStdLib/src/Std.ArrayOfSet.mod.html) .
 * Modules for string handling : [String](https://tenko.github.io/ECSStdLib/src/Std.String.mod.html) & [StringPattern](https://tenko.github.io/ECSStdLib/src/Std.StringPattern.mod.html).
 * Module for date and time support : [DateTime](https://tenko.github.io/ECSStdLib/src/Std.DateTime.mod.html).
 * Module for container and alogrithms : [ADTStream](https://tenko.github.io/ECSStdLib/src/Std.ADTStream.mod.html), [ADTDictionary](https://tenko.github.io/ECSStdLib/src/Std.ADTDictionary.mod.html), [ADTList](https://tenko.github.io/ECSStdLib/src/Std.ADTList.mod.html), [ADTSet](https://tenko.github.io/ECSStdLib/src/Std.ADTSet.mod.html), [ADTTree](https://tenko.github.io/ECSStdLib/src/Std.ADTTree.mod.html) & [ADTVector](https://tenko.github.io/ECSStdLib/src/Std.ADTVector.mod.html).
 * Module for cross platform basic OS support : [OS](https://tenko.github.io/ECSStdLib/src/Std.OS.mod.html), [OSDir](https://tenko.github.io/ECSStdLib/src/Std.OSDir.mod.html), [OSFile](https://tenko.github.io/ECSStdLib/src/Std.OSFile.mod.html), [OSPath](https://tenko.github.io/ECSStdLib/src/Std.OSPath.mod.html) & [OSStream](https://tenko.github.io/ECSStdLib/src/Std.OSStream.mod.html).
 * Module for testing & benchmark : [O2Testing](https://tenko.github.io/ECSStdLib/src/Std.O2Testing.mod.html) & [O2Timing](https://tenko.github.io/ECSStdLib/src/Std.O2Timing.mod.html).
 * Modules for data handling: [DataLZ4](https://tenko.github.io/ECSStdLib/src/Std.DataLZ4.mod.html) & [DataConfig](https://tenko.github.io/ECSStdLib/src/Std.DataConfig.mod.html) .
 * Module for memory allocation on embedded platforms : [SysMem](https://github.com/tenko/ECSStdLib/blob/main/src/Std.SysMem.mod).

The [Const](https://tenko.github.io/ECSStdLib/src/Std.Const.mod.html) module defines constants reused throughout the library.

The stream concept is used troughout the modules to support formatting, reading and writing of data.  
The module [Type](https://tenko.github.io/ECSStdLib/src/Std.Type.mod.html) defines the basic stream type interface.

OS support is covered for the **Windows** and **Linux** platforms.  
In addition for embedded platform basic support is added for the [ARMv7M](https://github.com/tenko/ECSStdLib/blob/main/src/armv7mrun.asm) platform using  
the semihost interface to the host computer.

The library is tested with about 750 unit tests.  
These tests also can be inspected for basic usage where the documentation is not clear.  

## Building & Running tests

Building

> make

Running tests (Linux)

> make TestMain

Running tests (Windows)

> make TestMain.exe

## Hello Word

HelloWorld.mod:

```modula-2
MODULE HelloWord;
 
IN Std IMPORT OSStream;
    
BEGIN
    OSStream.StdOut.WriteString("Hello World!");
    OSStream.StdOut.WriteNL;
END HelloWord.
```

Running (Windows)

```
ecsd HelloWorld.mod std.lib /c/EigenCompilerSuite/runtime/win64api.obf
./Test.exe
Hello World!
```

## Note

Complete API Documentation: [Link](https://tenko.github.io/ECSStdLib/)  
Currently a patched version of the **ECS** compiler is needed [Link](https://github.com/tenko/ECS)  
With the next release of the **ECS** compiler these patches should be included. 
