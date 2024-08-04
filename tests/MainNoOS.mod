MODULE TestMain;
IMPORT Testing := O2Testing IN Std;
IMPORT TestArrayOfByte;
IMPORT TestArrayOfChar;
IMPORT TestArrayOfSet;
IMPORT TestCardinal;
IMPORT TestInteger;
IMPORT TestString;
IMPORT TestRegex;
IMPORT TestDateTime;
IMPORT TestOSPath;
IMPORT TestOS;
IMPORT TestADTBasicType;
IMPORT TestADTList;
IMPORT TestADTSet;
IMPORT TestADTDictionary;
IMPORT TestADTVector;
IMPORT TestADTTree;
IMPORT TestADTStream;

CONST
    M = "TestMain";

VAR
    test : Testing.TEST;
BEGIN
    Testing.Initialize(test, M);
    TestArrayOfByte.Run(test);
    TestArrayOfChar.Run(test);
    TestArrayOfSet.Run(test);
    TestCardinal.Run(test);
    TestInteger.Run(test);
    TestString.Run(test);
    TestRegex.Run(test);
    (* TestDateTime.Run(test); *)
    TestOSPath.Run(test);
    TestOS.Run(test);
    TestADTBasicType.Run(test);
    TestADTList.Run(test);
    TestADTSet.Run(test);
    TestADTDictionary.Run(test);
    TestADTVector.Run(test);
    TestADTTree.Run(test);
    TestADTStream.Run(test);
    Testing.Finalize(test);
END TestMain.