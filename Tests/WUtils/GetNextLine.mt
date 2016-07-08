(* Tests for: CalculateParse`GeneralLibrary`GetNextLine

   Author: danielb
*)

TestExecute[$TestAbortTime = 600]

TestExecute[
    If[TrueQ[Quiet[Get["CalculateTestEnvironment.m"]]===$Failed],
        Get[
        StringCases[$CurrentFile,
        inputfile:(StartOfString~~___~~$PathnameSeparator~~"Tests"~~$PathnameSeparator)~~___
        :> inputfile<>"Utilities"<>$PathnameSeparator<>"CalculateTestEnvironment.m"][[1]]
        ]]
]

TestExecute[$CalculateDataPacletsInit = False;  << "CalculateLoader`"]

TestExecute[$TestAbortTime = $TestAbortTimeInitial]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\ndef\nghi", 1]
    ,
    {5, 7}
    ,
    TestID -> "GetNextLine-20150221-J7EP5L"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\ndef\nghi", 3]
    ,
    {5, 7}
    ,
    TestID -> "GetNextLine-20150221-0D4TWI"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\ndef\nghi", 4]
    ,
    {5, 7}
    ,
    TestID -> "GetNextLine-20150221-ZAJ8MC"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\ndef\nghi", 5]
    ,
    {9, 11}
    ,
    TestID -> "GetNextLine-20150221-5L33DN"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\ndef\nghi", 7]
    ,
    {9, 11}
    ,
    TestID -> "GetNextLine-20150221-NWVV6B"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\ndef\nghi", 8]
    ,
    {9, 11}
    ,
    TestID -> "GetNextLine-20150221-H5BLHT"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\ndef\nghi", 9]
    ,
    {12, 11}
    ,
    TestID -> "GetNextLine-20150221-VS35AO"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\ndef\nghi", 11]
    ,
    {12, 11}
    ,
    TestID -> "GetNextLine-20150221-E31BRS"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi", 1]
    ,
    {6, 8}
    ,
    TestID -> "GetNextLine-20150221-W2E35Q"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi", 3]
    ,
    {6, 8}
    ,
    TestID -> "GetNextLine-20150221-I69MFR"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi", 4]
    ,
    {6, 8}
    ,
    TestID -> "GetNextLine-20150221-FSI1SI"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi", 5]
    ,
    {6, 8}
    ,
    TestID -> "GetNextLine-20150221-HFBIPQ"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi", 6]
    ,
    {11, 13}
    ,
    TestID -> "GetNextLine-20150221-OAP3TO"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi", 8]
    ,
    {11, 13}
    ,
    TestID -> "GetNextLine-20150221-Q4C8V8"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi", 9]
    ,
    {11, 13}
    ,
    TestID -> "GetNextLine-20150221-HB7DW7"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi", 10]
    ,
    {11, 13}
    ,
    TestID -> "GetNextLine-20150221-8WSHXP"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi", 11]
    ,
    {14, 13}
    ,
    TestID -> "GetNextLine-20150221-BO7JGK"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi", 13]
    ,
    {14, 13}
    ,
    TestID -> "GetNextLine-20150221-YLCY6Y"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi\n", 14]
    ,
    {15, 14}
    ,
    TestID -> "GetNextLine-20150221-SEKUSE"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi\r\n", 14]
    ,
    {16, 15}
    ,
    TestID -> "GetNextLine-20150221-SINWCV"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\r\ndef\r\nghi\r\n", 15]
    ,
    {16, 15}
    ,
    TestID -> "GetNextLine-20150221-6OEV0J"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\n \nghi", 1]
    ,
    {7, 9}
    ,
    TestID -> "GetNextLine-20150221-PTOHKI"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\n \nghi", 3]
    ,
    {7, 9}
    ,
    TestID -> "GetNextLine-20150221-IN8ELC"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\n \nghi", 4]
    ,
    {7, 9}
    ,
    TestID -> "GetNextLine-20150221-83U9LI"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\n \nghi", 5]
    ,
    {7, 9}
    ,
    TestID -> "GetNextLine-20150221-MJSN42"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\n \nghi", 6]
    ,
    {7, 9}
    ,
    TestID -> "GetNextLine-20150221-RXE94Q"
]

Test[
    CalculateParse`GeneralLibrary`GetNextLine["abc\n \nghi", 7]
    ,
    {10, 9}
    ,
    TestID -> "GetNextLine-20150221-F4QMR0"
]