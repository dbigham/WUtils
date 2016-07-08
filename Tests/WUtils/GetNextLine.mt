(* Tests for: WUtils`WUtils`GetNextLine

   Author: danielb
*)

Test[
    WUtils`WUtils`GetNextLine["abc\ndef\nghi", 1]
    ,
    {5, 7}
    ,
    TestID -> "GetNextLine-20150221-J7EP5L"
]

Test[
    WUtils`WUtils`GetNextLine["abc\ndef\nghi", 3]
    ,
    {5, 7}
    ,
    TestID -> "GetNextLine-20150221-0D4TWI"
]

Test[
    WUtils`WUtils`GetNextLine["abc\ndef\nghi", 4]
    ,
    {5, 7}
    ,
    TestID -> "GetNextLine-20150221-ZAJ8MC"
]

Test[
    WUtils`WUtils`GetNextLine["abc\ndef\nghi", 5]
    ,
    {9, 11}
    ,
    TestID -> "GetNextLine-20150221-5L33DN"
]

Test[
    WUtils`WUtils`GetNextLine["abc\ndef\nghi", 7]
    ,
    {9, 11}
    ,
    TestID -> "GetNextLine-20150221-NWVV6B"
]

Test[
    WUtils`WUtils`GetNextLine["abc\ndef\nghi", 8]
    ,
    {9, 11}
    ,
    TestID -> "GetNextLine-20150221-H5BLHT"
]

Test[
    WUtils`WUtils`GetNextLine["abc\ndef\nghi", 9]
    ,
    {12, 11}
    ,
    TestID -> "GetNextLine-20150221-VS35AO"
]

Test[
    WUtils`WUtils`GetNextLine["abc\ndef\nghi", 11]
    ,
    {12, 11}
    ,
    TestID -> "GetNextLine-20150221-E31BRS"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi", 1]
    ,
    {6, 8}
    ,
    TestID -> "GetNextLine-20150221-W2E35Q"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi", 3]
    ,
    {6, 8}
    ,
    TestID -> "GetNextLine-20150221-I69MFR"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi", 4]
    ,
    {6, 8}
    ,
    TestID -> "GetNextLine-20150221-FSI1SI"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi", 5]
    ,
    {6, 8}
    ,
    TestID -> "GetNextLine-20150221-HFBIPQ"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi", 6]
    ,
    {11, 13}
    ,
    TestID -> "GetNextLine-20150221-OAP3TO"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi", 8]
    ,
    {11, 13}
    ,
    TestID -> "GetNextLine-20150221-Q4C8V8"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi", 9]
    ,
    {11, 13}
    ,
    TestID -> "GetNextLine-20150221-HB7DW7"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi", 10]
    ,
    {11, 13}
    ,
    TestID -> "GetNextLine-20150221-8WSHXP"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi", 11]
    ,
    {14, 13}
    ,
    TestID -> "GetNextLine-20150221-BO7JGK"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi", 13]
    ,
    {14, 13}
    ,
    TestID -> "GetNextLine-20150221-YLCY6Y"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi\n", 14]
    ,
    {15, 14}
    ,
    TestID -> "GetNextLine-20150221-SEKUSE"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi\r\n", 14]
    ,
    {16, 15}
    ,
    TestID -> "GetNextLine-20150221-SINWCV"
]

Test[
    WUtils`WUtils`GetNextLine["abc\r\ndef\r\nghi\r\n", 15]
    ,
    {16, 15}
    ,
    TestID -> "GetNextLine-20150221-6OEV0J"
]

Test[
    WUtils`WUtils`GetNextLine["abc\n \nghi", 1]
    ,
    {7, 9}
    ,
    TestID -> "GetNextLine-20150221-PTOHKI"
]

Test[
    WUtils`WUtils`GetNextLine["abc\n \nghi", 3]
    ,
    {7, 9}
    ,
    TestID -> "GetNextLine-20150221-IN8ELC"
]

Test[
    WUtils`WUtils`GetNextLine["abc\n \nghi", 4]
    ,
    {7, 9}
    ,
    TestID -> "GetNextLine-20150221-83U9LI"
]

Test[
    WUtils`WUtils`GetNextLine["abc\n \nghi", 5]
    ,
    {7, 9}
    ,
    TestID -> "GetNextLine-20150221-MJSN42"
]

Test[
    WUtils`WUtils`GetNextLine["abc\n \nghi", 6]
    ,
    {7, 9}
    ,
    TestID -> "GetNextLine-20150221-RXE94Q"
]

Test[
    WUtils`WUtils`GetNextLine["abc\n \nghi", 7]
    ,
    {10, 9}
    ,
    TestID -> "GetNextLine-20150221-F4QMR0"
]