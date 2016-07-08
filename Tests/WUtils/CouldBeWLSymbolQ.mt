(* Tests for: WUtils`WUtils`CouldBeWLSymbolQ

   Author: danielb
*)

Test[
    CouldBeWLSymbolQ["1"]
    ,
    False
    ,
    TestID -> "CouldBeWLSymbolQ-20150126-G7DUIN"
]

Test[
    CouldBeWLSymbolQ["Test"]
    ,
    True
    ,
    TestID -> "CouldBeWLSymbolQ-20150126-5BNT76"
]

Test[
    CouldBeWLSymbolQ["Test1"]
    ,
    True
    ,
    TestID -> "CouldBeWLSymbolQ-20150126-SCJXNH"
]

Test[
    WUtils`WUtils`CouldBeWLSymbolQ["#"]
    ,
    False
    ,
    TestID -> "CouldBeWLSymbolQ-20150128-3KCU4Q"
]

Test[
    WUtils`WUtils`CouldBeWLSymbolQ["1"]
    ,
    False
    ,
    TestID -> "CouldBeWLSymbolQ-20150128-XM9M4Z"
]