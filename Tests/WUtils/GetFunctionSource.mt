(* Tests for: WUtils`WUtils`GetFunctionSource

   Author: danielb
*)

Test[
	WUtils`WUtils`GetFunctionSource[MyFunc, "MyFunc[] := Module[{}, 1]"]
	,
	"MyFunc[] := Module[{}, 1]"
	,
	TestID -> "GetFunctionSource-20160706-L5VH6N"
]

Test[
	WUtils`WUtils`GetFunctionSource[MyFunc, "MyFunc[] := 1"]
	,
	"MyFunc[] := 1"
	,
	TestID -> "GetFunctionSource-20160706-YRHHPI"
]

Test[
	WUtils`WUtils`GetFunctionSource[MyFunc, "MyFunc[] := 1\n"]
	,
	"MyFunc[] := 1"
	,
	TestID -> "GetFunctionSource-20160706-QO5MO3"
]

Test[
	WUtils`WUtils`GetFunctionSource[MyFunc, "MyFunc[] := 1\r\n"]
	,
	"MyFunc[] := 1"
	,
	TestID -> "GetFunctionSource-20160706-4JG72Y"
]

Test[
	WUtils`WUtils`GetFunctionSource[MyFunc, "MyFunc[] := \"str\""]
	,
	"MyFunc[] := \"str\""
	,
	TestID -> "GetFunctionSource-20160706-BM2RZW"
]

Test[
    WUtils`WUtils`GetFunctionSource[MyFunc, "MyFunc[] :=\n\t1"]
    ,
    "MyFunc[] :=\n\t1"
    ,
    TestID -> "GetFunctionSource-20160706-B7I0US"
]

Test[
	WUtils`WUtils`GetFunctionSource[MyFunc, "MyFunc[] :=\n\t\"str\""]
	,
	"MyFunc[] :=\n\t\"str\""
	,
	TestID -> "GetFunctionSource-20160706-CYCFMX"
]

Test[
	WUtils`WUtils`GetFunctionSource[MyFunc, "MyFunc[] :=\n\tModule[{}, 1]"]
	,
	"MyFunc[] :=\n\tModule[{}, 1]"
	,
	TestID -> "GetFunctionSource-20160706-CJWXBD"
]

Test[
	WUtils`WUtils`GetFunctionSource[MyFunc, "MyFunc[] :=\n\tModule[{},\n\t\t1\n\t]"]
	,
	"MyFunc[] :=\n\tModule[{},\n\t\t1\n\t]"
	,
	TestID -> "GetFunctionSource-20160706-LMZISE"
]

Test[
    WUtils`WUtils`GetFunctionSource[
        MyFunc,
        "MyFunc[] :=\n\t(* Comment *)\n\tModule[{},\n\t\t1\n\t]"
    ],
    "MyFunc[] :=\n\t(* Comment *)\n\tModule[{},\n\t\t1\n\t]",
    TestID -> "GetFunctionSource-20160706-LMZIS1"
]
Test[
    WUtils`WUtils`GetFunctionSource[
        MyFunc,
        "MyFunc[] := Module[{}, 1]\nMyFunc[str_String] := Module[{}, str]"
    ]
    ,
    "MyFunc[] := Module[{}, 1]\nMyFunc[str_String] := Module[{}, str]"
    ,
    TestID -> "GetFunctionSource-20160706-HV592U"
]

Test[
    WUtils`WUtils`GetFunctionSource[
        MyFunc,
        "\n(*!\n\t\\function MyFunc\n\n\t\\calltable\n\t\tMyFunc[] ' does stuff and things.\n\n\t\\maintainer danielb\n*)\nOptions[MyFunc] = {\"MyOptions\" -> Automatic}\nAttributes[MyFunc] = {HoldAllComplete}\n(* Another comment *)\nMyFunc::msg = \"Message text\"\nMyFunc::msg2 = \"Message text\"\nMyFunc[OptionsPattern[]] :=\n\t(* Comment *)\n\tModule[{},\n\t\t1\n\t]"
    ],
    "(*!\n\t\\function MyFunc\n\n\t\\calltable\n\t\tMyFunc[] ' does stuff and things.\n\n\t\\maintainer danielb\n*)\nOptions[MyFunc] = {\"MyOptions\" -> Automatic}\nAttributes[MyFunc] = {HoldAllComplete}\n(* Another comment *)\nMyFunc::msg = \"Message text\"\nMyFunc::msg2 = \"Message text\"\nMyFunc[OptionsPattern[]] :=\n\t(* Comment *)\n\tModule[{},\n\t\t1\n\t]",
    TestID -> "GetFunctionSource-20160706-HV5921"
]