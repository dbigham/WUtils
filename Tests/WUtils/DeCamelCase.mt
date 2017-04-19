(* Tests for: WUtils`WUtils`DeCamelCase

   Author: danielb
*)

Test[
    WUtils`WUtils`DeCamelCase["JustTesting"]
    ,
    "Just Testing"
    ,
    TestID -> "DeCamelCase-20160119-R0QRYE"
]

Test[
    WUtils`WUtils`DeCamelCase["JustTestingAgain"]
    ,
    "Just Testing Again"
    ,
    TestID -> "DeCamelCase-20160119-5LL3A2"
]

Test[
    WUtils`WUtils`DeCamelCase["justTesting"]
    ,
    "Just Testing"
    ,
    TestID -> "DeCamelCase-20160119-Y8DVYE"
]

Test[
    WUtils`WUtils`DeCamelCase["notcamelcased"]
    ,
    "notcamelcased"
    ,
    TestID -> "DeCamelCase-20160119-2BBAG1"
]

Test[
    WUtils`WUtils`DeCamelCase["not camel cased"]
    ,
    "not camel cased"
    ,
    TestID -> "DeCamelCase-20160119-M6K4TR"
]

Test[
	WUtils`WUtils`DeCamelCase["JustTestingABC"]
	,
	"Just Testing ABC"
	,
	TestID -> "DeCamelCase-20160926-1B4HQZ"
]

Test[
	WUtils`WUtils`DeCamelCase["JustTesting123"]
	,
	"Just Testing 123"
	,
	TestID -> "DeCamelCase-20160926-PXJFY7"
]

Test[
	WUtils`WUtils`DeCamelCase["NLTools"]
	,
	"NL Tools"
	,
	TestID -> "DeCamelCase-20161005-1DLTDE"
]

Test[
	WUtils`WUtils`DeCamelCase["just testing"]
	,
	"just testing"
	,
	TestID -> "DeCamelCase-20170411-IEN3IL"
]