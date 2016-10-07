(* Tests for: WUtils`WUtils`ToCamelCase

   Author: danielb
*)

Test[
	WUtils`WUtils`ToCamelCase["just testing"]
	,
	"JustTesting"
	,
	TestID -> "ToCamelCase-20161006-96WBX2"
]

Test[
	WUtils`WUtils`ToCamelCase["is"]
	,
	"Is"
	,
	TestID -> "ToCamelCase-20161006-OJ9W4F"
]