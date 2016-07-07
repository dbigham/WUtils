(* Tests for: WUtils`WUtils`FindCodeSections

   Author: danielb
*)

Test[
	WUtils`WUtils`FindCodeSections["Options[Func] = { ... }", "Options[", "}", 0]
	,
	{{1, 23}}
	,
	TestID -> "FindCodeSections-20160706-3YON7O"
]

Test[
	WUtils`WUtils`FindCodeSections["Options[Func] = { ... }", "Options[Func] = {", "}", 1]
	,
	{{1, 23}}
	,
	TestID -> "FindCodeSections-20160706-83P95N"
]

Test[
	WUtils`WUtils`FindCodeSections[
		"Options[Func] = { ... }",
		"Options[Func]" | "Options[ Func ]",
		"}",
		0
	]
	,
	{{1, 23}}
	,
	TestID -> "FindCodeSections-20160706-TI8K7Q"
]