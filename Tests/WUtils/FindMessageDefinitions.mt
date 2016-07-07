(* Tests for: WUtils`WUtils`FindMessageDefinitions

   Author: danielb
*)

Test[
	WUtils`WUtils`FindMessageDefinitions["Func::msg = \"Message text\"", "Func"]
	,
	{{1, 26}}
	,
	TestID -> "FindMessageDefinitions-20160706-DX2B0J"
]