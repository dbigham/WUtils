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

(* Don't capture usage messages, since they need to go at the top of the file and AutoExport can help us with that anyway. *)
Test[
	WUtils`WUtils`FindMessageDefinitions["Func::usage = \"Usage information\"", "Func"]
	,
	{}
	,
	TestID -> "FindMessageDefinitions-20160707-CI5GE7"
]