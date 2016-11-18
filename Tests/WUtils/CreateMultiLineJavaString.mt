(* Tests for: WUtils`WUtils`CreateMultiLineJavaString

   Author: danielb
*)

Test[
	WUtils`WUtils`CreateMultiLineJavaString["just testing\n\n123"]
	,
	"\"just testing\\n\" + \n\"\\n\" + \n\"123\\n\""
	,
	TestID -> "CreateMultiLineJavaString-20161105-P815R2"
]