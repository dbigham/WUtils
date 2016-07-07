(* Tests for: WUtils`WUtils`FindAttributesCodeSections

   Author: danielb
*)

Test[
	WUtils`WUtils`FindAttributesCodeSections["Attributes[Func] = {HoldFirst}", "Func"]
	,
	{{1, 30}}
	,
	TestID -> "FindAttributesCodeSections-20160706-TN3VZZ"
]