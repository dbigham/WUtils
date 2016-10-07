(* Tests for: WUtils`WUtils`SimpleReap

   Author: danielb
*)

Test[
	WUtils`WUtils`SimpleReap["Tag", Sow[1, "Tag"]; Sow[2, "Tag"]]
	,
	{1, 2}
	,
	TestID -> "SimpleReap-20161007-5QIPZ9"
]