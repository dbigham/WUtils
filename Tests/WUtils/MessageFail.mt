(* Tests for: WUtils`WUtils`MessageFail

   Author: danielb
*)

Test[
	Block[{}, Blah::abc = "Just testing: `1`"; WUtils`WUtils`MessageFail[Blah::abc, "123"]]
	,
	$Failed
	,
	{Blah::abc}
	,
	TestID -> "MessageFail-20161005-1TLYNW"
]