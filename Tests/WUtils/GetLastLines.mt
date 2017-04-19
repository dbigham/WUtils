(* Tests for: WUtils`WUtils`GetLastLines

   Author: danielb
*)

Test[
	WUtils`WUtils`GetLastLines["a\nb\nc", 2]
	,
	"b\nc"
	,
	TestID -> "GetLastLines-20161120-R47TD9"
]

Test[
	WUtils`WUtils`GetLastLines["a\nb\nc", 1]
	,
	"c"
	,
	TestID -> "GetLastLines-20161120-0F06AZ"
]