(* Tests for: WUtils`WUtils`GetClipboard

   Author: danielb
*)

Test[
	Block[{}, CopyToClipboard["abc"]; WUtils`WUtils`GetClipboard[]]
	,
	"abc"
	,
	TestID -> "GetClipboard-20161007-AGSZRW"
]