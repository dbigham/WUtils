(* Tests for: WUtils`WUtils`GetClipboard

   Author: danielb
*)

(* Disabled: Seems to quasi lock up? (and fails) *)
(*
Test[
	Block[{}, CopyToClipboard["abc"]; WUtils`WUtils`GetClipboard[]]
	,
	"abc"
	,
	TestID -> "GetClipboard-20161007-AGSZRW"
]
*)