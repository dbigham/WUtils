(* Tests for: WUtils`WUtils`EscapeString

   Author: danielb
*)

Test[
	WUtils`WUtils`EscapeString["this is a \"test\""]
	,
	"this is a \\\"test\\\""
	,
	TestID -> "EscapeString-20161118-13992Z"
]

Test[
	WUtils`WUtils`EscapeString["just testing"]
	,
	"just testing"
	,
	TestID -> "EscapeString-20161118-5LKPUX"
]