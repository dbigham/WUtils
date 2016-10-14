(* Tests for: WUtils`WUtils`GetVariablePossiblyFromParentPackage

   Author: danielb
*)

Test[
    WUtils`WUtils`GetVariablePossiblyFromParentPackage["WUtils`WUtils`", "$UnitTestDir"],
    {"WUtils`", "E:\\Users\\Daniel\\Dropbox\\Projects\\WUtils\\Tests"},
    TestID -> "GetVariablePossiblyFromParentPackage-20150202-QTOU4E"
]