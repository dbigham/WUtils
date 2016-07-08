(* Tests for: WUtils`WUtils`ContextToFile

   Ignored by QA because I don't think they have Alpha on the
   $Path, rendering FindFile useless.

   Author: danielb
*)

Test[
    WUtils`WUtils`ContextToFile["WUtils`WUtils`"],
    "C:\\Users\\Daniel\\WolframWorkspaces\\Base2\\WUtils\\WUtils.m",
    TestID -> "ContextToFile-20150126-QUFW1C"
]

Test[
    WUtils`WUtils`ContextToFile["WUtils`WUtils`Private`"],
    "C:\\Users\\Daniel\\WolframWorkspaces\\Base2\\WUtils\\WUtils.m",
    TestID -> "ContextToFile-20150126-42UYIP"
]