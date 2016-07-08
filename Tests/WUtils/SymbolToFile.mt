(* Tests for: WUtils`WUtils`SymbolToFile

   Ignored by QA because I don't think they have Alpha on the
   $Path, rendering FindFile useless.

   Author: danielb
*)

Test[
    WUtils`WUtils`SymbolToFile[WUtils`WUtils`SymbolToFile],
    "C:\\Users\\Daniel\\WolframWorkspaces\\Base2\\WUtils\\WUtils.m",
    TestID -> "SymbolToFile-20150129-4RR5VA"
]

Test[
    WUtils`WUtils`SymbolToFile[WUtils`WUtils`Private`toSingleLine],
    "C:\\Users\\Daniel\\WolframWorkspaces\\Base2\\WUtils\\WUtils.m",
    TestID -> "SymbolToFile-20150129-KVX5GJ"
]
