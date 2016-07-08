(* Tests for: WUtils`WUtils`ListOfHeldMessagesToString

   Author: danielb
*)

Test[
    WUtils`WUtils`ListOfHeldMessagesToString[
        {Hold[Power::infy], Hold[Power::infy]}
    ]
    ,
    "{Power::infy, Power::infy}"
    ,
    TestID -> "ListOfHeldMessagesToString-20160610-S7BEJE"
]