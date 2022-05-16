open Parser
open FParsec

let main =
    test oconst "12.4"
    test oid "()hello!f?"
    test oid "(hello)testt!"
    test ocomment "(hello world)"
    0
