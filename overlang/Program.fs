open Parser
open FParsec

let main =
    test oconst "12.4"
    test oid "hello!f?"
    0
