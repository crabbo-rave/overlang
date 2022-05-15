open Parser
open FParsec

let main =
    test pfloat "1.25"
    test pfloat "1.25E 3"
    0
