module Parser

open System
open FParsec

type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>

let test p str =
    match run p str with 
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let ws = spaces

let oconst : Parser<_> = pfloat .>> ws 
let oid : Parser<_> =
    let isID c = isLetter c || Char.IsPunctuation c 
    many1SatisfyL isID "identifier" .>> ws