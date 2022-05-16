﻿module Parser

open AST
open System
open FParsec

type UserState = unit 
type Parser<'t> = Parser<'t, UserState>

let test p str =
    match run p str with 
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let ws = spaces
let str = pstring
let betweenStrings s1 s2 p = str s1 >>. p .>> str s2

let strcomment : Parser<_> = 
    let validChar c = c <> '(' && c <> ')'
    manySatisfy validChar
let pconst : Parser<_> = pfloat .>> ws 
let ocomment : Parser<_> = strcomment |> betweenStrings "(" ")" 
let oid : Parser<_> =
    let isID c = isLetter c || Char.IsPunctuation c && c <> '(' && c <> ')'
    ocomment >>. many1SatisfyL isID "identifier" .>> ws
                                        (* Work out how to get position *)
let oconst : Parser<Element> = (pconst, (getPosition .>> pconst)) ||> EConst
