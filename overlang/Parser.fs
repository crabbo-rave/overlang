module Parser

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
let withPosition p f = pipe2 p getPosition f 

let strcomment : Parser<_> = 
    let validChar c = c <> '(' && c <> ')'
    manySatisfy validChar

let pconst : Parser<_> = pfloat .>> ws 

let ocomment : Parser<_> = strcomment |> betweenStrings "(" ")" 

let oid : Parser<Element> =
    let isID c = isLetter c || Char.IsPunctuation c && c <> '(' && c <> ')'
    withPosition (ocomment >>. many1SatisfyL isID "identifier" .>> ws) (fun x y -> EConst(x, y))

let oconst : Parser<Element> =
    withPosition pconst (fun x y -> EConst(x, y))

let parseCommentIndices : Parser<int list> =
    
