module AST

open System.Collections.Generic

type Element = 
    | EConst of float
    | ESym of string
    | EId of string
type Function = { comment: string;
                  code: Element[] }
type Prog = Dictionary<string, Function>
