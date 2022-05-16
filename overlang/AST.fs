module AST

open System.Collections.Generic
open System.Linq
open FParsec

let nextByOrder (elems: Dictionary<'K, 'T>) (k: 'K) =
    let ordered = elems.Keys.OrderBy(fun x -> x).ToList()
    elems[ordered[ordered.IndexOf k]]

type Element = 
    | EConst of value: float * position: Position
    | ESym of symbol: string * position: Position
    | EId of comment: string * name: string * position: Position

type Function = { name: string; // Function name, including the comment
                  limit: int; // Overflow limit
                  code: Element[]; // Function code
                  position: Position // Position in file
                }

type Prog = Dictionary<string, Function>
