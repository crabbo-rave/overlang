module AST

open System.Collections.Generic
open FParsec;

type Element = 
    | EConst of value: float * position: Position
    | ESym of symbol: string * position: Position
    | EId of name: string * position: Position

type Function = { name: string; // Function name, including the comment
                  limit: int; // Overflow limit
                  code: Element[]; // Function code
                  position: Position // Position in file
                }

type Prog = Dictionary<string, Function>
