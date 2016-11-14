module ExtraParsecBinding
open FParsec
open System

let applyP fP xP =         
    fP >>= (fun f -> 
    xP >>= (fun x -> 
        preturn (f x) ))

/// infix version of apply
let ( <*> ) = applyP

let lift2 f xP yP =
    preturn f <*> xP <*> yP

let rec sequence parserList =
    // define the "cons" function, which is a two parameter function
    let cons head tail = head::tail

    // lift it to Parser World
    let consP = lift2 cons

    // process the list of parsers recursively
    match parserList with
    | [] -> 
        preturn []
    | head::tail ->
        consP head (sequence tail)

let charListToStr charList = 
     String(List.toArray charList)

let bindP f p = 
    p >>= f

let mapP f = 
    bindP (f >> preturn)


