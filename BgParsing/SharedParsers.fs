module SharedParsers
open FParsec
open System
open ExtraParsecBinding

let nTimes p n = 
      [1..n] 
      |> List.map (fun _ -> p)
      |> sequence
      |> mapP charListToStr


let charToInt = 
    string >> int

let date : Parser<DateTime,unit> = 
    let years = nTimes digit 4 
    let twoDigits = nTimes digit 2

    let toDate ((year, month), day) = 
        sprintf "%s-%s-%s" year month day |> DateTime.Parse

    let dp = years .>>. twoDigits .>>. twoDigits |>> toDate
    dp 

let singleSpace : Parser<char,unit> = pchar ' '

