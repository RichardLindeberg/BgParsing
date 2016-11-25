// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.


#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"

#load "Models\Types.fs"
#load "Models\Events.fs"
#load "Models\MandateAdviceTypes.fs"
#load "Models\SharedParsingTypes.fs"
#load "ExtraParsecBinding.fs"
#load "SharedParsers.fs"
#load "IncommingParsers\IncommingMandateFileParser.fs"
#load "IncommingParsers\IncommingPaymentFileParser.fs"

#load "Mandate.fs"
#load "Payment.fs"
#load "EventsPrinter.fs"
#load "OutgoingParsers\OutgoingPaymentFileParser.fs"
#load "OutgoingParsers\OutgoingMandateFileParser.fs"
#load "BgcFilerParser.fs"

open BgParsing.Models.Types

open EventsPrinter
open BgcFilerParser

let printAllEvents =  
    let files = System.IO.Directory.GetFiles(@"C:\temp\BG\joined")
   // let files = System.IO.Directory.GetFiles(@"C:\temp\BG\BG\Outgoing\Mandate\Archive")
    
    loadFiles files |> Seq.iter (fun y -> match y with 
                                                    | Passed evt -> printfn "%s" <| printEvents evt 
                                                    | Failed f -> printfn "Failed: %s" f )

    
//let fileToEvents file = 
//    
//
//let filesToEvents = 
//    Seq.map fileToEvents >> Seq.concat
//
//filesToEvents files |> Seq.iter (fun y -> match y with 
//                                            | Passed _ -> printfn "All good"
//                                            | Failed f -> printfn "Failed: %s" f )
//
