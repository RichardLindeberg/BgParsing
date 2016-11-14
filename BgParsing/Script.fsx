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
<<<<<<< HEAD
#load "Payment.fs"
#load "EventsPrinter.fs"
#load "OutgoingParsers\OutgoingPaymentFileParser.fs"
#load "OutgoingParsers\OutgoingMandateFileParser.fs"
#load "BgcFilerParser.fs"
=======
>>>>>>> parent of d88928f... more refactor

open BgParsing.Models.Types
<<<<<<< HEAD
open EventsPrinter
open BgcFilerParser

let printAllEvents =  
    let files = System.IO.Directory.GetFiles(@"C:\temp\BG\BG\joined")
   // let files = System.IO.Directory.GetFiles(@"C:\temp\BG\BG\Outgoing\Mandate\Archive")
    
    loadFiles files |> Seq.iter (fun y -> match y with 
                                                    | Passed evt -> printfn "%s" <| printEvents evt 
                                                    | Failed f -> printfn "Failed: %s" f )
=======
open ExtraParsecBinding
open SharedParsers
open System
open Mandate
open IncommingMandateFileParser
open BgParsing.Models.Events
open IncommingParsers.IncommingPaymentFileParser

let parseFile p fileName =
    match runParserOnFile p () fileName System.Text.Encoding.Default  with
    | Success(result, us, pos)   ->  Passed <| (fileName, result)
    | Failure(errorMsg, pe, us) -> Failed <| sprintf "Failure on file: %s, \n\n Error:  %s \n\n pe: %A \n\n userState: %A" fileName errorMsg pe us

let files = System.IO.Directory.GetFiles(@"C:\temp\BG\BG\joined")

open BgParsing.Models.SharedParsingTypes


let paymentRecordToEvent = 
    let incommingPaymentRecordToEvent a = 
        match a.paymentStatus with 
        | ApprovedExecuted
            ->
                {PaymentExecuted.Amount = a.Amount; CivicNumber = a.PayerNumber; Reference = a.Reference; WithdrawalDate = a.PayDate}
                |> PaymentExecuted
                |> PaymentEvent
                |> Passed
        | InsufficientFunds
        | NotExecuted
        | RenewedFunds
            -> 
                { PaymentRejected.CivicNumber = a.PayerNumber; Reference = a.Reference; RejectionDate = a.PayDate; PaymentRejected.PaymentStatus = a.paymentStatus}
                |> PaymentRejected
                |> PaymentEvent
                |> Passed
    function
        | DepositRecord a-> Failed "unhandled row type DepositRecord"
        | IncommingPaymentRecord a ->  incommingPaymentRecordToEvent a               
        | WithdrawlRecord a -> Failed "unhandled rowtype WithdrawlRecord"
        | OutgoingPaymentRecord a -> Failed "Unhandled row type OutgoingPaymentRecord"
        | WithdrawlForRefundRecord a -> Failed "Unhandled rowtype Withdrawl for refund"
        | PaymentRefundRecord a -> Failed "Unhandled refundRecord"
    
let fileToEvents = 
    let fileContentToEvents = 
        function
            | IncommingMandateAdviceFile mf -> mf.Rows |> Seq.map mandateAdviceRecordToEvent
            | IncommingPaymentFile pf -> pf.Rows |> Seq.map paymentRecordToEvent 

    let fte = 
        function
            | Passed (fileName, res) -> fileContentToEvents res
            | Failed f -> Seq.ofList [Failed f]

    (parseFile ((attempt parseIncommingPaymentFile) <|> incomingMandateFileParser)) >>  fte
    
let filesToEvents = 
    Seq.map fileToEvents >> Seq.concat

filesToEvents files |> Seq.iter (fun y -> match y with 
                                            | Passed _ -> printfn "All good"
                                            | Failed f -> printfn "Failed: %s" f )
>>>>>>> parent of d88928f... more refactor
