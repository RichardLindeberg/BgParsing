module BgcFilerParser
open BgParsing.Models.Types
open BgParsing.Models.MandateAdviceTypes
open IncommingParsers.IncommingMandateFileParser
open IncommingParsers.IncommingPaymentFileParser
open OutgoingParsers.OutgoingPaymentFileParser
open OutgoingParsers.OutgoingMandateFileParser
open Mandate
open Payment
open FParsec

let parseFile p fileName =
    match runParserOnFile p () fileName System.Text.Encoding.Default  with
    | Success(result, _, _)   ->  Passed <| (fileName, result)
    | Failure(errorMsg, pe, us) -> Failed <| sprintf "Failure on file: %s, \n\n Error:  %s \n\n pe: %A \n\n userState: %A" fileName errorMsg pe us

let files = System.IO.Directory.GetFiles(@"C:\temp\BG\joined")

open BgParsing.Models.SharedParsingTypes


let loadFiles (files : string []) = 
    let fileToEvents = 
        let fileContentToEvents = 
            function
                | IncommingMandateAdviceFile mf -> 
                //    printf "Parsed incommingMandate file"
                    mf.Rows |> Seq.map mandateAdviceRecordToEvent
                | IncommingPaymentFile pf -> 
                //    printf "Parsed incommingPayment file"
                    pf.Rows |> Seq.map outgoingPaymentFileRecordToEvent 
                | OutgoingPaymentFile pf -> 
                //    printf "Parsed outgoingPayment file"
                    pf.Rows |> Seq.map incommingPaymentFileRecordToEvent
                | OutgoingMandateFile pf -> 
                //    printf "Parsed outgoingMandate file"
                    pf.Rows |> Seq.map (outgoingMandateRecordToEvent pf.Header.Date)
        let fte = 
            function
                | Passed (_, res) -> fileContentToEvents res
                | Failed f -> Seq.ofList [Failed f]
        let fileParsers = 
            (attempt parseIncommingPaymentFile) 
            <|> (attempt incomingMandateFileParser)
            <|> (attempt outgoingPaymentFileParser)
            <|> (attempt outgoingMandateFileParser)
        (parseFile (fileParsers)) >>  fte
     
    let filesToEvents = 
        Seq.map (fun s -> 
              //  printfn "trying to parse %s" s
                fileToEvents s)
                >> Seq.concat

    filesToEvents files