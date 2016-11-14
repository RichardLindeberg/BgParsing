module OutgoingParsers.OutgoingPaymentFileParser  
open FParsec
open BgParsing.Models.Types
open SharedParsers

let outGoingPaymentFileHeader = 
    let s2Header = 
        let skipRow = 
            (nTimes anyChar  80)
        skipRow
        >>. newline
        >>. skipRow
        >>. newline

    let pTk = pstring "01" |>> TransactionCode

    let sentDate = date |>> BgDate

    let autogiro = pstring "AUTOGIRO" |>> StartRowDescription
    
    let reserved = 
        nTimes singleSpace 44 
    
    let customerKey = nTimes digit 6 |>> (int >> CustomerKey)

    let bgNumber = nTimes digit 10 |>> (int >> BgNumber)

    let reserved2 = nTimes singleSpace 2

    let toStartRow ((((((a1, a2), a3), a4), a5), a6), a7) = 
            {
            RowType = a1; 
            Date = a2;
            StartRowDescription = a3;
            CustomerKey = a5; 
            BgNumber = a6;
            }

    s2Header
    >>. pTk 
    .>>. sentDate 
    .>>. autogiro 
    .>>. reserved 
    .>>. customerKey 
    .>>. bgNumber 
    .>>. reserved2 
    .>> newline
    |>> toStartRow

let withDrawlOrRefund = 
    let payDate = 
        let isDate = date |>> BgPayDate
        let isImmediatly = pstring "GENAST" .>> nTimes singleSpace 2 >>% Immediately
        isDate <|> isImmediatly

    let logg s x = 
        printfn "%s has value: %A" s x
        x

    let periodCode = digit |>> (charToInt >> PeriodCode)

    let noOfRecurringPayments = 
        let reInt = nTimes digit 3 |>> (int >> Some)
        let reBlank = nTimes singleSpace 3 >>% None
        reInt <|> reBlank |>> NoOfRecurringPayments

    let reserved = nTimes singleSpace 1 <?> "Reserved"

    let payerNumber = nTimes digit 16 <?> "PayerNumber" |>> (int64 >> PayerNumber)

    let amount = nTimes digit 12 <?> "Amount" |>> (int >> Amount)

    let payeeBankGiroNo = nTimes digit 10 <?> "PayeeBankGiro" |>> (int >> BgNumber)

    let reference = nTimes anyChar 16 |>> PaymentReference

    let reserved2 = nTimes singleSpace 11 <?> "reserved2"
    
    payDate 
    .>>. periodCode 
    .>>. noOfRecurringPayments 
    .>>. reserved 
    .>>. payerNumber 
    .>>. amount 
    .>>. payeeBankGiroNo 
    .>>. reference 
    .>>. reserved2
    .>> ((skipNewline <?> "skipNewLine") <|> eof)
          
let inBetRow = 
      
    let pTk = pstring "82" |>> TransactionCode

    let toWithdrawl (((((((( d, period), noOfRec), reserved), payerNumber), amount), pbg), reference), res2) = 
        {
            OutGoingDepositOrWithDrawl.PayDate = d;
            PeriodCode = period; 
            NoOfRecurringPayments = noOfRec;
            PayerNumber = payerNumber;
            Amount = amount;
            PayeeBankiroNumber = pbg;
            Reference = reference 
        }

    pTk 
    >>. withDrawlOrRefund
    |>> (toWithdrawl >> OutgoingWithdrawl)

let parseDepositRow = 
      
    let pTk = pstring "32" |>> TransactionCode

    let toWithdrawl (((((((( d, period), noOfRec), reserved), payerNumber), amount), pbg), reference), res2) = 
        {
            OutGoingDepositOrWithDrawl.PayDate = d;
            PeriodCode = period; 
            NoOfRecurringPayments = noOfRec;
            PayerNumber = payerNumber;
            Amount = amount;
            PayeeBankiroNumber = pbg;
            Reference = reference 
        }

    let innerParser = 
        pTk 
        >>. withDrawlOrRefund
        |>> (toWithdrawl >> OutgoingDeposit)
    
    innerParser

let outgoingPaymentFileParser = 
    let parseRows = 
        inBetRow
        <|> parseDepositRow

    let innerParser = 
        outGoingPaymentFileHeader
        .>>. (many1 parseRows) 

    let totoOutgoingPaymentFile (h, rs) =
        {OutgoingPaymentFile.Header = h; Rows = rs} 
            |> BgParsing.Models.SharedParsingTypes.OutgoingPaymentFile
    innerParser |>> totoOutgoingPaymentFile 
    