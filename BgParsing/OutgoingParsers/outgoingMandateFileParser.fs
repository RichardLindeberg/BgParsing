module OutgoingParsers.OutgoingMandateFileParser
open FParsec
open BgParsing.Models.Types
open SharedParsers


let outGoingMandateFileHeader = 
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


let cancellationOfMandate = 
    let tc = pstring "03" |>> TransactionCode
    let bankAccount = 
        let someBA = 
            (nTimes digit 4 |>> (int >> ClearingNumber))
            .>>. (nTimes digit 12 |>> (int64 >> BankAccountNumber))
            |>> (fun (cl, ban) -> Some {BankAccount.ClearingNumber = cl; BankAccountNumber = ban})
        let noneBA = 
            nTimes singleSpace 16 >>% None
        someBA <|> noneBA
    let payerNumber = nTimes digit 16 |>> (int64 >> PayerNumber)
    let reserved = nTimes singleSpace 52

    let toCancellationOfMandate (ba, pn) = 
        {CancellationOfMandate.BankAccount = ba; PayerNumber = pn}  

    tc
    >>. bankAccount
    .>>. payerNumber
    .>> reserved
    .>> newline
    |>> toCancellationOfMandate 

let approvingRejectingMandate = 
    let tc = pstring "04" |>> TransactionCode
    let bgNumber = nTimes digit 10 |>> (int >> BgNumber)
    let payerNumber = nTimes digit 16 |>> (int64 >> PayerNumber)
    let civicNr = nTimes anyChar 12 |>> (int64 >> PayerNumber) <?> "CivicNumber"
    let bankAccount = 
        let someBA = 
            (nTimes digit 4 |>> (int >> ClearingNumber))
            .>>. (nTimes digit 12 |>> (int64 >> BankAccountNumber))
            |>> (fun (cl, ban) -> Some {BankAccount.ClearingNumber = cl; BankAccountNumber = ban})
        let noneBA = 
            nTimes singleSpace 16 >>% None
        someBA <|> noneBA
    
    let reserved = nTimes singleSpace 20
    
    let newAdditions = nTimes singleSpace 2
    
    let reserved2 = nTimes singleSpace 2

    let toApprovalRejectionOfMandate (((pbg, pn), ba), cn) = 
        {ApprovalRejectionOfMandate.PayyeBankGiro = pbg; PayerNumber = pn; PayerBankAccount = ba; PayerCivic = cn}

    tc
    >>. bgNumber
    .>>. payerNumber
    .>>. bankAccount
    .>>. civicNr
    .>> reserved
    .>> newAdditions
    .>> reserved2
    .>> newline
    |>> (toApprovalRejectionOfMandate >> ApprovalRejectionOfMandate)

let outgoingMandateFileParser = 
    let innerParser = 
        outGoingMandateFileHeader
        .>>. (many1 approvingRejectingMandate)
    let totoOutgoingPaymentFile (h, rs) =
        {OutgoingMandateFile.Header = h; Rows = rs} 
            |> BgParsing.Models.SharedParsingTypes.OutgoingMandateFile
    innerParser |>> totoOutgoingPaymentFile 