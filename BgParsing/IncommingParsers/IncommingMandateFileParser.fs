module IncommingParsers.IncommingMandateFileParser 
open FParsec
open FParsec
open BgParsing.Models.Types
open BgParsing.Models.MandateAdviceTypes
open ExtraParsecBinding
open SharedParsers
open System

let incommingPaymentHeaderParser = 
    let pTk = pstring "01" |>> TransactionCode <?> "tk"
    let autogiro = pstring "AUTOGIRO" <?> "AutoGiro"
    let reserved =  nTimes singleSpace 14 <?> "Reserved"
    let timeStamp = date |>> BgDate <?> "Date"
    let reserved2 =  nTimes singleSpace 12
    let betSpec = pstring "AG-MEDAVI" .>>. nTimes singleSpace 11 
    let customerKey = nTimes digit 6 |>> (int >> CustomerKey)
    let bgNumber = nTimes digit 10 |>> (int >> BgNumber)

    let toStartRow ((ts, cn), bg) = 
          {
             IncommingMandateHeaderRow.DateWritten = ts;
             PayeeCustomerNumber = cn;
             PayeeBankGiroNumber = bg;
          }
    printfn "Starting to parse!"
    pTk 
    >>. autogiro 
    >>. reserved 
    >>. timeStamp
    .>> reserved2
    .>> betSpec
    .>>. customerKey
    .>>. bgNumber
    .>> newline
    |>> toStartRow

let mandateAdviceRecordParser = 
    let tk = pstring "73"
    let payeesBg = nTimes digit 10 |>> (int >> BgNumber)
    let payerNumber = nTimes digit 16 |>> (int64 >> PayerNumber)
    let bankAccount = 
        let someBA = 
            (nTimes digit 4 |>> (int >> ClearingNumber))
            .>>. (nTimes digit 12 |>> (int64 >> BankAccountNumber))
            |>> (fun (cl, ban) -> Some {BankAccount.ClearingNumber = cl; BankAccountNumber = ban})
        let noneBA = 
            nTimes singleSpace 16 >>% None
        someBA <|> noneBA

    let civicNr = nTimes anyChar 12 |>> (int64 >> PayerNumber) <?> "CivicNumber"
    let reserved = nTimes singleSpace 5 <?> "5 single spaces (reserved)" 
    let informationCode = 
        (pstring "03" >>% CancellationInitiatedByPayee03                   )
        <|> (pstring "04" >>% NewAdditionInitiatedByPayee04                    )
        <|> (pstring "05" >>% ChangePayerNumber05                              )
        <|> (pstring "10" >>% CancelledDueToPayeesBankgironumberBeingClosed10  )
        <|> (pstring "42" >>% ResponseToAccountInquiryFromBank42               )
        <|> (pstring "43" >>% CancelledOrRemovedDueToUnansweredAccountInquiry43)
        <|> (pstring "44" >>% CancelledDueToPayersBankgiroNumberBeingClosed44  )
        <|> (pstring "46" >>% CancellationByBank46                             )
        <|> (pstring "93" >>% CancellationByPayer93                            )

    let commentCode =
        (pstring "02" >>% MandateCancelledOnInitiativeOfPayerOrPayersBank_2)
        <|> (pstring "03" >>% AccountTypeNotApprovedForAutogiro_3)
        <|> (pstring "04" >>% MandateNotFoundInBankgirotsMandateDirectory_4)
        <|> (pstring "05" >>% IncorrectBankAccountOrPersonalDetails_5)
        <|> (pstring "07" >>% CancelledOrRemovedDueToUnansweredAccountInquiry_7)
        <|> (pstring "09" >>% PayerBankgiroNumberNotFoundAtBankgirot_9)
        <|> (pstring "10" >>% MandateAlreadyRegisteredInBankgirotsDirectoryOrInquiryPending_10)
        <|> (pstring "20" >>% IncorrectCivicOrcompanyNumber_20)
        <|> (pstring "21" >>% IncorrectPayerNumber_21)
        <|> (pstring "23" >>% IncorrectBankAccountNumber_23)
        <|> (pstring "29" >>% IncorrectPayeeBankgiroNumber_29)
        <|> (pstring "30" >>% DeregisteredPayeeBankgiroNumber_30)
        <|> (pstring "32" >>% NewMandate_32)
        <|> (pstring "33" >>% Cancelled_33)
        <|> (pstring "98" >>% MandateCancelledDueToCancelledPayerBankgiroNumber_98)
    let actionDate = date |>> BgDate    
    let reserved2 = nTimes singleSpace 7

    let toMandateAdvice ((((((pbg, pn), ba), cn), ic), cc), ad) = 
      let ma = 
              {
                MandateAdviceRecordContent.PayeeBankGiro = pbg;
                BankAccount = ba;
                PayerNumber = pn;
                CivicNumber = cn;
                CommentCode = cc;
                ActionDate = ad;
              }
      ic ma

    tk
    >>. payeesBg
    .>>. payerNumber
    .>>. bankAccount
    .>>. civicNr
    .>> reserved
    .>>. informationCode
    .>>. commentCode
    .>>. actionDate
    .>> reserved2
    .>> newline
    |>> toMandateAdvice
    <?> "MandateParser"

let endRecordParser = 
    let innerParser = 
        pstring "09"
        >>. (date |>> BgDate)
        .>> pstring  "9900"
        .>>. (nTimes digit 7 |>> (int >> NumberOfAdviceRecords))
        .>> nTimes singleSpace 59
        .>> newline 
        .>> eof
    innerParser |>> (fun (d, r) -> {MandateAdviceEndRecord.DateWritten = d; NumberOfRecords = r})

let validate = 
    let endRecordValidation (adviceFile  : IncommingMandateAdviceFile) = 
        let toInt a = 
            match a with 
                | NumberOfAdviceRecords i -> i
        match adviceFile.Rows.Length = (toInt adviceFile.EndRecord.NumberOfRecords) with 
            | true -> Passed adviceFile
            | false -> Failed <| sprintf "Nr of records in mandateacdicefile missmatch. There are %i but supposed to be %A" adviceFile.Rows.Length adviceFile.EndRecord.NumberOfRecords
    
    
    endRecordValidation 




let incomingMandateFileParser = 
    let innerParser = 
        incommingPaymentHeaderParser
        .>>. (many1 mandateAdviceRecordParser) 
        .>>. endRecordParser
    let toIncommingMandateAdviceFile ((h, rs), e) =
        {IncommingMandateAdviceFile.StartRecord = h; Rows = rs; EndRecord = e} 
            |> BgParsing.Models.SharedParsingTypes.IncommingMandateAdviceFile
    innerParser |>> toIncommingMandateAdviceFile 
    