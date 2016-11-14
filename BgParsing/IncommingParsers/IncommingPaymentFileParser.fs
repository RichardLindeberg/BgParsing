
module IncommingParsers.IncommingPaymentFileParser  
    open FParsec
    open BgParsing.Models.Types
    open SharedParsers

    let incommingPaymentHeaderParser = 
        let pTk = pstring "01" |>> TransactionCode
        let autogiro = pstring "AUTOGIRO" .>> nTimes singleSpace 12 |>> StartRowDescription <?> "Description"
        let reserved =  nTimes singleSpace 2 <?> "reserved"
        let timeStamp = nTimes digit 20 |>> HeaderTimeStamp <?> "HeaderTimeStamp"
        let betSpec = pstring "BET. SPEC & STOPP TK"
        let customerKey = nTimes digit 6 |>> (int >> CustomerKey)
        let bgNumber = nTimes digit 10 |>> (int >> BgNumber)

        let toStartRow (((ts, bs), cn), bg) = 
              {
                 IncommingHeaderRow.TimeStamp = ts;
                 PayeeCustomerNumber = cn;
                 PayeeBankGiroNumber = bg;
              }

        pTk 
        >>. autogiro 
        >>. reserved 
        >>. timeStamp 
        .>>. betSpec 
        .>>. customerKey 
        .>>. bgNumber 
        .>> newline
        |>> toStartRow

    let depositOrWithdrawlRecordParser = 
        let payeeBankAccountNumber = nTimes digit 35
        let paymentDate = date 
        let depositSerialNumber = nTimes digit 5
        let amount = nTimes digit 18
        let reserved = nTimes singleSpace 3
        let nrOfPayments = nTimes digit 8
        let reserved2 = singleSpace |>> string

        let toDepositOrWithdrawlRecord ((((((ba, pd), sn), amount), rs1), nr), rs2) = 
            {
             PayeeBankAccountNumber = ba;
             PaymentDate = pd;
             DepositSerialNumber = sn;
             Amount = amount;
             NrOfPayments = nr
            }
    
        payeeBankAccountNumber 
        .>>. paymentDate
        .>>. depositSerialNumber
        .>>. amount
        .>>. reserved
        .>>. nrOfPayments
        .>>. reserved2
        .>> newline
        |>> toDepositOrWithdrawlRecord


    let depositRowParser = 
        let tk15 = pstring "15"
        tk15 >>. depositOrWithdrawlRecordParser |>> DepositRecord   

    let withdrawlRowParser = 
        let tk15 = pstring "16"
        tk15 >>. depositOrWithdrawlRecordParser |>> WithdrawlRecord

    let withdrawlForRefundRowParser = 
        let tk15 = pstring "17"
        tk15 >>. depositOrWithdrawlRecordParser |>> WithdrawlForRefundRecord


    let IncommingOrOutgoingPaymentRecord = 
        let toIncommingPaymentStatus x = 
            match x with 
                | '0' -> ApprovedExecuted
                | '1' -> InsufficientFunds
                | '2' -> NotExecuted
                | '9' -> RenewedFunds
                | _ -> failwithf "Could not match to incommingPaymentstatus %c" x
                
        let payDate = date |>> BgPayDate
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
        let reserved2 = nTimes singleSpace 8 <?> "reserved2"
        let reserved3 = nTimes singleSpace 2 <?> "reserved2"
        let paymentStatus = digit |>> toIncommingPaymentStatus
    
        let toIncommingGoingDepositOrWithDrawl (((((((((( d, period), noOfRec), reserved), payerNumber), amount), pbg), reference), res2), res3), ps) = 
            {
             InOrOutgoingPaymentRecord.PayDate = d;
             PeriodCode = period; 
             NoOfRecurringPayments = noOfRec;
             PayerNumber = payerNumber;
             Amount = amount;
             PayeeBankiroNumber = pbg;
             Reference = reference;
             paymentStatus = ps;
            }

        payDate 
        .>>. periodCode 
        .>>. noOfRecurringPayments 
        .>>. reserved 
        .>>. payerNumber 
        .>>. amount 
        .>>. payeeBankGiroNo 
        .>>. reference 
        .>>. reserved2
        .>>. reserved3
        .>>. paymentStatus
        .>> ((skipNewline <?> "skipNewLine") <|> eof)
        |>> toIncommingGoingDepositOrWithDrawl      

    let incommingPaymentRecordParser = 
        let pTk = pstring "82" |>> TransactionCode
        pTk >>. IncommingOrOutgoingPaymentRecord |>> IncommingPaymentRecord
  
    let outgoingPaymentRecordParser = 
        let pTk = pstring "32" |>> TransactionCode
        pTk >>. IncommingOrOutgoingPaymentRecord |>> OutgoingPaymentRecord

    let refundPaymentRecordParser = 
        let toPaymentRefundCode x = 
            match x with 
                | "01" -> MandateNotSubmitedToPayee
                | "02" -> MandateWithdrawn
                | "03" -> ToLargeAmount
                | _ -> failwithf "Unknown paymentRefundCode %s" x
                

        let tk = pstring "77"
        let payDate = date |>> BgDate
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
        let refundDate = date |>> BgDate
        let refundCode = nTimes digit 2 |>> toPaymentRefundCode
        let reserved3 = nTimes singleSpace 1 <?> "reserved2"
    
        let toPaymentRefundRecord (((((((((( d, period), noOfRec), reserved), payerNumber), amount), pbg), reference), refundDate), refundCode), res) = 
            {
             PaymentRefundRecord.PayDate = d;
             PeriodCode = period; 
             NoOfRecurringPayments = noOfRec;
             PayerNumber = payerNumber;
             Amount = amount;
             PayeeBankiroNumber = pbg;
             Reference = reference;
             RefundCode = refundCode;
            }

        tk 
        >>.  payDate 
        .>>. periodCode 
        .>>. noOfRecurringPayments 
        .>>. reserved 
        .>>. payerNumber 
        .>>. amount 
        .>>. payeeBankGiroNo 
        .>>. reference 
        .>>. refundDate
        .>>. refundCode
        .>>. reserved3
        .>> ((skipNewline <?> "skipNewLine") <|> eof)
        |>> (toPaymentRefundRecord >> PaymentRefundRecord)

    let endRowParser = 
        let tk = pstring "09"
        let dateWritten = date |>> BgDate
        let clearingNumber = pstring "9900"
        let nrOfDepositRecords = nTimes digit 6 |>> int
        let nrOfIncommingPaymentRecords = nTimes digit 12  |>> int
        let nrOfWithdrawlRecords = nTimes digit 6 |>> int
        let nrOfOutgoingPaymentRecords = nTimes digit 12 |>> int
        let nrOfWithdrawlRecordsForRefund = nTimes digit 6 |>> int
        let nrOfRefundRecords = nTimes digit 12 |>> int
        let reserved = nTimes singleSpace 12 

        let toPaymentEndRow ((((((((date, clearing), dep), inco), wi), og), wiref), ref), res) = 
            {
                PaymentEndRow.DateWritten = date; 
                NrOfDepositRecords = dep;
                NrOfIncommingPaymentRecords = inco;
                NrOfWithdrawlRecords = wi; 
                NrOfOutgoingPaymentRecords = og;
                NrOfWithdrawlRecordsForRefund = wiref; 
                NrOfRefundRecords = ref
             }

        tk
        >>. dateWritten
        .>>. clearingNumber
        .>>. nrOfDepositRecords
        .>>. nrOfIncommingPaymentRecords
        .>>. nrOfWithdrawlRecords
        .>>. nrOfOutgoingPaymentRecords
        .>>. nrOfWithdrawlRecordsForRefund
        .>>. nrOfRefundRecords
        .>>. reserved
        .>> eof
        |>> toPaymentEndRow
    //let parseOutGoing = outGoingPaymentFile .>>. many (inBetRow <|> parseDepositRow)
    let parseIncommingPaymentFile = 
        let parseRows = 
            depositRowParser
            <|> incommingPaymentRecordParser 
            <|> withdrawlRowParser 
            <|> outgoingPaymentRecordParser
            <|> withdrawlForRefundRowParser
            <|> refundPaymentRecordParser

        let toIpf ((h, r), e) = 
            {
                IncommingPaymentFile.Head = h;
                Rows = r;
                End = e;
            } |>    BgParsing.Models.SharedParsingTypes.IncommingPaymentFile
         
            
        incommingPaymentHeaderParser 
        .>>. many1 parseRows 
        .>>. endRowParser
        |>> toIpf
