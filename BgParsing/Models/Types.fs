namespace BgParsing.Models.Types

open System

type TransactionCode = TransactionCode of string
type BgDate = BgDate of DateTime
    
type BgPayDate = 
    | BgPayDate of DateTime
    | Immediately

type IncommingPaymentStatus = 
    | ApprovedExecuted
    | InsufficientFunds
    | NotExecuted
    | RenewedFunds

type PaymentRefundCode = 
    | MandateNotSubmitedToPayee
    | MandateWithdrawn
    | ToLargeAmount

type StartRowDescription = StartRowDescription of string
type CustomerKey = CustomerKey of int
type BgNumber = BgNumber of int
type NoOfRecurringPayments = NoOfRecurringPayments of Option<int>
type Amount = Amount of int
type PayerNumber = PayerNumber of int64
type PaymentReference = PaymentReference of string
type PeriodCode = PeriodCode of int
type PaymentStatus = PaymentStatus of int
type HeaderTimeStamp = HeaderTimeStamp of string 

type BgHeaderRow = 
    {
        RowType : TransactionCode; 
        Date : BgDate; 
        StartRowDescription : StartRowDescription;
        CustomerKey : CustomerKey;
        BgNumber : BgNumber;
    }

type IncommingHeaderRow =
    {
        TimeStamp : HeaderTimeStamp
        PayeeCustomerNumber : CustomerKey
        PayeeBankGiroNumber : BgNumber
    }

type OutGoingDepositOrWithDrawl = 
    {
        PayDate : BgPayDate; 
        PeriodCode : PeriodCode;
        NoOfRecurringPayments : NoOfRecurringPayments;
        PayerNumber : PayerNumber;
        Amount : Amount;
        PayeeBankiroNumber : BgNumber;
        Reference : PaymentReference;
    }

type InOrOutgoingPaymentRecord = 
    {
        PayDate : BgPayDate; 
        PeriodCode : PeriodCode;
        NoOfRecurringPayments : NoOfRecurringPayments;
        PayerNumber : PayerNumber;
        Amount : Amount;
        PayeeBankiroNumber : BgNumber;
        Reference : PaymentReference;
        paymentStatus : IncommingPaymentStatus
    }

type DepositOrWithdrawlRecord = 
    {
        PayeeBankAccountNumber : String;
        PaymentDate : DateTime;
        DepositSerialNumber : String;
        Amount : String;
        NrOfPayments : String;
    }

type PaymentRefundRecord = 
    {
        PayDate : BgDate; 
        PeriodCode : PeriodCode;
        NoOfRecurringPayments : NoOfRecurringPayments;
        PayerNumber : PayerNumber;
        Amount : Amount;
        PayeeBankiroNumber : BgNumber;
        Reference : PaymentReference;
        RefundCode : PaymentRefundCode
    }

type PaymentEndRow = 
    {
        DateWritten : BgDate;
        NrOfDepositRecords : int;
        NrOfIncommingPaymentRecords : int;
        NrOfWithdrawlRecords : int;
        NrOfOutgoingPaymentRecords : int;
        NrOfWithdrawlRecordsForRefund : int;
        NrOfRefundRecords : int;
    }

type OutgoingPaymentFileRow = 
    | OutgoingWithdrawl of OutGoingDepositOrWithDrawl
    | OutgoingDeposit of OutGoingDepositOrWithDrawl

type IncommingPaymentFileRow = 
    | DepositRecord of DepositOrWithdrawlRecord
    | IncommingPaymentRecord of InOrOutgoingPaymentRecord
    | WithdrawlRecord of DepositOrWithdrawlRecord
    | OutgoingPaymentRecord of InOrOutgoingPaymentRecord
    | WithdrawlForRefundRecord of DepositOrWithdrawlRecord
    | PaymentRefundRecord of PaymentRefundRecord

type IncommingPaymentFile =    
    {
        Head : IncommingHeaderRow;
        Rows : IncommingPaymentFileRow list
        End : PaymentEndRow
    }

type OutgoingPaymentFile =
    {
        Header : BgHeaderRow;
        Rows : OutgoingPaymentFileRow list
    }

type ClearingNumber = ClearingNumber of int 
type BankAccountNumber = BankAccountNumber of int64

type BankAccount = 
    {
        ClearingNumber : ClearingNumber
        BankAccountNumber : BankAccountNumber
    }

type CancellationOfMandate = 
    {
        BankAccount : BankAccount Option;
        PayerNumber : PayerNumber;
    }

type ApprovalRejectionOfMandate = 
    {
        PayyeBankGiro : BgNumber;
        PayerNumber : PayerNumber;
        PayerBankAccount : BankAccount Option
        PayerCivic : PayerNumber;
    }

type OutgoingMandateFileRecord = 
    | ApprovalRejectionOfMandate of ApprovalRejectionOfMandate
    | CancellationOfMandate of ApprovalRejectionOfMandate

type OutgoingMandateFile = 
    {
        Header : BgHeaderRow;
        Rows : OutgoingMandateFileRecord list
    }

type Result<'Result> = 
    | Passed of 'Result
    | Failed of string

module Result = 
    let bind f x = 
        match x with 
            | Passed r -> f r
            | Failed s -> Failed s

    let mapFold fn x = 
        let append state evts = 
            match state with 
                | oldEvents, errors -> (fn evts |> List.append oldEvents, errors)

        let appendErrors state error  = 
            match state with 
                | oldEvents, errors ->
                    let newErrorText = sprintf "%s \n\n %s" errors error  
                    (oldEvents, newErrorText)
        
        Seq.fold (fun s x -> match x with 
                                | Passed evts -> append s evts
                                | Failed e -> appendErrors s e)
                 ([], "")  x    
    
    let apply fn x = 
        match x with
            | Passed s -> fn s |> Passed
            | Failed f -> Failed f
