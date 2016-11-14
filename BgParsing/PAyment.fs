module Payment
open BgParsing.Models.Types
open BgParsing.Models.Events

let incommingPaymentFileRecordToEvent = 
    function
        | OutgoingWithdrawl r -> 
            
                {
                PaymentRequest.Amount = r.Amount;
                PaymentRequest.CivicNumber = r.PayerNumber
                PaymentRequest.Reference = r.Reference
                PaymentRequest.RequestdFor = r.PayDate
                }
                |> PaymentRequested
                |> PaymentEvent
                |> Passed
        | OutgoingDeposit r -> Failed <| sprintf "Unhandled record type OutgoingWithdrawl: %A" r

let outgoingPaymentFileRecordToEvent = 
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
        | DepositRecord _-> Failed "unhandled row type DepositRecord"
        | IncommingPaymentRecord a ->  incommingPaymentRecordToEvent a               
        | WithdrawlRecord _ -> Failed "unhandled rowtype WithdrawlRecord"
        | OutgoingPaymentRecord _ -> Failed "Unhandled row type OutgoingPaymentRecord"
        | WithdrawlForRefundRecord _ -> Failed "Unhandled rowtype Withdrawl for refund"
        | PaymentRefundRecord _ -> Failed "Unhandled refundRecord"