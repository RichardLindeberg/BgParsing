namespace BgParsing.Models.Events

open BgParsing.Models.Types

type MandateAdviceEventRecord = 
    {
     BankAccount : BankAccount Option;
     CivicNumber : PayerNumber;
     ActionDate  : BgDate
    }

type MandateAdviceEvent =
    | MandateRequested of MandateAdviceEventRecord
    | MandateCreated of MandateAdviceEventRecord
    | MandateCancelled of MandateAdviceEventRecord
    | MandateCreationFailed of MandateAdviceEventRecord

type PaymentRequest = 
    {
      CivicNumber : PayerNumber;
      Reference : PaymentReference;
      Amount : Amount;
      RequestdFor : BgPayDate
    }

type PaymentExecuted = 
    {
      CivicNumber : PayerNumber;
      Reference : PaymentReference;
      Amount : Amount;
      WithdrawalDate : BgPayDate
    }

type PaymentRejected = 
    {
        CivicNumber : PayerNumber;
        PaymentStatus : IncommingPaymentStatus
        Reference : PaymentReference;
        RejectionDate : BgPayDate
    }

type PaymentEvent = 
    | PaymentRequested of PaymentRequest
    | PaymentExecuted of PaymentExecuted
    | PaymentRejected of PaymentRejected
    | PaymentCancelled

type Event = 
    | PaymentEvent of PaymentEvent
    | MandateAdviceEvent of MandateAdviceEvent

type Mandate = 
    {
     CivicNumber : PayerNumber
     IsActive : bool
    }
