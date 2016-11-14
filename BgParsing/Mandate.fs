module Mandate
open BgParsing.Models.Types
open BgParsing.Models.MandateAdviceTypes
open BgParsing.Models.Events

let outgoingMandateRecordToEvent actionDate = 
    let toMAER (mar : ApprovalRejectionOfMandate) : MandateAdviceEventRecord  = 
        {
            MandateAdviceEventRecord.BankAccount = mar.PayerBankAccount; 
            CivicNumber = mar.PayerCivic; 
            ActionDate = actionDate;
        }
    function 
        | ApprovalRejectionOfMandate r ->
            r |> (toMAER >> MandateRequested >> MandateAdviceEvent >> Passed)
        | CancellationOfMandate _ -> Failed "Cant handle initiation of mandate cancellation"
            

let mandateAdviceRecordToEvent = 
    let toMAER (mar : MandateAdviceRecordContent) : MandateAdviceEventRecord  = 
        {
            MandateAdviceEventRecord.BankAccount = mar.BankAccount; 
            CivicNumber = mar.CivicNumber; 
            ActionDate = mar.ActionDate;
        }
    let toPassedEvent b = 
        toMAER  >> b >> MandateAdviceEvent >> Passed
        
    function 
        | CancellationInitiatedByPayee03 mr -> mr |> toPassedEvent MandateCancelled
        | NewAdditionInitiatedByPayee04 mr -> 
            match mr.CommentCode with 
                | NewMandate_32 _ -> mr |> toPassedEvent MandateCreated
                | _ -> mr |> toPassedEvent MandateCreationFailed
        | ChangePayerNumber05 mr -> Failed "Can handle changePayerNumber"
        | CancelledDueToPayeesBankgironumberBeingClosed10 mr -> mr |> toPassedEvent MandateCancelled 
        | ResponseToAccountInquiryFromBank42 mr -> mr |> toPassedEvent MandateCreationFailed 
        | CancelledOrRemovedDueToUnansweredAccountInquiry43 mr -> mr |> toPassedEvent MandateCancelled 
        | CancelledDueToPayersBankgiroNumberBeingClosed44 mr -> mr |> toPassedEvent MandateCancelled 
        | CancellationByBank46 mr -> mr |> toPassedEvent MandateCancelled 
        | CancellationByPayer93 mr -> mr |> toPassedEvent MandateCancelled


let sortEventsBy fn = 
    List.sortBy (fun ev -> 
        match ev with 
            | MandateCreated er 
            | MandateCreationFailed er 
            | MandateCancelled er 
            | MandateRequested er
                        -> fn er
            )
let sortEventsByDate = 
    let fn er = match er.ActionDate with BgDate d -> d 
    sortEventsBy fn
let sortEventsByCivicNumber = 
    let fn (er : MandateAdviceEventRecord) = match er.CivicNumber with PayerNumber p -> p 
    sortEventsBy fn

let groupEventsBy fn = 
    List.groupBy (fun ev -> 
                     match ev with 
                        | MandateRequested er
                        | MandateCreated er 
                        | MandateCreationFailed er 
                        | MandateCancelled er 
                            -> fn er
                  )

let groupEventsByCivicNumber = 
    let fn (er : MandateAdviceEventRecord) = er.CivicNumber
    groupEventsBy fn
    
let buildState = 
    let applyState state evt = 
         match evt with 
                | MandateRequested er -> {state with IsActive = false}
                | MandateCreated er -> {state with IsActive = true}
                | MandateCreationFailed er -> {state with IsActive = false}
                | MandateCancelled er -> {state with IsActive = false}
    let getStatOnPayer (payerNumber, eventsForPayer) = 
        let initialState = {Mandate.CivicNumber = payerNumber; IsActive = false}
        List.fold applyState initialState eventsForPayer

    List.map getStatOnPayer 
