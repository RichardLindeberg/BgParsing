namespace BgParsing.Models.MandateAdviceTypes

open BgParsing.Models.Types

type IncommingMandateHeaderRow =
    {
        DateWritten : BgDate
        PayeeCustomerNumber : CustomerKey
        PayeeBankGiroNumber : BgNumber
    }



type MandateCommentCode = 
    | MandateCancelledOnInitiativeOfPayerOrPayersBank_2
    | AccountTypeNotApprovedForAutogiro_3
    | MandateNotFoundInBankgirotsMandateDirectory_4 
    | IncorrectBankAccountOrPersonalDetails_5
    | CancelledOrRemovedDueToUnansweredAccountInquiry_7
    | PayerBankgiroNumberNotFoundAtBankgirot_9
    | MandateAlreadyRegisteredInBankgirotsDirectoryOrInquiryPending_10
    | IncorrectCivicOrcompanyNumber_20
    | IncorrectPayerNumber_21
    | IncorrectBankAccountNumber_23
    | IncorrectPayeeBankgiroNumber_29
    | DeregisteredPayeeBankgiroNumber_30
    | NewMandate_32
    | Cancelled_33
    | MandateCancelledDueToCancelledPayerBankgiroNumber_98


type MandateAdviceRecordContent = 
    {
        PayeeBankGiro : BgNumber;
        BankAccount : BankAccount Option;
        PayerNumber : PayerNumber;
        CivicNumber : PayerNumber;
        CommentCode : MandateCommentCode;
        ActionDate  : BgDate
    }

type MandateAdviceRecord = 
    | CancellationInitiatedByPayee03 of MandateAdviceRecordContent 
    | NewAdditionInitiatedByPayee04 of MandateAdviceRecordContent 
    | ChangePayerNumber05 of MandateAdviceRecordContent 
    | CancelledDueToPayeesBankgironumberBeingClosed10 of MandateAdviceRecordContent 
    | ResponseToAccountInquiryFromBank42 of MandateAdviceRecordContent 
    | CancelledOrRemovedDueToUnansweredAccountInquiry43 of MandateAdviceRecordContent 
    | CancelledDueToPayersBankgiroNumberBeingClosed44 of MandateAdviceRecordContent 
    | CancellationByBank46 of MandateAdviceRecordContent 
    | CancellationByPayer93 of MandateAdviceRecordContent 
    
type NumberOfAdviceRecords = NumberOfAdviceRecords of int 
type MandateAdviceEndRecord = 
    {
        DateWritten : BgDate
        NumberOfRecords : NumberOfAdviceRecords
    }

type IncommingMandateAdviceFile = 
    {
        StartRecord : IncommingMandateHeaderRow;
        Rows : MandateAdviceRecord list
        EndRecord : MandateAdviceEndRecord
    }

