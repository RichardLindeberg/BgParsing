namespace BgParsing.Models.SharedParsingTypes

type ParsedFile = 
    | IncommingMandateAdviceFile of BgParsing.Models.MandateAdviceTypes.IncommingMandateAdviceFile
    | IncommingPaymentFile of BgParsing.Models.Types.IncommingPaymentFile
    | OutgoingPaymentFile of BgParsing.Models.Types.OutgoingPaymentFile
    | OutgoingMandateFile of BgParsing.Models.Types.OutgoingMandateFile
