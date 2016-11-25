module EventsPrinter
open BgParsing.Models.Events

let printEvents evt = 
    match evt with 
        | PaymentEvent e -> sprintf "PaymentEvent %A" e
        | MandateAdviceEvent e -> sprintf "MandateAdvice %A" e