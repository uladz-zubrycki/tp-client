namespace TpClient.Common

[<AutoOpen>]   
module Common = 
    type Result<'a> = Success of 'a | Failure of string option