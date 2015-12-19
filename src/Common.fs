[<AutoOpen>]   
module internal TpClient.Common

type Result<'a> = Success of 'a | Failure of string option