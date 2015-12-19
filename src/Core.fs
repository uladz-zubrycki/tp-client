module TpClient.Core

open HttpClient
open System
open System.IO
open FSharp.Data
open Http
open FSharpCommon.ConvertUtils
open FSharpCommon.StringUtils

type internal TpUserInfo = { UserName: string; Password: string; }
//type private HttpErrorResponse = JsonProvider<".\JsonSamples\GithubClient\HttpError.json">
//type GetProjectsResponse = JsonProvider<"""..\JsonSamples\TargetProcess\GetProjects.json""">

let private curDir = Directory.GetCurrentDirectory()
let private apiRoot = "https://plan.tpondemand.com/api/v1"

[<AutoOpen>]
module Filtering = 
    type Filter<'a> = 
        | Equal of attr: string * value: 'a
        | NotEqual of attr: string * value: 'a
        | Greater of attr: string * value: 'a
        | GreaterOrEqual of attr: string * value: 'a
        | Less of attr: string * value: 'a
        | LessOrEqual of attr: string * value: 'a
        | InList of attr: string * values: 'a list
        | Contains of attr: string * value: 'a
        | IsNull of attr: string
        | IsNotNull of attr: string
    
    type Filter = 
        | StringFilter of Filter<string> 
        | IntFilter of Filter<int> 
        | FloatFilter of Filter<float> 
        | DateTimeFilter of Filter<DateTime> 
    
    type Filters = 
        static member equal(attr, value) = StringFilter(Equal(attr, value)) 
        static member equal(attr, value) = IntFilter(Equal(attr, value)) 
        static member equal(attr, value) = FloatFilter(Equal(attr, value)) 
        static member equal(attr, value) = DateTimeFilter(Equal(attr, value)) 
        static member notEqual(attr, value) = StringFilter(NotEqual(attr, value)) 
        static member notEqual(attr, value) = IntFilter(NotEqual(attr, value)) 
        static member notEqual(attr, value) = FloatFilter(NotEqual(attr, value)) 
        static member notEqual(attr, value) = DateTimeFilter(NotEqual(attr, value)) 
        static member greater(attr, value) = StringFilter(Greater(attr, value)) 
        static member greater(attr, value) = IntFilter(Greater(attr, value)) 
        static member greater(attr, value) = FloatFilter(Greater(attr, value)) 
        static member greater(attr, value) = DateTimeFilter(Greater(attr, value)) 
        static member greaterOrEqual(attr, value) = StringFilter(GreaterOrEqual(attr, value)) 
        static member greaterOrEqual(attr, value) = IntFilter(GreaterOrEqual(attr, value)) 
        static member greaterOrEqual(attr, value) = FloatFilter(GreaterOrEqual(attr, value)) 
        static member greaterOrEqual(attr, value) = DateTimeFilter(GreaterOrEqual(attr, value)) 
        static member less(attr, value) = StringFilter(Less(attr, value)) 
        static member less(attr, value) = IntFilter(Less(attr, value)) 
        static member less(attr, value) = FloatFilter(Less(attr, value)) 
        static member less(attr, value) = DateTimeFilter(Less(attr, value))
        static member lessOrEqual(attr, value) = StringFilter(LessOrEqual(attr, value)) 
        static member lessOrEqual(attr, value) = IntFilter(LessOrEqual(attr, value)) 
        static member lessOrEqual(attr, value) = FloatFilter(LessOrEqual(attr, value)) 
        static member lessOrEqual(attr, value) = DateTimeFilter(LessOrEqual(attr, value))
        static member inList(attr, values) = StringFilter(InList(attr, values)) 
        static member inList(attr, values) = IntFilter(InList(attr, values)) 
        static member inList(attr, values) = FloatFilter(InList(attr, values)) 
        static member inList(attr, values) = DateTimeFilter(InList(attr, values))
        static member contains(attr, value) = StringFilter(Contains(attr, value)) 
        static member contains(attr, value) = IntFilter(Contains(attr, value)) 
        static member contains(attr, value) = FloatFilter(Contains(attr, value)) 
        static member contains(attr, value) = DateTimeFilter(Contains(attr, value))
        static member isNull(attr) = StringFilter(IsNull(attr)) 
        static member isNotNull(attr) = StringFilter(IsNotNull(attr)) 
        
    let internal createFilterQueryValue (filters: Filter list) = 
        let getOperandStr value = 
            match value with
            | DateTime(value) -> "'" + value.ToString("yyyy-MM-dd") + "'"
            | Int32(value) -> value.ToString()
            | Float(value) -> value.ToString()
            | _ -> "'" + value.ToString() + "'"

        let getArrayOperandStr values = 
            values 
            |> Seq.map getOperandStr 
            |> String.concat ", "

        let createOperatorStr (op: Filter<'a>) = 
            match op with
            | Equal(attr, value) -> sprintf "%s eq %s" attr (getOperandStr value)
            | NotEqual(attr, value) -> sprintf "%s ne %s" attr (getOperandStr value)
            | Greater(attr, value) -> sprintf "%s gt %s" attr (getOperandStr value)
            | GreaterOrEqual(attr, value) -> sprintf "%s gte %s" attr (getOperandStr value)
            | Less(attr, value) -> sprintf "%s lt %s" attr (getOperandStr value)
            | LessOrEqual(attr, value) -> sprintf "%s lte %s" attr (getOperandStr value)
            | InList(attr, values) -> sprintf "%s in (%s)" attr (getArrayOperandStr values)
            | Contains(attr, value) -> sprintf "%s contains %s" attr (getOperandStr value)
            | IsNull(attr) -> sprintf "%s is null" attr
            | IsNotNull(attr) -> sprintf "%s is not null" attr

        filters 
        |> Seq.map (fun op -> 
            match op with 
            | StringFilter(op) -> createOperatorStr op
            | IntFilter(op) -> createOperatorStr op
            | FloatFilter(op) -> createOperatorStr op
            | DateTimeFilter(op) -> createOperatorStr op) 
        |> String.concat " and "

[<AutoOpen>]
module Paging = 
    type PagingOptions = 
        | SkipTake of skip: int * take: int
        | GetPage of pageSize: int * pageIndex: int

    let internal createPagingQueryParameters = function
        | SkipTake(skip, take) -> 
            seq { 
                yield ("skip", skip |> toString) 
                yield ("take", take |> toString)}
        | GetPage(pageSize, pageIndex) -> 
            seq { 
                yield ("skip", pageSize * pageIndex |> toString) 
                yield ("take", pageSize |> toString)}

type internal IResponseParser<'a> = abstract member Parse: string -> 'a 
type internal ICollectionResponseParser<'a> = abstract member Parse: string -> 'a []

let internal withDefaultParameters userInfo request = 
    request
    |> withBasicAuthentication userInfo.UserName userInfo.Password
    |> withHeader (Accept("application/json"))

let internal maybeWithFiltering filter request = 
    let maybeFilteringStr = filter |> Option.map createFilterQueryValue
    request |> maybeWithQueryStringParam ("where", maybeFilteringStr)

let internal maybeWithPaging pager request = 
    pager 
    |> Option.map createPagingQueryParameters
    |> Option.map (fun queryStringParams -> request |> withQueryStringParams queryStringParams)
    |> function
        | Some(newRequest) -> newRequest
        | None -> request

let internal processTPResponse handlers (response: Response) = 
    let handlersDict = handlers |> dict
    let createResponseMessage (message: string) =
        let message = message.Trim([|'.'|])
        match response.EntityBody with
        | Some(responseBody) -> sprintf "%s. Status code: '%d', response body: '%s'." message response.StatusCode responseBody
        | None -> sprintf "%s. Status code: '%d'." message response.StatusCode 
        
    match handlersDict.TryGetValue response.StatusCode with
    | true, handler -> response.EntityBody |> handler
    | false, _ -> 
        let statusCodeStr = response.StatusCode.ToString()
        match statusCodeStr with
        | "400" -> apiFailure "TargetProcess api communication error: Bad format. Incorrect parameter or query string."
        | "401" -> apiFailure "TargetProcess api communication error: Unauthorized. Wrong or missed credentials."
        | "403" -> apiFailure "TargetProcess api communication error: Forbidden. A user has insufficient rights to perform an action."
        | "404" -> apiFailure "TargetProcess api communication error: Requested Entity not found."
        | "500" -> apiFailure "TargetProcess api server-side error."
        | "501" -> apiFailure "TargetProcess api server-side error: Not implemented. The requested action is either not supported or not implemented yet."
        | StartsWith "4" -> apiFailure <| createResponseMessage "Unexpected TargetProcess api communication error."
        | StartsWith "5" -> apiFailure <| createResponseMessage "TargetProcess api server-side error."
        | _ -> apiFailure <| createResponseMessage "Nonsupported TargetProcess api response type."

let internal getTpResponse url userInfo filters pager = 
    createRequest Get url
    |> withDefaultParameters userInfo
    |> maybeWithFiltering filters
    |> maybeWithPaging pager
    |> getResponse

let internal getSingle<'a> (parser: IResponseParser<'a>, url, userInfo, filters, pager) = 
    getTpResponse url userInfo filters pager
    |> processTPResponse [200, (fun responseBody -> responseBody |> Option.map parser.Parse)] 
    
let internal getCollection<'a> (parser: ICollectionResponseParser<'a>, url, userInfo, filters, pager) = 
    getTpResponse url userInfo filters pager
    |> processTPResponse [
        200, (function 
                | Some(responseBody) -> parser.Parse responseBody  
                | None -> Array.empty<'a> )]
