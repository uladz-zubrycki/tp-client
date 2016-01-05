#load "RestModel.fsx"
#load @"..\paket-files\v-zubritsky\fsharp-common\PathUtils.fs"
#r @"..\packages\Http.fs\lib\net40\HttpClient.dll"

open HttpClient
open RestModel
open System.IO
open FSharpCommon.PathUtils

let curDir = __SOURCE_DIRECTORY__
let outputPath = curDir @@ @"..\misc\JsonSamples"
let endpoint = "https://plan.tpondemand.com"
let resources = getResources()

resources 
|> List.iter (fun r -> 
     let resourceUrl = endpoint + r.RelativeUrl
     let sampleOutputPath = outputPath @@ (sprintf "Get%s.json" r.ResourceName)

     createRequest Get resourceUrl
     |> withBasicAuthentication "LOGIN" "PASSWORD"
     |> withHeader (Accept("application/json"))
     |> getResponseBody
     |> (fun resourceSample -> File.WriteAllText(sampleOutputPath, resourceSample)))
