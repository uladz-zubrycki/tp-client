#load @"..\paket-files\v-zubritsky\fsharp-common\PathUtils.fs"
#load @".\RestModel.fsx"

open System
open System.IO
open FSharpCommon.PathUtils
open RestModel

Directory.SetCurrentDirectory __SOURCE_DIRECTORY__ 

let curDir = Directory.GetCurrentDirectory()
let outputPath = normalizePath (@".\..\src\Client.fs") curDir
let normalizeTemplate (s: string) = s.Trim(System.Environment.NewLine.ToCharArray())

let tpClientTemplate = normalizeTemplate """// ----
// THIS FILE IS AUTOGENERATED. ANY MANUAL MODIFICATIONS WILL BE LOST. 
// ----

namespace TpClient
open TpClient.Core
open FSharp.Data

[<AutoOpen>]
module Client = 
$ResourcesRequestModules

    type TargetProcessClient(targetProcessUrl: string, username, password) =
        let userInfo = { UserName = username; Password = password }
        let endpoint = targetProcessUrl.Trim([|'\\'; '/'|])
        
$ClientMembers
"""

let entityRequestTemplate = normalizeTemplate """
    module private $ResourceRequestModuleName =
        [<Literal>] 
        let private jsonSamplePath = @"$ResourceResponseSamplePath" 
        type private Response = JsonProvider<jsonSamplePath>
        let private parser = 
            { new IResponseParser<Response.Root> with 
                member this.Parse responseBody = Response.Parse responseBody }

        let execute(endpoint, userInfo, filters, pager) = 
            let url = endpoint + "$ResourceUrl" 
            getSingle(parser, url, userInfo, filters, pager) 
"""

let collectionRequestTemplate = normalizeTemplate """
    module private $ResourceRequestModuleName = 
        [<Literal>] 
        let private jsonSamplePath = @"$ResourceResponseSamplePath"
        type private Response = JsonProvider<jsonSamplePath>
        let private parser = 
            { new ICollectionResponseParser<Response.Item> with 
                member this.Parse responseBody = (Response.Parse responseBody).Items }

        let execute (endpoint, userInfo, filters, pager) = 
            let url = endpoint + "$ResourceUrl" 
            getCollection(parser, url, userInfo, filters, pager) 
"""

let tpClientMemberTemplate = normalizeTemplate """
        member __.$ResourceMemberName(?filters, ?pager) = $ResourceRequestModuleName.execute(endpoint, userInfo, filters, pager)
"""

let createRequestModuleName resource = sprintf "Get%sRequest" resource.ResourceName
let createMemberName resource = sprintf "Get%s" resource.ResourceName
let createJsonSamplePath resource = 
    let samplesPath = normalizePath (curDir @@ "../misc/JsonSamples") curDir
    samplesPath @@ sprintf "%s.json" (createMemberName resource) 

let buildResourceRequest resource = 
    let template = 
        match resource.Type with 
        | Entity -> entityRequestTemplate
        | Collection -> collectionRequestTemplate

    let resourceRequestModuleName = createRequestModuleName resource
    let resourceResponseSamplePath = createJsonSamplePath resource
    
    template
        .Replace("$ResourceRequestModuleName", resourceRequestModuleName)
        .Replace("$ResourceResponseSamplePath", resourceResponseSamplePath)
        .Replace("$ResourceUrl", resource.RelativeUrl)

let buildTpClientMember resource = 
    let resourceMemberName = resource |> createMemberName
    let resourceRequestModuleName = resource |> createRequestModuleName

    tpClientMemberTemplate
        .Replace("$ResourceMemberName", resourceMemberName)        
        .Replace("$ResourceRequestModuleName", resourceRequestModuleName)

let buildTpClient(resources: ResourceDescriptor list) = 
    let resourcesRequestModules = 
        resources 
        |> Seq.map buildResourceRequest
        |> String.concat (Environment.NewLine + Environment.NewLine)
        
    let clientMembers = 
        resources 
        |> Seq.map buildTpClientMember
        |> String.concat Environment.NewLine

    let content = 
        tpClientTemplate
            .Replace("$ResourcesRequestModules", resourcesRequestModules)
            .Replace("$ClientMembers", clientMembers)

    File.WriteAllText(outputPath, content)

getResources() |> buildTpClient 
