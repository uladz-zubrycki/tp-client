#r "System.Xml.Linq"
#I @"..\packages"
#r @"FSharp.Data\lib\net40\FSharp.Data.dll"

open System.Xml.Linq
open System.Text.RegularExpressions
open FSharp.Data

type private ResourcesMeta = XmlProvider<"""..\misc\JsonSamples\meta.xml""">

type ResourceType = Entity | Collection

type ResourceDescriptor = 
    { EntityName: string;
      ResourceName: string;
      RelativeUrl: string;
      Type: ResourceType;
      Description: string; }

let private getResourceName(r: ResourcesMeta.ResourceMetadataDescription) = 
    let uriMatch = r.Uri |> Regex(@".*/(?<Name>.*)/meta").Match
    uriMatch.Groups.["Name"].Value

let private getResourceRelativeUri(r: ResourcesMeta.ResourceMetadataDescription) = 
    let uriMatch = r.Uri |> Regex(@"https://plan.tpondemand.com(?<RelativeUri>/api/v1/.*)/meta").Match
    uriMatch.Groups.["RelativeUri"].Value

let getResources() = 
    ResourcesMeta.GetSample()
    |> (fun meta -> meta.ResourceMetadataDescriptions) 
    |> Array.map (fun r -> 
         let resourceName = r |> getResourceName
         // Asssuming that EntityName (ex. Assignable) differs from
         // ResourceName (ex. Assignables) only because of plural form for collections
         let resourceType = if resourceName = r.Name then Entity else Collection

         { EntityName = r.Name;
           ResourceName = resourceName;
           RelativeUrl = r |> getResourceRelativeUri; 
           Type = resourceType;
           Description = r.Description})
    |> List.ofArray