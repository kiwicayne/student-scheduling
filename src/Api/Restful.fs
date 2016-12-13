module Student.Api.Restful

open Student.Api.Dtos
open Student.Infrastructure.Repositories
open Newtonsoft.Json
open Suave.Successful
open Newtonsoft.Json.Serialization
open Suave
open Student.Core
open Student.Core.Groups
open Student.Core.Utils

let fromJson<'a> json = JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

let getResourceFromReq<'a> (req : HttpRequest) = 
    let getString rawForm = System.Text.Encoding.UTF8.GetString(rawForm)
    req.rawForm
    |> getString
    |> fromJson<'a>

let JSON v = 
    let jsonSerializerSettings = new JsonSerializerSettings()
    jsonSerializerSettings.ContractResolver <- new CamelCasePropertyNamesContractResolver()
    JsonConvert.SerializeObject(v, jsonSerializerSettings) |> OK

module Student = 
    let getAll studentFile = 
        StudentRepository.getStudents studentFile
        |> List.map toStudentDto
        |> JSON

module Mentor = 
    let getAll() = MentorRepository.getMentors() |> JSON

module GroupScore = 
    open Student.Core.Groups
    
    let calculateHouseScore housesDto = 
        match housesDto |> dtoToHouses with
        | Some houses -> 
            houses
            |> FitnessScore.getFitnessScores
            |> JSON
        | None -> Suave.RequestErrors.BAD_REQUEST "The request is not valid"

module House = 
    let createHouses createHousesDto = 
        match dtoToStudentsAndMentors createHousesDto with
        | Some(students, mentorNames) -> 
            // todo: pass with dto instead
            let config = 
                { PopulationSize = 100
                  MaxEvolutions = 200
                  AcceptableScore = 100.0 }
            
            // todo: pass with dto instead
            let houseNames = [ "Catalina"; "Rincon"; "Santa Rita"; "Tortolita" ]
            let groups = GeneticAlgorithm.createGrouping mentorNames students config |> toGroupsDto
            
            let houses = 
                groups
                |> Random.listShuffle
                |> List.chunkBySize (groups.Length / (houseNames.Length))
                |> List.zip houseNames
                |> List.map (fun (hn, g) -> 
                       { Name = hn
                         Groups = (g |> List.sortBy (fun g -> g.Students.Length)) })
            { Houses = houses } |> JSON
        | None -> Suave.RequestErrors.BAD_REQUEST "The request is not valid"
