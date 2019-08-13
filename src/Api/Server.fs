module Student.Api.Server

open Suave
open Suave.Filters
open Suave.Operators
open Student.Api.Restful

type ServerConfig =
    { StudentFile: string
      Port: int }

let app config =
  choose
    [ GET >=> choose
        [ path "/students" >=> Student.getAll config.StudentFile
          path "/mentors" >=> Mentor.getAll() ]
      POST >=> choose
        [ path "/houses/score" >=> request (getResourceFromReq >> GroupScore.calculateHouseScore)
          path "/houses" >=> request (getResourceFromReq >> House.createHouses) ] ]

let configServer port token =
    { defaultConfig with
        cancellationToken = token
        bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" port ] }

let start config token =
    let webServerconfig = configServer config.Port token
    let _, server = startWebServerAsync webServerconfig (app config)
    Async.Start(server, token)