module ApiServer

open Student.Api
open System
open System.IO
open System.Threading
open Student.Core.Utils.Convert
open Student.Api.Server

/// Load server configuration from the arguments passed to the console application
let parseArgs (args: string[]) =
    if args.Length <> 2 then failwith "Invalid number of args, expecting a path to student.csv file and port number"
    if not <| (args.[0] |> File.Exists) then failwith (sprintf "Unable to find file '%s'" args.[0])
    match args.[1] with
    | Int i ->
        { StudentFile = args.[0]
          Port = i }
    | _ -> failwith (sprintf "The second parameter must be a valid port number '%s'" args.[1])

/// This is a simple console application which will run the API server
/// locally.  There are two required arguments:
/// 1. Path to student.csv
/// 2. Port number to run server on
[<EntryPoint>]
let main args =
    try
        let config = args |> parseArgs

        printfn "Web server started on port %d" config.Port

        let cts = new CancellationTokenSource()
        Server.start config cts.Token

        printfn "Press Enter to quit"
        Console.ReadLine() |> ignore
        cts.Cancel()
        0
    with
    | ex ->
        printfn "Oops, an error occurred '%s'" ex.Message
        1