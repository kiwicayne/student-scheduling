module MakeGroups

open Student.Core.Groups
open Student.Core.Domain
open Print.Groups
open System.Diagnostics
open System
open Student.Infrastructure.Repositories

let cprintf _ fmt args =
    Console.BackgroundColor <- ConsoleColor.Blue
    printf fmt args
    Console.ResetColor()
    ()

let cprintfn c fmt args =
    Console.ForegroundColor <- c
    printfn fmt args
    Console.ResetColor()
    ()

let runGenetic mentors students =

    let config =
        { PopulationSize = 100
          MaxEvolutions = 500
          AcceptableScore = 95.0 }

    let stopwatch = Stopwatch.StartNew()
    printf "Generating initial population..."
    let bestSolution =
        GeneticAlgorithm.createGrouping mentors students config
    stopwatch.Stop()
    cprintfn ConsoleColor.Cyan "done %dms" stopwatch.ElapsedMilliseconds
    printfn ""
    printFitness bestSolution true
    GroupFileRepository.writeGroups @"genetic.csv" bestSolution

let runSorted mentors students =
    let stopwatch = Stopwatch.StartNew()
    printf "Running sorted group algorithm..."
    let result = SortAgorithm.createGrouping mentors students
    stopwatch.Stop()
    cprintfn ConsoleColor.Cyan "done %dms" stopwatch.ElapsedMilliseconds
    printFitness result false

let runRandom mentors students =
    let stopwatch = Stopwatch.StartNew()
    printf "Running random group algorithm..."

    let getFitness (grouping: Group list) =
        grouping
        |> List.map (fun x -> x.Students)
        |> FitnessScore.getGroupingFitnessScore

    let _, result =
        Async.Parallel [ for _ in 1..1000 -> async { return RandomAlgorithm.createGrouping mentors students } ]
        |> Async.RunSynchronously
        |> Array.toList
        |> List.map (fun x -> (getFitness x, x))
        |> List.sortByDescending (fun (x,_) -> x)
        |> List.head

    stopwatch.Stop()
    cprintfn ConsoleColor.Cyan "done %dms" stopwatch.ElapsedMilliseconds
    printFitness result false

let getFilename (args: string[]) =
    if args.Length <> 1 then failwith "Invalid number of args, expecting a single path to student.csv file"
    if not <| (args.[0] |> System.IO.File.Exists) then failwith (sprintf "Unable to find file '%s'" args.[0])
    args.[0]

[<EntryPoint>]
let main args =
    try
        let getStudents () = StudentRepository.getStudents (args |> getFilename)

        printfn "Making %d groups for %d students\n" (MentorRepository.getMentors().Length)
            (getStudents().Length)

        let mentors = MentorRepository.getMentors()
        let students = getStudents()

        runRandom mentors students
        printfn ""

        runSorted mentors students
        printfn ""

        runGenetic mentors students

        printfn "\nAll done, press Enter to exit."
        Console.ReadLine() |> ignore
        0
    with
    | ex ->
        printfn "Oops, an error occurred '%s'" ex.Message
        1
