module MakeSchedule

open System
open System.Diagnostics
open Student.Core.Domain
open Student.Core.Groups
open Student.Core.Scheduling
open Student.Infrastructure.Repositories
open Microsoft.FSharp.Collections

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

// Helpers
let ``12pm`` = TimeSpan(12, 0, 0)
let ``1pm`` = TimeSpan(13, 0, 0)
let ``3pm`` = TimeSpan(15, 0, 0)
let ``5pm`` = TimeSpan(17, 0, 0)
let ``6pm`` = TimeSpan(18, 0, 0)

let ``2015-5-5`` = DateTime(2015, 5, 5)
let ``2015-9-29`` = DateTime(2015, 9, 29)
let ``2015-10-13`` = DateTime(2015, 10, 13)
let ``2015-10-20`` = DateTime(2015, 10, 20)
let ``2015-10-27`` = DateTime(2015, 10, 27)
let ``2015-11-13`` = DateTime(2015, 11, 13)
let ``2015-12-1`` = DateTime(2015, 12, 1)

let getTestGroups getMentors getStudents = SortAgorithm.createGrouping getMentors getStudents

let runAlgorithm block house attendanceRecord fitness count schedulingAlgorithm =
    let generateSchedule () = Schedule.createSchedule block house attendanceRecord schedulingAlgorithm

    // Run Random/Ordered algorithms
    Async.Parallel [ for _ in 1..count -> async { return generateSchedule() } ]
    |> Async.RunSynchronously
    |> Array.map (fun x -> async { return x |> fitness, x })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.toList
    |> List.sortByDescending (fun (x, _) -> x)
    |> List.head |> snd

let getFilename (args: string[]) =
    if args.Length <> 1 then failwith "Invalid number of args, expecting a single path to student.csv file"
    if not <| (args.[0] |> System.IO.File.Exists) then failwith (sprintf "Unable to find file '%s'" args.[0])
    args.[0]

[<EntryPoint>]
let main args =
    try
        let studentFile = getFilename args

        printfn "Setting up domain objects"

        // Generate 5 test groups to make a house to be used in block scheduling
        let house = getTestGroups (MentorRepository.getMentors()) (StudentRepository.getStudents studentFile) |> List.take 5

        // ********* Enrollment criteria - combines the source and attendance requirement *************

        let ``2 peers from each group must perform each role 3 times per block`` =
            FromGroup (SelectTwoPeers (RequireEachPeerToPerformBothRolesPerBlock 3<Times>))

        let ``each session has a maximum of 10 students from the house, each student must attend once this year`` =
            FromHouse (SelectMaxStudents (10, RequireEachStudentToAttendOnceThisYear))

        let ``each session has a maximum of 6 students from the house, each student must attend once this year`` =
            FromHouse (SelectMaxStudents (6, RequireEachStudentToAttendOnceThisYear))

        let ``all students in house must attend all sessions`` =
            FromHouse (SelectAllStudents RequireEachStudentToAttendEverySession)

        // *********** Activity definitions *********************

        // Create the "fixed" mandatory activities
        let pePractice =
            { Name = "PE Practice"
              Frequency = SpecificTimes [ (``2015-9-29``, ``1pm``, ``3pm``)
                                          (``2015-10-13``, ``3pm``, ``5pm``) ]
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }

        let peFinalClinic =
            { Name = "PE Final Clinic"
              Frequency = Once(``2015-10-27``, ``12pm``, ``6pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }

        let medicalIntFinalClinic =
            { Name = "Med int. Final Clinic"
              Frequency = Once(``2015-10-20``, ``1pm``, ``5pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }

        let professionalAndPersonalDevelopment =
            { Name = "PPD"
              Frequency = Once(``2015-11-13``, ``1pm``, ``3pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }

        // Create H&P Activities
        let historyAndPhysical =
            { Name = "H&P"
              Frequency = Weekly [ (``1pm``, ``3pm``)
                                   (``3pm``, ``5pm``) ]
              EnrollmentPriority = Highest
              EnrollmentCriteria = ``2 peers from each group must perform each role 3 times per block`` }

        let mentorDirectedTime =
            { Name = "Mentor Directed Time"
              Frequency = Weekly [ (``1pm``, ``3pm``)
                                   (``3pm``, ``5pm``) ]
              EnrollmentPriority = Neutral
              EnrollmentCriteria = OverflowFrom historyAndPhysical }

        // Create once per year activities
        let ophthalmologyClinic =
            { Name = "Ophtho"
              Frequency = Weekly [ (``1pm``, ``3pm``) ]
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }

        let standardizedPatientLabWeaknessClinic =
            { Name = "SP Lab Weekness Clinic"
              Frequency = Weekly [ (``1pm``, ``5pm``) ]
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 10 students from the house, each student must attend once this year`` }

        let standardizedPatientLabTremorClinic =
            { Name = "SP Lab Tremor Clinic"
              Frequency = Weekly [ (``1pm``, ``5pm``) ]
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 10 students from the house, each student must attend once this year`` }

        let standardizedPatientLabHeadacheClinic =
            { Name = "SP Lab Headache Clinic"
              Frequency = Weekly [ (``1pm``, ``5pm``) ]
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 10 students from the house, each student must attend once this year`` }

        // **** Create the block from the activities above ********

        // Create the block and assign the unsheduled activities to it
        let block =
            { Course = { Name = "Doctor-Patient" }
              Name = "Foundations/Nervous System - Societies Tuesday Activities"
              StartDate = ``2015-9-29``
              EndDate = ``2015-12-1``
              House = house
              Activities =
              [ pePractice; peFinalClinic; medicalIntFinalClinic; professionalAndPersonalDevelopment; historyAndPhysical;
                ophthalmologyClinic; standardizedPatientLabWeaknessClinic; standardizedPatientLabTremorClinic;
                standardizedPatientLabHeadacheClinic; mentorDirectedTime ] }

        // Create a history of student attendance which will be used by the scheduling algorithm to determine if
        // a student needs to be scheduled to some activities
        let createTestAttendance student (activity: Activity) =
            let students = Set.ofList (block.House |> getHouseStudents)
            let emptySession = createEmptySession students (``2015-5-5``, ``1pm``, ``3pm``)
            let session = { emptySession with Enrollment = StudentEnrollment student }

            { Student = student
              Sessions = [ (activity, session) ] }

        let attendanceRecord =
            [ createTestAttendance house.Head.Students.Head ophthalmologyClinic
              createTestAttendance house.Head.Students.[2] ophthalmologyClinic ]

        let stopwatch = Stopwatch.StartNew()
        printfn "Generating Schedule"

        let fitness = FitnessScore.getScheduleFitnessScore attendanceRecord house

        // Genetic algorithm
        let config =
            { PopulationSize = 20
              MaxEvolutions = 500
              AcceptableScore = 98.0 }

        let result = Schedule.Genetic.createSchedule config block house attendanceRecord
        stopwatch.Stop()

        result |> BlockFileRepository.writeBlockSchedule "schedule.csv" house
        printfn "\n\nResult %.2f" (result |> fitness)

        cprintfn ConsoleColor.Cyan "done %dms" stopwatch.ElapsedMilliseconds

        Console.ReadLine() |> ignore
        0
    with
    | ex ->
        printfn "Oops, an error occurred '%s'" ex.Message
        1