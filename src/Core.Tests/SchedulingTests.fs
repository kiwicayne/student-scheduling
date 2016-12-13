namespace Societies.Core.Tests
open Xunit
open FsUnit.Xunit
open Societies.Core.Scheduling.FitnessScore
open Societies.Core.Domain

module SchedulingTests =
    
    module FitnessTests =
        [<Fact>]
        let ``Calculate student fitness score should return 0 when house and activity schedule is empty`` () =
            let score = calculateStudentsFitnessScore [] [] []
            score |> should equal 0.0

       [<Fact>]
        let ``Calculate student fitness score should return 0 when house has a student but activity schedule is empty`` () =
            let house = 
                [ { Students = [ { Name = { FirstName = "Bob"; LastName = "Smith" }; Gender = Male; Age = 26; Major = "Physics" } ]
                    Mentor = "Mentor"  } ]
            let score = calculateStudentsFitnessScore [] house []
            score |> should equal 0.0


       