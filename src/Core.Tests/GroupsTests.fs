namespace Student.Core.Tests
open Xunit
open FsUnit.Xunit
open Student.Core.Domain
open Student.Core.Utils

module GroupsTests =
    
    module GeneticAlgorithmTests =
        open Student.Core.Groups

        let rndInt = Probability.randomIntGenerator
            
        let rndGender () = 
            match Probability.rndMinMax rndInt 0 2 with
            | 0 -> NotSpecified
            | 1 -> Male
            | _ -> Female

        let rndAge () = 20 + (Probability.rndMinMax rndInt 0 15)
        let rndMajor () = sprintf "Major %d" (Probability.rndMinMax rndInt 1 10)

        /// Asserts that the students are evenly distributed across groups      
        let private shouldBeDistributedEvenly groupCount studentCount (population: Group list) =
            let groupLengths = population |> List.map (fun x -> List.length x.Students)                            
            let smallestAllowableGroupLength = studentCount / groupCount

            let smallest = groupLengths |> List.min
            smallest |> should be (greaterThanOrEqualTo smallestAllowableGroupLength)

        /// This reproduces Bug 210 - The grouping algorithm will create groups that don't have an even
        /// distribution of students (e.g. some with 6 students, others with 3).
        [<Fact>]
        let ``Grouping should work with odd number of students`` () =
            let mentorNames = seq { for i in 1..20 do yield (sprintf "Mentor%d" i) } |> Seq.toList

            let students = 
                seq { for i in 1..114 do 
                        yield 
                            { Name = {FirstName = (sprintf "First%d" i); LastName = (sprintf "Last%d" i)} 
                              Gender = rndGender()
                              Age = rndAge()
                              Major = rndMajor() } }
                |> Seq.toList

            let config = 
                { PopulationSize = 100
                  MaxEvolutions = 200
                  AcceptableScore = 100.0 }                
                            
            let groups = GeneticAlgorithm.createGrouping mentorNames students config
            groups |> should not' (be Empty)
            groups |> shouldBeDistributedEvenly mentorNames.Length students.Length




