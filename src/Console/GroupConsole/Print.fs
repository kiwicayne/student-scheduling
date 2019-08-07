module Print
open Student.Core.Domain

module Groups =

  open Student.Core.Groups.FitnessScore

  let private printGroupFitness group =
    let ageScore, genderScore, majorScore = getFitnessScoresForGroup group
    printfn "%.2f [%.2f, %.2f, %.2f]" (getTotalFitnessScoreForGroup group) ageScore genderScore majorScore |> ignore

  /// Display results in a friendly way
  let private printGroup group =
    printf "%-15s| " group.Mentor

    let printStudent (s: Student) = printf "%-20s %d %s| " (s.Name |> getFullName) s.Age (s.Major.Substring(0, 3))
    group.Students |> List.iter printStudent
    printGroupFitness group.Students

  let printGroups (grouping: Group list) =
    printfn "Fitness: %.2f" (getGroupingFitnessScore (grouping |> List.map (fun x -> x.Students)))
    grouping |> Seq.iter printGroup

  let printFitness (grouping: Group list) includeGroups =
    printfn "Fitness: %.2f" (getGroupingFitnessScore (grouping |> List.map (fun x -> x.Students)))
    if includeGroups then grouping |> Seq.iter (fun x -> printGroupFitness x.Students)