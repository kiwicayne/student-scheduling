module Student.Core.Groups

open System
open System.Linq
open Student.Core.Utils

module FitnessScore = 
    /// Fitness Algorithms
    /// There are 3 fitness measures, Gender, Age, Major.  Each check for diversity.  The more diverse, the
    /// better the score.  Each score can be calculated with a 0 base, where 0 is best, as this is easiest to 
    /// represent.  The Scaled score inverts this and makes it out of 100 where 100 is best and 0 worst which
    /// is easier to work with with a genetic algorithm and allows the 3 scores to easily be combined to create
    /// a score for a complete chromosome (all groups in a solution).
    /// 
    /// All fitness scores should be 0 if two students in a list have the same last name.  
    /// The gender fitness score should be as close to 0 as possible.
    /// Score is calculated by suming the students based on: male +1, female -1, notspecified 0
    /// A score of 0 is therefore an equal number of males and females, or completely unknowns
    let private getGenderFitnessScore student = 
        match (student.Gender) with
        | Male -> 1
        | Female -> -1
        | NotSpecified -> 0
    
    let private getGenderFitnessScoreForGroup (students: Student list) = 
        students
        |> Seq.sumBy getGenderFitnessScore
        |> Math.Abs
    
    /// This converts the gender score to be out of 100, where 100 is perfect and 0 is terrible.
    /// To get 100 there would need to be an equal number of males and females, to get a 0 all
    /// students would be either male or female.  Unscaled (score) best = 0, worst = # students in group (n).
    /// Invert using: i = n - score.  Out of 100 using: i / n * 100
    let private getScaledGenderFitnessScoreForGroup (students: Student list) = 
        let groupSize = students.Length |> double
        let score = getGenderFitnessScoreForGroup students |> double
        let inverted = groupSize - score
        inverted / groupSize * 100.0
    
    // Want the largest spread of ages in each group - don't want an old or young (outlier) to affect score
    // too much, so average age isn't perfect.
    // todo: Update to use varience
    // Simple Idea for now: same as for major below - but doesn't take into account the "spread" just that there
    // are many differences. e.g. 21, 22, 23 would rank the same as 21, 25, 40 which should be better.
    // NOTE: **When this is improved, you will need to change how the scaled fitness score is calculated also**
    let private getAgeFitnessScoreForGroup (students: Student list) = 
        let score = 
            query { 
                for s in students do
                    groupBy s.Age into g
                    select (g.Count())
            }
        
        let squareSum =
            score
            |> Seq.sumBy (fun x -> x * x)
        
        squareSum - students.Length
    
    /// This converts the age score to be out of 100, where 100 is perfect and 0 is terrible.
    /// To get 100 each student would be in a different age group, to get a 0 all
    /// students would be the same age.  Unscaled (score) best = 0, worst = # students in group ^ 2 (n^2).
    /// Invert using: i = n^2 - score.  Out of 100 using: i / n^2 * 100
    let private getScaledAgeFitnessScoreForGroup (students: Student list) = 
        let groupSize = students.Length |> double
        let worstScore = groupSize * groupSize
        let score = getAgeFitnessScoreForGroup students |> double
        let inverted = worstScore - score
        inverted / worstScore * 100.0
    
    // We want the largest number of majors in each group
    // The best we could hope for would be having one of each major for the student base
    // Simple idea: get distinct count for each major, sum the square of the counts (larger counts
    // have exponentially worse rank), then add together.  Finally subtract the student group size,
    // so that 0 represents each student have a different major.  The larger the number the worse the
    // score.
    let private getMajorFitnessScoreForGroup (students: Student list) = 
        let score = 
            query { 
                for s in students do
                groupBy s.Major into g
                select (g.Count())
            }

        let squareSum =
            score
            |> Seq.sumBy (fun x -> x * x)             
           
        squareSum - students.Length
    
    /// This converts the major score to be out of 100, where 100 is perfect and 0 is terrible.
    /// To get 100 each student would have a different major, to get a 0 all
    /// students would have the same major.  Unscaled (score) best = 0, worst = # students in group ^ 2 (n^2).
    /// Invert using: i = n^2 - score.  Out of 100 using: i / n^2 * 100
    let private getScaledMajorFitnessScoreForGroup (students: Student list) = 
        let groupSize = students.Length |> double
        let worstScore = groupSize * groupSize
        let score = getMajorFitnessScoreForGroup students |> double
        let inverted = worstScore - score
        inverted / worstScore * 100.0
    
    let listContainsStudentsWithSameLastName (students: Student list) = 
        let lastNames = 
            students
            |> List.map (fun x -> x.Name.LastName)
            |> List.distinct
        lastNames.Length <> students.Length
    
    /// Returns the fitness scores for gender, age, major in a student list.  
    /// If the list of students is invalid, returns 0 for each score.
    /// A list can be invalid because two students with the same last name exist in a list
    let getFitnessScoresForGroup (students: Student list) = 
        match listContainsStudentsWithSameLastName students with
        | true -> (0.0, 0.0, 0.0)
        | _ -> 
            (getScaledAgeFitnessScoreForGroup students, getScaledGenderFitnessScoreForGroup students, 
             getScaledMajorFitnessScoreForGroup students)
    
    let getTotalFitnessScoreForGroup (students: Student list) = 
        let s1, s2, s3 = getFitnessScoresForGroup students
        (s1 + s2 + s3) / 3.0
    
    let getGroupingFitnessScores (studentGroups: Student list list) = 
        let groupScores = studentGroups |> Seq.map getFitnessScoresForGroup
        
        let averageScore scores = 
            let ageTotal, genderTotal, majorTotal, count = 
                scores 
                |> Seq.fold (fun (a1, g1, m1, count) (a2, g2, m2) -> (a1 + a2, g1 + g2, m1 + m2, count + 1.0)) 
                       (0.0, 0.0, 0.0, 0.0)
            (ageTotal / count, genderTotal / count, majorTotal / count)
        averageScore groupScores
    
    let getGroupingFitnessScore (studentGroups: Student list list) = 
        let s1, s2, s3 = getGroupingFitnessScores studentGroups
        (s1 + s2 + s3) / 3.0
    
    let private getGroupFitnessScores (group: Group) = 
        let ageScore, genderScore, majorScore = getFitnessScoresForGroup group.Students
        { MentorId = group.Mentor
          TotalScore = getTotalFitnessScoreForGroup group.Students
          AgeScore = ageScore
          GenderScore = genderScore
          MajorScore = majorScore }
    
    let private getHouseFitnessScores (house: House) = 
        let studentGroups = house |> List.map (fun x -> x.Students)
        let ageScore, genderScore, majorScore = getGroupingFitnessScores studentGroups
        { TotalScore = getGroupingFitnessScore studentGroups
          AgeScore = ageScore
          GenderScore = genderScore
          MajorScore = majorScore
          GroupScores = house |> List.map getGroupFitnessScores }
    
    let getFitnessScores (houses: House list) = 
        let studentGroups = 
            houses
            |> List.concat
            |> List.map (fun x -> x.Students)
        
        let ageScore, genderScore, majorScore = getGroupingFitnessScores studentGroups
        { TotalScore = getGroupingFitnessScore studentGroups
          AgeScore = ageScore
          GenderScore = genderScore
          MajorScore = majorScore
          HouseScores = houses |> List.map getHouseFitnessScores }

/// *******************************************************************************************************
/// PROBLEM: Create a group for each mentor with even number of students with as much diversity as possible
/// *******************************************************************************************************
[<RequireQualifiedAccess>]
module SortAgorithm = 
    let createStudentGroups groupCount (students: Student list) = 
        let sortedStudents = 
            query { 
                for s in students do
                    sortBy s.Gender
                    thenBy s.Age
                    thenBy s.Major
            }
            |> Seq.toArray
        
        let studentGroups = Array.create groupCount []
        // todo: replace with a more idomatic solution
        let mutable i = 0
        for s in sortedStudents do
            studentGroups.[i] <- s :: studentGroups.[i]
            i <- i + 1
            if i >= groupCount then i <- 0
        studentGroups |> Array.toList
    
    /// Sort by age then gender then major then place into each group using round robin
    /// If for any age the genders are evenly distributed, and for each gender the majors are evening 
    /// distributed, this will come up with an optimal solution.  But
    /// doesn't do well if the diversity attributes aren't evenly distributed.
    /// - This doesn't enforce requirement for students in a group to have different last names
    let createGrouping mentors students = 
        let groupCount = List.length mentors
        let studentGroups = createStudentGroups groupCount students
        List.zip mentors studentGroups |> List.map createGroup

[<RequireQualifiedAccess>]
module RandomAlgorithm = 
    let createStudentGroups groupCount students = 
        students
        |> Random.listShuffle
        |> List.splitInto groupCount
    
    /// Create a completely random grouping  
    /// - This doesn't enforce requirement for students in a group to have different last names
    let createGrouping mentors students = 
        let groupCount = List.length mentors
        let studentGroups = createStudentGroups groupCount students
        List.zip mentors studentGroups |> List.map createGroup

[<RequireQualifiedAccess>]
module GeneticAlgorithm = 
    open FitnessScore
    
    type Chromosome = Student list list
    
    type Fitness = double
    
    type Individual = Chromosome * Fitness
    
    /// Percentage of mutations in each evoluation
    let mutationPercentage = 5
    /// percentage of top individuals to move to next generation unchanged
    let elitePercentage = 10.0

    let rndInt = Probability.randomIntGenerator
    let rndNext() = Probability.rndNext rndInt
    let rndMax = Probability.rndMax rndInt
    let rndMinMax = Probability.rndMinMax rndInt
    let private getFitness (chromosome: Chromosome) = getGroupingFitnessScore chromosome
    
    let private createRandomIndividual groupCount students = 
        let chromosome = RandomAlgorithm.createStudentGroups groupCount students
        let fitness = getFitness chromosome
        (chromosome, fitness)
    
    let private createIndividualFromSortAlgorithm groupCount students = 
        let chromosome = SortAgorithm.createStudentGroups groupCount students
        let fitness = getFitness chromosome
        (chromosome, fitness)
    
    /// Initialize the population with the solution of running the sort alogorithm and then
    /// as many random solutions as required.
    let private initializePopulation withSize groupCount students = 
        Array.init<Individual> withSize (fun x -> 
            if (x = 0) then createIndividualFromSortAlgorithm groupCount students
            else createRandomIndividual groupCount students)
        |> Array.sortByDescending (fun (_, f) -> f)
    
    let private getElite (population: Individual []) = 
        let size = ((population.Length |> double) * (elitePercentage / 100.0)) |> int
        population.[..size]
    
    let private getRandomGroups numGroups chromosome = 
        chromosome
        |> List.sortBy (fun _ -> rndNext())
        |> List.take numGroups                       

    let private crossoverParents (allStudents: Student Set) (mum: Chromosome) (dad: Chromosome): Chromosome = 
        // Select random number of groups from Mum
        // Select groups at random from Dad to make up remaining group number required
        //   Modify these groups to make them valid by replacing students already in Mum groups with
        //   students not in either Mum or Dad selections
        let numMumGroups = rndMinMax 1 mum.Length
        let mumGroups = getRandomGroups numMumGroups mum
        let dadGroups = getRandomGroups (mum.Length - numMumGroups) dad
        let mumGroupStudentSet = set (mumGroups |> Seq.concat)
        let dadGroupStudentSet = set (dadGroups |> Seq.concat)
        
        let unassignedStudents = 
            Set.difference (Set.difference allStudents mumGroupStudentSet) dadGroupStudentSet
            |> Set.toList
            |> List.sortBy (fun _ -> rndNext())
        
        let isValidStudent (student: Student) = (mumGroupStudentSet.Contains student) |> not
        
        // remove students from Dad groups which are in Mum groups
        let dadGroupsWithoutMumStudents = 
            dadGroups
            |> List.map (fun x -> x |> List.filter isValidStudent)
        
        // Add any students not assigned to the Dad groups, distributing them in a way to make the group
        // sizes as even as possible.
        // It is possible that some of the Dad groups will end up with no enough students.  This can happen when
        // there are an odd number of students and the crossover selection chooses large Mum groups which result
        // in too many groups with the max number of students to allow the smaller groups to enough students in them.
        // So if any group has 2 or more fewer students than the largest groups, rebalance the groups.  This only
        // needs to be done within the Dad groups, Mum groups will not be modified.
        let validDadGroups = 
            unassignedStudents
            |> List.distributeEvenly dadGroupsWithoutMumStudents
            |> List.rebalance
                    
        mumGroups @ validDadGroups
   
    let rec private selectParents (population: Individual []) = 
        let top50percent = population.Length / 2
        match rndMax top50percent, rndMax top50percent with
        | mumIdx, dadIdx when mumIdx = dadIdx -> selectParents population
        | mumIdx, dadIdx -> 
            let mum = fst population.[mumIdx]
            let dad = fst population.[dadIdx]
            (mum, dad)     

    let private crossover (allStudents: Student Set) (population: Individual []) = 
        let mum, dad = selectParents population
        let child = crossoverParents allStudents mum dad      
        (child, getFitness child)
    
    let rec private getRandomGroupIndexExcept excludeIndex (chromosome: Chromosome) = 
        match rndMax (chromosome.Length - 1) with
        | x when x = excludeIndex -> getRandomGroupIndexExcept excludeIndex chromosome
        | x -> x
    
    let private getRandomGroupIndex (chromosome: Chromosome) = getRandomGroupIndexExcept -1 chromosome
    
    let private selectRandomStudentPair studentLists = 
        let group1Index = getRandomGroupIndex studentLists
        let group2Index = getRandomGroupIndexExcept group1Index studentLists
        let group1 = studentLists.[group1Index]
        let group2 = studentLists.[group2Index]
        let student1 = group1.[rndMax (group1.Length - 1)]
        let student2 = group2.[rndMax (group2.Length - 1)]
        (student1, student2)
    
    /// Determine if a mutation should occur
    let private shouldMutate() = rndMinMax 1 100 <= mutationPercentage
    
    /// A mutation occurs by swaping the group two random students belong to    
    let private mutate (individual: Individual) = 
        if shouldMutate() then 
            let chromosome, _ = individual
            let studentPair = selectRandomStudentPair chromosome
            
            let createMutantChromosome studentPair (studentLists: Student list list) = 
                let swapStudents studentPair students = 
                    let student1, student2 = studentPair
                    students |> List.map (function 
                                    | s when s = student1 -> student2
                                    | s when s = student2 -> student1
                                    | s -> s)
                
                let swapStudentsInGroups studentPair (studentLists: Student list list) = 
                    let swapWithStudents = swapStudents studentPair
                    studentLists |> List.map swapWithStudents
                
                let newChromosome = swapStudentsInGroups studentPair studentLists
                (newChromosome, getFitness newChromosome)

            createMutantChromosome studentPair chromosome

        else individual
    
    let private newGeneration (allStudents: Student Set) population = 
        let elite = getElite population
        let withSize = population.Length - elite.Length
        Async.Parallel [ for _ in 1..withSize -> async { return crossover allStudents population } ]
        |> Async.RunSynchronously
        |> Array.map mutate
        |> Array.append elite // elite are not mutated
        |> Array.sortByDescending (fun (_, f) -> f)
    
    let printBest (population: Individual []) = (snd population.[0]) |> printf "%.2f "
        
    let private evolve (allStudents: Student Set) population maxEvolutions acceptableScore =         
        let rec evolve (population: Individual []) n = 
            printBest population
            match (n, (snd population.[0])) with
            | (0, _) -> population.[0] // Max evolutions attempted, best result so far will be returned
            | (_, score) when score >= acceptableScore -> population.[0]
            | _ -> 
                let newPopulation = newGeneration allStudents population                              
                evolve newPopulation (n - 1)

        evolve population maxEvolutions
    
    let createGrouping mentors students config = 
        let population = initializePopulation config.PopulationSize (List.length mentors) students
        let allStudents = set students
        let bestSolution, _ = evolve allStudents population config.MaxEvolutions config.AcceptableScore
        bestSolution
        |> List.zip mentors
        |> List.map (fun (mentor, students) -> 
               { Mentor = mentor
                 Students = students })
