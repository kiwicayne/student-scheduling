// *******************************************************************************************************
// PROBLEM: Create sessions and assign students to them based on activity rules 
// *******************************************************************************************************
module Student.Core.Scheduling
open Utils

let private rndInt = Probability.randomIntGenerator
let private rndNext() = Probability.rndNext rndInt
let private rndMinMax = Probability.rndMinMax rndInt
let private rndMax = Probability.rndMax rndInt

module FitnessScore =
    open Enrollment
    open Utils.Statistics

    type private ActivityCreditCount =
        { Activity: Activity
          CreditCount: int }

    type private ActivityEnrollmentCount =
        { Activity: Activity 
          CapacityCount: int
          EnrollmentCounts: int list }
    
    /// Get the number of credits that are required for an activity.
    /// This represents the maximum number of sessions that *could* be required this block.
    ///
    /// However if the requirement is once per year for example, then it may have already been 
    /// satisfied, so isn't required this block.  This must still be added as a potiential requirement.
    /// Hence this returns the MAX requirements that could be required this block.
    let private activityCreditRequirement (activitySessions: ActivitySessions) =

        let creditCount =
            match getAttendanceRequirement activitySessions.Activity with
            | GroupRequirement r ->
                match r with
                | RequireEachPeerToPerformBothRolesPerBlock x -> (x |> int) * 2
            | HouseRequirement r ->
                match r with
                | RequireEachStudentToAttendEverySession -> activitySessions.Sessions.Length
                | RequireEachStudentToAttendOnceThisYear -> 1
            | NoRequirement -> 0
        
        { Activity = activitySessions.Activity
          CreditCount = creditCount }

    /// Determine the number of credits which are required for each activity
    let private activityCreditRequirements (activitySchedule: ActivitySessions list) =
        activitySchedule
        |> List.map activityCreditRequirement

    let studentEnrollmentCount sessions student = 
        sessions
        |> List.filter (fun session -> isStudentEnrolledInSession student session)
        |> List.length    

    /// The count of credits that will be satisfied for a student based on attendance record and schedule for this
    /// block.  If a student is enrolled in extra sessions over the absolute requirement, they do not count.
    let private activityCreditsSatisfied attendanceRecord student (activitySessions: ActivitySessions) =
        let activity = activitySessions.Activity

        let creditCount =
            match getAttendanceRequirement activity with
            | GroupRequirement r ->
                match r with
                | RequireEachPeerToPerformBothRolesPerBlock xTimes -> 
                    let x = (xTimes |> int)
                    let bedsideCount, peerCount = student |> studentEnrollmentCountPerPeerRole activitySessions.Sessions
                    let bedsideCredits = min bedsideCount x
                    let peerCredits = min peerCount x
                    bedsideCredits + peerCredits
            | HouseRequirement r ->
                match r with
                | RequireEachStudentToAttendEverySession -> 
                    activitySessions.Sessions
                    |> List.sumBy (fun session -> if isStudentEnrolledInSession student session then 1 else 0)
                | RequireEachStudentToAttendOnceThisYear -> 
                    // 1 credit if enrolled at least one in past block or this block
                    let alreadyAttended = student |> hasStudentAlreadyAttended attendanceRecord activity
                    let willAttendThisBlock = student |> isStudentEnrolledInAny activitySessions.Sessions
                    if alreadyAttended || willAttendThisBlock then 1
                    else 0               
            | NoRequirement -> 0 // No credits required        
                
        { Activity = activity
          CreditCount = creditCount }
                    
    let private studentCreditsSatisfied attendanceRecord activitySchedule student =        
        let satisfiedCreditCount = activityCreditsSatisfied attendanceRecord student
        activitySchedule
        |> List.map satisfiedCreditCount
    
    /// Calculate a fitness score out of 100 based on the student requirements and the 
    /// proposed schedule
    let calculateStudentsFitnessScore attendanceRecord house activitySchedule =
        let creditsRequired = activityCreditRequirements activitySchedule
        let creditsSatisfied = studentCreditsSatisfied attendanceRecord activitySchedule
        
        // for each student work out how many are satisfied in previous and current block
        let studentCreditsSatisfied =
            house
            |> List.collect (fun x -> x.Students)
            |> List.map (fun student -> student, creditsSatisfied student)
        
        // Students should complete as many credits as possible. 
        let creditsComplete creditRequirement creditsSatisfied = 
            if creditRequirement.CreditCount = 0 then 100.0
            else (creditsSatisfied.CreditCount |> float) / (creditRequirement.CreditCount |> float) * 100.0

        let avgCreditsCompletePerStudent = 
            studentCreditsSatisfied
            |> List.map 
                (fun (_, credits) -> 
                    let complete = List.map2 creditsComplete creditsRequired credits 
                    if complete.IsEmpty then 0.0
                    else complete |> List.average )
                    
        let avgCreditsComplete, stdDev = meanAndStdDev avgCreditsCompletePerStudent                   

        // Use the average credits complete per student less std deviation which 
        // will be 0 for perfect or large if the schedule contains lots of varience with 
        // some students completing a lot of credits and others completing none
        avgCreditsComplete - stdDev

    let rec private activityCapacity studentCount activity =
        match activity.EnrollmentCriteria with
        | FromGroup selectionCriteria ->
            match selectionCriteria with
            | SelectTwoPeers _ -> 2
        | FromHouse selectionCriteria ->
            match selectionCriteria with 
            | SelectMaxStudents (x, _) -> x
            | SelectAllStudents _ -> studentCount
        | OverflowFrom masterActivity -> activityCapacity studentCount masterActivity               

    let private activityCapicities studentCount activities =
        activities
        |> List.map (fun x -> x, activityCapacity studentCount x)
        
    /// Get the number of students enrolled in a session
    let private sessionEnrollmentCount session =
        match session.Enrollment with
        | StudentEnrollment _ -> 1
        | StudentsEnrollment students -> students.Count
        | PeerEnrollment _ -> 2
        | GroupsEnrollment grouping -> grouping |> List.sumBy (fun x -> x.Students.Length)
        | Empty -> 0

    let private sessionsEnrollmentCount sessions = sessions |> List.map sessionEnrollmentCount

    let private activityEnrollmentCount (activitySchedule: ActivitySessions list) = 
        activitySchedule 
        |> List.map (fun x -> x.Activity, sessionsEnrollmentCount x.Sessions)
        
    /// Get the enrollment capacity and count from the schedule for each activity
    let private activitiesEnrollment (house: House) (activitySchedule: ActivitySessions list) =
        let studentCount = getHouseStudentCount house
        let activities = activitySchedule |> List.map (fun x -> x.Activity)
        let capacity = activityCapicities studentCount activities
        let enrollment = activityEnrollmentCount activitySchedule
        let combineCapacityAndEnrollment (activity, capacityCount) (_, enrollmentCounts) = 
            { Activity = activity
              CapacityCount = capacityCount
              EnrollmentCounts = enrollmentCounts }
        List.map2 combineCapacityAndEnrollment capacity enrollment
        
    /// Calculate the percentage of capacity that an activity is enrolled.  
    /// Group overflow activities have a capacity, but we don't care about it
    let private activityEnrollmentScore activityEnrollment =
        activityEnrollment.EnrollmentCounts
        |> List.map 
            (fun enrollmentCount ->
                if enrollmentCount = 0 then 100.0 // Empty enrollment has the same score as completely full
                else (float enrollmentCount) / (float activityEnrollment.CapacityCount) * 100.0)

    let private activitiesEnrollmentFitnessScore house activitySchedule =
        let isOverflowActivity activityEnrollmentCount = isOverflowActivity activityEnrollmentCount.Activity
        
        let sessionEnrollment = 
            activitiesEnrollment house activitySchedule
            |> List.filter (not << isOverflowActivity)
            |> List.collect activityEnrollmentScore

        let enrollmentAvg, enrollmentStdDev = meanAndStdDev sessionEnrollment                   
         
        // Use the average enrollment percentage per session less std deviation which 
        // will be 0 for all sessions completely enrolled to capacity or large if the schedule 
        // contains lots of varience with some sessions empty and others completely full
        enrollmentAvg - enrollmentStdDev

    let private uniqueStudentsEnrolled (activitySessions: ActivitySessions) =
        activitySessions.Sessions
        |> List.map (fun x -> getEnrolledStudents x.Enrollment)
        |> List.fold Set.union Set.empty       
        |> Set.count

    /// Score based on the average percentage of the students in a house that are enrolled in each activity        
    /// This is used to help ensure that students are spread across activites instead of few students doing
    /// the same activity many times.
    let private studentDistributionScore house (activitySchedule: ActivitySessions list) =   
        let studentCount = getHouseStudentCount house |> float
        let studentPercentage n = (float n) / studentCount * 100.0

        let distinctStudentsInActivities = 
            activitySchedule
            |> List.map (uniqueStudentsEnrolled >> studentPercentage)

        let avg, stdDev = meanAndStdDev distinctStudentsInActivities                   
         
        // Use the average distinct students enrolled percentage per activity less the std deviation which 
        // will be 0 for all activities with every student in house enrolled or large if the schedule 
        // contains lots of varience with some activites having all students and others a few that repeat them.
        avg - stdDev

    let private studentActivityEnrollmentCount students (activitySessions: ActivitySessions) =        
        let enrollmentCount = studentEnrollmentCount activitySessions.Sessions

        students 
        |> List.map enrollmentCount

    /// Students should be enrolled evenly within activities
    /// We want an even distribution of the count that each student is enrolled
    /// in each activity
    let private activityDiversityScore house (activitySchedule: ActivitySessions list) =   
        let activityScore studentCounts = 
           // let mean, stdDev = meanAndStdDev studentCounts
            let min = List.min studentCounts
            let max = List.max studentCounts
            
            // Scale the results so that they are in a range of 0 to 1
            // if max = min, then all values are the same so scaled value is 1
            let scaled = studentCounts |> List.map (fun x -> if max > min then (x - min) / (max - min) else 1.0)
            
            scaled |> List.average |> (*) 100.0

        let students = getHouseStudents house
        let enrollmentCounts = studentActivityEnrollmentCount students
        let activityScores =
            activitySchedule
            |> List.map (enrollmentCounts >> List.map float >> activityScore)

        // The activity scores represent a "percentage of even distribution" so just use the average
        activityScores |> List.average

    let calculateActivitiesFitnessScore house activitySchedule =   
        let enrollmentScore = activitiesEnrollmentFitnessScore house activitySchedule
        let studentDistributionScore = studentDistributionScore house activitySchedule
        let activityDiversityScore = activityDiversityScore house activitySchedule

        (enrollmentScore + studentDistributionScore + activityDiversityScore) / 3.0

    let getScheduleFitnessScore (attendanceRecord: AttendanceRecord) (house: House) (blockSchedule: BlockSchedule) =
        let studentScore = calculateStudentsFitnessScore attendanceRecord house blockSchedule.ActivitySchedule
        let activityScore = calculateActivitiesFitnessScore house blockSchedule.ActivitySchedule        
        (studentScore + activityScore) / 2.0
                     
module SchedulingAlgorithms =
    open Enrollment 
    open Sessions

    let private getPeerEnrollmentForSession attendanceRecord allSessions scheduledSessionsThisActivity activity session =
        if session.Enrollment = Empty then 
            let overlappingSessions = getOverlappingSessions allSessions session
            let hasConflict = isStudentEnrolledInAny overlappingSessions
            let canEnroll = not << hasConflict

            // Work out which students still require each role in activity then filter out those that have a conflicting 
            // session for different activity already        
            let bedsideStudents = 
                getStudentsRequiringBedsideActivity attendanceRecord session.EnrollableStudents activity 
                    scheduledSessionsThisActivity
                |> Set.filter canEnroll
                |> Set.toList
                |> List.sortBy (fun _ -> rndNext())
                
            let peerStudents = 
                getStudentsRequiringPeerActivity attendanceRecord session.EnrollableStudents activity 
                    scheduledSessionsThisActivity
                |> Set.filter canEnroll
                |> Set.toList
                |> List.sortBy (fun _ -> rndNext())
                
            if bedsideStudents.Length > 0 && peerStudents.Length > 0 then 
                PeerEnrollment { BedsideStudent = bedsideStudents |> List.head 
                                 PeerStudent = peerStudents |> List.head }
            else 
                // If there aren't enough students to make up a pair, then don't schedule anyone
                // todo: if requirement changes that doing extra above the min required is ok, we can extend this
                // to select from the students with the lowest count of each activity
                Empty
        else
            session.Enrollment

    let private getEnrollmentForSessionWithMaxStudents attendanceRecord allSessions scheduledSessionsThisActivity activity session max =
        let studentsEnrolled = getEnrolledStudents session.Enrollment                                             
        if studentsEnrolled.Count < max then
            let overlappingSessions = getOverlappingSessions allSessions session
            let hasConflict = isStudentEnrolledInAny overlappingSessions
            let canEnroll = not << hasConflict
                        
            let isAlreadyEnrolled student = studentsEnrolled |> Set.contains student

            // Work out which students still require the activity then filter out those that have a conflicting 
            // session for different activity already        
            let newStudents = 
                getStudentsRequiringActivity attendanceRecord session.EnrollableStudents activity 
                    scheduledSessionsThisActivity
                |> Set.filter (not << isAlreadyEnrolled)
                |> Set.filter canEnroll
                |> Set.toList
                |> List.sortBy (fun _ -> rndNext())
                    
            let studentsToEnroll = 
                if newStudents.Length > 0 then
                    let newStudentsToEnrollCount = max - studentsEnrolled.Count
                    studentsEnrolled |> Set.union (newStudents |> List.truncate newStudentsToEnrollCount |> Set.ofList)
                else 
                    studentsEnrolled

            StudentsEnrollment studentsToEnroll
        else
            session.Enrollment

    /// Enroll as many randomly selected students as possible into an activity session.
    /// The activity session may be empty, partially filled or full.      
    let fillSessionWithRandomStudents attendanceRecord allSessions scheduledSessionsThisActivity activity session =
        let getPeerEnrollment = getPeerEnrollmentForSession attendanceRecord allSessions scheduledSessionsThisActivity activity
        let getMaxEnrollment = getEnrollmentForSessionWithMaxStudents attendanceRecord allSessions scheduledSessionsThisActivity activity

        let enrollment = 
            match activity.EnrollmentCriteria with            
            | FromGroup criteria ->  
                match criteria with
                | SelectTwoPeers _ -> getPeerEnrollment session                    
            | FromHouse criteria ->
                match criteria with
                | SelectMaxStudents (max, _) -> getMaxEnrollment session max                    
                | SelectAllStudents _ -> failwith "All student sessions cannot be part of the scheduling algorithm"
            | OverflowFrom _ -> failwith "Overflow sessions cannot be part of the scheduling algorithm"
        
        { session with Enrollment = enrollment }
    

    /// Enrolls students into activity sessions as randomly as possible while obeying enrollment priority
    /// Works by randomizing sessions, then for each session, randomize students and for each student attempt to enroll into session
    [<RequireQualifiedAccess>]
    module Random = 

        let private enrollStudentsIntoSession attendanceRecord (enrolledSessions: (Activity*Session) list) activitySession = 
            let activity, session = activitySession
            let allSessions = enrolledSessions |> List.map (fun (_,s) -> s)

            let scheduledSessionsThisActivity = 
                enrolledSessions                     
                |> List.filter (fun (a, _) -> a = activity)
                |> List.map (fun (_, s) -> s)

            let enrolledSession = fillSessionWithRandomStudents attendanceRecord allSessions scheduledSessionsThisActivity activity session
               
            (activity, enrolledSession) :: enrolledSessions
    
        /// Enroll students into the given activity sessions.  Sessions may be empty, partially enrolled or full.
        /// Only activities which are not mandatory or are not populated as overflow from other
        /// activities are supported.
        let enrollStudents (emptySessions: ActivitySessions list) (attendanceRecord: AttendanceRecord)  = 
            let emptyActivitySessions =
                emptySessions
                |> List.collect (fun x -> x.Sessions |> List.map (fun y -> (x.Activity, y)))
                |> List.sortBy (fun (activity, _) -> activity.EnrollmentPriority, rndNext())
        
            let enrollIntoSession = enrollStudentsIntoSession attendanceRecord
                                
            let sessions = 
                emptyActivitySessions
                |> List.fold enrollIntoSession []
            
            // Map from tuple list of activity*session back into a list of ActivitySession's        
            let getEnrolledSessions activity =
                sessions 
                |> List.where (fun (a,_) -> a = activity)
                |> List.sortByDescending (fun (_,s) -> s.Date, s.StartTime)       
                |> List.map (fun (_,s) -> s)
             
            emptySessions
            |> List.map (fun x -> { Activity = x.Activity; Sessions = (getEnrolledSessions x.Activity) })  

    /// Enrolls students into activity sessions obeying enrollment priority
    /// Works by ordering sessions by priority then when they occur, then for each session, randomize students and for each student attempt to enroll into session
    [<RequireQualifiedAccess>]
    module OrderedSessions = 

        let private enrollStudentsIntoSession attendanceRecord (enrolledSessions: (Activity*Session) list) activitySession = 
            let activity, session = activitySession
            let allSessions = enrolledSessions |> List.map (fun (_,s) -> s)

            let scheduledSessionsThisActivity = 
                enrolledSessions                     
                |> List.filter (fun (a, _) -> a = activity)
                |> List.map (fun (_, s) -> s)

            let enrolledSession = fillSessionWithRandomStudents attendanceRecord allSessions scheduledSessionsThisActivity activity session
               
            (activity, enrolledSession) :: enrolledSessions
    
        /// Enroll students into the given activity sessions.  Sessions may be empty, partially enrolled or full.
        /// Only activities which are not mandatory or are not populated as overflow from other
        /// activities are supported.
        let enrollStudents (emptySessions: ActivitySessions list) (attendanceRecord: AttendanceRecord)  = 
            let emptyActivitySessions =
                emptySessions
                |> List.collect (fun x -> x.Sessions |> List.map (fun y -> (x.Activity, y)))
                |> List.sortBy (fun (a, s) -> a.EnrollmentPriority, s.Date, s.StartTime)
        
            let enrollIntoSession = enrollStudentsIntoSession attendanceRecord
                                
            let sessions = 
                emptyActivitySessions
                |> List.fold enrollIntoSession []
            
            // Map from tuple list of activity*session back into a list of ActivitySession's        
            let getEnrolledSessions activity =
                sessions 
                |> List.where (fun (a,_) -> a = activity)
                |> List.sortByDescending (fun (_,s) -> s.Date, s.StartTime)       
                |> List.map (fun (_,s) -> s)
             
            emptySessions
            |> List.map (fun x -> { Activity = x.Activity; Sessions = (getEnrolledSessions x.Activity) })  

    /// A simple alogrithm which fills up activities, in a random order, one at a time
    [<RequireQualifiedAccess>]
    module Ordered = 
                     
        let private enrollStudentsIntoSession attendanceRecord (activitySchedule: ActivitySessions list) 
            (activitySessions: ActivitySessions) (scheduledSessionsThisActivity: Session list) session = 
            let activity = activitySessions.Activity
            let allSessions = getAllSessionsFromActivitySchedule activitySchedule

            let enrolledSession = fillSessionWithRandomStudents attendanceRecord allSessions scheduledSessionsThisActivity activity session
               
            enrolledSession :: scheduledSessionsThisActivity               
    
        let private enrollStudentsIntoActivity attendanceRecord (activitySchedule: ActivitySessions list) 
            (activitySessions: ActivitySessions) = 
            let enrollStudents = enrollStudentsIntoSession attendanceRecord activitySchedule activitySessions
        
            let sessions = 
                activitySessions.Sessions
                |> List.fold enrollStudents []
                |> List.rev
            { activitySessions with Sessions = sessions } :: activitySchedule
    
        /// Enroll students into the given empty activity sessions
        /// Only activities which are not mandatory or are not populated as overflow from other
        /// activities are supported.
        let enrollStudents emptySessions attendanceRecord = 
            let enrollStudentsIntoActivity = enrollStudentsIntoActivity attendanceRecord
                        
            emptySessions
            |> List.sortBy (fun x -> x.Activity.EnrollmentPriority, rndNext())
            |> List.fold enrollStudentsIntoActivity []
            |> List.rev
    
module Schedule =
    open Enrollment
   
    /// Enroll students into sessions for a block
    ///   block - The block definition which is used to create the activity sessions that students need to be enrolled in
    ///   house - The groups of students being enrolled into the same activities
    ///   attendanceRecord - The attendance for all students in the house in previous blocks this year for the same course
    ///   enrollStudents - The algorithm to use to create the enrollment schedule
    let createSchedule block house attendanceRecord (enrollStudents: ActivitySessions list -> AttendanceRecord -> ActivitySessions list) =         
        let emptyMandatorySessions, emptyUnorderedSessions, emptyOverflowSessions = Sessions.createSessions block

        let unorderedSchedule = enrollStudents emptyUnorderedSessions attendanceRecord
        
        let mandatorySchedule = 
            emptyMandatorySessions
            |> List.map (fun x -> enrollStudentsIntoMandatoryActivity house x)

        let overflowSchedule =
            emptyOverflowSessions
            |> List.map (fun x -> enrollStudentsIntoOverflowActivity unorderedSchedule x)
        
        { Block = block 
          ActivitySchedule = mandatorySchedule @ unorderedSchedule @ overflowSchedule }

    /// Split the activity sessions in a block schedule into the three types of activities
    let private splitSessions blockSchedule =
        let isMandatory activitySessions = activitySessions.Activity |> isMandatoryActivity
        let isOverflow activitySessions = activitySessions.Activity |> isOverflowActivity
        let isUnordered activitySessions = not <| ((activitySessions |> isMandatory) || (activitySessions |> isOverflow))

        let mandatorySessions = blockSchedule.ActivitySchedule |> List.filter isMandatory
        let overflowSessions = blockSchedule.ActivitySchedule |> List.filter isOverflow
        let unorderedSessions = blockSchedule.ActivitySchedule |> List.filter isUnordered

        (mandatorySessions, unorderedSessions, overflowSessions)

    /// Enroll students into sessions for an existing populated schedule    
    ///   attendanceRecord - The attendance for all students in the house in previous blocks this year for the same course    
    ///   enrollStudents - The algorithm to use to create the enrollment schedule
    ///   blockSchedule - The schedule which may contain some students already enrolled in sessions
    let fillSchedule house attendanceRecord (enrollStudents: ActivitySessions list -> AttendanceRecord -> ActivitySessions list) blockSchedule =         
        let mandatorySessions, unorderedSessions, overflowSessions = splitSessions blockSchedule

        let unorderedSchedule = enrollStudents unorderedSessions attendanceRecord
        
        // Add all students to mandatory sessions where enrollment is empty
        let mandatorySchedule = 
            mandatorySessions
            |> List.map (fun x -> enrollStudentsIntoMandatoryActivity house x)

        // Always update enrollment for overflow activities as the previous two types of activites can result in
        // these changing
        let overflowSchedule =
            overflowSessions
            |> List.map (fun x -> enrollStudentsIntoOverflowActivity unorderedSchedule x)
        
        { blockSchedule with ActivitySchedule = mandatorySchedule @ unorderedSchedule @ overflowSchedule }

    /// A alogrithm which fills up activities, in a random order, one at a time
    [<RequireQualifiedAccess>]
    module Genetic = 
        open Student.Core.Sessions

        type Chromosome = BlockSchedule
  
        type Fitness = double
  
        type Individual = Chromosome * Fitness

        /// Percentage of mutations in each evoluation
        let mutationPercentage = 1
  
        let elitePercentage = 10.0    
        
        let intialPopulationMutliple = 1  

        let private getFitness attendanceRecord house (chromosome: Chromosome) =             
            FitnessScore.getScheduleFitnessScore attendanceRecord house chromosome

        let private createIndividual attendanceRecord house blockSchedule =
            let fitness = getFitness attendanceRecord house blockSchedule
            (blockSchedule, fitness)

        let private createRandomIndividual block house attendanceRecord schedulingAlgorithm =             
            let chromosome = createSchedule block house attendanceRecord schedulingAlgorithm            
            createIndividual attendanceRecord house chromosome            

        let private initializePopulation populationSize block house attendanceRecord schedulingAlgorithm : Individual list =            
            Async.Parallel [ for _ in 1..(populationSize * intialPopulationMutliple) -> async { return createRandomIndividual block house attendanceRecord schedulingAlgorithm } ]
            |> Async.RunSynchronously
            |> Array.toList
            |> List.sortByDescending (fun (_, score) -> score)
            |> List.take populationSize

        let printBest (population: Individual list) = (snd population.Head) |> printf "%.2f "

        /// Determine if a mutation should occur
        let private shouldMutate () = rndMinMax 1 100 <= mutationPercentage
  
        /// Create a new schedule with the mutable session empty and group overflow schedules empty
        let private clearScheduleForMutation blockSchedule mutableSession =            
            let clearSessionForMutation activity session =
                let emptyIfMutant s = if s = mutableSession then emptySession s else s
                
                match activity.EnrollmentCriteria with
                | FromGroup selectionCriteria ->
                    match selectionCriteria with
                    | SelectTwoPeers _ -> emptyIfMutant session                
                | FromHouse selectionCriteria ->
                    match selectionCriteria with 
                    | SelectMaxStudents _ -> emptyIfMutant session                        
                    | SelectAllStudents _ -> session // Mandatory sessions can be mutated                       
                | OverflowFrom _ -> emptySession session // All overflow sessions must be cleared and repopulated after mutation               

            let clearActivitySessionsForMutation (activitySessions: ActivitySessions) =                    
                { activitySessions with Sessions = activitySessions.Sessions |> List.map (clearSessionForMutation activitySessions.Activity) }

            { blockSchedule with ActivitySchedule = blockSchedule.ActivitySchedule |> List.map clearActivitySessionsForMutation }

        /// A mutable session is one for any activity that isn't mandatory or a group overflow
        let rec private randomMutableSession (allSessions: (Activity*Session) list) =
            let i = rndInt 0 (allSessions.Length - 1)
            let activity, session = allSessions.[i]

            match activity.EnrollmentCriteria with
            | FromGroup selectionCriteria ->
                match selectionCriteria with
                | SelectTwoPeers _ -> session                
            | FromHouse selectionCriteria ->
                match selectionCriteria with 
                | SelectMaxStudents _ -> session                        
                | SelectAllStudents _ -> randomMutableSession allSessions // Mandatory sessions cannot be mutated
            | OverflowFrom _ -> randomMutableSession allSessions // Overflow sessions cannot be mutated  

        /// A mutation occurs by setting the enrollment of a random session to empty and then 
        /// attempting to fill up enrollment for all sessions again with random students.
        let private mutate createIndividual fillSchedule (individual: Individual) = 
            if shouldMutate() then 
                // select a random session                
                let chromosome, _ = individual
                let allSessions = getAllSessionsWithActivityFromActivitySchedule chromosome.ActivitySchedule                              
                let mutableSession = randomMutableSession allSessions                
                
                // Unenroll students from mutable session and any overflow sessions
                let clearedSchedule = clearScheduleForMutation chromosome mutableSession

                // Attempt to enroll students in all sessions
                let newSchedule = fillSchedule clearedSchedule 
                   
                createIndividual newSchedule
                         
            else individual

        /// Randomly select two parents from the top 50% of the population
        let rec private selectParents (population: Individual list) = 
            let top50percent = population.Length / 2
            match rndMax top50percent, rndMax top50percent with
            | mumIdx, dadIdx when mumIdx = dadIdx -> selectParents population
            | mumIdx, dadIdx -> 
              let mum = fst population.[mumIdx]
              let dad = fst population.[dadIdx]
              (mum, dad)
    
        let private getRandomSessions count activitySessions = 
            activitySessions
            |> List.sortBy (fun _ -> rndNext())
            |> List.take count
      
        let private sessionExistsIn activitySessions activitySession =
            let activity, session = activitySession
            activitySessions 
            |> List.exists (fun (a, s) -> a = activity && s = session)

        let private getStudentsEnrolledInTooManySessions getStudentsRequiringActivity allActivitySessions activitySession =
            let activity, session = activitySession
            
            let otherSessionsThisActivity =
                allActivitySessions
                |> List.filter (fun (a, s) -> a = activity && s <> session)
                |> List.map (fun (_, s) -> s)

            // The list of students who could be enrolled if this session didn't exist
            let enrollableStudents = getStudentsRequiringActivity session.EnrollableStudents activity otherSessionsThisActivity

            // The list of students who are enrolled but shouldn't be            
            getEnrolledStudents session.Enrollment
            |> Set.difference enrollableStudents
        
        let private getStudentsWithConflicts allActivitySessions activitySession =
            let _, session = activitySession
            let allSessions = allActivitySessions |> List.map (fun (_, s) -> s)

            let overlappingSessions = getOverlappingSessions allSessions session
            let hasConflict = isStudentEnrolledInAny overlappingSessions    
            
            // The list of students who are enrolled but shouldn't be            
            getEnrolledStudents session.Enrollment
            |> Set.filter hasConflict
            
        let removeStudents students enrollment =
            if students |> Set.isEmpty then
                enrollment                
            else
                match enrollment with
                | StudentEnrollment s -> 
                    if students |> Set.contains s then Empty
                    else enrollment
                | StudentsEnrollment sl ->
                    let remaining = sl |> Set.difference students
                    if remaining.IsEmpty then Empty
                    else if remaining.Count = 1 then StudentEnrollment (remaining |> Set.maxElement)
                    else StudentsEnrollment remaining
                | PeerEnrollment ps ->
                    if (students |> Set.contains ps.BedsideStudent) || (students |> Set.contains ps.PeerStudent) then Empty
                    else enrollment                
                | GroupsEnrollment _ -> 
                    failwith "removing students from groups enrollment not yet supported"
                | Empty -> Empty

        let private fixSessionEnrollment getStudentsRequiringActivity allActivitySessions activitySession =
            let studentsWithConflicts = getStudentsWithConflicts allActivitySessions activitySession
            let studentsEnrolledInTooManySessions = getStudentsEnrolledInTooManySessions getStudentsRequiringActivity allActivitySessions activitySession
            let studentsToRemove = studentsWithConflicts |> Set.union studentsEnrolledInTooManySessions
            if studentsToRemove.Count > 0 then 
                let activity, session = activitySession                
                (activity, { session with Enrollment = session.Enrollment |> removeStudents studentsToRemove }) :: allActivitySessions
            else 
                activitySession :: allActivitySessions

        let private createChild fillSchedule getStudentsRequiringActivity parent1Sessions parent2Sessions numMumSessions block =
            let mumSessions = getRandomSessions numMumSessions parent1Sessions
            let dadSessions = 
                parent2Sessions 
                |> List.filter (not << sessionExistsIn mumSessions)              
            
            let childSessions = 
                dadSessions
                |> List.fold (fixSessionEnrollment getStudentsRequiringActivity) mumSessions
                |> List.rev
                                   
            let activitySchedule =
                childSessions        
                |> List.groupBy fst
                |> List.map (fun (activity, sessions) -> { Activity = activity; Sessions = sessions |> List.map snd })
            
            // Attempt to enroll students in all sessions in case there were gaps created
            fillSchedule { Block = block; ActivitySchedule = activitySchedule }

        /// Create two children
        let private crossoverParents fillSchedule getStudentsRequiringActivity (mum: Chromosome) (dad: Chromosome): Chromosome*Chromosome = 
            // Create first child:
            // Select random number of sessions from mum
            // Select sessions from dad which aren't in mum selection
            //   Modify these sessions to make them valid by removing students who 
            //   have conflicts or are enrolled in an activity too many times
            // Attempt to fill up all sessions to complete the schedule

            // Create second child by swapping mum and dad              
            
            let parent1ActivitySessions = getAllSessionsWithActivityFromActivitySchedule mum.ActivitySchedule
            let parent2ActivitySessions = getAllSessionsWithActivityFromActivitySchedule dad.ActivitySchedule
            let block = mum.Block
            
            let numSessionsFromFirstParent = rndMinMax 1 parent1ActivitySessions.Length
            let child1 = createChild fillSchedule getStudentsRequiringActivity parent1ActivitySessions parent2ActivitySessions numSessionsFromFirstParent block
            let child2 = createChild fillSchedule getStudentsRequiringActivity parent2ActivitySessions parent1ActivitySessions numSessionsFromFirstParent block

            (child1, child2)

        let private crossover createIndividual fillSchedule getStudentsRequiringActivity (population: Individual list) =       
            let mum, dad = selectParents population
            let child1, child2 = crossoverParents fillSchedule getStudentsRequiringActivity mum dad
                       
            (createIndividual child1, createIndividual child2)

        let private getElite (population: Individual list) = 
            let size = max 1 (((population.Length |> double) * (elitePercentage / 100.0)) |> int)
            population
            |> List.take size         
        
        let private newGeneration createIndividual fillSchedule getStudentsRequiringActivity population = 
            let elite = getElite population
            let numCrossovers = (population.Length - elite.Length) / 2 // each crossover produces 2 children
            let mutate = mutate createIndividual fillSchedule
            let crossover = crossover createIndividual fillSchedule getStudentsRequiringActivity

            Async.Parallel [ for _ in 1..numCrossovers -> async { return crossover population } ]
            |> Async.RunSynchronously
            |> Array.toList
            |> List.collect (fun (child1, child2) -> [child1; child2])
            |> List.map mutate
            |> List.append elite // elite are not mutated
            |> List.sortByDescending (fun (_, f) -> f)   

        let private evolve createIndividual fillSchedule getStudentsRequiringActivity population maxEvolutions acceptableScore =
            let rec evolve (population: Individual list) n = 
              printBest population
              match (n, (snd population.Head)) with
              | (0, _) -> population.Head // Max evolutions attempted, best result so far will be returned
              | (_, score) when score >= acceptableScore -> population.Head
              | _ -> 
                let newPopulation = newGeneration createIndividual fillSchedule getStudentsRequiringActivity population
                evolve newPopulation (n - 1)
            
            evolve population maxEvolutions
        
        /// Enroll students into the given empty activity sessions
        /// Only activities which are not mandatory or are not populated as overflow from other
        /// activities are supported.
        let createSchedule config block house attendanceRecord =                                       
            let schedulingAlgorithm = SchedulingAlgorithms.Random.enrollStudents
            let fillSchedule = fillSchedule house attendanceRecord schedulingAlgorithm
            let createIndividual = createIndividual attendanceRecord house
            let initialPopulation = initializePopulation config.PopulationSize block house attendanceRecord schedulingAlgorithm
            let getStudentsRequiringActivity = getStudentsRequiringActivity attendanceRecord

            let result, _ = evolve createIndividual fillSchedule getStudentsRequiringActivity initialPopulation config.MaxEvolutions config.AcceptableScore
            result       