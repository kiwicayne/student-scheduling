/// Create valid session list for a block
/// Note: Does not support holiday blackout dates at this time 
module Student.Core.Sessions
open System

/// If the start and end times are the same, it isn't considered an overlap  
let private sessionsOverlap s1 s2 = s1.Date = s2.Date && s1.StartTime < s2.EndTime && s1.EndTime > s2.StartTime
    
/// Get all sessions that overlap with the session
let getOverlappingSessions allSessions session = 
    let overlapsWithSession s = sessionsOverlap s session              
    allSessions        
    |> List.filter (fun x -> x <> session && overlapsWithSession x)

/// Create a session with no students enrolled for the time period weekly.  
/// The first date is the start date of the block
let private createEmptyWeeklySessionForPeriod block enrollableStudents timePeriod = 
    let startTime, endTime = timePeriod
        
    let rec weeklyDates (fromDate: DateTime) = 
        seq { 
            yield fromDate
            yield! weeklyDates (fromDate.AddDays 7.0)
        }
    let dates = 
        weeklyDates block.StartDate
        |> Seq.takeWhile (fun currentDate -> currentDate <= block.EndDate)
        |> Seq.toList

    dates
    |> List.map (fun date -> createEmptySession enrollableStudents (date, startTime, endTime))
    
    
/// Create a session with no students enrolled for each time period weekly.  
let private createEmptyWeeklySessions block enrollableStudents times = 
    times
    |> List.collect (createEmptyWeeklySessionForPeriod block enrollableStudents)
    
/// Create empty sessions based on frequency of activity
let private createEmptySessions block enrollableStudents activity = 
    match activity.Frequency with
    | Once(date, startTime, endTime) -> [ createEmptySession enrollableStudents (date, startTime, endTime) ]
    | SpecificTimes times -> times |> List.map (createEmptySession enrollableStudents)
    | Weekly times -> createEmptyWeeklySessions block enrollableStudents times
    
/// Create sessions for all activites.
///
/// Note: this will create sessions for activities, even if they conflict with each other.
///       Any such "invalid" sessions (e.g. a normal activity at the same time as a mandatory activity)
///       will need to be filtered out.
let rec private createDefaultSessions block activity = 

    match activity.EnrollmentCriteria with
    | FromGroup _ ->         
        // create sessions for each group
        let sessions = 
            block.House
            |> List.collect
                (fun group -> 
                    let enrollableStudents = Set.ofList group.Students
                    createEmptySessions block enrollableStudents activity )
            |> List.sortBy (fun x -> x.Date, x.StartTime, x.EndTime)
      
        { Activity = activity
          Sessions = sessions } 

    | FromHouse _ -> 
        let enrollableStudents = Set.ofList (block.House |> getHouseStudents)
        { Activity = activity
          Sessions = createEmptySessions block enrollableStudents activity }

    | OverflowFrom masterActivity -> 
        if masterActivity.Frequency <> activity.Frequency then 
            failwith (sprintf "Overflow activity '%s' and master activity '%s' must have the same frequency" activity.Name masterActivity.Name)

        // Overflow activities have the same sessions and enrollable students as the master activity
        let masterActivitySessions = createDefaultSessions block masterActivity
        { Activity = activity
          Sessions = masterActivitySessions.Sessions }
                
let private filterConflictingSessions mandatorySessions sessions = 
    let hasConflict session = 
        let overlapWithSession = sessionsOverlap session
        mandatorySessions |> List.exists overlapWithSession
    sessions |> List.filter (not << hasConflict)
        
let private removeConflictingSessions mandatorySessions activitySession = 
    if activitySession.Activity |> isMandatoryActivity then
        activitySession // Mandatory activity so keep all sessions
    else
        { activitySession with Sessions = activitySession.Sessions |> filterConflictingSessions mandatorySessions }    

/// Create sessions for a block. 
/// Sessions will have an empty enrollment.
let createSessions block = 
    // Create sessions for activities without regard for overlapping/conflicting sessions between activities
    let createActivitySessions = createDefaultSessions block    
    let activitiesWithDefaultSessions = block.Activities |> List.map createActivitySessions
            
    // Now filter out sessions that conflict with mandatory sessions.
    // If a session is mandatory, the students can't do any other activities at the same time
    let mandatorySessions = 
        activitiesWithDefaultSessions
        |> List.filter (fun x -> x.Activity |> isMandatoryActivity)
        |> List.collect (fun x -> x.Sessions)
                
    let validActivitySessions = 
        activitiesWithDefaultSessions
        |> List.map (removeConflictingSessions mandatorySessions)

    let areMandatorySessions activitySessions = activitySessions.Activity |> isMandatoryActivity
    let mandatoryActivitySessions = validActivitySessions |> List.filter areMandatorySessions
    let overflowActivitySessions = validActivitySessions |> List.filter (fun x -> isOverflowActivity x.Activity)
        
    let unorderedActivitySessions = 
        validActivitySessions
        |> List.filter 
                (fun x -> 
                (not <| (mandatoryActivitySessions |> List.contains x)) 
                && (not <| (overflowActivitySessions |> List.contains x)))            
        
    mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions

