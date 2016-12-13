module Student.Core.Enrollment 
        
open Student.Core.Sessions

/// Get the list of enrolled students from an enrollment
let getEnrolledStudents = function
    | StudentEnrollment s -> Set.singleton s
    | StudentsEnrollment sl -> sl
    | PeerEnrollment ps -> Set.empty.Add(ps.BedsideStudent).Add(ps.PeerStudent)
    | GroupsEnrollment gl -> gl |> List.collect (fun x -> x.Students) |> Set.ofList
    | Empty -> Set.empty

let isStudentEnrolledInSession student session = 
    match session.Enrollment with
    | StudentEnrollment s -> s = student
    | StudentsEnrollment sl -> sl |> Set.contains student
    | PeerEnrollment ps -> ps.BedsideStudent = student || ps.PeerStudent = student
    | GroupsEnrollment gl -> gl |> List.exists (fun g -> g.Students |> List.contains student)
    | Empty -> false

/// Determine if a student has an activity session already scheduled that overlaps the one given  
///   allSessions - Previous sessions which have already been scheduled in this block
///   session - The current session for the activity being scheduled
///   student - The student being scheduled
let doesStudentHaveSessionConflict (allSessions: Session list) session student = 
    let isEnrolledInOther otherSession = isStudentEnrolledInSession student otherSession && otherSession <> session
              
    session 
    |> getOverlappingSessions allSessions
    |> List.exists isEnrolledInOther   

let hasStudentAlreadyAttended (attendanceRecord: AttendanceRecord) (activity: Activity) student = 
    let hasActivityBeenAttended sa = sa.Sessions |> List.exists (fun (a, _) -> a.Name = activity.Name)
    attendanceRecord |> List.exists (fun sa -> sa.Student = student && hasActivityBeenAttended sa)

let isStudentEnrolledInAny (sessions: Session list) student = 
    let isStudentEnrolled = isStudentEnrolledInSession student
    sessions |> List.exists isStudentEnrolled
    
let studentEnrollmentCount (sessions: Session list) student = 
    let isStudentEnrolled = isStudentEnrolledInSession student
    sessions
    |> List.where isStudentEnrolled
    |> List.length
    
/// Some activities have PeeredStudents which contain the roles bedside student and peer student.
/// For these activites count the number of times that the student is in each role.  
let studentEnrollmentCountPerPeerRole (peerSessions: Session list) student = 
    let getRoleCounts peerSession = 
        match peerSession.Enrollment with
        | PeerEnrollment ps -> 
            let bedsideCount = 
                if ps.BedsideStudent = student then 1
                else 0
                
            let peerCount = 
                if ps.PeerStudent = student then 1
                else 0
                
            (bedsideCount, peerCount)
        | Empty -> (0, 0)
        | _ -> failwith "Only mutlirole activities can be counted per role"
    peerSessions
    |> List.map getRoleCounts
    |> List.fold (fun (b1, p1) (b2, p2) -> (b1 + b2, p1 + p2)) (0, 0)
    
        
/// Return all students which need to be enrolled in an activity
///   attendanceRecord - Passed completed activities for course from previous blocks this year
///   enrollableStudents - all students who can be enrolled in the activity
///   scheduledActivitiesThisBlock - Activities in this block for which scheduling has been completed already
///   activity - The current activity being scheduled
let getStudentsRequiringActivity attendanceRecord enrollableStudents activity scheduledSessionsThisActivity = 
    let isAlreadyEnrolledThisBlock = isStudentEnrolledInAny scheduledSessionsThisActivity
    let hasAlreadAttendedInPreviousBlock = hasStudentAlreadyAttended attendanceRecord activity
        
    let studentCanBeEnrolled student =         
        match getAttendanceRequirement activity with
        | GroupRequirement r ->
            match r with
            | RequireEachPeerToPerformBothRolesPerBlock xTimes -> 
                let x = (xTimes |> int)                    
                let bedsideCount, peerCount = 
                    studentEnrollmentCountPerPeerRole scheduledSessionsThisActivity student
                bedsideCount < x || peerCount < x
        | HouseRequirement r ->
            match r with
            | RequireEachStudentToAttendEverySession -> true                    
            | RequireEachStudentToAttendOnceThisYear -> 
                not <| (isAlreadyEnrolledThisBlock student || hasAlreadAttendedInPreviousBlock student)
        | NoRequirement -> true        
                    
    enrollableStudents
    |> Set.filter studentCanBeEnrolled    
    
/// Return all students which need to be enrolled in a bedside activity
///   attendanceRecord - Passed completed activities for course from previous blocks this year
///   enrollableStudents - all students who can be enrolled in the activity
///   scheduledActivitiesThisBlock - Activities in this block for which scheduling has been completed already
///   activity - The current activity being scheduled
let getStudentsRequiringBedsideActivity attendanceRecord enrollableStudents activity scheduledSessionsThisActivity = 
    let studentCanBeEnrolled student =
        match getAttendanceRequirement activity with
        | GroupRequirement r  -> 
            match r with            
            | RequireEachPeerToPerformBothRolesPerBlock xTimes -> 
                let x = xTimes |> int
                let bedsideCount, _ = studentEnrollmentCountPerPeerRole scheduledSessionsThisActivity student
                bedsideCount < x
        | HouseRequirement _ -> false
        | NoRequirement -> false    
            
    if activity |> isPeerActivity then 
        enrollableStudents
        |> Set.filter studentCanBeEnrolled        
    else 
        Set.empty
    
/// Return all students which need to be enrolled in a peer activity
///   attendanceRecord - Passed completed activities for course from previous blocks this year
///   enrollableStudents - all students who can be enrolled in the activity
///   scheduledActivitiesThisBlock - Activities in this block for which scheduling has been completed already
///   activity - The current activity being scheduled
let getStudentsRequiringPeerActivity attendanceRecord enrollableStudents activity scheduledSessionsThisActivity = 
    let studentCanBeEnrolled student = 
        match getAttendanceRequirement activity with
        | GroupRequirement r  -> 
            match r with            
            | RequireEachPeerToPerformBothRolesPerBlock xTimes -> 
                let x = xTimes |> int
                let _, peerCount = studentEnrollmentCountPerPeerRole scheduledSessionsThisActivity student
                peerCount < x
        | HouseRequirement _ -> false
        | NoRequirement -> false                                  

    if activity |> isPeerActivity then 
        enrollableStudents
        |> Set.filter studentCanBeEnrolled        
    else 
        Set.empty

/// Enroll all students into the mandatory activity sessions with empty enrollment.  
/// If the enrollment is already set then do nothing.
let enrollStudentsIntoMandatoryActivity house (activitySessions: ActivitySessions) = 
    if not <| (activitySessions.Activity |> isMandatoryActivity) then 
        failwith "Only mandatory sessions are supported" 

    let enrolledSessions = 
        activitySessions.Sessions
        |> List.map (fun x -> if x.Enrollment = Empty then { x with Enrollment = GroupsEnrollment house } else x )            

    { activitySessions with Sessions = enrolledSessions }

let private getSessionsForActivity activitySchedule activity =        
    activitySchedule
    |> List.tryFind (fun x -> x.Activity = activity)
    |> function 
    | Some s -> s.Sessions
    | None -> []
              
/// Get the first session in the session list that occurs at the same time as session
let private getMatchingSession session sessions =
    sessions
    |> List.tryFind (fun x -> x.Date = session.Date && x.StartTime = session.StartTime && x.EndTime = x.EndTime)    

let private getEnrollmentForOverflowActivitySession populatedSchedule masterActivity session =
    let allSessions = getAllSessionsFromActivitySchedule populatedSchedule
    
    // Select all students not enrolled in the matching parent activity for this session   
    let masterSession = 
        getSessionsForActivity populatedSchedule masterActivity
        |> getMatchingSession session
        |> function 
        | Some s -> s
        | None -> failwith "An overflow session could not be filled as it doesn't have a matching master session"
    
    // which sessions overlap with this one, calculate once then reuse to improve performance
    let overlappingSessions = getOverlappingSessions allSessions session
    let hasConflict = isStudentEnrolledInAny overlappingSessions
    let canEnroll = not << hasConflict

    let students = 
        session.EnrollableStudents
        |> Set.filter (fun s -> (not <| isStudentEnrolledInSession s masterSession) && canEnroll s)
         
    StudentsEnrollment students

/// Enroll students into sessions for an overflow activity.  Even if the enrollment for a session is already set, 
/// it will be recalculated.
///   populatedShedule - The schedule which will be used to determine which students can be assigned to the overflow sessions
///   activitySessions - The list of empty sessions for the overflow activity
let enrollStudentsIntoOverflowActivity populatedSchedule (activitySessions: ActivitySessions) = 
    match activitySessions.Activity.EnrollmentCriteria with
    | OverflowFrom masterActivity ->
        let enrolledSessions = 
            activitySessions.Sessions
            |> List.map (fun x -> { x with Enrollment = getEnrollmentForOverflowActivitySession populatedSchedule masterActivity x } )            

        { activitySessions with Sessions = enrolledSessions }

    | _ -> failwith "Only sessions with enrollment populated as overflow from a master activity are supported"              

