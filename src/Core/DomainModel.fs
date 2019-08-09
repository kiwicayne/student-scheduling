[<AutoOpen>]
module Student.Core.Domain

open System

type Gender = 
    | Male
    | Female
    | NotSpecified

type Name = 
    { FirstName: string
      LastName: string }

type Student = 
    { Name: Name
      Gender: Gender
      Age: int
      Major: string }   

/// A group of students and their mentor
type Group = 
    { Students: Student list
      Mentor: string }

/// A list of groups which make up a societies house
type House = Group list

/// Two Students who will be involved in a bedside activity
/// The Bedside student will do an H&P (History and Physical)
/// The Peer student will observe the Bedside student and complete the H&P checklist
type PeeredStudents = 
    { BedsideStudent: Student 
      PeerStudent: Student }

/// How often an event occurs
type Frequency = 
    | Once of DateTime * TimeSpan * TimeSpan
    | SpecificTimes of (DateTime * TimeSpan * TimeSpan) list
    | Weekly of (TimeSpan * TimeSpan) list

/// The Enrollment of students within an activity.
type Enrollment = 
    | StudentEnrollment of Student
    | StudentsEnrollment of Set<Student>
    | PeerEnrollment of PeeredStudents
    | GroupsEnrollment of Group list
    | Empty

type EnrollmentPriority = 
    | Highest
    | High
    | Neutral
    | Low
    | Lowest

[<Measure>] type Times

type TwoPeersAttendanceRequirement =
    | RequireEachPeerToPerformBothRolesPerBlock of int<Times>
  
type HouseAttendanceRequirement =
    | RequireEachStudentToAttendEverySession
    | RequireEachStudentToAttendOnceThisYear  
  
and GroupEnrollmentSelection =
    | SelectTwoPeers of TwoPeersAttendanceRequirement

and HouseEnrollmentSelection =
    | SelectMaxStudents of int * HouseAttendanceRequirement
    | SelectAllStudents of HouseAttendanceRequirement 

and EnrollmentCriteria =
    | FromGroup of GroupEnrollmentSelection
    | FromHouse of HouseEnrollmentSelection  
    | OverflowFrom of Activity

/// An activity that can occur within a course (block).  
/// e.g. A lab, exam, lecture, etc
and [<CustomEquality; NoComparison>] Activity = 
    { Name: string
      Frequency: Frequency    
      EnrollmentPriority: EnrollmentPriority
      EnrollmentCriteria: EnrollmentCriteria }
    
    /// Two activities are considered the same if they have the same name
    /// This significanty improves performance
    override x.Equals(y) =
        match y with
        | :? Activity as y -> (x.Name = y.Name)
        | _ -> false
 
    override x.GetHashCode() = hash x.Name
 
/// A conveniance type to allow representing any type of attenance requirement in one place
/// Useful to reduce match pattern matching and simplifying code
type AttendanceRequirement =
    | GroupRequirement of TwoPeersAttendanceRequirement
    | HouseRequirement of HouseAttendanceRequirement
    | NoRequirement

/// An instance of an activity at a specific time
[<CustomEquality; NoComparison>] // todo: add comparison based on date/time
type Session = 
    { SessionId: string // An id to make it efficient and easy to compare two sessions while also being readable to a human
      Date: DateTime
      StartTime: TimeSpan
      EndTime: TimeSpan
      EnrollableStudents: Set<Student>
      Enrollment: Enrollment }
  
    /// Two sessions are considered the same if they have the same id
    /// This significanty improves performance
    override x.Equals(y) =
        match y with
        | :? Session as y -> (x.SessionId = y.SessionId)
        | _ -> false
 
    override x.GetHashCode() = hash x.SessionId

/// The sessions for an activity
type ActivitySessions =
    { Activity: Activity
      Sessions: Session list }
    
/// A course that students enroll in
type Course = 
    { Name: string }

/// An instance of a course for a specific period
type Block = 
    { Course: Course
      Name: string
      StartDate: DateTime
      EndDate: DateTime
      House: House
      Activities: Activity list }

/// The schedule for a block
type BlockSchedule =
    { Block: Block
      ActivitySchedule: ActivitySessions list }

/// The sessions that a student has attended
type StudentAttendance =
    { Student: Student
      Sessions: (Activity * Session) list } // todo: should this use ActivitySessions instead?

/// Record of attendance of activities by all students
type AttendanceRecord = StudentAttendance list

/// Configuration for a genetic algorithm
type GeneticConfig =
    { PopulationSize: int
      MaxEvolutions: int
      AcceptableScore: double }

/// Fitness score for a group
type GroupScore =
    { MentorId: string
      TotalScore: double
      AgeScore: double
      GenderScore: double
      MajorScore: double }

/// Fitness score for a house
type HouseScore =
    { TotalScore: double
      AgeScore: double
      GenderScore: double
      MajorScore: double
      GroupScores: GroupScore list }

/// The fitness score for a list of houses which make up a student grouping algorithm result
type HouseScores =
    { TotalScore: double
      AgeScore: double
      GenderScore: double
      MajorScore: double
      HouseScores: HouseScore list }

/// ************************
/// Domain Helpers
/// ************************

/// Create a group from a mentor and list of students
let createGroup t = 
    let (mentor, studentList) = t
    { Mentor = mentor
      Students = studentList }

let getFullName name = name.FirstName + " " + name.LastName

let isOverflowActivity activity = 
    match activity.EnrollmentCriteria with
    | OverflowFrom _ -> true
    | _ -> false

let isMandatoryActivity activity = 
    match activity.EnrollmentCriteria with
    | FromHouse hc -> 
        match hc with
        | SelectAllStudents _ -> true
        | _ -> false
    | _ -> false

let isPeerActivity activity =
    match activity.EnrollmentCriteria with
    | FromGroup gc ->
        match gc with 
        | SelectTwoPeers _ -> true
    | _ -> false

let getOverflowFromActivity activity = 
    match activity.EnrollmentCriteria with
    | OverflowFrom a -> Some a
    | _ -> option.None

let getAllSessionsFromActivitySchedule (activitySchedule: ActivitySessions list) = 
    activitySchedule
    |> List.collect (fun x -> x.Sessions)

let getAllSessionsWithActivityFromActivitySchedule (activitySchedule: ActivitySessions list) = 
    activitySchedule
    |> List.collect (fun x -> x.Sessions |> List.map (fun y -> x.Activity, y))

let getHouseStudents house = house |> List.collect (fun x -> x.Students)
let getHouseStudentCount house = house |> List.sumBy (fun x -> x.Students.Length)

let emptySession session = { session with Enrollment = Empty }

/// Create a session with no students enrolled
let createEmptySession enrollableStudents (date: DateTime, startTime: TimeSpan, endTime: TimeSpan) = 
    let id = date.ToString("yyyy-MM-dd") + "|" + startTime.ToString("hh\:mm") + "|" + endTime.ToString("hh\:mm") + "|" + enrollableStudents.GetHashCode().ToString()

    { SessionId = id
      Date = date
      StartTime = startTime
      EndTime = endTime
      EnrollableStudents = enrollableStudents
      Enrollment = Empty }

/// Get the attendance requirment from group enrollment selection
let private getGroupAttendanceRequirement = function
    | SelectTwoPeers requirement -> requirement
            
/// Get the attendance requirment from house enrollment selection
let private getHouseAttendanceRequirement = function
    | SelectMaxStudents (_, requirement) -> requirement
    | SelectAllStudents requirement -> requirement

/// What attendance requirement, if any, does the activity have
let getAttendanceRequirement activity = 
    match activity.EnrollmentCriteria with
    | FromGroup selection -> GroupRequirement (getGroupAttendanceRequirement selection)
    | FromHouse selection -> HouseRequirement (getHouseAttendanceRequirement selection)
    | OverflowFrom _ -> NoRequirement