namespace Student.Core.Tests

open Xunit
open FsUnit.Xunit
open Student.Core.Domain
open Student.Core.Sessions
open System

module SessionsTests = 
    // Helpers
    let ``12pm`` = TimeSpan(12, 0, 0)
    let ``1pm`` = TimeSpan(13, 0, 0)
    let ``3pm`` = TimeSpan(15, 0, 0)
    let ``5pm`` = TimeSpan(17, 0, 0)
    let ``6pm`` = TimeSpan(18, 0, 0)

    let ``2015-9-29`` = DateTime(2015, 9, 29)
    let ``2015-10-6`` = DateTime(2015, 10, 6)
    let ``2015-10-13`` = DateTime(2015, 10, 13)
    let ``2015-10-20`` = DateTime(2015, 10, 20)
    let ``2015-10-27`` = DateTime(2015, 10, 27)
    let ``2015-11-3`` = DateTime(2015, 11, 3)
    let ``2015-11-10`` = DateTime(2015, 11, 10)
    let ``2015-11-17`` = DateTime(2015, 11, 17)
    let ``2015-11-24`` = DateTime(2015, 11, 24)    
    let ``2015-12-1`` = DateTime(2015, 12, 1)
    
    let Empty = FsUnit.Xunit.Empty
    
    let ``all students in house must attend all sessions`` = 
        FromHouse(SelectAllStudents RequireEachStudentToAttendEverySession)

    let ``each session has a maximum of 6 students from the house, each student must attend once this year`` =
        FromHouse (SelectMaxStudents (6, RequireEachStudentToAttendOnceThisYear))

    let emptyBlock = 
        { Course = { Name = "Doctor-Patient" }
          Name = "Foundations/Nervous System - Societies Tuesday Activities"
          StartDate = ``2015-9-29``
          EndDate = ``2015-12-1``
          House = []
          Activities = [] }

    [<Fact>]
    let ``Create sessions with empty block should return no sessions``() =         
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions emptyBlock

        mandatoryActivitySessions |> should be Empty
        unorderedActivitySessions |> should be Empty
        overflowActivitySessions |> should be Empty
    
    [<Fact>]
    let ``Create sessions with one mandatory activity on a set date should return one mandatory session only``() = 
        let peFinalClinic = 
            { Name = "PE Final Clinic"
              Frequency = Once(``2015-10-27``, ``12pm``, ``6pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }
        
        let block = { emptyBlock with Activities = [ peFinalClinic ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should haveLength 1        
        unorderedActivitySessions |> should be Empty
        overflowActivitySessions |> should be Empty

        let expectedResult = 
            [ { Activity = peFinalClinic
                Sessions = 
                    [ createEmptySession Set.empty (``2015-10-27``, ``12pm``, ``6pm``) ] } ]
        mandatoryActivitySessions |> should equal expectedResult

    [<Fact>]
    let ``Create sessions with one unordered activity on a set date should return one unordered session only``() = 
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = Once (``2015-10-27``, ``1pm``, ``3pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }
        
        let block = { emptyBlock with Activities = [ ophthalmologyClinic ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should be Empty        
        unorderedActivitySessions |> should haveLength 1   
        overflowActivitySessions |> should be Empty

        let expectedResult = 
            [ { Activity = ophthalmologyClinic
                Sessions = 
                    [ createEmptySession Set.empty (``2015-10-27``, ``1pm``, ``3pm``) ] } ]
        unorderedActivitySessions |> should equal expectedResult

    [<Fact>]
    let ``Create sessions with one master and one overflow activity on a set date should return master and overflow sessions``() = 
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = Once (``2015-10-27``, ``1pm``, ``3pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }

        let mentorDirectedTime =
            { Name = "Mentor Directed Time"
              Frequency = Once (``2015-10-27``, ``1pm``, ``3pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = OverflowFrom ophthalmologyClinic } 
        
        let block = { emptyBlock with Activities = [ ophthalmologyClinic; mentorDirectedTime ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should be Empty        
        unorderedActivitySessions |> should haveLength 1   
        overflowActivitySessions |> should haveLength 1 

        let expectedUnorderedSessions = 
            [ { Activity = ophthalmologyClinic
                Sessions = 
                    [ createEmptySession Set.empty (``2015-10-27``, ``1pm``, ``3pm``) ] } ]
        unorderedActivitySessions |> should equal expectedUnorderedSessions

        let expectedOverflowSessions = 
            [ { Activity = mentorDirectedTime
                Sessions = 
                    [ createEmptySession Set.empty (``2015-10-27``, ``1pm``, ``3pm``) ] } ]
        overflowActivitySessions |> should equal expectedOverflowSessions

    // todo: any way to update the domain model to avoid allowing even creating this invalid state?
    [<Fact>]
    let ``Create sessions with one master and one overflow activity with different frequencies should fail``() = 
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = Once (``2015-10-27``, ``1pm``, ``3pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }

        let mentorDirectedTime =
            { Name = "Mentor Directed Time"
              Frequency = Weekly [ (``1pm``, ``3pm``)
                                   (``3pm``, ``5pm``) ]
              EnrollmentPriority = Neutral
              EnrollmentCriteria = OverflowFrom ophthalmologyClinic }               
        
        let block = { emptyBlock with Activities = [ ophthalmologyClinic; mentorDirectedTime ] }
        
        (fun () -> createSessions block |> ignore) |> should (throwWithMessage "Overflow activity 'Mentor Directed Time' and master activity 'Ophtho' must have the same frequency") typeof<System.Exception>
    
    [<Fact>]
    let ``Create sessions with one weekly activity should return one session for each week of block``() = 
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = Weekly [ (``1pm``, ``3pm``) ]
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }
        
        let block = { emptyBlock with Activities = [ ophthalmologyClinic ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should be Empty        
        unorderedActivitySessions |> should haveLength 1   
        overflowActivitySessions |> should be Empty

        let sessionForDate date = createEmptySession Set.empty (date, ``1pm``, ``3pm``)
        
        let expectedResult = 
            [ { Activity = ophthalmologyClinic
                Sessions = 
                    [ sessionForDate ``2015-9-29`` 
                      sessionForDate ``2015-10-6``  
                      sessionForDate ``2015-10-13`` 
                      sessionForDate ``2015-10-20`` 
                      sessionForDate ``2015-10-27`` 
                      sessionForDate ``2015-11-3``  
                      sessionForDate ``2015-11-10`` 
                      sessionForDate ``2015-11-17`` 
                      sessionForDate ``2015-11-24`` 
                      sessionForDate ``2015-12-1`` ] } ]
        unorderedActivitySessions |> should equal expectedResult

    [<Fact>]
    let ``Create sessions with activity held on two specfic times should return one session for each time``() = 
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = SpecificTimes [ (``2015-9-29``, ``1pm``, ``3pm``)
                                          (``2015-10-13``, ``3pm``, ``5pm``) ]
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }
        
        let block = { emptyBlock with Activities = [ ophthalmologyClinic ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should be Empty        
        unorderedActivitySessions |> should haveLength 1   
        overflowActivitySessions |> should be Empty

        let expectedResult = 
            [ { Activity = ophthalmologyClinic
                Sessions = 
                    [ createEmptySession Set.empty (``2015-9-29``, ``1pm``, ``3pm``)  
                      createEmptySession Set.empty (``2015-10-13``, ``3pm``, ``5pm``) ] } ]
        unorderedActivitySessions |> should equal expectedResult
        
    [<Fact>]
    let ``Create sessions with one mandatory activity and one unordered activity should not create unordered activity if they are held at same time``() = 
        let peFinalClinic = 
            { Name = "PE Final Clinic"
              Frequency = Once (``2015-10-27``, ``12pm``, ``6pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }
        
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = Once (``2015-10-27``, ``12pm``, ``6pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }

        let block = { emptyBlock with Activities = [ peFinalClinic; ophthalmologyClinic ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should haveLength 1        
        unorderedActivitySessions |> should haveLength 1
        overflowActivitySessions |> should be Empty

        let unorderedActivityWithNoSessions = 
            [ { Activity = ophthalmologyClinic
                Sessions = [] } ]
        unorderedActivitySessions |> should equal unorderedActivityWithNoSessions

    [<Fact>]
    let ``Create sessions with one mandatory activity and one unordered activity should not create unordered activity if it is within mandatory session time``() = 
        let peFinalClinic = 
            { Name = "PE Final Clinic"
              Frequency = Once (``2015-10-27``, ``12pm``, ``6pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }
        
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = Once (``2015-10-27``, ``1pm``, ``3pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }

        let block = { emptyBlock with Activities = [ peFinalClinic; ophthalmologyClinic ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should haveLength 1        
        unorderedActivitySessions |> should haveLength 1
        overflowActivitySessions |> should be Empty

        let unorderedActivityWithNoSessions = 
            [ { Activity = ophthalmologyClinic
                Sessions = [] } ]
        unorderedActivitySessions |> should equal unorderedActivityWithNoSessions

    [<Fact>]
    let ``Create sessions with one mandatory activity and one unordered activity should not create unordered activity if starts before mandatory session but ends during it``() = 
        let peFinalClinic = 
            { Name = "PE Final Clinic"
              Frequency = Once (``2015-10-27``, ``1pm``, ``6pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }
        
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = Once (``2015-10-27``, ``12pm``, ``3pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }

        let block = { emptyBlock with Activities = [ peFinalClinic; ophthalmologyClinic ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should haveLength 1        
        unorderedActivitySessions |> should haveLength 1
        overflowActivitySessions |> should be Empty

        let unorderedActivityWithNoSessions = 
            [ { Activity = ophthalmologyClinic
                Sessions = [] } ]
        unorderedActivitySessions |> should equal unorderedActivityWithNoSessions

    [<Fact>]
    let ``Create sessions with one mandatory activity and one unordered activity should not create unordered activity if starts during mandatory session but ends after it``() = 
        let peFinalClinic = 
            { Name = "PE Final Clinic"
              Frequency = Once (``2015-10-27``, ``12pm``, ``3pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }
        
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = Once (``2015-10-27``, ``1pm``, ``6pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }

        let block = { emptyBlock with Activities = [ peFinalClinic; ophthalmologyClinic ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should haveLength 1        
        unorderedActivitySessions |> should haveLength 1
        overflowActivitySessions |> should be Empty

        let unorderedActivityWithNoSessions = 
            [ { Activity = ophthalmologyClinic
                Sessions = [] } ]
        unorderedActivitySessions |> should equal unorderedActivityWithNoSessions

    [<Fact>]
    let ``Create sessions with one mandatory activity and one unordered activity should create unordered activity if starts when mandatory session ends``() = 
        let peFinalClinic = 
            { Name = "PE Final Clinic"
              Frequency = Once (``2015-10-27``, ``12pm``, ``3pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }
        
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = Once (``2015-10-27``, ``3pm``, ``6pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }

        let block = { emptyBlock with Activities = [ peFinalClinic; ophthalmologyClinic ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should haveLength 1        
        unorderedActivitySessions |> should haveLength 1        
        overflowActivitySessions |> should be Empty

    [<Fact>]
    let ``Create sessions with one mandatory activity and one unordered activity should create unordered activity if ends when mandatory session starts``() = 
        let peFinalClinic = 
            { Name = "PE Final Clinic"
              Frequency = Once (``2015-10-27``, ``3pm``, ``6pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }
        
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = Once (``2015-10-27``, ``1pm``, ``3pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }

        let block = { emptyBlock with Activities = [ peFinalClinic; ophthalmologyClinic ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should haveLength 1        
        unorderedActivitySessions |> should haveLength 1        
        overflowActivitySessions |> should be Empty

    [<Fact>]
    let ``Create sessions with one mandatory activity and one unordered activity with multiple sessions should not create the unordered activity session that overlaps with the mandatory one``() = 
        let peFinalClinic = 
            { Name = "PE Final Clinic"
              Frequency = Once (``2015-10-27``, ``12pm``, ``6pm``)
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``all students in house must attend all sessions`` }
        
        let ophthalmologyClinic = 
            { Name = "Ophtho"
              Frequency = SpecificTimes [ (``2015-9-29``, ``1pm``, ``3pm``)
                                          (``2015-10-27``, ``1pm``, ``3pm``) ] 
              EnrollmentPriority = Neutral
              EnrollmentCriteria = ``each session has a maximum of 6 students from the house, each student must attend once this year`` }

        let block = { emptyBlock with Activities = [ peFinalClinic; ophthalmologyClinic ] }
        
        let mandatoryActivitySessions, unorderedActivitySessions, overflowActivitySessions = createSessions block
        mandatoryActivitySessions |> should haveLength 1        
        unorderedActivitySessions |> should haveLength 1        
        overflowActivitySessions |> should be Empty

        let expectedResult = 
            [ { Activity = ophthalmologyClinic
                Sessions = 
                    [ createEmptySession Set.empty (``2015-9-29``, ``1pm``, ``3pm``) ] } ]
        unorderedActivitySessions |> should equal expectedResult

    // todo: write a test for activity populated from group to ensure a copy of each session is created for each group

    // todo: write a test for overflow activity with is populated from group to ensure a copy of each session is created for each group