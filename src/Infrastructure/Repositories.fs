namespace Student.Infrastructure.Repositories

open Student.Core.Domain

module StudentRepository =
  open FSharp.Data
  open Student.Core.Utils
  open System
    
  let private majors = 
    [|"Genetics";"Humanities";"Hospital Administration";"English";"Molecular Biology";"Social Science";"Medicine";
      "Science (Other Biology)";"Mathematics";"Pharmacy";"Engineering";"Nursing";"International Relations";
      "Environmental Studies";"Honors Program";"General Studies";"Other Major";"Chemical Engineering";"Pre-Professional";
      "Music";"Religion";"Law";"Women's Studies";"Zoology";"Interdisciplinary Studies";"Chemistry";"Art";"Anthropology";
      "Human Biology";"Literature";"Public Health";"Science General";"Biomedical Science";"Education";"Accounting";
      "Political Science";"Physical Education";"Natural Sciences";"Nutrition";"Biochemistry";"Linguistics";"Biology";
      "Mechanical Engineering";"Sociology";"Business";"Classics";"Physiology";"Botany";"Microbiology / Bacteriology";
      "Biomedical Engineering";"Economics";"Psychology";"Communications";"Foreign Language";"Dentistry";"Psychobiology";
      "History";"Pre-Medical";"Neuroscience";"Philosophy";"Computer Science";"Electrical Engineering"|]

  let private rndInt = Probability.randomIntGenerator
  let private rndMax = Probability.rndMax rndInt

  let private getRandomMajor () =
    majors.[rndMax (majors.Length - 1)]
    
  type Students = CsvProvider<"../../data/students.csv">

  let getStudents (filename: string) = 
    let toGender =  function
        | "M" -> Male
        | "F" -> Female
        | _ -> NotSpecified

    let toAge (dob: DateTime) = DateTime.Today.Year - dob.Year

    use fs = new System.IO.FileStream(filename, IO.FileMode.Open)
    let students = Students.Load(fs)
    
    students.Rows
    |> Seq.map (fun x -> 
        { Name = { FirstName = x.FirstName; LastName = x.LastName }; 
          Gender = (toGender x.Gender)
          Age = toAge x.Dob
          Major = getRandomMajor() })
    |> Seq.toList

module MentorRepository =
  // 20 Mentors which will be assigned a group of students
  let private mentorList = 
    [ "Moira Cadieux"
      "Cassidy Gwynn"
      "Maude Cornell"
      "Sommer Mcgriff"
      "Tamekia Woolard"
      "Amiee Shattuck"
      "Lionel Heyd"
      "Heide Corr"
      "Celina Hammaker"
      "Maribel Berndt"
      "Christen Marotz"
      "Gaynelle Peppard"
      "Avril Broe"
      "Arnetta King"
      "Pilar Hoggatt"
      "Kendra Hamada"
      "Vincenzo Vitiello"
      "Zona Dehart"
      "Lemuel Walkes"
      "Bettie Pellegrini" ]

  let getMentors () = mentorList

module GroupFileRepository =
  open System.IO

  let private writeGroup (writer: StreamWriter) group =
    writer.Write(sprintf "%s, " group.Mentor)

    let writeStudent (s: Student) = writer.Write(sprintf "%s|%d|%s, " (s.Name |> getFullName) s.Age s.Major)
    group.Students |> List.iter writeStudent    
    writer.WriteLine()

  let writeGroups filename (groups: Group list) =
    use writer = new StreamWriter(filename, false)
    groups |> Seq.iter (writeGroup writer)

module BlockFileRepository =
  open System
  open System.IO

  let private writeHeader (writer: StreamWriter) blockSchedule = 
    writer.WriteLine (sprintf "%s Course Schedule\n%s [%s to %s]" blockSchedule.Block.Course.Name blockSchedule.Block.Name (blockSchedule.Block.StartDate.ToString("MM-yyyy")) (blockSchedule.Block.EndDate.ToString("MM-yyyy")))

  let private writeColumnHeadings (writer: StreamWriter) blockSchedule = 
    writer.Write "Name,Mentor,"

    let toCsv a1 a2 = a1 + "," + a2
    let toPeriodStr (s: TimeSpan) (e: TimeSpan) = s.ToString("hh\\:mm") + " " + e.ToString("hh\\:mm")    

    let getActivityNames (as': ActivitySessions) = 
      let toActivityDescription startTime endTime = as'.Activity.Name + " [" + (toPeriodStr startTime endTime) + "]"

      match as'.Activity.Frequency with
      | Once (_, startTime, endTime) -> [ toActivityDescription startTime endTime ]
      | SpecificTimes times -> times |> List.map (fun (_, startTime, endTime) -> toActivityDescription startTime endTime)
      | Weekly times -> times |> List.map (fun (startTime, endTime) -> toActivityDescription startTime endTime)
    
    blockSchedule.ActivitySchedule 
      |> List.map (fun x -> getActivityNames x |> (List.reduce toCsv))
      |> List.reduce toCsv
      |> writer.WriteLine

  let private writeStudentSchedule (writer: StreamWriter) blockSchedule group (student: Student) =
    writer.Write (sprintf "%s,%s," (student.Name |> getFullName) group.Mentor)

    let toDateStr (x: DateTime) =  x.ToString("MM/dd")

    let getPeerDateStr session pe = 
        if pe.BedsideStudent = student then 
            "bs " + (session.Date |> toDateStr)
        else if pe.PeerStudent = student then
            "pr " + (session.Date |> toDateStr)
        else
            ""
    let getStudentEnrollmentDate session =        
        match session.Enrollment with      
        | StudentEnrollment s -> if s = student then session.Date |> toDateStr else ""
        | StudentsEnrollment sl -> if sl |> Set.contains student then session.Date |> toDateStr else ""
        | PeerEnrollment pe -> getPeerDateStr session pe
        | GroupsEnrollment gl -> if gl |> List.contains group then session.Date |> toDateStr else ""
        | Empty -> ""

    let getSessionDatesForStudent sessions =         
      let dates = 
        sessions
        |> List.map getStudentEnrollmentDate
        |> List.filter (fun x -> x <> "")
      String.Join(";", dates)

    let getSessionsForPeriod (activitySessions: ActivitySessions) period =
      let startTime, endTime = period
      activitySessions.Sessions
      |> List.filter (fun x -> x.StartTime = startTime && x.EndTime = endTime)

    let getSessionsForDatePeriod (activitySessions: ActivitySessions) datePeriod =
      let date, startTime, endTime = datePeriod
      activitySessions.Sessions
      |> List.filter (fun x -> x.Date = date && x.StartTime = startTime && x.EndTime = endTime)

    let toCsv a1 a2 = a1 + "," + a2

    let getSessionDates (activitySessions: ActivitySessions) =
      match activitySessions.Activity.Frequency with
      | Once _ -> activitySessions.Sessions |> getSessionDatesForStudent
      | SpecificTimes times -> 
        times 
        |> List.map (getSessionsForDatePeriod activitySessions) 
        |> List.map getSessionDatesForStudent
        |> List.reduce toCsv
       
      | Weekly times -> 
        times 
        |> List.map (getSessionsForPeriod activitySessions) 
        |> List.map getSessionDatesForStudent
        |> List.reduce toCsv

    blockSchedule.ActivitySchedule
    |> List.map getSessionDates
    |> List.reduce toCsv
    |> writer.WriteLine

  let private writeGroupScheduleForBlock writer block group =
    let studentScheduleWriter = writeStudentSchedule writer block group

    group.Students
    |> List.iter studentScheduleWriter

  let writeBlockSchedule filename groups blockSchedule =
    use writer = new StreamWriter(filename, false)
    let groupScheduleWriter = writeGroupScheduleForBlock writer blockSchedule

    writeHeader writer blockSchedule
    writeColumnHeadings writer blockSchedule
    groups
    |> List.iter groupScheduleWriter
