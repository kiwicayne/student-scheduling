module Student.Api.Dtos

open System
open Student.Core.Domain

// ============================== 
// DTOs
// ============================== 

[<CLIMutable>]
type StudentDto = 
    { FirstName: string
      LastName: string
      Gender: string
      Age: int
      Major: string } 

[<CLIMutable>]
type MentorDto = string

[<CLIMutable>]
type CreateHousesDto =
    { Students: StudentDto list
      Mentors: MentorDto list }

[<CLIMutable>]
type GroupDto =
    { Mentor: MentorDto
      Students: StudentDto list }

[<CLIMutable>]
type HouseDto = 
    { Name: string
      Groups: GroupDto list }

[<CLIMutable>]
type HousesDto =
    { Houses: HouseDto list }

// ============================== 
// DTO Converters
// ============================== 

let (|InvariantEqual|_|) (str:string) arg = 
  if String.Compare(str, arg, StringComparison.OrdinalIgnoreCase) = 0
    then Some() else None

let toGender = function
    | InvariantEqual "male" -> Male
    | InvariantEqual "female" -> Female
    | _ -> NotSpecified    

let toGenderStr = function
    | Male -> "male"
    | Female -> "female"
    | NotSpecified -> "notspecified"

let private dtoToStudent dto =
    { Name = { FirstName = dto.FirstName
               LastName = dto.LastName }
      Gender = toGender dto.Gender
      Age = dto.Age
      Major = dto.Major }

let dtoToStudentsAndMentors (dto: CreateHousesDto) =
    try
        let students = 
            dto.Students 
            |> List.map dtoToStudent
      
        Some (students, dto.Mentors)
    with
    | :? NullReferenceException -> None // Dto is clrmutable, so can be null

let toStudentDto (student: Student) =
    { FirstName = student.Name.FirstName
      LastName = student.Name.LastName
      Gender = toGenderStr student.Gender
      Age = student.Age
      Major = student.Major }

let toGroupDto (group: Group) = 
    { Students = group.Students |> List.map toStudentDto
      Mentor = group.Mentor }

let toGroupsDto groups = groups |> List.map toGroupDto

let dtoToGroup (dto: GroupDto) =
    { Group.Mentor = dto.Mentor
      Students = dto.Students |> List.map dtoToStudent }

let dtoToHouse (dto: HouseDto) =
    dto.Groups
    |> List.map dtoToGroup

let dtoToHouses (dto: HousesDto): House list option =
    try 
        dto.Houses
        |> List.map dtoToHouse  
        |> Some  
    with
    | :? NullReferenceException -> None // Dto is clrmutable, so can be null