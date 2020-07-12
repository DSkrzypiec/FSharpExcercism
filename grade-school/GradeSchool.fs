module GradeSchool

type School = Map<int, string list>

let empty: School = 
    Map.empty

let add (student: string) (grade: int) (school: School): School = 
    let exist, students = school.TryGetValue grade
    match exist with
    | true -> school.Add(grade, (students @ [student]))
    | false -> school.Add(grade, [student])

let roster (school: School): string list = 
    school 
        |> Map.toList
        |> List.sortBy (fun p -> (fst p)) 
        |> List.collect (fun p -> (snd p |> List.sort))

let grade (number: int) (school: School): string list = 
    let exists, students = school.TryGetValue number
    match exists with
    | true -> students |> List.sort
    | false -> []
