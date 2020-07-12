module Leap

let leapYear (year: int): bool =
    match (year % 4, year % 100 = 0, year % 400 = 0) with
    | (0, false, _) -> true
    | (0, true, true) -> true
    | _ -> false
