module Clock

type Clock =
    { Minutes: int }

[<Literal>]
let MaxMinutes = 1440

let create hours minutes =
    let m = 60 * hours + minutes
    match m < 0 with
    | true -> { Minutes = MaxMinutes + (m % MaxMinutes) }
    | false -> { Minutes = m % MaxMinutes }

let add minutes clock =
    let m = minutes
    let newMinutes = (clock.Minutes + m) % MaxMinutes
    { Minutes = newMinutes }

let subtract minutes clock =
    let newMinutes = (clock.Minutes - minutes) % MaxMinutes
    match newMinutes < 0 with
    | true -> { Minutes = MaxMinutes + newMinutes }
    | false -> { Minutes = newMinutes }

let display clock =
    let hour = clock.Minutes / 60
    let minutes = clock.Minutes % 60

    let hourString =
        match hour < 10 with
        | true -> "0" + string hour
        | false -> string hour

    let minutesString =
        match minutes < 10 with
        | true -> "0" + string minutes
        | false -> string minutes

    hourString + ":" + minutesString
