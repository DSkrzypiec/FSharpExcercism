module TwoFer

let twoFer (input: string option): string = 
    match input with
    | Some s -> sprintf "One for %A one for you" s
    | None -> "One for you one for me"
