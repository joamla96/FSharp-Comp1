// Learn more about F# at http://fsharp.org

// Ex 1
let rec count aList key = match aList with
    | [] -> 0
    | h :: t when h < key -> count t key
    | h :: t when h = key -> 1 + (count t key)
    | _ -> 0


// Ex 2
let rec insert item aList = match aList with
    | [] -> [ item ]
    | h :: t when item <= h -> [ item ] @ aList
    | h :: t when item > h -> [ h ] @ insert item t
    | _ -> failwith "Seek Shelter"
    
// Ex 3 - intersect
//let rec intersect (list1, list2) =

// Ex 4 - Plus
//let plus (list1, list2) =

// Ex 5 - Minus
//let minus (minuendList, subtrahendList) =

open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
