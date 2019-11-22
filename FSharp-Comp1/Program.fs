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
// https://stackoverflow.com/questions/20248006/f-intersection-of-lists
let rec mem list x =
  match list with
  | [] -> false
  | head :: tail ->
    if x = head then true else mem tail x

let rec intersection list1 list2 =
  match list1 with
  | head :: tail ->
      let rest = intersection tail list2
      if mem list2 head then head :: rest
      else rest
  | [] -> []

// Ex 4 - Plus
// https://stackoverflow.com/questions/13530495/union-of-two-lists-in-order-f
let rec plus list1 list2 =
    match list1, list2 with
    | [], other | other, [] -> other
    | x::xs, y::ys when x < y -> x :: (plus xs list2)
    | x::xs, y::ys -> y :: (plus list1 ys)

// Ex 5 - Minus
//let minus (minuendList, subtrahendList) =

open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
