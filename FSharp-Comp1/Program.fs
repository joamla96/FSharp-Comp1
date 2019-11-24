// Learn more about F# at http://fsharp.org

// Ex 1
let rec count aList key = match aList with
    | [] -> 0
    | h :: t when h < key -> count t key
    | h :: t when h = key -> 1 + (count t key)
    | _ -> 0

// Ex 1 - using fold
let f_count list number =
    let folder state value =
        if value = number then
            state + 1
        else
            state + 0
    List.fold folder 0 list

// Ex 2
let rec insert item aList = match aList with
    | [] -> [ item ]
    | h :: t when item <= h -> [ item ] @ aList
    | h :: t when item > h -> [ h ] @ insert item t
    | _ -> failwith "Seek Shelter" // Throw exception

// Ex 3 - intersect
//let rec intersect (list1, list2) =
// https://stackoverflow.com/questions/20248006/f-intersection-of-lists
let rec mem list x =
  match list with
  | [] -> false
  | head :: tail -> if x = head then true else mem tail x

let rec intersection list1 list2 =
  match list1 with
  | head :: tail ->
      let rest = intersection tail list2
      if mem list2 head then head :: rest
      else rest
  | [] -> []

// Ex 3 - seconday
let rec intersection2 list1 list2 = 
  match list1 with 
  | head :: tail -> [head] @ intersection2 list2 tail
  | [] -> []

// Ex 4 - Plus
let rec plus list1 list2 =
    match list1, list2 with
    | [], other | other, [] -> other
    | xh::xt, yh::yt when xh < yh -> xh :: (plus xt list2)
    | xh::xt, yh::yt -> yh :: (plus list1 yt)

// Ex 5 - Minus
let rec minus list1 list2 =
    match list1, list2 with
    | [], other | other, [] -> other
    | x::xs, y::ys when x = y -> (minus xs ys)
    | x::xs, y::ys -> (minus list1 ys)


let rec minus2 list1 list2 =
    match list1, list2 with
    | [], other | other, [] -> other
    | x::xs, y::ys when x > y -> (minus2 list1 ys)
    | x::xs, y::ys when x < y -> x :: (minus2 xs ys)
    | x::xs, y::ys when x = y -> (minus2 xs ys)
    | x::xs, y::ys -> (minus2 xs list2)

open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
