module Clojure 

let map f xs =
  let rec map' f xs acc =
    match xs with 
    | [] -> List.rev acc 
    | x::xs' -> map' f xs' (f x :: acc)
  map' f xs []

let filter f xs =
  let rec filter' f xs acc =
    match xs with 
    | [] -> List.rev acc 
    | x::xs' -> 
      match f x with 
      | true -> filter' f xs' (x :: acc)
      | false -> filter' f xs' acc
  filter' f xs []

let last xs = 
  let rec last' xs =
    match xs with
    | [] -> failwith "Empty list"
    | [x] -> x
    | _::xs' -> last' xs'
  last' xs


let isPrime num =
  let rec isPrime' num i =
    match i with 
    | _ when i * i > num -> true
    | _ when num % i = 0L -> false
    | _ -> isPrime' num (i + 2L)
  match num with
  | 2L -> true
  | _ when num < 2L -> false
  | _ when num % 2L = 0L -> false
  | _ -> isPrime' num 3L

