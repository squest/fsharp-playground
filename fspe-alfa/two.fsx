open System

let isPrime n =
  match n with 
  | _ when n < 2 -> false
  | _ when n = 2 -> true
  | _ when n % 2 = 0 -> false
  | _ -> List.forall (fun i -> n % i <> 0) [3 .. 2 .. int(sqrt(float(n)))]

let isOddPrime n = 
  List.forall (fun i -> n % i <> 0) [3..2..int(sqrt(float(n)))]

let nextPrime n =
  let rec nextrec i =
    match i with 
    | _ when isOddPrime i -> i
    | _ -> nextrec (i + 2)
  nextrec (n + 2)

  