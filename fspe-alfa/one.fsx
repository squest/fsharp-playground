module One

// This is a function that accepts a number and returns a list of digits of the number
let numcol (num : int64) =
  let rec numccol' (num : int64) (res : int64 list) =
    match num with
    | 0L -> res
    | _ -> numccol' (num / 10L) (num % 10L :: res)
  numccol' num []

// Create a function that accepts a number and check whether it is a prime number
let isPrime (num : int64) =
  let rec isPrime' (num : int64) (i : int64) =
    match i with
    | _ when i * i > num -> true
    | _ when num % i = 0L -> false
    | _ -> isPrime' num (i + 1L)
  isPrime' num 2L

let primeCheck (num : int64) = 
  let rec primeCheck' (i : int64) =
    match i with 
    | _ when (i * i) > num -> true 
    | _ when (num % i) = 0L -> false 
    | _ -> primeCheck' (i + 2L)
  match num with 
  | 2L -> true
  | _ when num < 2L -> false 
  | _ when num % 2L = 0L -> false
  | _ -> primeCheck' 3L

// Create boolean array  gimana caranya?
// gue cuma butuh elo bikin boolean array size lim
// gue mau bikin array of boolean yang isinya true semua
// nama arraynya arr 

let arr = Array.create 1000000 true

// gue pengen bikin lazy sieve of eratosthenes
let sieve (n : int64) =
  let rec sieve' (n : int64) (i : int64) (primes : int64 list) =
    match i with
    | _ when i * i > n -> primes
    | _ when primes |> List.exists (fun x -> i % x = 0L) -> sieve' n (i + 2L) primes
    | _ -> sieve' n (i + 2L) (i :: primes)
  sieve' n 3L [2L]

// Gue mau bikin function untuk timing the function call and print to console the time it takes to execute the function
let time (f : 'a -> 'b) (x : 'a) =
  let sw = System.Diagnostics.Stopwatch()
  sw.Start()
  let res = f x
  sw.Stop()
  printfn "Elapsed time: %A" sw.Elapsed
  res

let primeList lim =
  let primes = Array.create (int lim) true 
  let rec markFalse i =
    match i with 
    | _ when i >= lim -> ()
    | _ -> primes.[i] <- false; markFalse (i+2)
  markFalse 4
  let rec sieve i =
    match i with 
    | _ when i * i >= lim -> 
      List.filter (fun x -> primes.[x]) [2..lim-1]
      |> List.fold (fun acc x -> acc + x) 0
    | _ when primes.[i] ->
      let rec mark j =
        match j with 
        | _ when j >= lim -> ()
        | _ -> primes.[j] <- false; mark (j + i + i)
      mark (i * i)
      sieve (i + 2)
    | _ -> sieve (i + 2)
  sieve 3

let isOddPrime (num : int64) =
  let rec check (i : int64) =
    match i with 
    | _ when i * i > num -> true 
    | _ when num % i = 0L -> false 
    | _ -> check (i + 2L)
  check 3L

let nextPrime (p : int64) =
  let rec nextPrime' (i : int64) =
    match i with 
    | _ when isOddPrime i -> i 
    | _ -> nextPrime' (i + 2L)
  nextPrime' (p + 2L)

// Create a function that accepts a number and returns a list of prime factors of the number
let primeFactors (num : int64) = 
  let rec findFactors (n : int64) (p : int64) (factors : int64 list) =
    match p with 
    | _ when p * p > num -> List.rev factors 
    | _ when n % p = 0L -> findFactors (n / p) p (p :: factors)
    | _ -> findFactors n (nextPrime p) factors
  findFactors num 3L []

let sieveFactors (n) =
  let numbers = Array.ofList [0..n]
  let rec sieve (i : int) =
    match i with 
    | _ when i > n -> List.ofArray numbers  
    | _ ->
      let rec mark (j : int) =
        match j with 
        | _ when j > n -> ()
        | _ -> numbers.[j] <- numbers.[j] / numbers.[i]; mark (j + i)
      mark (i + i)
      sieve (i + 1)
  sieve 2 |> List.tail |> List.fold (fun acc x -> acc * x) 1

let sol7 idx =
  let rec findPrime (i : int) (p : int64) =
    match i with 
    | _ when i = idx -> p 
    | _ -> findPrime (i + 1) (nextPrime p)
  findPrime 2 3L


