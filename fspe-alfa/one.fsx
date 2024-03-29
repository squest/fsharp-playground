module One
// Gue mau bikin function untuk timing the function call and print to console the time it takes to execute the function
let time (f : 'a -> 'b) (x : 'a) =
  let sw = System.Diagnostics.Stopwatch()
  sw.Start()
  let res = f x
  sw.Stop()
  printfn "Elapsed time: %A" sw.Elapsed
  res

let sol01a lim =
  let rec funrec i acc =
    match i with
    | _ when i >= lim -> acc
    | _ when (i % 3 = 0 || i % 5 = 0) -> funrec (i + 1) (acc + i)
    | _ -> funrec (i + 1) acc
  funrec 0 0

let fibolist lim =
  let rec fiborec a b acc =
    match a with 
    | _ when a >= lim -> List.rev acc
    | _ -> fiborec b (a + b) (a :: acc)
  fiborec 1 2 []

let fibosum lim = 
  let rec fiborec a b acc =
    match a with 
    | _ when a >= lim -> acc 
    | _ when a % 2 = 0 -> fiborec b (a + b) (acc + a)
    | _ -> fiborec b (a + b) acc
  fiborec 1 2 0

let sol02a lim = fibosum lim
let sol02b lim = List.sum (fibolist lim)

// L is for signed long
// UL is for unsigned long
// U is for unsigned int
let isOddPrime n =
  let rec funprime i =
    match i with
    | _ when i * i > n -> true
    | _ when n % i = 0 -> false
    | _ -> funprime (i + 2)
  funprime 3

let nextPrime n = 
  let rec nextrec i =
    match i with 
    | _ when isOddPrime i -> i
    | _ -> nextrec (i + 2)
  nextrec (n + 2)

let rec largestPrimeFactor n = 
  let rec recprime i res =
    match i with
    | _ when i * i > n -> res
    | _ when n % i = 0 -> recprime (nextPrime i) i
    | _ -> recprime (nextPrime i) res
  recprime 3 1

let numcol n =
  let rec numrec n acc =
    match n with 
    | _ when n < 10 -> n :: acc 
    | _ -> numrec (n / 10) (n % 10 :: acc)
  numrec n []

let isPalin n =
  let numlist = numcol n
  numlist = List.rev numlist

// How to create an array from a list?
// let arr = Array.create (List.length list) list

let rec palinprod i j =
  match i with
  | _ when i < 100 -> palinprod 1000 j
  | _ when j < 100 -> palinprod (i - 1) 999
  | _ when isPalin (i * j) -> i * j
  | _ -> palinprod (i - 1) j

let sol04a = palinprod 999 999

let sum xs = List.fold (fun acc x -> acc + x) 0 xs
let product xs = List.fold (fun acc x -> acc * x) 1 xs

// contohin gue pemakaian pipe yg panjang harus beberapa baris tapi ada di dalem function dong

let rec maxprod xs n =
  let rec maxprodrec xs n acc =
    match xs with
    | _ when List.length xs < n -> acc
    | _ -> maxprodrec (List.tail xs) n (max (product (List.take n xs)) acc)
  maxprodrec xs n 0

  // mana bro? ga ada piping di situ, yg ada piping panjang dong di dalem function

let sol08a n =
  let numstr = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
  let numlist = List.map (fun x -> int x - int '0') (List.ofSeq numstr)
  maxprod numlist n
    
let sieveFactor lim =
  let nums = Array.ofList [0..lim]
  let rec sieve i =
    match i with 
    | _ when i + i > lim -> Array.toList nums
    | _ -> 
    let rec inner j =
      match j with 
      | _ when j > lim -> () 
      | _ -> nums.[j] <- nums.[j] / nums.[i]; 
             inner (j + i)
    inner (i + i)
    sieve (i + 1)
  sieve 2

let sol5 lim = sieveFactor lim |> List.tail |> product

let rec gcd a b =
  match b with
  | _ when b = 0 -> a
  | _ -> gcd b (a % b)

let lcm a b = a * b / gcd a b

let sol5b lim = List.fold (fun acc x -> lcm acc x) 1 [2..lim]

let iterate f x = Seq.unfold (fun x -> Some(x, f x)) x

let sol7 lim = 
  iterate nextPrime 3
  |> Seq.take (lim - 1)
  |> Seq.last

let numstr = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

let sol8 = 
  Seq.windowed 13 numstr
  |> Seq.map (fun x -> List.map (fun y -> int y - int '0') (List.ofSeq x))
  |> Seq.map product
  |> Seq.max
  
let sievePrimes lim =
  let primes = Array.create (lim + 1) true
  let rec sieve i acc =
    match i with 
    | _ when i >= lim -> acc 
    | _ when i * i >= lim -> if primes.[i] then sieve (i + 2) (i + acc) else sieve (i + 2) acc 
    | _ when primes.[i] -> 
        let rec inner j =
          match j with 
          | _ when j >= lim -> ()
          | _ -> primes.[j] <- false; inner (j + i + i) 
          inner (i * i)
        sieve (i + 2) (i + acc)
    | _ -> sieve (i + 2) acc
  sieve 3 2

let sol10 lim = sievePrimes lim

let rec collatz n =
  match n with
  | _ when n = 1 -> 1
  | _ when n % 2 = 0 -> 1 + collatz (n / 2)
  | _ -> 1 + collatz (3 * n + 1)

let sol14 lim =
  let rec collatzrec i acc res =
    match i with
    | _ when i >= lim -> res
    | _ -> 
        let col = collatz i
        if col > acc then collatzrec (i + 1) col i
        else collatzrec (i + 1) acc res
  collatzrec 1 0 0

let rec factorial n =
  match n with
  | _ when n = 0 -> 1
  | _ -> n * factorial (n - 1)

let sol15 n = factorial (2 * n) / (factorial n * factorial n)