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

// Gue mau bikin function yg inputnya lim, and return the list of primes that are less than lim
// sekalian return juga jumlah all primes yg kurang dari lim
// pake sieve ya, pake array
// salah lo, gue pengen yang returnnya tuh list of primes less than lim, and then jumlahnya



// Gue mau bikin function untuk timing the function call and print to console the time it takes to execute the function
let time (f : 'a -> 'b) (x : 'a) =
  let sw = System.Diagnostics.Stopwatch()
  sw.Start()
  let res = f x
  sw.Stop()
  printfn "Elapsed time: %A" sw.Elapsed
  res


