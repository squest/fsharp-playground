module One 

printfn "Hello world"

// ini comment bukan?
let x = 10
let y = 20
let sum = x + y
printfn "The sum of %d and %d is %d" x y sum

// Gue butuh elo bikin prime-checking function here

let isPrime n =
  if n < 2 then
    false
  else
    let rec isDivisibleBy k =
      if k * k > n then
        true
      else if n % k = 0 then
        false
      else
        isDivisibleBy (k + 1)
    isDivisibleBy 2