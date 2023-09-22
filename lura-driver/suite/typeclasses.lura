public id : forall ^a. ^a -> ^a

public data List (^a) {}

public trait Show (^a) {}

println : [Show ^a] => ^a -> ()
putStrLn : [Show ^a] => ^a -> ()
putStrLn = println
instance Show of String {}
instance Show of (List ^a) {}

fa (args: List String)
fb (args: List Int)

main (args: List String) {
  let a = fa args
  let b = fb args
  let x = id 10
  let y = id "hello"
  let z = println "string"
  let g = println 10
  let f = println args
  y
}
