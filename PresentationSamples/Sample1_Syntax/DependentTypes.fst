module DependentTypes

type vector (a:Type) : nat -> Type =
    | Nil : vector a 0
    | Cons : #n:nat -> hd:a -> tl:vector a n -> vector a (n+1)

val incr (x:int) : y:int{y = x + 1}

// (Error 19) Subtyping check failed; expected type y: Prims.int{y = x + 1}; got type Prims.int;
[@@ expect_failure [19]]
let incr x = x + 2

let incr x = x + 1



