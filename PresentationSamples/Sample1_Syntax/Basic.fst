module Basic

open FStar.Mul

let rec factorial (n:int) : int =
    if n <= 0 then 1 else n * (factorial (n - 1))

type mylist (a : Type) = 
    | Nil : mylist a
    | Cons : hd : a -> tl : mylist a -> mylist a

let rec map (#a : Type) (#b : Type) (f: a -> b) (l : mylist a) : mylist b = 
    match l with
    | Nil -> Nil
    | Cons hd tl -> Cons (f hd) (map f tl)

let nat = x:int{x >= 0}
let rec length (#a : Type) (l : mylist a) : nat = 
    match l with
    | Nil -> 0
    | Cons _ tl -> 1 + length tl

let rec map2 (#a : Type) (#b : Type) (f: a -> b) (l : mylist a) : z:mylist b{length z = length l} = 
    match l with
    | Nil -> Nil
    | Cons hd tl -> Cons (f hd) (map2 f tl)

let non_empty_list (a : Type) = l : mylist a{length l <> 0}

let head (#a : Type)  (l : non_empty_list a) : a = 
    match l with
    // No need to handle Nil case
    | Cons hd _ -> hd

// Fails with:
// (Error 19) Patterns are incomplete
[@@ expect_failure [19]]
let head_fails (#a : Type)  (l : mylist a) : a = 
    match l with
    | Cons hd _ -> hd