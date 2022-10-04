-- https://leanprover.github.io/functional_programming_in_lean/getting-to-know/polymorphism.html

-- 1. Write a function to find the last entry in a list. It should return an Option.

---- Version 1 (method)

def List.llast? {α : Type} (xs : List α) : Option α :=
  match xs with
  | [] => Option.none
  | y :: aa => if length aa == 0 then Option.some y else aa.llast?

#eval ['a', 'b', 'c'].llast?

---- Version 2 (function)

def llast? {α : Type} (xs : List α) : Option α :=
  match xs with
  | [] => Option.none
  | y :: aa => if aa.length == 0 then Option.some y else llast? aa

#eval llast? ['a', 'b', 'c'] 


-- 2. Write a function that finds the first entry in a list that satisfies a given predicate. Start the definition with def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=

---- Version 1 (method)

def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  match xs with
  | [] => Option.none
  | y :: _ => if predicate y then Option.some y else Option.none

#eval ['a', 'b', 'c'].findFirst? λ x => x == 'a' -- some 'a'
#eval ['a', 'b', 'c'].findFirst? λ x => x == 'w' -- none

---- Version 2 (function)

def findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  match xs with
  | [] => Option.none
  | y :: _ => if predicate y then Option.some y else Option.none

#eval findFirst? ['a', 'b', 'c'] λ x => x == 'a' -- some 'a'
#eval findFirst? ['a', 'b', 'c'] λ x => x == 'w' -- none



-- 3. Write a function Prod.swap that swaps the two fields in a pair. Start the definition with def Prod.swap {α β : Type} (pair : α × β) : β × α :=

def Prod.swap {α β : Type} (pair : α × β) : β × α :=
  (pair.snd, pair.fst)

#eval (3, 'a').swap //=> ('a', 3)
