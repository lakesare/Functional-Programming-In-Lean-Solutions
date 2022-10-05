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


-- 4. Rewrite the PetName example to use a custom datatype and compare it to the version that uses Sum.

-- Sum example from the book
def PetName : Type := String ⊕ String
def animals : List PetName :=
  [Sum.inl "Spot", Sum.inr "Tiger", Sum.inl "Fifi", Sum.inl "Rex", Sum.inr "Floof"]

-- Same example, but with a custom data type
inductive OurPetName : Type where
  | cat : String → OurPetName
  | dog : String → OurPetName

def ourAnimnals : List OurPetName :=
  [OurPetName.dog "Spot", OurPetName.cat "Toger", OurPetName.dog "Fifi", OurPetName.dog "Rex", OurPetName.cat "Floof"]



-- 5. Write a function zip that combines two lists into a list of pairs. The resulting list should be as long as the shortest input list.

-- Full solution
def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs, ys with
  | List.nil, List.nil => List.nil
  | List.nil, List.cons n ns => List.nil
  | List.cons m ms, List.nil => List.nil
  | List.cons m ms, List.cons n ns => List.cons (m, n) (zip ms ns)

-- Full solution, with "Automatic Implicit Arguments" syntax sugar (see https://leanprover.github.io/functional_programming_in_lean/getting-to-know/conveniences.html)
def zip (xs : List α) (ys : List β) : List (α × β) :=
  match xs, ys with
  | List.nil, List.nil => List.nil
  | List.nil, List.cons n ns => List.nil
  | List.cons m ms, List.nil => List.nil
  | List.cons m ms, List.cons n ns => List.cons (m, n) (zip ms ns)

-- Full solution, with "Pattern-Matching Definitions" syntax sugar (see https://leanprover.github.io/functional_programming_in_lean/getting-to-know/conveniences.html)
def zip : List α → List β → List (α × β)
  | List.nil, List.nil => List.nil
  | List.nil, List.cons n ns => List.nil
  | List.cons m ms, List.nil => List.nil
  | List.cons m ms, List.cons n ns => List.cons (m, n) (zip ms ns)

#eval zip [1, 2, 3] ['a', 'b', 'c'] --=> [(1, 'a'), (2, 'b'), (3, 'c')]
#eval zip [1, 2, 3] ['a', 'b'] --=> [(1, 'a'), (2, 'b')]


