-- https://leanprover.github.io/functional_programming_in_lean/getting-to-know/polymorphism.html

-- 1. Write a function to find the last entry in a list. It should return an Option.

def List.llast? {α : Type} (xs : List α) : Option α :=
  match xs with
  | [] => Option.none
  | y :: aa => if length aa == 0 then Option.some y else aa.llast?

#eval ['a', 'b', 'c'].llast?
