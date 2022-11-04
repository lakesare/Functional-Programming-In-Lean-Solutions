import mathlib

structure NonEmptyList (α : Type) : Type where
  head : α
  tail : List α
deriving Repr

def colors : NonEmptyList String := { head := "red", tail := ["yellow", "white"] }

-- 1. Write an instance of HAppend (List α) (NonEmptyList α) (NonEmptyList α) and test it.

def List.from (xs : List α) (fr : ℕ) : List α :=
  match xs, fr with
  | [], _ => []
  | _ :: xs, a + 1 => List.from xs a
  | xs, 0 => xs

instance : HAppend (List α) (NonEmptyList α) (NonEmptyList α) where
  hAppend xs ys := {
    -- Alternatively, we can use the shortcut:
    -- head := if let some a := xs[0]? then a else ys.head,
    head := match xs[0]? with
      | some a => a
      | none => ys.head,
    -- Alternatively, we can use the standard library method:
    -- xs.drop 1
    tail := xs.from 1 ++ [ys.head] ++ ys.tail
  }

#eval ["Trapdoor Spider", "SPIDER"] ++ colors
