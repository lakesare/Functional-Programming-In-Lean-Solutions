-- 1. Prove the following theorems using rfl: 2 + 3 = 5, 15 - 8 = 7, "Hello, ".append "world" = "Hello, world", 5 < 18.

def a : 2 + 3 = 5 := rfl
def b : 15 - 8 = 7 := rfl
def c : "Hello, ".append "world" = "Hello, world" := rfl
-- def d : 5 < 18 := by rfl ???

-- 2. Prove the following theorems using by simp: 2 + 3 = 5, 15 - 8 = 7, "Hello, ".append "world" = "Hello, world", 5 < 18.

def e : 2 + 3 = 5 := by simp
def f : 15 - 8 = 7 := by simp
def g : "Hello, ".append "world" = "Hello, world" := by simp
def h : 5 < 18 := by simp

-- 3. Write a function that looks up the fifth entry in a list. Pass the evidence that this lookup is safe as an argument to the function.

def get_fifth (xs : List String) (long_enough_evidence: xs.length > 5) : String :=
  xs[5]

def woodlandCritters := ["hedgehog", "deer", "snail", "mother", "father", "daughter", "mouse"]
#eval get_fifth woodlandCritters (by simp)
