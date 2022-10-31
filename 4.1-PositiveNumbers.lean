-- 1. Another representation
-- An alternative way to represent a positive number is as the successor of some Nat. Replace the definition of Pos with the following structure.
-- Define instances of Add, Mul, ToString, and OfNat that allow this version of Pos to be used conveniently.

structure PosType where
  succ ::
  pred : Nat

#reduce PosType.succ 4            -- 5
#reduce PosType.succ 0            -- 1
#reduce ({ pred := 0 } : PosType) -- 1

def add (x : PosType) (y : PosType) : PosType :=
  match x, y with
  | PosType.succ a, PosType.succ b => PosType.succ (a + b + 1)

def three := PosType.succ 2
def four := PosType.succ 3
#reduce add three four -- seven (aka { pred := 6 })

-- def mul (x : PosType) (y : PosType) : PosType :=
--   match x, y with
--   | PosType.succ (Nat.succ a), b => add (mul (PosType.succ a) b) b
--   | PosType.succ Nat.zero, PosType.succ (Nat.succ b) => PosType.succ (b + 1)
--   | PosType.succ Nat.zero, PosType.succ Nat.zero => PosType.succ 0

def mul : PosType → PosType → PosType
  | PosType.succ (Nat.succ a), b => add (mul (PosType.succ a) b) b
  | PosType.succ Nat.zero, PosType.succ (Nat.succ b) => PosType.succ (b + 1)
  | PosType.succ Nat.zero, PosType.succ Nat.zero => PosType.succ 0

#reduce mul three four -- twelve (aka { pred := 11 })

-- Now we want to make it possible to use `add` and `mul` conveniently.
-- To achieve this, let's make PosType an instance of the native `Add` & `Mul` typeclasses.

instance : Add PosType where
  add := add
instance : Mul PosType where
  mul := mul

#reduce three + four -- seven (aka pred := 6)
#reduce three * four -- twelve (aka { pred := 11 })

-- Now let's create a ToString function that would allow us to call #eval

def toNat (x : PosType) : Nat :=
  match x with
  | PosType.succ a => a + 1

instance : ToString PosType where
  toString x := toString (toNat x)

#eval three

-- Now let's make it possible to use natural number literals to represent PosType

def literalToPosType (n: Nat) : PosType :=
  match n with
  | 0 => PosType.succ 0
  | k + 1 => PosType.succ (k + 1)

instance : OfNat PosType (n + 1) where
  ofNat := literalToPosType n

#eval (5 : PosType)


-- 2. Define a datatype that represents only even numbers. Define instances of Add, Mul, and ToString that allow it to be used conveniently.

inductive EvenType where
  | zero : EvenType
  | plusTwo : EvenType → EvenType
  | minusTwo : EvenType → EvenType

def mTwo := EvenType.minusTwo EvenType.zero
def pTwo := EvenType.plusTwo EvenType.zero
def pFour := EvenType.plusTwo (EvenType.plusTwo EvenType.zero)

def even_add (x : EvenType) (y : EvenType) :=
  match x, y with
  | a, EvenType.zero => a
  | EvenType.zero, b => b
  | EvenType.minusTwo a, EvenType.minusTwo b => EvenType.minusTwo (EvenType.minusTwo (even_add a b))
  | EvenType.plusTwo a, EvenType.plusTwo b => EvenType.plusTwo (EvenType.plusTwo (even_add a b))
  | EvenType.minusTwo a, EvenType.plusTwo b => even_add a b
  | EvenType.plusTwo a, EvenType.minusTwo b => even_add a b

#reduce even_add pFour mTwo -- EvenType.plusTwo EvenType.zero

def even_minus (x : EvenType) (y : EvenType) :=
  match x, y with
  | a, EvenType.zero => a
  | a, EvenType.minusTwo b => even_minus (EvenType.plusTwo a) b
  | a, EvenType.plusTwo b => even_minus (EvenType.minusTwo a) b

def even_mul (x : EvenType) (y: EvenType) :=
  match x, y with
  | _, EvenType.zero => EvenType.zero
  | EvenType.zero, _ => EvenType.zero
  -- (a - 2) * b = ab - 2b
  | EvenType.minusTwo a, b => even_minus (even_minus (even_mul a b) b) b
  | EvenType.plusTwo a, b => even_add (even_add (even_mul a b) b) b

#reduce even_mul pFour mTwo -- -8 aka EvenType.minusTwo (EvenType.minusTwo (EvenType.minusTwo (EvenType.minusTwo EvenType.zero)))

instance : Add EvenType where
  add := even_add

#reduce pFour + mTwo

instance : Mul EvenType where
  mul := even_mul

#reduce pFour * mTwo

def evenToString (x : EvenType) : String :=
  match x with
  | EvenType.zero => "+0"
  | EvenType.minusTwo a => "-2" ++ evenToString a
  | EvenType.plusTwo a => "+2"++ evenToString a

#eval evenToString pFour

instance : ToString EvenType where
  toString := evenToString

#print pFour
