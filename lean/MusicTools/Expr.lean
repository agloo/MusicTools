namespace Expr

abbrev Dict := List (String × Int)

def lookup : Dict → String → Int
  | [], _ => 0
  | (x, n) :: xs, s => if x == s then n else lookup xs s

def lookupM : Dict → String → Option Int
  | [], _ => none
  | (x, n) :: xs, s => if x == s then some n else lookupM xs s

mutual

inductive BExpr : Type where
  | falseE : BExpr
  | trueE  : BExpr
  | and    : BExpr → BExpr → BExpr
  | or     : BExpr → BExpr → BExpr
  | not    : BExpr → BExpr
  | eq     : IExpr → IExpr → BExpr
  | ne     : IExpr → IExpr → BExpr
  | lt     : IExpr → IExpr → BExpr
  | le     : IExpr → IExpr → BExpr
  | gt     : IExpr → IExpr → BExpr
  | ge     : IExpr → IExpr → BExpr
  deriving Repr

inductive IExpr : Type where
  | lit : Int → IExpr
  | var : String → IExpr
  | add : IExpr → IExpr → IExpr
  | sub : IExpr → IExpr → IExpr
  | mod : IExpr → IExpr → IExpr
  | ite : BExpr → IExpr → IExpr → IExpr
  deriving Repr

end

mutual

def evalI (d : Dict) : IExpr → Int
  | .lit n => n
  | .var s => lookup d s
  | .add a b => evalI d a + evalI d b
  | .sub a b => evalI d a - evalI d b
  | .mod a b => (evalI d a) % (evalI d b)
  | .ite b a c => if evalB d b then evalI d a else evalI d c

def evalB (d : Dict) : BExpr → Bool
  | .falseE => false
  | .trueE => true
  | .and x y => evalB d x && evalB d y
  | .or x y => evalB d x || evalB d y
  | .not x => !(evalB d x)
  | .eq x y => (evalI d x) == (evalI d y)
  | .ne x y => (evalI d x) != (evalI d y)
  | .lt x y => (evalI d x) < (evalI d y)
  | .le x y => (evalI d x) <= (evalI d y)
  | .gt x y => (evalI d x) > (evalI d y)
  | .ge x y => (evalI d x) >= (evalI d y)

end

namespace IExpr

def modNat (e : IExpr) (n : Int) : IExpr := .mod e (.lit n)

def abs (e : IExpr) : IExpr :=
  .ite (.ge e ((.lit 0))) e (.sub (.lit 0) e)

end IExpr

-- Indicator function χ.
def χ (b : BExpr) : IExpr :=
  .ite b (.lit 1) (.lit 0)

-- A pair of pitch pairs: ((a,b),(c,d)).
abbrev PP := (IExpr × IExpr) × (IExpr × IExpr)

def three := IExpr.lit 3

end Expr
