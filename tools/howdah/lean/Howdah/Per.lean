import Howdah.Eval

abbrev Per : Type 1 := Val → Val → Prop

structure IsPer (R : Per) : Prop where
  symm : ∀ {a b : Val}, R @a @b → R @b @a
  trans : ∀ {a b c : Val}, R @a @b → R @b @c → R @a @c

def Per.Refines (R S : Per) : Prop := ∀ {a b : Val}, R @a @b → S @a @b

def Val.false : Val := Val.immediate 0
def Val.true : Val := Val.immediate 1
def Val.none : Val := Val.immediate 0
def Val.some (v : Val) : Val := Val.values [@v]

def Per.int : Per := fun a b =>
  ∃ bits, @a = @Val.immediate bits ∧ @b = @Val.immediate bits

def Per.option (R : Per) : Per := fun a b =>
  (@a = @Val.none ∧ @b = @Val.none) ∨
  ∃ x y : Val, @a = @Val.some @x ∧ @b = @Val.some @y ∧ R @x @y

def Per.arrow (R S : Per) : Per := fun f g =>
  ∀ x y : Val, R @x @y → ∃ fx gy : Val,
    Expr.apply (Expr.value @f) [Expr.value @x] ⇓ @fx ∧
    Expr.apply (Expr.value @g) [Expr.value @y] ⇓ @gy ∧
    S @fx @gy

def Per.fieldwise : List Per → List Val → List Val → Prop
  | [], [], [] => True
  | R :: Rs, v :: vs, w :: ws => R @v @w ∧ Per.fieldwise Rs vs ws
  | _, _, _ => False

def Per.module (fields : List Per) : Per := fun m m' =>
  ∃ vs ws, @m = @Val.values vs ∧ @m' = @Val.values ws ∧
    Per.fieldwise fields vs ws
