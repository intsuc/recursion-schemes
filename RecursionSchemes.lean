abbrev Algebra (f : Type → Type) (α : Type) := f α → α

abbrev Coalgebra (f : Type → Type) (α : Type) := α → f α

constant Fix (f : Type → Type) : Type

instance : Inhabited (Fix f) where
  default := sorry

unsafe def projectUnsafe : Fix f → f (Fix f) := unsafeCast

@[implementedBy projectUnsafe]
constant project : Fix f → f (Fix f) := sorry

prefix:max "↓" => project

unsafe def embedUnsafe : f (Fix f) → Fix f := unsafeCast

@[implementedBy embedUnsafe]
constant embed : f (Fix f) → Fix f := sorry

prefix:max "↑" => embed

partial def cata [Functor f] [Inhabited α] (algebra : Algebra f α) (fix : Fix f) : α :=
  algebra (cata algebra <$> ↓fix)

partial def ana [Functor f] (coalgebra : Coalgebra f α) (a : α) : Fix f :=
  ↑(ana coalgebra <$> coalgebra a)

inductive ExpF (α : Type) where
  | nat : Nat → ExpF α
  | add : α → α → ExpF α
  | mul : α → α → ExpF α

instance : Functor ExpF where
  map f
    | ExpF.nat n₁    => ExpF.nat n₁
    | ExpF.add a₁ a₂ => ExpF.add (f a₁) (f a₂)
    | ExpF.mul a₁ a₂ => ExpF.mul (f a₁) (f a₂)

abbrev Exp := Fix ExpF

#eval
  let eval : Algebra ExpF Nat
    | ExpF.nat n₁    => n₁
    | ExpF.add a₁ a₂ => a₁ + a₂
    | ExpF.mul a₁ a₂ => a₁ * a₂
  let exp : Exp := ↑(ExpF.mul ↑(ExpF.add ↑(ExpF.nat 1) ↑(ExpF.nat 2)) ↑(ExpF.nat 3))
  cata eval exp
