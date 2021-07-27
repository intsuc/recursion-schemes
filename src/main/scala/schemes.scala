extension [F[*]: Functor, A](fix: Fix[F])
  def cata(algebra: Algebra[F, A]): A =
    algebra(summon[Functor[F]](fix.unfix, _ cata algebra))

extension [F[*]: Functor, A](a: A)
  def ana(coalgebra: Coalgebra[F, A]): Fix[F] =
    summon[Functor[F]](coalgebra(a), _ ana coalgebra).fix
