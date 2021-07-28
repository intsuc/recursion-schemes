extension [F[*]: Functor, A, B](a: A)
  def hylo(coalgebra: Coalgebra[F, A], algebra: Algebra[F, B]): B =
    algebra(summon(coalgebra(a))(_ hylo (coalgebra, algebra)))

extension [F[*]: Functor, A](fix: Fix[F])
  def cata(algebra: Algebra[F, A]): A =
    algebra(summon(fix.unfix)(_ cata algebra))

extension [F[*]: Functor, A](a: A)
  def ana(coalgebra: Coalgebra[F, A]): Fix[F] =
    summon(coalgebra(a))(_ ana coalgebra).fix

extension [F[*]: Functor, A, B](fix: Fix[F])
  def mutu(
      algebras: (Algebra[[A] =>> F[(A, B)], A], Algebra[[B] =>> F[(A, B)], B])
  ): (A, B) =
    fix cata (fab => (algebras._1(fab), algebras._2(fab)))

extension [F[*]: Functor, A, B](fix: Fix[F])
  def zygo(algebras: (Algebra[[A] =>> F[(A, B)], A], Algebra[F, B])): A =
    (fix mutu (algebras._1, fab => algebras._2(summon(fab)(_._2))))._1
