extension [F[*]: Functor, A, B](a: A)
  def hylo(coalgebra: Coalgebra[F, A], algebra: Algebra[F, B]): B =
    algebra(Functor(coalgebra(a))(_ hylo (coalgebra, algebra)))

extension [F[*]: Functor, A](fix: Fix[F])
  def cata(algebra: Algebra[F, A]): A =
    fix hylo (_.unfix, algebra)

extension [F[*]: Functor, A](a: A)
  def ana(coalgebra: Coalgebra[F, A]): Fix[F] =
    a hylo (coalgebra, _.fix)

extension [F[*]: Functor, A](fix: Fix[F])
  def para(ralgebra: RAlgebra[F, A]): A =
    ralgebra(Functor(fix.unfix)(fix => (fix, fix para ralgebra)))

extension [F[*]: Functor, A](a: A)
  def apo(rcoalgebra: RCoalgebra[F, A]): Fix[F] =
    Functor(rcoalgebra(a)) {
      case Left(fix) => fix
      case Right(a)  => a apo rcoalgebra
    }.fix

extension [F[*]: Functor, A, B](fix: Fix[F])
  def mutu(
      left: Algebra[[A] =>> F[(A, B)], A],
      right: Algebra[[B] =>> F[(A, B)], B]
  ): (A, B) =
    fix cata (fab => (left(fab), right(fab)))

extension [F[*]: Functor, A, B](fix: Fix[F])
  def zygo(left: Algebra[[A] =>> F[(A, B)], A], right: Algebra[F, B]): A =
    (fix mutu (left, fab => right(Functor(fab)(_._2))))._1
