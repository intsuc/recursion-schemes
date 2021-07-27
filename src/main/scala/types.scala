trait Functor[F[*]]:
  def apply[A, B](fa: F[A], f: A => B): F[B]

type Algebra[F[*], A] = Functor[F] ?=> F[A] => A

type Coalgebra[F[*], A] = Functor[F] ?=> A => F[A]

type Fix[F[*]]

extension [F[*]: Functor](unfix: F[Fix[F]])
  inline def fix: Fix[F] = unfix.asInstanceOf

extension [F[*]: Functor](fix: Fix[F])
  inline def unfix: F[Fix[F]] = fix.asInstanceOf
