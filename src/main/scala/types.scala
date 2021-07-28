trait Functor[F[*]]:
  def apply[A, B](fa: F[A])(f: A => B): F[B]

object Functor:
  inline def apply[F[*]: Functor, A, B](fa: F[A])(f: A => B): F[B] =
    summon(fa)(f)

type Algebra[F[*], A] = F[A] => A

type Coalgebra[F[*], A] = A => F[A]

type Fix[F[*]]

extension [F[*]: Functor](unfix: F[Fix[F]])
  inline def fix: Fix[F] = unfix.asInstanceOf

extension [F[*]: Functor](fix: Fix[F])
  inline def unfix: F[Fix[F]] = fix.asInstanceOf
