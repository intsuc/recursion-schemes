import munit.FunSuite

class NatSuite extends FunSuite:
  enum NatF[+A]:
    case ZeroF
    case SuccF(a1: A)
  import NatF.*

  given Functor[NatF] with
    def apply[A, B](fa: NatF[A])(f: A => B): NatF[B] =
      fa match
        case fa @ ZeroF => fa.asInstanceOf[NatF[B]]
        case SuccF(a1)  => SuccF(f(a1))

  type Nat = Fix[NatF]
  inline def Zero: Nat = ZeroF.fix
  inline def Succ(a1: Nat): Nat = SuccF(a1).fix

  test("para-pred") {
    val nat: Nat = Succ(Succ(Succ(Zero)))

    val pred: RAlgebra[NatF, Nat] =
      case ZeroF          => Zero
      case SuccF((n1, _)) => n1

    assertEquals(nat para pred, Succ(Succ(Zero)))
  }

  test("mutu-even-odd") {
    val nat: Nat = Succ(Succ(Succ(Zero)))

    val even: Algebra[[A] =>> NatF[(A, Boolean)], Boolean] =
      case ZeroF        => true
      case SuccF(_, a1) => a1

    val odd: Algebra[[B] =>> NatF[(Boolean, B)], Boolean] =
      case ZeroF        => false
      case SuccF(a1, _) => a1

    assertEquals(nat mutu (even, odd), (false, true))
  }
