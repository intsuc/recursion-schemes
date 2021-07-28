import munit.FunSuite

class ArithmeticSuite extends FunSuite:
  enum ExpF[A]:
    case NumF(i: Int)
    case AddF(a1: A, a2: A)
    case MulF(a1: A, a2: A)
  import ExpF.*

  given Functor[ExpF] with
    def apply[A, B](fa: ExpF[A])(f: A => B): ExpF[B] =
      fa match
        case fa @ NumF(_) => fa.asInstanceOf[ExpF[B]]
        case AddF(a1, a2) => AddF(f(a1), f(a2))
        case MulF(a1, a2) => MulF(f(a1), f(a2))

  type Exp = Fix[ExpF]
  inline def Num(i: Int): Exp = NumF(i).fix
  inline def Add(a1: Exp, a2: Exp): Exp = AddF(a1, a2).fix
  inline def Mul(a1: Exp, a2: Exp): Exp = MulF(a1, a2).fix

  test("cata-eval") {
    val exp: Exp = Mul(Add(Num(1), Num(2)), Num(3))
    val eval: Algebra[ExpF, Int] =
      case NumF(i)      => i
      case AddF(a1, a2) => a1 + a2
      case MulF(a1, a2) => a1 * a2
    assertEquals(exp cata eval, (1 + 2) * 3)
  }

  test("cata-show") {
    val exp: Exp = Mul(Add(Num(1), Num(2)), Num(3))
    val show: Algebra[ExpF, String] =
      case NumF(i)      => s"$i"
      case AddF(a1, a2) => s"($a1 + $a2)"
      case MulF(a1, a2) => s"($a1 * $a2)"
    assertEquals(exp cata show, "((1 + 2) * 3)")
  }

  test("ana-bin") {
    val exp: Exp = Add(Num(1), Mul(Num(2), Add(Num(1), Num(2))))
    val bin: Coalgebra[ExpF, Int] =
      case i if i <= 2     => NumF(i)
      case i if i % 2 != 0 => AddF(1, i - 1)
      case i               => MulF(2, i / 2)
    assertEquals(7 ana bin, exp)
  }
