import munit.FunSuite

class BidirectionalTypingSuite extends FunSuite:
  enum Typ:
    case Unit
    case Fun(t1: Typ, t2: Typ)

  enum ExpF[+A]:
    case VarF(i: Int)
    case UnitF
    case AbsF(a1: A)
    case AppF(a1: A, a2: A)
    case AnnF(a1: A, t: Typ)
  import ExpF.*

  given Functor[ExpF] with
    def apply[A, B](fa: ExpF[A], f: A => B): ExpF[B] =
      fa match
        case fa @ VarF(_) => fa.asInstanceOf[ExpF[B]]
        case fa @ UnitF   => fa.asInstanceOf[ExpF[B]]
        case AbsF(a1)     => AbsF(f(a1))
        case AppF(a1, a2) => AppF(f(a1), f(a2))
        case AnnF(a1, t)  => AnnF(f(a1), t)

  type Exp = Fix[ExpF]
  inline def Var(i: Int): Exp = VarF(i).fix
  inline def Unit: Exp = UnitF.fix
  inline def Abs(a1: Exp): Exp = AbsF(a1).fix
  inline def App(a1: Exp, a2: Exp): Exp = AppF(a1, a2).fix
  inline def Ann(a1: Exp, t: Typ): Exp = AnnF(a1, t).fix

  type Ctx[A] = Seq[Typ] => A
  type Check = Ctx[Typ => Option[Unit]]
  type Infer = Ctx[Option[Typ]]

  val check: Algebra[[Check] =>> ExpF[(Check, Infer)], Check] =
    case VarF(i) => ctx => typ => for t <- ctx.lift(i) if t == typ yield ()
    case UnitF =>
      ctx =>
        case Typ.Unit => Some(())
        case _        => None
    case AbsF((c1, _)) =>
      ctx =>
        case Typ.Fun(t1, t2) => c1(t1 +: ctx)(t2)
        case _               => None
    case AppF((c1, _), (_, i2)) =>
      ctx =>
        typ =>
          for
            t1 <- i2(ctx)
            _ <- c1(ctx)(Typ.Fun(t1, typ))
          yield ()
    case AnnF((_, i1), t) => ctx => typ => for _ <- i1(ctx) yield ()

  val infer: Algebra[[Infer] =>> ExpF[(Check, Infer)], Infer] =
    case VarF(i)        => ctx => ctx.lift(i)
    case UnitF          => ctx => Some(Typ.Unit)
    case AbsF((c1, i1)) => ctx => None
    case AppF((_, i1), (c2, _)) =>
      ctx =>
        for
          Typ.Fun(t1, t2) <- i1(ctx)
          _ <- c2(ctx)(t1)
        yield t2
    case AnnF((c1, _), t) => ctx => for _ <- c1(ctx)(t) yield t

  def run(exp: Exp): Option[Typ] = (exp mutu (check, infer))._2(Seq.empty)

  test("unit ⇒ Unit") {
    assertEquals(run(Unit), Some(Typ.Unit))
  }

  test("(unit : Unit) ⇒ Unit") {
    assertEquals(run(Ann(Unit, Typ.Unit)), Some(Typ.Unit))
  }

  test("((unit : Unit) : Unit) ⇒ Unit") {
    assertEquals(run(Ann(Ann(Unit, Typ.Unit), Typ.Unit)), Some(Typ.Unit))
  }

  test("(unit : Unit → Unit) ⇒") {
    assertEquals(run(Ann(Unit, Typ.Fun(Typ.Unit, Typ.Unit))), None)
  }

  test("0 ⇒") {
    assertEquals(run(Var(0)), None)
  }

  test("λ. 0 ⇒") {
    assertEquals(run(Abs(Var(0))), None)
  }

  test("(λ. 0) : Unit → Unit ⇒ Unit → Unit") {
    assertEquals(
      run(Ann(Abs(Var(0)), Typ.Fun(Typ.Unit, Typ.Unit))),
      Some(Typ.Fun(Typ.Unit, Typ.Unit))
    )
  }

  test("(λ. 0) unit ⇒") {
    assertEquals(run(Abs(Var(0))), None)
  }

  test("(λ. 0) unit : Unit ⇒") {
    assertEquals(run(Ann(App(Abs(Var(0)), Unit), Typ.Unit)), Some(Typ.Unit))
  }

  test("((λ. 0) : Unit → Unit) unit ⇒ Unit") {
    assertEquals(
      run(App(Ann(Abs(Var(0)), Typ.Fun(Typ.Unit, Typ.Unit)), Unit)),
      Some(Typ.Unit)
    )
  }

  test("((λ. λ. 1 0) : (Unit → Unit) → Unit → Unit) (λ. 0) unit ⇒ Unit") {
    assertEquals(
      run(
        App(
          App(
            Ann(
              Abs(Abs(App(Var(1), Var(0)))),
              Typ.Fun(Typ.Fun(Typ.Unit, Typ.Unit), Typ.Fun(Typ.Unit, Typ.Unit))
            ),
            Abs(Var(0))
          ),
          Unit
        )
      ),
      Some(Typ.Unit)
    )
  }
