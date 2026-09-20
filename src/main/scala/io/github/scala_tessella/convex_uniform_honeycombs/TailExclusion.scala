package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import CertifiedDihedrals.Iv

/** The tails: prisms and antiprisms beyond the corona fixpoint's pool caps are excluded — over the FULL pool
  * (core ∪ ALL prisms ∪ ALL antiprisms ∪ pentagon/decagon family ∪ snubs), with no cap left anywhere.
  *
  * ANTIPRISM CLOSED FORMS (derived from the scaled coordinate model B1=(1,0,0), B2=(cos2θ,sin2θ,0),
  * T=(cosθ,sinθ,η), T2=(cos3θ,sin3θ,η) with θ = π/q, η² = 4s²(4c²−1), c = cos(π/2q), s = sin(π/2q); the
  * factored face normals give |m̂1| = √3·c and |m̂2| = 2√3·c², whence):
  *
  * cos a3q(q) = −tan(π/2q)/√3 (base·lateral 3·q dihedral) cos a33(q) = (1 − 4cos(π/q))/3 (lateral·lateral 3·3
  * dihedral)
  *
  * validated against the interval reconstruction for q = 4, 5, 6, 10, 50 and — at q = 5 — against the exact
  * ℚ(√5) certification (tan²18° = (5−2√5)/5 gives cos² = (5−2√5)/15, and (1−4cos36°)/3 = −√5/3, the
  * icosahedron of [[IcosahedralIdentities]]). Immediate exact lemmas:
  *
  *   - LEMMA A (strict): a3q(q) > 90° for every q ≥ 4, since tan(π/2q) > 0.
  *   - LEMMA B (strict): a33(q) < 180° for every q ≥ 4, since (1 − 4cos(π/q))/3 > −1 ⟺ cos(π/q) < 1.
  *   - LEMMA E (the antiprism-pair equation): 2·a3q(q) + a33(r) ≠ 360° for ALL q, r ≥ 4. Via the closed
  *     forms, equality on the relevant branch is EXACTLY tan(π/2q) = 2·sin(π/2r); with g(q) = tan(π/2q) and
  *     h(r) = 2 sin(π/2r) strictly decreasing, the interleaving h(2q) < g(q) < h(2q−1) forces the real
  *     solution of h(r) = g(q) strictly inside (2q−1, 2q) — never an integer. Left inequality: with v = π/4q,
  *     2 sin v < 2 tan v ≤ tan 2v (v < 45°). Right inequality, all q ≥ 4, with x = π/2q ≤ π/8 and y =
  *     π/(4q−2): (i) 2y − x = π/(2q(2q−1)) ≥ x²/π since (2q)(2q−1) ≤ 4q²; (ii) y ≤ 4x/7 ⟺ q ≥ 4; (iii) sin y
  *     ≥ y − y³/6 (alternating series); (iv) tan x ≤ x/(1 − x²/2) = x + x³/(2 − x²) for 0 < x < √2 (sin x ≤ x
  *     and cos x ≥ 1 − x²/2 > 0). Then 2 sin y − tan x ≥ (2y − x) − y³/3 − x³/(2 − x²) ≥ x²/π − ((4/7)³/3 +
  *     0.5418)·x³ ≥ x²(1/π − 0.6041·x) > 0 for x ≤ π/8, as 0.6041·π/8 < 0.2373 < 1/π. The constant chain is
  *     re-verified by interval arithmetic in [[lemmaEConstants]], and the interleaving itself on a q-grid in
  *     the spec.
  *
  * With Lemma E the value-level analysis closes:
  *
  *   - T1 (prisms p > 500): the lateral (4·4) edge needs partners summing to 180 + 360/p ∈ (180, 180.72];
  *     partner chains 4→…→4 either sum to EXACTLY 180 (refuted: 360/p > 0) or miss the window by certified
  *     intervals; α-charged sums are irrational (Niven) and cannot equal the rational target.
  *   - T2 (antiprisms q > 300, and the sweep 101 ≤ q ≤ 300 above the fixpoint's target cap): the base (3·q)
  *     edge's q-gon face glues only P_q or A_q. With P_q: the rest is a 4→3 chain in (269−a3q)−90 < 90 + … —
  *     dead by intervals. With A_q: the rest is a 3→3 chain summing to 360 − 2·a3q(q) < 180 STRICTLY (Lemma
  *     A): the exact-180 completions (tet+oct, P3+P3) die by strictness, a lone A_r(3·3) is Lemma E, and an
  *     A_r(3·r)+A_r(r·3) pair sums > 180 strictly (Lemma A) — everything else misses by intervals.
  *   - The finite prism sweep 5 ≤ p ≤ 500 over the full pool re-finds EXACTLY the six prism-grid exotic
  *     families (surviving p = {5,7,9,10,15,18,20,24,42}) and excludes every other p — so the alphabet
  *     question is reduced, with no cap anywhere, to the six planar-species lifts (the remaining exclusion
  *     work).
  */
object TailExclusion:

  private val sqrt3 = Iv.point(3.0).sqrtIv

  private def uOf(q2: Double): Iv = Iv.pi / Iv.point(q2) // π / q2

  /** Closed form: a3q(q) in degrees, interval; also usable for a q-interval via q2 = 2q. */
  def a3qDeg(q: Int): Iv =
    val u = uOf(2.0 * q)
    Iv.toDeg(Iv.acosDec(-(Iv.sinGen(u) / Iv.cosGen(u)) / sqrt3))

  /** Closed form: a33(q) in degrees, interval. */
  def a33Deg(q: Int): Iv =
    val t = Iv.cosGen(uOf(q.toDouble))
    Iv.toDeg(Iv.acosDec((Iv.point(1.0) - Iv.point(4.0) * t) / Iv.point(3.0)))

  /** Enclosure of a3q over ALL q ≥ qMin: u ∈ (0, π/(2·qMin)]. */
  def a3qTail(qMin: Int): Iv =
    val uHi = uOf(2.0 * qMin)
    val tan = Iv(0.0, (Iv.sinGen(uHi) / Iv.cosGen(uHi)).hi)
    Iv.toDeg(Iv.acosDec(-(tan / sqrt3)))

  /** Enclosure of a33 over ALL q ≥ qMin. */
  def a33Tail(qMin: Int): Iv =
    val c = Iv(Iv.cosGen(uOf(qMin.toDouble)).lo, 1.0)
    Iv.toDeg(Iv.acosDec((Iv.point(1.0) - Iv.point(4.0) * c) / Iv.point(3.0)))

  /** The explicit-constant chain of Lemma E's right inequality, re-verified with intervals: (4/7)³/3 <
    * 0.0623, 1/(2 − (π/8)²) < 0.5418, their sum < 0.6041, and 1/π − 0.6041·(π/8) > 0 (with 0.6041·π/8 <
    * 0.2373).
    */
  lazy val lemmaEConstants: Boolean =
    val x0 = Iv.pi / Iv.point(8.0)
    val c1 = Iv.point(4.0 / 7.0) * Iv.point(4.0 / 7.0) * Iv.point(4.0 / 7.0) / Iv.point(3.0)
    val c2 = Iv.point(1.0) / (Iv.point(2.0) - x0 * x0)
    val cc = c1 + c2
    c1.hi < 0.0623 && c2.hi < 0.5418 && cc.hi < 0.6041 && (Iv.point(0.6041) * x0).hi < 0.2373 &&
    (Iv.point(1.0) / Iv.pi - Iv.point(0.6041) * x0).lo > 0

  /** Interleaving h(2q) < g(q) < h(2q−1) of Lemma E, certified for one q. */
  def interleaves(q: Int): Boolean =
    val g  = Iv.sinGen(uOf(2.0 * q)) / Iv.cosGen(uOf(2.0 * q))
    val h2 = Iv.point(2.0) * Iv.sinGen(uOf(4.0 * q))
    val h1 = Iv.point(2.0) * Iv.sinGen(uOf(4.0 * q - 2.0))
    h2.hi < g.lo && g.hi < h1.lo

  // ---------- the value classes of the full pool ----------

  /** Exact content of a class value: Some((rational degrees, α-charge)); None = certified interval only. */
  type Exact = Option[(Frac, Int)]

  final case class Cls(label: String, f1: Int, f2: Int, iv: Iv, exact: Exact, aTag: Char):
    // aTag: 'b' = antiprism base 3·q, 'l' = antiprism lateral 3·3, '.' = anything else
    def facesWith(open: Int): Option[Int] =
      if f1 == open then Some(f2) else if f2 == open then Some(f1) else None

  private val BIG = -1 // a face of size > the finite pool bound (tail prisms/antiprisms)

  val maxPoolPrism     = 500
  val maxPoolAntiprism = 300

  private lazy val alphaDeg = Iv.toDeg(Iv.atanInc(Iv.point(2.0).sqrtIv))

  lazy val pool: Vector[Cls] =
    val core      = HoneycombAlphabet.edgeTypes.map { et =>
      val iv = Iv.point(et.angle.r.toDouble) + alphaDeg * Iv.point(et.angle.n.toDouble)
      Cls(
        s"${et.cell.label}(${et.faces._1}·${et.faces._2})",
        et.faces._1,
        et.faces._2,
        iv,
        Some((Frac(et.angle.r.toLong, 1L), et.angle.n)),
        '.'
      )
    }
    val prismLat  = (5 to maxPoolPrism).filterNot(Set(6, 8, 12)).map { r =>
      val v = Frac(180L * r - 360L, r.toLong)
      Cls(s"P$r(4·4)", 4, 4, Iv.widen(v.toDouble, v.toDouble), Some((v, 0)), '.')
    }.toVector
    val prismHor  = (5 to maxPoolPrism).filterNot(Set(6, 8, 12)).map { r =>
      Cls(s"P$r(4·$r)", 4, r, Iv.point(90.0), Some((Frac(90, 1), 0)), '.')
    }.toVector
    val prismTail = Vector(
      // aTag 'p': a prism lateral 180 − 360/r, STRICTLY below 180 for every finite r
      Cls(s"P>${maxPoolPrism}(4·4)", 4, 4, Iv(180.0 - 360.0 / (maxPoolPrism + 1), 180.0), None, 'p'),
      Cls(s"P>${maxPoolPrism}(4·big)", 4, BIG, Iv.point(90.0), Some((Frac(90, 1), 0)), '.')
    )
    val antis     = (4 to maxPoolAntiprism).toVector.flatMap { r =>
      Vector(
        Cls(s"A$r(3·$r)", 3, r, a3qDeg(r), None, 'b'),
        Cls(s"A$r(3·3)", 3, 3, a33Deg(r), None, 'l')
      )
    }
    val antiTail  = Vector(
      Cls(s"A>${maxPoolAntiprism}(3·big)", 3, BIG, a3qTail(maxPoolAntiprism + 1), None, 'b'),
      Cls(s"A>${maxPoolAntiprism}(3·3)", 3, 3, a33Tail(maxPoolAntiprism + 1), None, 'l')
    )
    val outsiders = CertifiedDihedrals.outsiderConfigs.toVector.sortBy(_._1).flatMap { (name, cfg) =>
      CertifiedDihedrals.edgeTypesOf(cfg).toVector.map { (pair, iv) =>
        Cls(s"$name(${pair._1}·${pair._2})", pair._1, pair._2, iv, None, '.')
      }
    }
    core ++ prismLat ++ prismHor ++ prismTail ++ antis ++ antiTail ++ outsiders

  // ---------- windowed face-chain completion search ----------

  /** Pool classes carrying face f, as (class, other face), sorted by iv.lo. */
  private lazy val poolByFace: Map[Int, Vector[(Cls, Int)]] =
    pool
      .flatMap { cls =>
        if cls.f1 == cls.f2 then Vector((cls.f1, cls, cls.f2))
        else Vector((cls.f1, cls, cls.f2), (cls.f2, cls, cls.f1))
      }
      .groupMap(_._1)(t => (t._2, t._3))
      .view
      .mapValues(_.sortBy(_._1.iv.lo))
      .toMap

  /** Chains c₁…c_k (k ≤ maxLen) with faces startFace → … → endFace whose dihedral sum intersects `window`,
    * deduplicated up to reversal. Every partner contributes ≥ 60°, which prunes the sorted candidate lists.
    */
  def chainsInWindow(startFace: Int, endFace: Int, window: Iv, maxLen: Int): Vector[Vector[Cls]] =
    import scala.math.Ordering.Implicits.seqOrdering
    val found                                                         = collection.mutable.Map.empty[Vector[String], Vector[Cls]]
    def rec(open: Int, acc: List[Cls], sum: Iv, slotsLeft: Int): Unit =
      val budget = window.hi - sum.lo + 1e-9 // the chain may end at this slot: no reservation beyond it
      for (cls, other) <- poolByFace.getOrElse(open, Vector.empty).takeWhile(_._1.iv.lo <= budget) do
        val s2 = sum + cls.iv
        if other == endFace && s2.intersects(window) then
          val chain = (cls :: acc).reverse.toVector
          val key   = Vector(chain.map(_.label), chain.map(_.label).reverse).min
          found.getOrElseUpdate(key, chain)
        if slotsLeft > 1 && s2.lo + 60.0 <= window.hi + 1e-9 then rec(other, cls :: acc, s2, slotsLeft - 1)
    rec(startFace, Nil, Iv.point(0.0), maxLen)
    found.values.toVector

  private def exactSum(chain: Vector[Cls]): Exact =
    chain.foldLeft(Option((Frac(0, 1), 0))) { (acc, cls) =>
      for
        (v, n)   <- acc
        (v2, n2) <- cls.exact
      yield (v + v2, n + n2)
    }

  // ---------- targets and verdicts ----------

  enum Refutation:
    case AlphaIrrational           // charged exact sum vs rational target (Niven)
    case RationalMismatch          // exact rational sum, no admissible integer parameter
    case StrictLemmaA(sum: String) // exact sum 180 vs window strictly off 180 by a3q > 90
    case LemmaE                    // the antiprism-pair equation, refuted for all q, r ≥ 4
    case AboveLemmaA               // ≥1 antiprism base (> 90 each) + exact rest ⇒ sum > 180 strictly
    case StrictBelow180            // a lone prism-lateral tail: < 180 strictly vs window > 180 strictly
    case Unrefuted // would demand exact follow-up (expect none)

  final case class Threat(chain: Vector[Cls], refutation: Refutation):
    def show: String = chain.map(_.label).mkString(" + ") + s"  [$refutation]"

  final case class Verdict(target: String, threats: Vector[Threat]):
    def excluded: Boolean = threats.forall(_.refutation != Refutation.Unrefuted)

  /** T1 for one tail-prism window: partners of the lateral edge must equal 180 + 360/p for an integer p >
    * maxPoolPrism; every chain landing in the window must be refuted exactly.
    */
  lazy val prismTailVerdict: Vector[Threat] =
    val window = Iv(180.0, 180.0 + 360.0 / (maxPoolPrism + 1))
    chainsInWindow(4, 4, window, 3).map { chain =>
      val ref = exactSum(chain) match
        case Some((_, n)) if n != 0 => Refutation.AlphaIrrational
        case Some((v, 0))           =>
          val d = v - Frac(180, 1)
          if d.signum <= 0 then Refutation.RationalMismatch
          else
            val p = Frac(360, 1) / d // must be an integer > maxPoolPrism for the theorem to FAIL
            if p.den == 1 && p.num > maxPoolPrism then Refutation.Unrefuted
            else Refutation.RationalMismatch
        case _                      =>
          if chain.forall(_.aTag == 'p') then Refutation.StrictBelow180
          else Refutation.Unrefuted
      Threat(chain, ref)
    }

  /** T2 / antiprism sweep for one q-window: the base (3·q) edge; `a3q` is the certified enclosure of the
    * target's base dihedral (a single q or a tail corridor). Case P_q: rest is a 4→3 chain in 270 − a3q − ε;
    * case A_q: rest is a 3→3 chain in 360 − 2·a3q, strictly below 180 (Lemma A).
    */
  def antiprismVerdict(label: String, a3q: Iv): Verdict =
    val caseP = chainsInWindow(4, 3, Iv.point(270.0) - a3q, 3)
    val caseA = chainsInWindow(3, 3, Iv.point(360.0) - a3q - a3q, 3)
    val ts    =
      caseP.map(c => Threat(c, refuteAnti(c))) ++ caseA.map(c => Threat(c, refuteAnti(c)))
    Verdict(label, ts)

  private def refuteAnti(chain: Vector[Cls]): Refutation =
    val bases = chain.count(_.aTag == 'b')
    val rest  = chain.filter(_.aTag != 'b')
    if chain.size == 1 && chain(0).aTag == 'l' then Refutation.LemmaE
    else if bases >= 1 && rest.forall(_.exact.exists((v, n) => n == 0)) &&
      (rest.flatMap(_.exact).map(_._1).foldLeft(Frac(0, 1))(_ + _) - Frac(180 - 90 * bases, 1)).signum >= 0
    then
      // each antiprism base is > 90 strictly (Lemma A), so the sum exceeds 180 strictly while both windows
      // (270 − a3q and 360 − 2·a3q) are strictly below 180, again by Lemma A on the target
      Refutation.AboveLemmaA
    else
      exactSum(chain) match
        // 180 exactly (tet+oct with cancelling charges, P3+P3, …) sits on the boundary of both windows,
        // which are strictly off 180 by Lemma A — the only lattice point the windows can touch
        case Some((v, 0)) if v == Frac(180, 1) => Refutation.StrictLemmaA("exact 180")
        case _                                 => Refutation.Unrefuted

  /** The finite prism sweep, 5 ≤ p ≤ 500 non-core: which lateral edges complete over the FULL pool. */
  lazy val prismSweep
      : Vector[(Int, Vector[Threat])] = (5 to maxPoolPrism).filterNot(Set(6, 8, 12)).toVector.map { p =>
    val target = Frac(180L * p - 360L, p.toLong)
    val window = Iv.point(360.0) - Iv.widen(target.toDouble, target.toDouble)
    val ts     = chainsInWindow(4, 4, window, 4).map { chain =>
      val ref = exactSum(chain) match
        case Some((v, n)) if n != 0                     => Refutation.AlphaIrrational
        case Some((v, 0)) if v + target == Frac(360, 1) => Refutation.Unrefuted // genuine exact completion
        case Some((v, 0))                               => Refutation.RationalMismatch
        case _                                          => Refutation.Unrefuted
      Threat(chain, ref)
    }
    p -> ts.filter(_.refutation == Refutation.Unrefuted)
  }

  /** The antiprism sweep above the fixpoint's target cap: 101 ≤ q ≤ 300, plus the two tails. */
  lazy val results: (Vector[Int], Vector[(String, Boolean)]) =
    val prismsSurviving = prismSweep.filter(_._2.nonEmpty).map(_._1)
    val antiVerdicts    = (101 to maxPoolAntiprism).toVector.map(q => antiprismVerdict(s"A$q", a3qDeg(q))) :+
      antiprismVerdict(s"A>${maxPoolAntiprism}", a3qTail(maxPoolAntiprism + 1))
    val tailPrismOk     = prismTailVerdict.forall(_.refutation != Refutation.Unrefuted)
    (
      prismsSurviving,
      antiVerdicts.map(v => v.target -> v.threats.forall(_.refutation != Refutation.Unrefuted)) :+
        (s"P>${maxPoolPrism}" -> tailPrismOk)
    )
