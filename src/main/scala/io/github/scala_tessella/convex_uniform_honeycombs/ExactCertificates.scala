package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import MonoShell.{Glu, StarGeom, Vec}
import SpeciesEnumerator.species
import TransitivePatterns.{acceptedOf, developBall, fingerprintOf, Accepted, Iso, Mat, Pattern}

/** Exactness escalation of the pipeline geometry, from the species assembly to the audit: the certified
  * objects of the completeness theorem — the 34 vertex stars, the 28 class-representative patterns and their
  * periodization certificates — are upgraded from interval-midpoint doubles to EXACT arithmetic in ℚ(√2,√3),
  * the two-tier field of the programme.
  *
  * THE FRAME PROBLEM AND THE INTERNAL BASIS. The pipeline's ambient coordinates live in the seed corner's
  * frame, which is not field-aligned (corner circumradii involve nested radicals), and unit normalization
  * leaves the field (|v| = √(field element)). Both are avoided at once: all exact data is expressed in the
  * INTERNAL BASIS f = (u_a, u_b, u_c) of three independent star directions. Frame-independent quantities —
  * the Gram matrix ⟨u_x, u_y⟩ of the star directions — are field elements (in any exact honeycomb realization
  * every u_x is a difference of vertex positions in ℚ(√2,√3)³), and every other certified object is DERIVED
  * from them by field arithmetic: internal coordinates C_x of each direction solve the base Gram system,
  * isometries become matrices M with MᵀG₃M = G₃ (G₃ the base Gram), inner products are vᵀG₃w, and
  * parallelism/coplanarity tests are coordinate determinants. Nothing is ever normalized.
  *
  * RECOGNITION + EXACT VERIFICATION. The numeric pipeline is the discovery tool: each needed scalar (a Gram
  * entry, an internal coordinate of a gluing image) is RECOGNIZED as a + b√2 + c√3 + d√6 with small rational
  * coefficients from its double value, then every certificate condition is re-proven exactly on the
  * recognized objects — recognition errors cannot survive, because the exact identities they must satisfy
  * (Gram consistency of rank 3, G₃-orthogonality, back-vertex images, ring agreement, face-cycle closure,
  * translation words with identity rotational part, ball periodicity, stabilizer membership, lattice
  * invariance) are overdetermined. Gram entries are additionally certified against the assembly's interval
  * enclosures (the exact value must lie inside the certified interval).
  *
  * WHAT IS PROVEN EXACTLY, per class of the 28: (i) the exact star model — Gram recognized and
  * interval-contained, base minors positive, all n(n+1)/2 products C_xᵀG₃C_y equal to the Gram (rank-3
  * realizability); (ii) each pattern gluing is an exact isometry (MᵀG₃M = G₃) mapping the back-vertex
  * direction to −u_x and the neighbor ring onto the star ring (germ agreement up to positive scale, exact);
  * (iii) R1 and R2 hold exactly (reverse-pair products and face-cycle words are exact stabilizer elements,
  * face words with exactly zero translation); (iv) the three translation words compose to exact translations
  * (rotational part the identity matrix, exactly) with det(τ₁,τ₂,τ₃) ≠ 0; (v) the exact development of the
  * ball of radius R_per is collision-free (coinciding exact positions carry Stab-conjugate stars, exactly),
  * Λ-periodic under all ±τᵢ, Λ is invariant under all generator point parts (integer coordinates, exactly),
  * and every generator acts as a symmetry of the ball within R_per (the entry at the image position carries
  * the image star up to Stab±(S), exactly — checked, not derived: the development expands only the first word
  * reaching a position, so collision-freeness never compares g_x·w against the field); (vi) the coverage
  * inequality holds with exact rational upper bounds for the irrational norms (R_per := ub(covBound) +
  * ub(max|τ|) + 8/5 ≥ covBound + max|τ| + 3/2); (vii) for the two doubled species the two class
  * representatives have DIFFERENT exact canonical fingerprints over Stab±(S) — separation with no rounding.
  * Stabilizers themselves are exact: each is determined by the permutation of star directions it induces, its
  * matrix is read off the internal coordinates, and closure under products is verified.
  *
  * BEYOND THE REPRESENTATIVES. (viii) CLASS COHERENCE, for every accepted pattern within the caps
  * (`coherenceResults`, opt-in — an exact ball per pattern): the pattern's own certificate (i)–(vi) with its
  * ball reaching the representative's basis and determination ball, an exact element of Stab±(S) carrying the
  * representative's ball onto the pattern's at the determination radius, and the aligned representative basis
  * acting by exact symmetries on the pattern's ball — the three hypotheses of the coherence lemma. (ix) GERM
  * FORCING on every skeleton, the audit's test replayed on exact placements and positions; it forces exactly
  * the skeletons the numeric audit forces, and the ten it leaves are the ten the audit closes by exhaustion.
  * (x) THE EXHAUSTIONS replayed: on each of those ten skeletons every (R1)+(R2)-consistent pattern is
  * re-enumerated uncapped and every accepted one is cohered exactly with its class — so no honeycomb hides
  * beyond a cap, exactly.
  *
  * Together: every equality asserted by the periodization certificates of the 28, by the coherence of every
  * accepted pattern (within the caps and beyond them) and by the germ forcing of every forced skeleton is an
  * exact identity in ℚ(√2,√3) — every POSITIVE certificate on the theorem's critical path is exact
  * (inequalities are interval-certified or exact-sign decisions, and the numeric pipeline's identifications
  * are re-proven). What stays numeric: the enumerations' negative decisions (interval misses in the species
  * assembly; tolerance equality tests in the shell filter, the gluing atlas and the R1/R2 search).
  */
object ExactCertificates:

  import scala.math.Ordering.Implicits.seqOrdering

  // ---------- exact rationals (BigInt) ----------

  final case class Rat private (n: BigInt, d: BigInt):
    def +(o: Rat): Rat   = Rat.make(n * o.d + o.n * d, d * o.d)
    def -(o: Rat): Rat   = Rat.make(n * o.d - o.n * d, d * o.d)
    def *(o: Rat): Rat   = Rat.make(n * o.n, d * o.d)
    def /(o: Rat): Rat   = Rat.make(n * o.d, d * o.n)
    def unary_- : Rat    = Rat(-n, d)
    def signum: Int      = n.signum
    def isZero: Boolean  = n == 0
    def isInt: Boolean   = d == 1
    def toDouble: Double = BigDecimal(n).toDouble / BigDecimal(d).toDouble

  object Rat:
    val zero: Rat                       = Rat(BigInt(0), BigInt(1))
    val one: Rat                        = Rat(BigInt(1), BigInt(1))
    def ofInt(k: Long): Rat             = Rat(BigInt(k), BigInt(1))
    def make(n: BigInt, d: BigInt): Rat =
      require(d != 0, "Rat: zero denominator")
      if n == 0 then zero
      else
        val s = d.signum
        val g = n.gcd(d)
        Rat(s * n / g, s * d / g)
    def frac(n: Long, d: Long): Rat     = make(BigInt(n), BigInt(d))

    /** Exact value of the double (every finite double is a rational). */
    def fromDouble(x: Double): Rat =
      val bd = new java.math.BigDecimal(x)
      val u  = BigInt(bd.unscaledValue)
      val s  = bd.scale
      if s >= 0 then make(u, BigInt(10).pow(s)) else make(u * BigInt(10).pow(-s), BigInt(1))

  given Ordering[Rat] = (x, y) => (x.n * y.d).compare(y.n * x.d)

  // ---------- the quartic tower ℚ(√2,√3) ----------

  /** a + b√2 + c√3 + d√6, exact. */
  final case class Q23(a: Rat, b: Rat, c: Rat, d: Rat):
    def +(o: Q23): Q23  = Q23(a + o.a, b + o.b, c + o.c, d + o.d)
    def -(o: Q23): Q23  = Q23(a - o.a, b - o.b, c - o.c, d - o.d)
    def unary_- : Q23   = Q23(-a, -b, -c, -d)
    def *(o: Q23): Q23  =
      val two = Rat.frac(2, 1); val three = Rat.frac(3, 1); val six = Rat.frac(6, 1)
      Q23(
        a * o.a + two * (b * o.b) + three * (c * o.c) + six * (d * o.d),
        a * o.b + b * o.a + three * (c * o.d) + three * (d * o.c),
        a * o.c + c * o.a + two * (b * o.d) + two * (d * o.b),
        a * o.d + d * o.a + b * o.c + c * o.b
      )
    def isZero: Boolean = a.isZero && b.isZero && c.isZero && d.isZero
    def isRat: Boolean  = b.isZero && c.isZero && d.isZero

    /** Exact sign via x = A + B√3 with A = a + b√2, B = c + d√2 ∈ ℚ(√2). */
    def signum: Int =
      def sign2(p: Rat, q: Rat): Int =
        if q.isZero then p.signum
        else if p.isZero then q.signum
        else if p.signum == q.signum then p.signum
        else p.signum * (p * p - Rat.frac(2, 1) * (q * q)).signum
      val sA                         = sign2(a, b)
      val sB                         = sign2(c, d)
      if sB == 0 then sA
      else if sA == 0 then sB
      else if sA == sB then sA
      else // sign(A + B√3) = sign(A)·sign(A² − 3B²), A² − 3B² ≠ 0 since √3 ∉ ℚ(√2)
        val p = a * a + Rat.frac(2, 1) * (b * b) - Rat.frac(3, 1) * (c * c) - Rat.frac(6, 1) * (d * d)
        val q = Rat.frac(2, 1) * (a * b) - Rat.frac(6, 1) * (c * d)
        sA * sign2(p, q)

    /** Exact inverse: (A + B√3)⁻¹ = (A − B√3)/(A² − 3B²), then invert in ℚ(√2). */
    def inverse: Q23     =
      require(!isZero, "Q23: inverse of zero")
      // A² − 3B² = p + q√2
      val p   = a * a + Rat.frac(2, 1) * (b * b) - Rat.frac(3, 1) * (c * c) - Rat.frac(6, 1) * (d * d)
      val q   = Rat.frac(2, 1) * (a * b) - Rat.frac(6, 1) * (c * d)
      // (p + q√2)⁻¹ = (p − q√2)/(p² − 2q²)
      val nrm = p * p - Rat.frac(2, 1) * (q * q)
      val ip  = p / nrm
      val iq  = -q / nrm
      // (A − B√3)(ip + iq√2) = (a + b√2 − c√3 − d√6)(ip + iq√2)
      Q23(
        a * ip + Rat.frac(2, 1) * (b * iq),
        a * iq + b * ip,
        -(c * ip) - Rat.frac(2, 1) * (d * iq),
        -(c * iq) - (d * ip)
      )
    def toDouble: Double =
      a.toDouble + b.toDouble * math.sqrt(2.0) + c.toDouble * math.sqrt(3.0) + d.toDouble * math.sqrt(6.0)

  object Q23:
    val zero: Q23                                                 = Q23(Rat.zero, Rat.zero, Rat.zero, Rat.zero)
    val one: Q23                                                  = Q23(Rat.one, Rat.zero, Rat.zero, Rat.zero)
    def ofRat(r: Rat): Q23                                        = Q23(r, Rat.zero, Rat.zero, Rat.zero)
    def ofInt(k: Long): Q23                                       = ofRat(Rat.ofInt(k))
    def build(p: Long, q: Long, r: Long, s: Long, den: Long): Q23 =
      Q23(Rat.frac(p, den), Rat.frac(q, den), Rat.frac(r, den), Rat.frac(s, den))
    val sqrt2: Q23                                                = build(0, 1, 0, 0, 1)
    val sqrt3: Q23                                                = build(0, 0, 1, 0, 1)
    val sqrt6: Q23                                                = build(0, 0, 0, 1, 1)

  given Ordering[Q23] = Ordering.by(x => (x.a, x.b, x.c, x.d))

  // ---------- vectors and matrices over Q23 (internal coordinates) ----------

  type VQ = Vector[Q23] // length 3
  type MQ = Vector[VQ]  // 3 rows of 3

  private def vqAdd(u: VQ, v: VQ): VQ    = Vector(u(0) + v(0), u(1) + v(1), u(2) + v(2))
  private def vqSub(u: VQ, v: VQ): VQ    = Vector(u(0) - v(0), u(1) - v(1), u(2) - v(2))
  private def vqNeg(u: VQ): VQ           = Vector(-u(0), -u(1), -u(2))
  private def vqScale(u: VQ, s: Q23): VQ = Vector(u(0) * s, u(1) * s, u(2) * s)
  private val vqZero: VQ                 = Vector(Q23.zero, Q23.zero, Q23.zero)
  private def vqIsZero(u: VQ): Boolean   = u.forall(_.isZero)

  private def mRows(r1: VQ, r2: VQ, r3: VQ): MQ = Vector(r1, r2, r3)
  private def mCols(c1: VQ, c2: VQ, c3: VQ): MQ =
    mRows(
      Vector(c1(0), c2(0), c3(0)),
      Vector(c1(1), c2(1), c3(1)),
      Vector(c1(2), c2(2), c3(2))
    )
  private val idMQ: MQ                          =
    mRows(
      Vector(Q23.one, Q23.zero, Q23.zero),
      Vector(Q23.zero, Q23.one, Q23.zero),
      Vector(Q23.zero, Q23.zero, Q23.one)
    )
  private def mT(m: MQ): MQ                     = mCols(m(0), m(1), m(2))
  private def mVec(m: MQ, v: VQ): VQ            =
    Vector(
      m(0)(0) * v(0) + m(0)(1) * v(1) + m(0)(2) * v(2),
      m(1)(0) * v(0) + m(1)(1) * v(1) + m(1)(2) * v(2),
      m(2)(0) * v(0) + m(2)(1) * v(1) + m(2)(2) * v(2)
    )
  private def mMul(x: MQ, y: MQ): MQ            =
    Vector.tabulate(3, 3)((i, j) => x(i)(0) * y(0)(j) + x(i)(1) * y(1)(j) + x(i)(2) * y(2)(j))
  private def det3(m: MQ): Q23                  =
    m(0)(0) *
      (m(1)(1) * m(2)(2) - m(1)(2) * m(2)(1)) -
      m(0)(1) *
      (m(1)(0) * m(2)(2) - m(1)(2) * m(2)(0)) +
      m(0)(2) *
      (m(1)(0) * m(2)(1) - m(1)(1) * m(2)(0))
  private def detCols(a: VQ, b: VQ, c: VQ): Q23 = det3(mCols(a, b, c))
  private def mInv(m: MQ): MQ                   =
    val dt                       = det3(m)
    require(!dt.isZero, "MQ: singular")
    val di                       = dt.inverse
    def cof(i: Int, j: Int): Q23 =
      val (r1, r2) = ((0 to 2).filter(_ != i): @unchecked) match { case Seq(x, y) => (x, y) }
      val (c1, c2) = ((0 to 2).filter(_ != j): @unchecked) match { case Seq(x, y) => (x, y) }
      val minor    = m(r1)(c1) * m(r2)(c2) - m(r1)(c2) * m(r2)(c1)
      if (i + j) % 2 == 0 then minor * di else -(minor * di)
    Vector.tabulate(3, 3)((i, j) => cof(j, i)) // adjugate transpose

  /** Cramer solve m·x = rhs, exact. */
  private def solve3(m: MQ, rhs: VQ): VQ =
    val dt = det3(m)
    require(!dt.isZero, "solve3: singular")
    val di = dt.inverse
    Vector.tabulate(3) { j =>
      val cols = Vector.tabulate(3)(k => if k == j then rhs else Vector(m(0)(k), m(1)(k), m(2)(k)))
      detCols(cols(0), cols(1), cols(2)) * di
    }

  /** Coordinate parallelism (basis-independent): all 2×2 minors vanish. */
  private def vqParallel(u: VQ, v: VQ): Boolean = (u(0) * v(1) - u(1) * v(0)).isZero &&
    (u(0) * v(2) - u(2) * v(0)).isZero &&
    (u(1) * v(2) - u(2) * v(1)).isZero

  // ---------- recognition: double → ℚ(√2,√3) ----------

  private val s2d = math.sqrt(2.0)
  private val s3d = math.sqrt(3.0)
  private val s6d = math.sqrt(6.0)

  private val recogDens           = Vector(1L, 2L, 3L, 4L, 6L, 8L, 12L, 24L)
  private val recogBound          = 10
  private val recogTol            = 1e-8
  private val recogCache          = collection.mutable.HashMap.empty[Long, Option[Q23]]
  private def recogKey(x: Double) = math.round(x * 4e9)

  /** Recognize x as a + b√2 + c√3 + d√6 with denominators dividing 24 and small numerators; None (and a
    * caller-side flag) if no candidate — or more than one distinct candidate — fits within 1e-8.
    */
  def recognize(x: Double): Option[Q23] =
    recogCache.getOrElseUpdate(
      recogKey(x), {
        val cands = collection.mutable.LinkedHashSet.empty[Q23]
        for den <- recogDens do
          val y = x * den
          var q = -recogBound
          while q <= recogBound do
            var r = -recogBound
            while r <= recogBound do
              val rem1 = y - q * s2d - r * s3d
              var s    = -recogBound
              while s <= recogBound do
                val rem2 = rem1 - s * s6d
                val p    = math.round(rem2)
                if math.abs(rem2 - p) < recogTol * den then cands += Q23.build(p, q, r, s, den)
                s += 1
              r += 1
            q += 1
        cands.toList match
          case single :: Nil => Some(single)
          case _             => None // zero or ambiguous — caller flags
      }
    )

  // ---------- the exact star model ----------

  /** The exact model of a species' star: Gram matrix of the n unit directions, a base triple with positive-
    * definite base Gram g3, and internal coordinates C_x of every direction (u_x = Σ C_x(i) f_i). The model
    * is certified by: every Gram entry inside its interval enclosure, diag exactly 1, g3 minors positive, and
    * C_xᵀ g3 C_y = gram(x)(y) for ALL pairs (rank-3 realizability of the whole Gram).
    */
  final case class ExactStar(
      idx: Int,
      n: Int,
      gram: Vector[Vector[Q23]],
      base: (Int, Int, Int),
      g3: MQ,
      coords: Vector[VQ],
      maxIvWidth: Double
  ):
    def inner(u: VQ, v: VQ): Q23 = // ⟨u, v⟩ through the base Gram
      val gv = mVec(g3, v)
      u(0) * gv(0) + u(1) * gv(1) + u(2) * gv(2)
    def norm2(u: VQ): Q23        = inner(u, u)

  final case class StarReport(idx: Int, n: Int, ok: Boolean, maxIvWidth: Double, msg: String)

  def buildStar(idx: Int, flags: collection.mutable.ListBuffer[String]): Either[String, ExactStar] =
    val sp                    = species(idx)
    val pos                   = sp.state.positions
    val n                     = pos.size
    val mids                  = sp.state.posMid
    def ivDot(i: Int, j: Int) =
      val (a, b) = (pos(i), pos(j))
      a._1 * b._1 + a._2 * b._2 + a._3 * b._3
    // recognize the Gram, certify interval containment
    var maxW                  = 0.0
    val gram                  = Array.ofDim[Q23](n, n)
    var err                   = Option.empty[String]
    for i <- 0 until n if err.isEmpty; j <- i until n if err.isEmpty do
      val iv = ivDot(i, j)
      maxW = math.max(maxW, iv.width)
      recognize(iv.mid) match
        case None    => err = Some(f"gram($i)($j) unrecognized (mid=${iv.mid}%.12f)")
        case Some(q) =>
          val lo = q - Q23.ofRat(Rat.fromDouble(iv.lo))
          val hi = Q23.ofRat(Rat.fromDouble(iv.hi)) - q
          if lo.signum < 0 || hi.signum < 0 then err = Some(s"gram($i)($j) outside its interval")
          else
            gram(i)(j) = q
            gram(j)(i) = q
    err match
      case Some(e) => Left(s"species $idx: $e")
      case None    =>
        if (0 until n).exists(i => !(gram(i)(i) - Q23.one).isZero) then
          Left(s"species $idx: non-unit diagonal")
        else
          // base triple: maximize |det| of double midpoints for conditioning
          def dDet(i: Int, j: Int, k: Int): Double =
            val (a, b, c) = (mids(i), mids(j), mids(k))
            a._1 *
              (b._2 * c._3 - b._3 * c._2) -
              a._2 *
              (b._1 * c._3 - b._3 * c._1) +
              a._3 *
              (b._1 * c._2 - b._2 * c._1)
          val triples                              =
            for i <- 0 until n; j <- i + 1 until n; k <- j + 1 until n yield (i, j, k)
          val (bi, bj, bk)                         = triples.maxBy((i, j, k) => math.abs(dDet(i, j, k)))
          val g3                                   = mRows(
            Vector(gram(bi)(bi), gram(bi)(bj), gram(bi)(bk)),
            Vector(gram(bj)(bi), gram(bj)(bj), gram(bj)(bk)),
            Vector(gram(bk)(bi), gram(bk)(bj), gram(bk)(bk))
          )
          val m1                                   = g3(0)(0)
          val m2                                   = g3(0)(0) * g3(1)(1) - g3(0)(1) * g3(1)(0)
          val m3                                   = det3(g3)
          if m1.signum <= 0 || m2.signum <= 0 || m3.signum <= 0 then
            Left(s"species $idx: base Gram not positive definite")
          else
            val coords = Vector.tabulate(n) { x =>
              solve3(g3, Vector(gram(bi)(x), gram(bj)(x), gram(bk)(x)))
            }
            val star   = ExactStar(idx, n, gram.map(_.toVector).toVector, (bi, bj, bk), g3, coords, maxW)
            val rankOk = (0 until n).forall { x =>
              (x until n).forall(y => (star.inner(coords(x), coords(y)) - gram(x)(y)).isZero)
            }
            if !rankOk then Left(s"species $idx: Gram not rank-3 consistent")
            else Right(star)

  // ---------- the numeric ↔ exact bridge ----------

  /** Solves ambient double vectors into internal coordinates (double Cramer against the base-triple
    * midpoints) and recognizes them as field elements.
    */
  final class Bridge(star: ExactStar, g: StarGeom):
    private val (bi, bj, bk)                                                                     = star.base
    private val (fa, fb, fc)                                                                     = (g.u(bi), g.u(bj), g.u(bk))
    private def dDet(a: Vec, b: Vec, c: Vec): Double                                             =
      a._1 *
        (b._2 * c._3 - b._3 * c._2) -
        a._2 *
        (b._1 * c._3 - b._3 * c._1) +
        a._3 *
        (b._1 * c._2 - b._2 * c._1)
    private val d0                                                                               = dDet(fa, fb, fc)
    def coordsOf(v: Vec, flags: collection.mutable.ListBuffer[String], what: String): Option[VQ] =
      val ca = dDet(v, fb, fc) / d0
      val cb = dDet(fa, v, fc) / d0
      val cc = dDet(fa, fb, v) / d0
      val rs = Vector(ca, cb, cc).map(recognize)
      if rs.exists(_.isEmpty) then
        flags += s"unrecognized internal coordinate ($what)"
        None
      else Some(rs.map(_.get))

  // ---------- exact stabilizers ----------

  /** Exact Stab(S): each numeric stabilizer permutes the star directions; its exact matrix is read off the
    * internal coordinates of the permuted base triple. Certified: M C_x = C_π(x) for all x, MᵀG₃M = G₃, the
    * set contains the identity and is closed under products.
    */
  def exactStab(
      star: ExactStar,
      g: StarGeom,
      stabNum: Vector[Mat],
      flags: collection.mutable.ListBuffer[String]
  ): Either[String, Vector[MQ]] =
    val n                                   = star.n
    def permOf(m: Mat): Option[Vector[Int]] =
      val imgs = (0 until n).map(x => m(g.u(x)))
      val p    = imgs.map { im =>
        (0 until n).find { y =>
          val d = (im._1 - g.u(y)._1, im._2 - g.u(y)._2, im._3 - g.u(y)._3)
          math.sqrt(d._1 * d._1 + d._2 * d._2 + d._3 * d._3) < 1e-6
        }
      }
      if p.exists(_.isEmpty) then None else Some(p.map(_.get).toVector)
    val perms                               = stabNum.map(permOf)
    if perms.exists(_.isEmpty) then Left("a stabilizer does not permute the star directions")
    else
      val (bi, bj, bk) = star.base
      val mats         = perms.map(_.get).map { p =>
        mCols(star.coords(p(bi)), star.coords(p(bj)), star.coords(p(bk)))
      }
      val mapOk        = perms.map(_.get).zip(mats).forall { (p, m) =>
        (0 until n).forall(x => mVec(m, star.coords(x)) == star.coords(p(x)))
      }
      val orthoOk      = mats.forall(m => mMul(mMul(mT(m), star.g3), m) == star.g3)
      val hasId        = mats.contains(idMQ)
      val closed       = mats.forall(a => mats.forall(b => mats.contains(mMul(a, b))))
      if !mapOk then Left("stabilizer matrix does not reproduce its permutation")
      else if !orthoOk then Left("stabilizer not G3-orthogonal")
      else if !hasId then Left("stabilizer set has no identity")
      else if !closed then Left("stabilizer set not closed under products")
      else Right(mats)

  private def inStabE(m: MQ, stabE: Vector[MQ]): Boolean = stabE.contains(m)

  // ---------- exact gluings ----------

  /** An exact atlas gluing at tiling-vertex x: the isometry p ↦ M p + C_x in internal coordinates. */
  final case class EGlu(x: Int, y: Int, m: MQ, mInvM: MQ)

  /** Interior direction of the face germ (v, w) at the edge through u_v: component of u_w ⊥ u_v, unnormalized
    * — germs are compared up to positive scale, exactly.
    */
  private def interiorE(star: ExactStar, v: Int, w: Int): VQ =
    vqSub(star.coords(w), vqScale(star.coords(v), star.gram(w)(v)))

  /** Germ equality up to positive scale: parallel coordinates with positive inner product. */
  private def germEqE(star: ExactStar, p1: VQ, p2: VQ): Boolean =
    vqParallel(p1, p2) && star.inner(p1, p2).signum > 0

  /** The ring descriptors at tiling-vertex v, as (cell ordinal, the two interior directions), transported by
    * `m` (identity for the base star). The face plane is span(edge, interior), so the interior direction
    * alone determines the germ at the shared edge.
    */
  private def ringDescE(star: ExactStar, g: StarGeom, v: Int, m: MQ): Vector[(Int, Vector[VQ])] =
    g.rings(v).map { (ci, aIn, aOut) =>
      val germs = Vector(aIn, aOut).map { arc =>
        val w = arc._1 + arc._2 - v
        mVec(m, interiorE(star, v, w))
      }
      (g.cellOrd(ci), germs)
    }

  private def descMatchE(
      star: ExactStar,
      a: Vector[(Int, Vector[VQ])],
      b: Vector[(Int, Vector[VQ])]
  ): Boolean =
    a.size == b.size && {
      val used = Array.fill(b.size)(false)
      a.forall { (ca, ga) =>
        val j = b.indices.find { j =>
          !used(j) && b(j)._1 == ca && {
            val Vector(g1, g2) = ga
            val Vector(h1, h2) = b(j)._2
            (germEqE(star, g1, h1) && germEqE(star, g2, h2)) ||
            (germEqE(star, g1, h2) && germEqE(star, g2, h1))
          }
        }
        j.foreach(used(_) = true)
        j.isDefined
      }
    }

  /** Build and certify the exact gluing: recognize the internal coordinates of all images rot(u_z), read M
    * off the base triple, then verify MᵀG₃M = G₃, M C_z = R_z for every z, R_y = −C_x (the back-vertex
    * condition), and exact ring agreement around the glued edge.
    */
  def exactGlu(
      star: ExactStar,
      g: StarGeom,
      bridge: Bridge,
      x: Int,
      glu: Glu,
      flags: collection.mutable.ListBuffer[String]
  ): Either[String, EGlu] =
    val n    = star.n
    val imgs = (0 until n).map { z =>
      bridge.coordsOf(glu.rot(g.u(z)), flags, s"glu image x=$x z=$z")
    }
    if imgs.exists(_.isEmpty) then Left(s"gluing at $x: unrecognized image")
    else
      val rs           = imgs.map(_.get).toVector
      val (bi, bj, bk) = star.base
      val m            = mCols(rs(bi), rs(bj), rs(bk))
      val orthoOk      = mMul(mMul(mT(m), star.g3), m) == star.g3
      val mapOk        = (0 until n).forall(z => mVec(m, star.coords(z)) == rs(z))
      val backOk       = rs(glu.y) == vqNeg(star.coords(x))
      val ringOk       =
        descMatchE(star, ringDescE(star, g, x, idMQ), ringDescE(star, g, glu.y, m))
      if !orthoOk then Left(s"gluing at $x: not G3-orthogonal")
      else if !mapOk then Left(s"gluing at $x: image mismatch")
      else if !backOk then Left(s"gluing at $x: back-vertex image is not −u_x")
      else if !ringOk then Left(s"gluing at $x: ring descriptors do not match")
      else Right(EGlu(x, glu.y, m, mInv(m)))

  // ---------- exact isometries ----------

  final case class EIso(m: MQ, t: VQ):
    def compose(o: EIso): EIso = EIso(mMul(m, o.m), vqAdd(mVec(m, o.t), t))

  private val idEIso = EIso(idMQ, vqZero)

  // ---------- R1, R2 (exact) ----------

  private def r1Exact(glus: Vector[EGlu], stabE: Vector[MQ]): Boolean =
    glus.indices.forall(x => inStabE(mMul(glus(x).m, glus(glus(x).y).m), stabE))

  /** Exact face-cycle words: mirror of TransitivePatterns.walkFace on internal coordinates — the interior
    * germ is transported by M⁻¹ and matched up to positive scale; the closed word must be an exact stabilizer
    * element with exactly zero translation.
    */
  private def r2Exact(star: ExactStar, g: StarGeom, glus: Vector[EGlu], stabE: Vector[MQ]): Boolean =
    def arcsAt(v: Int): Vector[Int] =
      g.st.arcs.keys.toVector.collect {
        case (a, b) if a == v => b
        case (a, b) if b == v => a
      }
    g.st.arcs.keys.toVector.sorted.forall { key =>
      val p        = g.st.arcs(key).face
      var exit     = key._1
      var interior = interiorE(star, key._1, key._2)
      var word     = idEIso
      var steps    = 0
      var ok       = true
      while steps < p && ok do
        val glu = glus(exit)
        word = word.compose(EIso(glu.m, star.coords(exit)))
        val dI  = mVec(glu.mInvM, interior)
        val y   = glu.y
        arcsAt(y).find(z => germEqE(star, interiorE(star, y, z), dI)) match
          case None    => ok = false
          case Some(z) =>
            exit = z
            interior = interiorE(star, z, y)
        steps += 1
      ok && vqIsZero(word.t) && inStabE(word.m, stabE)
    }

  // ---------- translation words (numeric discovery, exact verification) ----------

  private def dIsId(m: Mat): Boolean    = m.dist(TransitivePatterns.Mat(
    (1.0, 0.0, 0.0),
    (0.0, 1.0, 0.0),
    (0.0, 0.0, 1.0)
  )) < 1e-6
  private def dNorm(v: Vec): Double     =
    math.sqrt(v._1 * v._1 + v._2 * v._2 + v._3 * v._3)
  private def dSub(a: Vec, b: Vec): Vec = (a._1 - b._1, a._2 - b._2, a._3 - b._3)

  /** Translation words with their generator sequences (composition order), numeric — the discovery step. */
  private def translationWordsW(pat: Pattern, maxLen: Int): Vector[(Vec, List[Int])] =
    val gens                                      = pat.g.u.indices.map(pat.iso).toVector
    val found                                     = collection.mutable.ArrayBuffer.empty[(Vec, List[Int])]
    def rec(t: Iso, w: List[Int], len: Int): Unit =
      if dIsId(t.m) && dNorm(t.t) > 1e-4 && !found.exists(v => dNorm(dSub(v._1, t.t)) < 1e-4) then
        found += ((t.t, w.reverse))
      if len < maxLen then gens.indices.foreach(i => rec(t.compose(gens(i)), i :: w, len + 1))
    rec(
      Iso(TransitivePatterns.Mat((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)), (0.0, 0.0, 0.0)),
      Nil,
      0
    )
    found.toVector.sortBy(v => dNorm(v._1))

  private def numericBasisWords(pat: Pattern): Option[Vector[List[Int]]] =
    def dDet(a: Vec, b: Vec, c: Vec): Double =
      a._1 *
        (b._2 * c._3 - b._3 * c._2) -
        a._2 *
        (b._1 * c._3 - b._3 * c._1) +
        a._3 *
        (b._1 * c._2 - b._2 * c._1)
    LazyList(4, 5, 6, 7).map { l =>
      val ts = translationWordsW(pat, l)
      for
        t1 <- ts.headOption
        t2 <- ts.find { t =>
                val pr = t1._1._1 * t._1._1 + t1._1._2 * t._1._2 + t1._1._3 * t._1._3
                val nn = t1._1._1 * t1._1._1 + t1._1._2 * t1._1._2 + t1._1._3 * t1._1._3
                dNorm(dSub(t._1, (t1._1._1 * pr / nn, t1._1._2 * pr / nn, t1._1._3 * pr / nn))) > 1e-3
              }
        t3 <- ts.find(t => math.abs(dDet(t1._1, t2._1, t._1)) > 1e-3)
      yield Vector(t1._2, t2._2, t3._2)
    }.collectFirst { case Some(ws) => ws }

  // ---------- rational upper bounds for irrational norms ----------

  /** A rational u with u² ≥ q (q ≥ 0 exact), for the coverage arithmetic. */
  private def sqrtUB(q: Q23): Rat =
    require(q.signum >= 0, "sqrtUB: negative")
    var x = math.sqrt(math.max(0.0, q.toDouble)) * (1 + 1e-9) + 1e-12
    var r = Rat.fromDouble(x)
    while (Q23.ofRat(r * r) - q).signum < 0 do
      x *= 1.0000001
      r = Rat.fromDouble(x)
    r

  // ---------- exact ball development ----------

  final private case class BallEntry(iso: EIso, inv: MQ, seenMs: collection.mutable.ArrayBuffer[MQ])

  /** Exact mirror of TransitivePatterns.developBall: BFS over words with exact position keys; coinciding
    * positions must carry Stab-conjugate stars (exact matrix identities). Left = collision.
    */
  private def developExact(
      star: ExactStar,
      isos: Vector[EIso],
      stabE: Vector[MQ],
      slack2: Q23,
      maxDepth: Int
  ): Either[String, collection.mutable.HashMap[VQ, BallEntry]] =
    val seen     = collection.mutable.HashMap.empty[VQ, BallEntry]
    var frontier = Vector(idEIso)
    seen(vqZero) = BallEntry(idEIso, idMQ, collection.mutable.ArrayBuffer(idMQ))
    var depth    = 0
    var clash    = false
    while frontier.nonEmpty && depth < maxDepth && !clash do
      frontier = frontier.flatMap { t =>
        isos.flatMap { gx =>
          if clash then None
          else
            val t2 = t.compose(gx)
            if (slack2 - star.norm2(t2.t)).signum >= 0 then
              seen.get(t2.t) match
                case Some(e) =>
                  if !e.seenMs.contains(t2.m) then
                    if inStabE(mMul(e.inv, t2.m), stabE) then e.seenMs += t2.m
                    else clash = true
                  None
                case None    =>
                  seen(t2.t) = BallEntry(t2, mInv(t2.m), collection.mutable.ArrayBuffer(t2.m))
                  Some(t2)
            else None
        }
      }
      depth += 1
    if clash then Left("development collision (exact)") else Right(seen)

  // ---------- the per-class certificate ----------

  final case class ClassReport(
      idx: Int,
      classIdx: Int,
      gluOk: Boolean,                 // every pattern gluing exact: G3-orthogonal, back-vertex, ring agreement
      r1Ok: Boolean,
      r2Ok: Boolean,
      transOk: Boolean,               // three words compose to exact translations (rotational part = identity)
      indepOk: Boolean,               // det(τ1, τ2, τ3) ≠ 0 exactly
      ballVerts: Int,
      collisionFree: Boolean,
      periodic: Boolean,
      latInv: Boolean,
      coverage: Boolean,
      genEquiv: Boolean = false,      // every generator is an exact symmetry of the ball within R_per
      taus: Vector[VQ] = Vector.empty // the exact certified lattice basis (internal coordinates)
  ):
    def ok: Boolean =
      gluOk && r1Ok && r2Ok && transOk && indepOk && collisionFree && periodic && latInv && coverage &&
        genEquiv

  /** A certified pattern with the exact ball its certificate was verified on, the radius R_per within which
    * the developed field is the honeycomb, and the rational bound ub(covBound) of its lattice.
    */
  final private case class Certified(
      report: ClassReport,
      ball: collection.mutable.HashMap[VQ, BallEntry],
      rPer2: Q23,
      covUB: Rat
  )

  /** Does the isometry `iso` act as a symmetry of the certified ball: every entry whose image stays within
    * R_per finds at the image position an entry whose star is the image star up to Stab±(S), exactly. With
    * the generators this is generator equivariance; with ±τ for a lattice basis it is Λ-periodicity; with a
    * transported basis it is the lattice-transport check of coherence.
    */
  private def isometrySymmetryExact(
      star: ExactStar,
      stabE: Vector[MQ],
      ball: collection.mutable.HashMap[VQ, BallEntry],
      rPer2: Q23,
      iso: EIso
  ): Boolean =
    ball.values.forall { e =>
      val img = iso.compose(e.iso)
      if (rPer2 - star.norm2(img.t)).signum < 0 then true
      else
        ball.get(img.t) match
          case None     => false
          case Some(e2) => inStabE(mMul(e2.inv, img.m), stabE)
    }

  private def translationSymmetryExact(
      star: ExactStar,
      stabE: Vector[MQ],
      ball: collection.mutable.HashMap[VQ, BallEntry],
      rPer2: Q23,
      t: VQ
  ): Boolean = isometrySymmetryExact(star, stabE, ball, rPer2, EIso(idMQ, t))

  /** The exact periodization certificate of one pattern. `reach` is a floor on the translation lengths the
    * certified ball must accommodate beyond the pattern's own basis and `floor` a floor on R_per itself (both
    * zero for a class representative; the representative's ub(max|τ|) and ub(covBound) for a member under
    * coherence): R_per = max(ub(covBound) + max(ub(max|τ|), reach), floor) + 8/5.
    */
  private def certifyPattern(
      star: ExactStar,
      g: StarGeom,
      bridge: Bridge,
      stabE: Vector[MQ],
      pat: Pattern,
      idx: Int,
      classIdx: Int,
      reach: Rat,
      floor: Rat,
      what: String,
      flags: collection.mutable.ListBuffer[String]
  ): (ClassReport, Option[Certified]) =
    def fail(msg: String): (ClassReport, Option[Certified]) =
      flags += s"$what: $msg"
      (ClassReport(idx, classIdx, false, false, false, false, false, 0, false, false, false, false), None)
    val glusE                                               = pat.glus.indices.toVector.map(x => exactGlu(star, g, bridge, x, pat.glus(x), flags))
    glusE.find(_.isLeft).flatMap(_.left.toOption) match
      case Some(e) => fail(e)
      case None    =>
        val glus = glusE.map(_.toOption.get)
        val r1   = r1Exact(glus, stabE)
        val r2   = r2Exact(star, g, glus, stabE)
        val isos = glus.map(gl => EIso(gl.m, star.coords(gl.x)))
        numericBasisWords(pat) match
          case None        => fail("no translation basis found numerically")
          case Some(words) =>
            val taus    = words.map(_.foldLeft(idEIso)((acc, x) => acc.compose(isos(x))))
            val transOk = taus.forall(t => t.m == idMQ && !vqIsZero(t.t))
            val tv      = taus.map(_.t)
            val indep   = transOk && !detCols(tv(0), tv(1), tv(2)).isZero
            if !transOk || !indep then
              flags += s"$what: translation words not exact translations"
              (ClassReport(idx, classIdx, true, r1, r2, transOk, indep, 0, false, false, false, false), None)
            else
              // coverage radii: exact rational upper bounds for the irrational norms
              val diagUB   = (for
                e1 <- Vector(Q23.one, -Q23.one)
                e2 <- Vector(Q23.one, -Q23.one)
                e3 <- Vector(Q23.one, -Q23.one)
              yield
                val d = vqAdd(vqAdd(vqScale(tv(0), e1), vqScale(tv(1), e2)), vqScale(tv(2), e3))
                sqrtUB(star.norm2(d))
              ).max
              val covUB    = diagUB / Rat.frac(2, 1)
              val maxTUB   = tv.map(t => sqrtUB(star.norm2(t))).max
              val rPer     = Vector(covUB + Vector(maxTUB, reach).max, floor).max + Rat.frac(8, 5)
              val coverage = (rPer - (covUB + maxTUB + Rat.frac(3, 2))).signum >= 0
              val slack    = rPer + Rat.frac(8, 5)
              val slack2   = Q23.ofRat(slack * slack)
              val rPer2    = Q23.ofRat(rPer * rPer)
              val maxDepth = math.max(14, (3.0 * slack.toDouble).toInt)
              developExact(star, isos, stabE, slack2, maxDepth) match
                case Left(e)     =>
                  flags += s"$what: $e"
                  (
                    ClassReport(idx, classIdx, true, r1, r2, true, true, 0, false, false, false, coverage),
                    None
                  )
                case Right(ball) =>
                  val periodic = tv.flatMap(t => Vector(t, vqNeg(t))).forall { tau =>
                    translationSymmetryExact(star, stabE, ball, rPer2, tau)
                  }
                  val tmat     = mCols(tv(0), tv(1), tv(2))
                  val latInv   = glus.forall { gl =>
                    tv.forall { tau =>
                      solve3(tmat, mVec(gl.m, tau)).forall(c => c.isRat && c.a.isInt)
                    }
                  }
                  val genEquiv = isos.forall(iso => isometrySymmetryExact(star, stabE, ball, rPer2, iso))
                  if !periodic then flags += s"$what: ball not periodic (exact)"
                  if !latInv then flags += s"$what: lattice not generator-invariant (exact)"
                  if !genEquiv then flags += s"$what: a generator is not a symmetry of the ball (exact)"
                  val report   = ClassReport(
                    idx,
                    classIdx,
                    true,
                    r1,
                    r2,
                    true,
                    true,
                    ball.size,
                    true,
                    periodic,
                    latInv,
                    coverage,
                    genEquiv,
                    tv
                  )
                  (report, Option.when(report.ok)(Certified(report, ball, rPer2, covUB)))

  // ---------- exact class coherence (every accepted pattern) ----------

  /** Coherence of one accepted pattern with its class representative, exact — the three checks of the
    * coherence lemma: `certified`, the pattern's own periodization certificate with its ball reaching the
    * representative's basis and its determination ball; `aligned`, an element of Stab±(S) carrying the
    * representative's exact ball onto the pattern's at the determination radius ub(covBound*) + 3/2; and
    * `transported`, the aligned representative basis acting by symmetries on the pattern's certified ball.
    */
  final case class CoherenceReport(
      idx: Int,
      classIdx: Int,
      skeleton: Int,
      certified: Boolean,
      aligned: Boolean,
      transported: Boolean
  ):
    def ok: Boolean = certified && aligned && transported

  private def encodeExact(
      star: ExactStar,
      stabE: Vector[MQ],
      ball: collection.mutable.HashMap[VQ, BallEntry],
      r2: Q23,
      s: MQ
  ): Vector[Vector[Q23]] =
    def canonStar(m: MQ): Vector[Q23] = stabE.map(s2 => mMul(m, s2).flatten).min
    ball.values.toVector
      .filter(e => (r2 - star.norm2(e.iso.t)).signum >= 0)
      .map(e => mVec(s, e.iso.t) ++ canonStar(mMul(s, e.iso.m)))
      .sorted

  private def cohereExact(
      star: ExactStar,
      g: StarGeom,
      bridge: Bridge,
      stabE: Vector[MQ],
      pat: Pattern,
      rep: Certified,
      idx: Int,
      classIdx: Int,
      skeleton: Int,
      flags: collection.mutable.ListBuffer[String]
  ): CoherenceReport =
    val what         = s"species $idx class $classIdx skeleton $skeleton coherence"
    val taus         = rep.report.taus
    val maxTUB       = taus.map(t => sqrtUB(star.norm2(t))).max
    val rDet         = rep.covUB + Rat.frac(3, 2)
    val rDet2        = Q23.ofRat(rDet * rDet)
    val (_, certOpt) =
      certifyPattern(star, g, bridge, stabE, pat, idx, classIdx, maxTUB, rep.covUB, what, flags)
    certOpt match
      case None      => CoherenceReport(idx, classIdx, skeleton, false, false, false)
      case Some(own) =>
        val target = encodeExact(star, stabE, own.ball, rDet2, idMQ)
        stabE.find(s => encodeExact(star, stabE, rep.ball, rDet2, s) == target) match
          case None    =>
            flags += s"$what: no exact alignment with the class representative"
            CoherenceReport(idx, classIdx, skeleton, true, false, false)
          case Some(s) =>
            val transported = taus.forall { tau =>
              val t = mVec(s, tau)
              translationSymmetryExact(star, stabE, own.ball, own.rPer2, t) &&
              translationSymmetryExact(star, stabE, own.ball, own.rPer2, vqNeg(t))
            }
            if !transported then
              flags += s"$what: the aligned representative lattice is not a symmetry of the pattern's ball"
            CoherenceReport(idx, classIdx, skeleton, true, true, transported)

  // ---------- exact germ forcing (every skeleton) ----------

  final case class GermReport(idx: Int, skeleton: Int, forced: Boolean)

  /** The germ-forcing test of the audit, exact: the 1-shell germ (the star and the coset-representative
    * placements at every tiling-vertex) forces the germ of every neighbor — for each tiling-vertex w, every
    * candidate isometry q_w·s (s over Stab±(S)) whose placements agree with the known stars at the known
    * positions places identical stars, modulo Stab±(S), at the unknown positions. None when a placement's
    * rotation is not recognized in the field.
    */
  private def germForcesExact(
      star: ExactStar,
      g: StarGeom,
      bridge: Bridge,
      stabE: Vector[MQ],
      skeleton: Vector[Vector[Glu]],
      idx: Int,
      si: Int,
      flags: collection.mutable.ListBuffer[String]
  ): Option[Boolean] =
    val n      = star.n
    val placed = skeleton.indices.toVector.map { x =>
      exactGlu(star, g, bridge, x, skeleton(x).head, flags).toOption.map(_.m)
    }
    if placed.exists(_.isEmpty) then
      flags += s"species $idx skeleton $si germ: a placement rotation is not exact"
      None
    else
      val starAt                       = placed.map(_.get)
      def knownAt(pos: VQ): Option[MQ] =
        if pos == vqZero then Some(idMQ)
        else (0 until n).find(x => star.coords(x) == pos).map(starAt)
      def conj(a: MQ, b: MQ): Boolean  = inStabE(mMul(mInv(a), b), stabE)
      Some((0 until n).forall { w =>
        val candidates = stabE.map(s => mMul(starAt(w), s))
        val placedByC  = candidates.map { m =>
          (0 until n).toVector.map(z => (vqAdd(star.coords(w), mVec(m, star.coords(z))), mMul(m, starAt(z))))
        }
        val admissible = placedByC.filter(_.forall((pos, rot) => knownAt(pos).forall(k => conj(k, rot))))
        admissible.nonEmpty && {
          val unknowns = admissible.map(_.filter((pos, _) => knownAt(pos).isEmpty).sortBy(_._1))
          unknowns.forall { u =>
            u.size == unknowns.head.size &&
            u.zip(unknowns.head).forall { case ((p1, r1), (p2, r2)) => p1 == p2 && conj(r1, r2) }
          }
        }
      })

  // ---------- exact fingerprints and class separation ----------

  /** Exact canonical fingerprint of the ball restricted to radius r, over Stab±(S): min over s ∈ Stab of the
    * sorted entry encodings (s-transformed exact position, canonical star code min over the right Stab
    * action). A congruence invariant with no rounding.
    */
  private def fingerprintExact(
      star: ExactStar,
      stabE: Vector[MQ],
      ball: collection.mutable.HashMap[VQ, BallEntry],
      r2: Q23
  ): Vector[Vector[Q23]] =
    stabE.map(s => encodeExact(star, stabE, ball, r2, s)).min

  final case class SeparationReport(idx: Int, classes: Int, distinct: Boolean)

  // ---------- the driver ----------

  /** Progress through a flushing sink: the exact replays are long and ScalaTest buffers a suite's output. */
  private def progress(msg: String): Unit =
    System.err.println(s"[exact] $msg")
    System.err.flush()

  /** The exact setting of one surviving species: its star model, the accepted patterns grouped into
    * fingerprint classes exactly as the audit groups them (numeric keys, stable order), and the exact
    * stabilizer.
    */
  final private case class Setting(
      idx: Int,
      star: ExactStar,
      acc: Accepted,
      bridge: Bridge,
      stabE: Vector[MQ],
      classes: Vector[Vector[(Int, Pattern)]]
  )

  private def settingOf(
      idx: Int,
      star: ExactStar,
      nflags: MonoShell.Flags,
      flags: collection.mutable.ListBuffer[String]
  ): Option[Setting] =
    val acc    = acceptedOf(idx, nflags)
    val g      = acc.g
    val bridge = Bridge(star, g)
    exactStab(star, g, acc.stab, flags) match
      case Left(e)      =>
        flags += s"species $idx: $e"
        None
      case Right(stabE) =>
        val byClass = collection.mutable.LinkedHashMap
          .empty[Vector[(Long, Long, Long)], Vector[(Int, Pattern)]]
        for (si, pat) <- acc.patterns do
          val ball = developBall(g, pat, acc.stab, 3.05).get
          val f2   = fingerprintOf(acc.corners, acc.stab, ball, 2.05)
          byClass(f2) = byClass.getOrElse(f2, Vector.empty) :+ (si, pat)
        Some(Setting(idx, star, acc, bridge, stabE, byClass.values.toVector))

  /** The exact star models of the 34 species, and the surviving species' settings. */
  private lazy val settings: (Vector[StarReport], Map[Int, ExactStar], Vector[Setting], Vector[String]) =
    val flags       = collection.mutable.ListBuffer.empty[String]
    val nflags      = MonoShell.Flags()
    val built       = species.indices.toVector.map(i => i -> buildStar(i, flags))
    val starReports = built.map { (i, e) =>
      e match
        case Left(msg)   => StarReport(i, species(i).state.positions.size, false, Double.NaN, msg)
        case Right(star) => StarReport(i, star.n, true, star.maxIvWidth, "")
    }
    val starOf      = built.collect { case (i, Right(s)) => i -> s }.toMap
    val sat         = MonoShell.results._1.filter(_._2.sat).map(_._1)
    val ss          = sat.flatMap { idx =>
      starOf.get(idx) match
        case None       =>
          flags += s"species $idx: no exact star model — certificates skipped"
          None
        case Some(star) => settingOf(idx, star, nflags, flags)
    }
    flags ++= nflags.items.distinct
    (starReports, starOf, ss, flags.distinct.toVector)

  final case class Results(
      stars: Vector[StarReport],
      classes: Vector[ClassReport],
      germs: Vector[GermReport], // every skeleton of every surviving species
      separations: Vector[SeparationReport],
      flags: Vector[String],
      starModels: Map[Int, ExactStar]
  ):
    def allOk: Boolean =
      stars.forall(_.ok) && classes.forall(_.ok) && classes.size == 28 &&
        separations.forall(_.distinct) && flags.isEmpty

  /** (a) the 34 exact star models, (b) the 28 exact class-representative certificates, (d) exact germ forcing
    * on every skeleton, (e) the exact separation of the doubled species. About a minute.
    */
  lazy val results: Results =
    val (starReports, starOf, ss, sflags) = settings
    val flags                             = collection.mutable.ListBuffer.from(sflags)
    val classReps                         = collection.mutable.ArrayBuffer.empty[ClassReport]
    val germs                             = collection.mutable.ArrayBuffer.empty[GermReport]
    val seps                              = collection.mutable.ArrayBuffer.empty[SeparationReport]
    for Setting(idx, star, acc, bridge, stabE, classes) <- ss do
      val g    = acc.g
      val reps = classes.map(_.head._2)
      val fps  = collection.mutable.ArrayBuffer.empty[Vector[Vector[Q23]]]
      for si <- acc.skeletons.indices do
        germForcesExact(star, g, bridge, stabE, acc.skeletons(si), idx, si, flags).foreach { forced =>
          germs += GermReport(idx, si, forced)
        }
      for (rep, ci) <- reps.zipWithIndex do
        classReps += certifyPattern(
          star,
          g,
          bridge,
          stabE,
          rep,
          idx,
          ci,
          Rat.zero,
          Rat.zero,
          s"class $idx#$ci",
          flags
        )._1
        if reps.size > 1 then
          val rSep   = Rat.frac(41, 20) // 2.05
          val sSep   = rSep + Rat.frac(8, 5)
          val slack2 = Q23.ofRat(sSep * sSep)
          val isosE  = rep.glus.indices.toVector.map { x =>
            exactGlu(star, g, bridge, x, rep.glus(x), flags).toOption.map(gl => EIso(gl.m, star.coords(gl.x)))
          }
          if isosE.forall(_.isDefined) then
            developExact(star, isosE.map(_.get), stabE, slack2, 14) match
              case Right(ball) =>
                fps += fingerprintExact(star, stabE, ball, Q23.ofRat(rSep * rSep))
              case Left(e)     => flags += s"species $idx class $ci: separation ball — $e"
      if reps.size > 1 then
        val distinct = fps.size == reps.size && fps.distinct.size == fps.size
        seps += SeparationReport(idx, reps.size, distinct)
        if !distinct then flags += s"species $idx: exact fingerprints not distinct"
    Results(starReports, classReps.toVector, germs.toVector, seps.toVector, flags.distinct.toVector, starOf)

  /** The exact replay of one exhaustion (Section 6.4, cap closure): every (R1)+(R2)-consistent pattern of a
    * skeleton germ forcing does not close, enumerated uncapped; the accepted ones (collision-free
    * development) must fingerprint into a known class and cohere with its representative exactly.
    */
  final case class ExhaustionReport(
      idx: Int,
      skeleton: Int,
      patterns: Int, // (R1)+(R2)-consistent patterns enumerated
      accepted: Int, // of which collision-free to radius 3.05
      cohered: Int,  // of which exactly cohered with their class representative
      capped: Boolean
  ):
    def ok: Boolean = !capped && cohered == accepted

  final case class CoherenceResults(
      coherences: Vector[CoherenceReport],
      exhaustions: Vector[ExhaustionReport],
      flags: Vector[String]
  ):
    def allOk: Boolean =
      coherences.nonEmpty && coherences.forall(_.ok) && exhaustions.forall(_.ok) && flags.isEmpty

  /** (c) exact coherence of every accepted pattern within the caps with its class representative — the
    * representative's certificate rebuilt, then every member's own — and (f) the exact replay of every
    * exhaustion: on each skeleton the exact germ test leaves open, all consistent patterns re-enumerated
    * uncapped and every accepted one cohered exactly with its class. Long (an exact ball per pattern):
    * opt-in, with progress on stderr.
    */
  lazy val coherenceResults: CoherenceResults =
    val (_, _, ss, sflags) = settings
    val flags              = collection.mutable.ListBuffer.from(sflags)
    val cohs               = collection.mutable.ArrayBuffer.empty[CoherenceReport]
    val exhs               = collection.mutable.ArrayBuffer.empty[ExhaustionReport]
    val open               = results.germs.filterNot(_.forced)
    val t0                 = System.nanoTime()
    for Setting(idx, star, acc, bridge, stabE, classes) <- ss do
      val g     = acc.g
      progress(s"species $idx: ${classes.size} classes, ${classes.map(_.size).sum} accepted patterns")
      val certs = classes.zipWithIndex.map { (members, ci) =>
        val rep     = members.head._2
        val certOpt =
          certifyPattern(
            star,
            g,
            bridge,
            stabE,
            rep,
            idx,
            ci,
            Rat.zero,
            Rat.zero,
            s"class $idx#$ci",
            flags
          )._2
        certOpt match
          case None       =>
            members.foreach((si, _) => cohs += CoherenceReport(idx, ci, si, false, false, false))
          case Some(cert) =>
            for (si, pat) <- members do
              val t1 = System.nanoTime()
              val c  = cohereExact(star, g, bridge, stabE, pat, cert, idx, ci, si, flags)
              cohs += c
              progress(f"  class $ci skeleton $si: ${if c.ok then "ok" else "FAIL"} " +
                f"(${(System.nanoTime() - t1) / 1e9}%.1f s, ${(System.nanoTime() - t0) / 1e9}%.0f s total)")
        (rep, ci, certOpt)
      }
      // the class of an accepted pattern, keyed exactly as the audit keys it
      val byKey = certs.flatMap { (rep, ci, certOpt) =>
        certOpt.map { cert =>
          val key = fingerprintOf(acc.corners, acc.stab, developBall(g, rep, acc.stab, 3.05).get, 2.05)
          key -> (ci, cert)
        }
      }.toMap
      for GermReport(_, si, _) <- open.filter(_.idx == idx) do
        var total       = 0
        var accepted    = 0
        var cohered     = 0
        val (_, capped) = TransitivePatterns.searchPatterns(
          g,
          acc.skeletons(si),
          acc.stab,
          500000,
          glus => {
            val pat = Pattern(g, glus)
            total += 1
            developBall(g, pat, acc.stab, 3.05).foreach { ball =>
              accepted += 1
              byKey.get(fingerprintOf(acc.corners, acc.stab, ball, 2.05)) match
                case Some((ci, cert)) =>
                  val t1 = System.nanoTime()
                  val c  = cohereExact(star, g, bridge, stabE, pat, cert, idx, ci, si, flags)
                  if c.ok then cohered += 1
                  progress(f"  exhaustion skeleton $si pattern $total: ${if c.ok then "ok" else "FAIL"} " +
                    f"(${(System.nanoTime() - t1) / 1e9}%.1f s, ${(System.nanoTime() - t0) / 1e9}%.0f s total)")
                case None             =>
                  flags += s"species $idx skeleton $si exhaustion: an accepted pattern of no known class"
            }
          }
        )
        if capped then flags += s"species $idx skeleton $si exhaustion: capped"
        exhs += ExhaustionReport(idx, si, total, accepted, cohered, capped)
        progress(s"  exhaustion skeleton $si: $total patterns, $accepted accepted, $cohered cohered exactly")
    CoherenceResults(cohs.toVector, exhs.toVector, flags.distinct.toVector)
