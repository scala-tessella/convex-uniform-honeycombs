package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import TailExclusion.Cls
import CertifiedDihedrals.Iv

/** The six planar-species lifts — the nine exotic prism families p ∈ {5,7,9,10,15,18,20,24,42} are excluded,
  * completing the ALPHABET THEOREM: every face-to-face unit-edge honeycomb by convex uniform cells uses only
  * the 13 core cells.
  *
  * Standing hypotheses (the exclusion chain): by the corona fixpoint and the tails, the only cells that can
  * appear in a honeycomb are the 13 core cells and the nine exotic prisms — every antiprism, every
  * pentagon/decagon-family cell, both snubs, and every other prism is excluded. So ring enumerations below
  * run over pools of core classes plus the still-alive exotic laterals AND horizontals (a class = a cell with
  * a specific edge type, so upright/sideways attitudes are distinguished by construction).
  *
  * Structural lemmas used by the walks:
  *   - RING FORCING: around any lateral (4·4) edge of a prism, the ring is a face-closing chain of classes
  *     with dihedrals summing to exactly 360°; the completions are enumerated EXACTLY (charged sums are
  *     irrational — Niven — and cannot hit the rational target). The exotic lateral values 180 − 360/p are
  *     pairwise distinct and distinct from every core value, so participants are identified by value.
  *   - LEMMA OPP (flank propagation): the two axis-parallel edges of a prism's lateral square are OPPOSITE
  *     edges of that square, and opposite edges of every rco square face carry the same edge type (machine-
  *     verified below on the exact ℚ(√2) model); cubes are (4·4) everywhere. Hence the cell glued across a
  *     lateral square of a prism presents the SAME class at both vertical edges of that square, and the prism
  *     itself presents its lateral class at all p of its lateral edges.
  *
  * The walks (the 2D odd-face arguments, lifted):
  *   - SELF-WALK around an exotic P_p: its p lateral squares carry flank cells t₁…t_p (cyclic); at each
  *     lateral edge the ring is a completion of 180 − 360/p, whose two center-adjacent cells are (tᵢ, tᵢ₊₁) —
  *     so consecutive flanks must be co-endpoints of a completion. If the pair graph admits no closed p-walk,
  *     P_p cannot exist. This kills P7, P9, P15 (odd p, forced alternation {P42,p3}, {P18,p3}, {P10,p3}) and
  *     — after round 1 — P5 (pairs {P20,cube} and {P5,P10} are two disjoint edges: no closed walk of odd
  *     length 5).
  *   - P3-WALK for even exotics forced against a P3: when EVERY completion of P_p's lateral contains p3(4·4),
  *     any P_p has an upright P3 sharing a square, with anchor flank class P_p(4·4); the walk around the P3's
  *     THREE lateral squares (3 = odd) needs a closed 3-walk through the anchor in the pair graph of P3-ring
  *     completions — and there is none: P42 → P7 → P42 → (P42,P42) needs a 60+2·(1440/7)° ring (no exact
  *     closure), P18 → P9 → P18 → ✗, and the sharp P24 → {P8, rco} → P24 → ✗ (the closing ring [p3, X, X] =
  *     60+135+135 = 330 misses 360 by less than any dihedral). This is the exact 3D transposition of the
  *     third-triangle-edge clash that kills 3.8.24 in 2D.
  *   - CORONA: kills propagate — P10 and P20 lose their last completions once P15 and P5 are gone.
  *
  * The campaign terminates with all nine exotic families dead in 3 rounds.
  */
object ExoticLifts:

  val exoticPs: Vector[Int] = Vector(5, 7, 9, 10, 15, 18, 20, 24, 42)

  def lateral(p: Int): Frac = Frac.make(180L * p - 360L, p.toLong)

  private lazy val alphaDeg = Iv.toDeg(Iv.atanInc(Iv.point(2.0).sqrtIv))

  private lazy val coreClasses: Vector[Cls] =
    HoneycombAlphabet.edgeTypes.map { et =>
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

  /** The alive pool of a campaign round: core classes + laterals and horizontals of alive exotics. */
  def roundPool(alive: Set[Int]): Vector[Cls] =
    coreClasses ++ alive.toVector.sorted.flatMap { p =>
      val v = lateral(p)
      Vector(
        Cls(s"P$p(4·4)", 4, 4, Iv.widen(v.toDouble, v.toDouble), Some((v, 0)), '.'),
        Cls(s"P$p(4·$p)", 4, p, Iv.point(90.0), Some((Frac(90, 1), 0)), '.')
      )
    }

  // ---------- exact ring completions ----------

  private def exactSum(chain: Vector[Cls]): Option[(Frac, Int)] =
    chain.foldLeft(Option((Frac(0, 1), 0))) { (acc, cls) =>
      for
        (v, n)   <- acc
        (v2, n2) <- cls.exact
      yield (v + v2, n + n2)
    }

  /** The EXACT ring completions around a (4·4) edge whose center cell contributes `center` degrees:
    * face-closing chains 4→…→4 over `pool` with dihedral sum exactly 360 − center. Charged chains cannot hit
    * the rational target (Niven); the interval search over-approximates and the exact filter decides.
    */
  def completions(pool: Vector[Cls], center: Frac, maxLen: Int): Vector[Vector[Cls]] =
    import scala.math.Ordering.Implicits.seqOrdering
    val target                                                        = Frac(360, 1) - center
    val window                                                        = Iv.widen(target.toDouble - 1e-6, target.toDouble + 1e-6)
    val byFace                                                        =
      pool
        .flatMap { cls =>
          if cls.f1 == cls.f2 then Vector((cls.f1, cls, cls.f2))
          else Vector((cls.f1, cls, cls.f2), (cls.f2, cls, cls.f1))
        }
        .groupMap(_._1)(t => (t._2, t._3))
        .view
        .mapValues(_.sortBy(_._1.iv.lo))
        .toMap
    val found                                                         = collection.mutable.Map.empty[Vector[String], Vector[Cls]]
    def rec(open: Int, acc: List[Cls], sum: Iv, slotsLeft: Int): Unit =
      val budget = window.hi - sum.lo + 1e-9
      for (cls, other) <- byFace.getOrElse(open, Vector.empty).takeWhile(_._1.iv.lo <= budget) do
        val s2 = sum + cls.iv
        if other == 4 && s2.intersects(window) then
          val chain = (cls :: acc).reverse.toVector
          if exactSum(chain).contains((target, 0)) then
            val key = Vector(chain.map(_.label), chain.map(_.label).reverse).min
            found.getOrElseUpdate(key, chain)
        if slotsLeft > 1 && s2.lo + 60.0 <= window.hi + 1e-9 then rec(other, cls :: acc, s2, slotsLeft - 1)
    rec(4, Nil, Iv.point(0.0), maxLen)
    found.values.toVector

  // ---------- flank-pair graphs and closed walks ----------

  /** The flank-pair graph of a center value: the two center-adjacent cells of each exact ring completion, as
    * directed label pairs (both orientations).
    */
  def flankPairs(pool: Vector[Cls], center: Frac, maxLen: Int): Set[(String, String)] =
    completions(pool, center, maxLen).flatMap { c =>
      Vector(c.head.label -> c.last.label, c.last.label -> c.head.label)
    }.toSet

  /** Is there a closed walk of length exactly n through `start` in the pair graph? */
  def closedWalkThrough(pairs: Set[(String, String)], n: Int, start: String): Boolean =
    val succ                                 = pairs.groupMap(_._1)(_._2)
    def rec(cur: String, left: Int): Boolean =
      if left == 0 then cur == start
      else succ.getOrElse(cur, Set.empty).exists(rec(_, left - 1))
    rec(start, n)

  /** Is there a closed walk of length exactly n anywhere in the pair graph? */
  def closedWalkExists(pairs: Set[(String, String)], n: Int): Boolean =
    pairs.map(_._1).exists(closedWalkThrough(pairs, n, _))

  // ---------- the campaign ----------

  final case class Kill(p: Int, round: Int, mechanism: String, completions: Vector[String])

  /** The three-round exclusion campaign. Mechanisms, in order per target: no completion left (corona);
    * self-walk (no closed p-walk in its own flank-pair graph); P3-walk (all completions contain p3, and no
    * closed 3-walk through the anchor class in the P3 flank-pair graph).
    */
  lazy val campaign: Vector[Kill] =
    val kills = Vector.newBuilder[Kill]
    var alive = exoticPs.toSet
    var round = 1
    while alive.nonEmpty do
      val pool  = roundPool(alive)
      val w1    = flankPairs(pool, Frac(60, 1), 5) // the P3 ring pair graph (up to 5 partners: 5 × 60 = 300)
      val newly = alive.toVector.sorted.flatMap { p =>
        val comps = completions(pool, lateral(p), 4)
        val shows = comps.map(_.map(_.label).mkString(" + "))
        if comps.isEmpty then Some(Kill(p, round, "corona: no completion", shows))
        else
          val self = comps.flatMap(c =>
            Vector(c.head.label -> c.last.label, c.last.label -> c.head.label)
          ).toSet
          if !closedWalkExists(self, p) then Some(Kill(p, round, s"self-walk: no closed $p-walk", shows))
          else if comps.forall(_.exists(_.label == "p3(4·4)")) &&
            !closedWalkThrough(w1, 3, s"P$p(4·4)")
          then Some(Kill(p, round, "P3-walk: no closed 3-walk through the anchor", shows))
          else None
      }
      require(newly.nonEmpty, s"campaign stalled with alive = $alive")
      kills ++= newly
      alive --= newly.map(_.p).toSet
      round += 1
    kills.result()

  /** THE ALPHABET THEOREM endgame: all nine exotic families die. */
  lazy val alphabetClosed: Boolean = campaign.map(_.p).sorted == exoticPs.sorted

  // ---------- Lemma OPP for the rhombicuboctahedron, exact over ℚ(√2) ----------

  /** Exact ℚ(√2): a + b·√2. */
  final case class Q2(a: Frac, b: Frac):
    def +(o: Q2): Q2    = Q2(a + o.a, b + o.b)
    def -(o: Q2): Q2    = Q2(a - o.a, b - o.b)
    def *(o: Q2): Q2    = Q2(a * o.a + Frac(2, 1) * (b * o.b), a * o.b + b * o.a)
    def unary_- : Q2    = Q2(Frac(0, 1) - a, Frac(0, 1) - b)
    def isZero: Boolean = a.isZero && b.isZero
    def signum: Int     =
      (a.signum, b.signum) match
        case (0, sb)              => sb
        case (sa, 0)              => sa
        case (sa, sb) if sa == sb => sa
        case (sa, _)              => sa * (a * a - Frac(2, 1) * (b * b)).signum

  private type V2 = (Q2, Q2, Q2)
  private def q2(n: Int): Q2           = Q2(Frac(n, 1), Frac(0, 1))
  private def vSub(u: V2, v: V2): V2   = (u._1 - v._1, u._2 - v._2, u._3 - v._3)
  private def dot2(u: V2, v: V2): Q2   = u._1 * v._1 + u._2 * v._2 + u._3 * v._3
  private def cross2(u: V2, v: V2): V2 =
    (u._2 * v._3 - u._3 * v._2, u._3 * v._1 - u._1 * v._3, u._1 * v._2 - u._2 * v._1)
  private def dist22(u: V2, v: V2): Q2 = { val d = vSub(u, v); dot2(d, d) }

  /** rco, edge 2: all permutations of (±1, ±1, ±(1+√2)). */
  lazy val rcoVertices: Vector[V2] =
    val h  = Q2(Frac(1, 1), Frac(1, 1))
    val cs =
      for
        s1 <- Vector(1, -1)
        s2 <- Vector(1, -1)
        s3 <- Vector(1, -1)
      yield (s1, s2, s3)
    cs.flatMap { (s1, s2, s3) =>
      val (x, y, z) = (q2(s1), q2(s2), if s3 > 0 then h else -h)
      Vector((z, x, y), (x, z, y), (x, y, z))
    }.distinct

  /** (triangles, squares) of the rco, built intrinsically from the exact edge graph. */
  lazy val rcoFaces: (Vector[Vector[Int]], Vector[Vector[Int]]) =
    val n           = rcoVertices.size
    val four        = q2(4)
    val adj         = (0 until n).map { i =>
      (0 until n).filter(j => j != i && dist22(rcoVertices(i), rcoVertices(j)) == four).toVector
    }
    val found       = collection.mutable.Map.empty[Set[Int], Vector[Int]]
    for
      a <- 0 until n
      b <- adj(a)
      c <- adj(b)
      if c != a
    do
      if adj(c).contains(a) then found.getOrElseUpdate(Set(a, b, c), Vector(a, b, c))
      else
        val nrm = cross2(vSub(rcoVertices(b), rcoVertices(a)), vSub(rcoVertices(c), rcoVertices(a)))
        adj(c)
          .find(d => d != b && d != a && dot2(vSub(rcoVertices(d), rcoVertices(a)), nrm).isZero)
          .foreach { d =>
            if adj(d).contains(a) then found.getOrElseUpdate(Set(a, b, c, d), Vector(a, b, c, d))
          }
    val (tris, sqs) = found.values.toVector.partition(_.size == 3)
    (tris, sqs)

  /** LEMMA OPP for the rco: every square face has its (3·4)-edges opposite and its (4·4)-edges opposite, so
    * the edge type at one vertical edge of a shared square propagates to the other. Also certifies the
    * dihedral values behind the labels: (4·4) at cos² = 1/2 and (3·4) at cos² = 2/3, both obtuse.
    */
  lazy val rcoOppositeEdgeLemma: Boolean =
    val (tris, sqs)                                              = rcoFaces
    val triEdges                                                 = tris.flatMap(t => t.combinations(2).map(_.toSet)).toSet
    def edgeType(u: Int, v: Int): Int                            = if triEdges(Set(u, v)) then 3 else 4
    val structure                                                = tris.size == 8 && sqs.size == 18 &&
      sqs.forall { s =>
        edgeType(s(0), s(1)) == edgeType(s(2), s(3)) && edgeType(s(1), s(2)) == edgeType(s(3), s(0))
      }
    // dihedral certification: outward normals of two faces sharing an edge
    def outward(f: Vector[Int]): V2                              =
      val nrm = cross2(
        vSub(rcoVertices(f(1)), rcoVertices(f(0))),
        vSub(rcoVertices(f(2)), rcoVertices(f(0)))
      )
      val c   = f.map(rcoVertices).reduce((u, v) => (u._1 + v._1, u._2 + v._2, u._3 + v._3))
      if dot2(nrm, c).signum > 0 then nrm else (-nrm._1, -nrm._2, -nrm._3)
    def cos2TimesIs(n1: V2, n2: V2, num: Int, den: Int): Boolean =
      val d = dot2(n1, n2)
      d.signum > 0 && (q2(den) * (d * d) - q2(num) * dot2(n1, n1) * dot2(n2, n2)).isZero
    val sq44                                                     = // two squares sharing an edge
      (for
        i <- sqs.indices
        j <- i + 1 until sqs.size
        if sqs(i).toSet.intersect(sqs(j).toSet).size == 2
      yield (sqs(i), sqs(j))).head
    val sq34                                                     = // a triangle and a square sharing an edge
      (for
        t <- tris
        s <- sqs
        if t.toSet.intersect(s.toSet).size == 2
      yield (t, s)).head
    structure &&
    cos2TimesIs(outward(sq44._1), outward(sq44._2), 1, 2) && // cos(135°)² = 1/2, normals dot > 0
    cos2TimesIs(outward(sq34._1), outward(sq34._2), 2, 3) // cos(144.74°)² = 2/3
