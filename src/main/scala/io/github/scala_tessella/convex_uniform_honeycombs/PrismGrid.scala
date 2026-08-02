package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

/** The prism grid. Every prism P_p has vertical (square·square) edges of dihedral 180 −
  * 360/p, so a honeycomb containing P_p realizes an edge figure at that angle. At value level the
  * participants available from the core alphabet + all prisms are: the rational lattice {60, 90, 120, 135,
  * 150}, the four α-charged values (whose charges must cancel), and other prism verticals 180 − 360/q. The
  * edge equation then reduces to an exact Egyptian-fraction Diophantine problem — SOLVED COMPLETELY here.
  *
  * Scope: participants from core cells and prisms only; antiprism and pentagon-family participants carry
  * irrationalities outside the (15°, α)-lattice and are handled by the corona fixpoint's independence lemmas.
  */
object PrismGrid:

  /** Rational core dihedrals, degrees (prism verticals for p ∈ {3,4,6,8,12} appear as these values). */
  val lattice: Vector[Int] = Vector(60, 90, 120, 135, 150)

  /** α-charged core dihedrals as (rational part, charge): values r + n·α. */
  val charged: Vector[(Int, Int)] = Vector((180, -2), (0, 2), (180, -1), (90, 1))

  val corePrisms: Set[Int] = Set(3, 4, 6, 8, 12)

  /** A value-level solution of the vertical-edge equation with at least one exotic prism: Σ(180 − 360/p) + Σ
    * lattice + Σ charged = 360 with cancelling charges and 3..6 terms in all.
    */
  final case class Solution(exotic: List[Int], latticeVals: List[Int], chargedVals: List[(Int, Int)]):
    def size: Int = exotic.size + latticeVals.size + chargedVals.size

    /** The 2D shadow: every term read as a polygon interior angle, when all of them are ones. */
    def species: Option[List[Int]] =
      if chargedVals.nonEmpty then None
      else Some((exotic ++ latticeVals.map(v => 360 / (180 - v))).sorted)

    def show: String =
      def deg(p: Int) = String.format(java.util.Locale.ROOT, "%.2f", 180.0 - 360.0 / p)
      val e           = exotic.map(p => s"P$p:${deg(p)}")
      val l           = latticeVals.map(v => s"$v")
      val c           = chargedVals.map((r, n) => s"($r${if n >= 0 then "+" else ""}${n}α)")
      (e ++ l ++ c).mkString("[", " ", "]") +
        species.map(s => "  = 2D species " + s.mkString(".")).getOrElse("")

  /** Nondecreasing multisets over `pool` of size ≤ maxSize. */
  private def multisets[A](pool: Vector[A], maxSize: Int): Vector[List[A]] =
    def rec(from: Int, budget: Int): Vector[List[A]] =
      if budget == 0 then Vector(Nil)
      else
        Vector(List.empty[A]) ++ (
          for
            i    <- (from until pool.size).toVector
            rest <- rec(i, budget - 1)
          yield pool(i) :: rest
        )
    rec(0, maxSize)

  /** Egyptian step: all nondecreasing k-tuples of EXOTIC integers p ≥ minP with Σ 1/pᵢ = t, exactly. */
  private def egyptian(k: Int, t: Frac, minP: Int): Vector[List[Int]] =
    if k == 0 then if t.isZero then Vector(Nil) else Vector.empty
    else if t.signum <= 0 then Vector.empty
    else
      val lo = math.max(minP.toLong, (t.den + t.num - 1) / t.num) // p ≥ 1/t
      val hi = k.toLong * t.den / t.num                           // p ≤ k/t
      (lo to hi).toVector
        .map(_.toInt)
        .filterNot(corePrisms.contains)
        .flatMap(p => egyptian(k - 1, t - Frac(1L, p.toLong), p).map(p :: _))

  /** The COMPLETE solution set of the exotic vertical-edge equation. */
  lazy val solutions: Vector[Solution] =
    for
      cs <- multisets(charged, 5)
      if cs.map(_._2).sum == 0
      ls <- multisets(lattice, 5 - cs.size)
      k  <- (1 to (6 - cs.size - ls.size)).toVector
      if cs.size + ls.size + k >= 3
      sc  = cs.map(_._1).sum + ls.sum
      ps <- egyptian(k, Frac(sc.toLong + 180L * k - 360L, 360L), 5)
    yield Solution(ps, ls.sorted, cs.sortBy(c => (c._1, c._2)))

  /** The exotic p-values that survive the vertical-edge equation at all. */
  lazy val survivingExoticP: Set[Int] = solutions.flatMap(_.exotic).toSet
