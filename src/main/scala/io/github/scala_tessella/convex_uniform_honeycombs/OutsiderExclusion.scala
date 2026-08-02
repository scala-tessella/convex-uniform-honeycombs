package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import CertifiedDihedrals.*

/** The value-level edge-completion FIXPOINT. For each outsider cell and each of its
  * edge types, search for multisets of participant dihedrals summing to 360° (3..6 cells around an edge). A
  * cell in a honeycomb realizes a figure at EVERY edge type, so an edge type with no completion excludes the
  * cell — a genuine proof, since interval misses certify non-equality. Excluded cells then leave the
  * participant pool (corona closure) and the search repeats until stable: a completion that only uses
  * excluded participants is no completion.
  *
  * Participant scope (stated hypotheses): core cells, prism verticals p ≤ 500, the pentagon/decagon family,
  * both snubs, antiprisms A_q for q ≤ 300 (targets: named outsiders and A_q for q ≤ 100). Interval widths and
  * superset pools only weaken exclusions, never falsify them.
  */
object OutsiderExclusion:

  final case class Value(label: String, iv: Iv)

  private val MinAngle = 60.0 // P3 vertical, the global minimum dihedral
  private val MaxAngle = 180.0

  val maxPoolPrism: Int       = 500
  val maxPoolAntiprism: Int   = 300
  val maxTargetAntiprism: Int = 100

  private lazy val alphaDeg = Iv.toDeg(Iv.atanInc(Iv.point(2.0).sqrtIv))

  private lazy val coreValues: Vector[Value] =
    HoneycombAlphabet.edgeTypes.map { et =>
      val iv = Iv.point(et.angle.r.toDouble) + alphaDeg * Iv.point(et.angle.n.toDouble)
      Value(s"${et.cell.label}(${et.faces._1}·${et.faces._2})", iv)
    } ++
      (5 to maxPoolPrism).filterNot(Set(6, 8, 12).contains).map { p =>
        Value(s"P$p(4·4)", Iv.widen(180.0 - 360.0 / p, 180.0 - 360.0 / p))
      }

  private lazy val namedValues: Map[String, Vector[Value]] =
    outsiderConfigs.map { (name, cfg) =>
      name -> edgeTypesOf(cfg).toVector.map((pair, iv) => Value(s"$name(${pair._1}·${pair._2})", iv))
    }

  private lazy val antiValues: Map[Int, Vector[Value]] = (4 to maxPoolAntiprism).map { q =>
    q -> edgeTypesOf(antiprismConfig(q)).toVector.map((pair, iv) =>
      Value(s"A$q(${pair._1}·${pair._2})", iv)
    )
  }.toMap

  /** Completions of a target dihedral over the given pool (sorted by lo): multisets of 2..5 values whose sum
    * with the target intersects 360. Capped per target.
    */
  def completions(target: Iv, pool: Vector[Value], cap: Int = 8): Vector[List[Value]] =
    val found                                                       = Vector.newBuilder[List[Value]]
    var count                                                       = 0
    def rec(from: Int, slots: Int, acc: List[Value], sum: Iv): Unit =
      if count >= cap then ()
      else if slots == 0 then
        if sum.intersects(Iv.point(360.0)) then
          found += acc.reverse
          count += 1
      else
        val residual = Iv.point(360.0) - sum
        if residual.hi >= MinAngle * slots - 1e-9 && residual.lo <= MaxAngle * slots then
          var i = from
          while i < pool.size && count < cap do
            val v = pool(i)
            if v.iv.lo <= residual.hi - MinAngle * (slots - 1) + 1e-9 then
              if v.iv.hi >= residual.lo - MaxAngle * (slots - 1) - 1e-9 then
                rec(i, slots - 1, v :: acc, sum + v.iv)
              i += 1
            else i = pool.size
    for slots <- 2 to 5 do rec(0, slots, Nil, target)
    found.result()

  final case class Verdict(cell: String, edgeType: (Int, Int), angle: Iv, found: Vector[List[Value]]):
    def excludedHere: Boolean = found.isEmpty
    def show: String          =
      val a = String.format(java.util.Locale.ROOT, "%.5f", angle.mid)
      s"$cell(${edgeType._1}·${edgeType._2}) @ $a: " +
        (if found.isEmpty then "NO COMPLETION"
         else
           found.take(4).map(_.map(_.label).mkString("+")).mkString(
             "completions: ",
             " | ",
             if found.size > 4 then " …" else ""
           ))

  final case class FixpointResult(
      rounds: Vector[Vector[String]],              // cells newly excluded per round
      survivors: Vector[(String, Vector[Verdict])] // final verdicts of surviving targets
  ):
    def excluded: Vector[String] = rounds.flatten

  /** The corona fixpoint: iterate exclusion with excluded cells removed from the pool. */
  lazy val fixpoint: FixpointResult =
    var aliveNamed = outsiderConfigs.keySet
    var aliveAnti  = (4 to maxPoolAntiprism).toSet
    val rounds     = Vector.newBuilder[Vector[String]]
    var stable     = false
    var lastVs     = Vector.empty[(String, Vector[Verdict])]
    while !stable do
      val pool     =
        (coreValues ++
          aliveNamed.toVector.flatMap(namedValues) ++
          aliveAnti.toVector.flatMap(antiValues)).sortBy(_.iv.lo)
      val targets  =
        aliveNamed.toVector.sorted.map(n => (n, namedValues(n).map(_.iv), outsiderConfigs(n))) ++
          aliveAnti.toVector.sorted.filter(_ <= maxTargetAntiprism).map(q =>
            (s"A$q", antiValues(q).map(_.iv), antiprismConfig(q))
          )
      lastVs = targets.map { (name, _, cfg) =>
        name -> edgeTypesOf(cfg).toVector.sortBy(_._1).map((pair, iv) =>
          Verdict(name, pair, iv, completions(iv, pool))
        )
      }
      val newlyOut = lastVs.collect { case (n, ts) if ts.exists(_.excludedHere) => n }
      if newlyOut.isEmpty then stable = true
      else
        rounds += newlyOut
        aliveNamed = aliveNamed -- newlyOut
        aliveAnti = aliveAnti -- newlyOut.filter(_.startsWith("A")).map(_.drop(1).toInt)
    FixpointResult(rounds.result(), lastVs)
