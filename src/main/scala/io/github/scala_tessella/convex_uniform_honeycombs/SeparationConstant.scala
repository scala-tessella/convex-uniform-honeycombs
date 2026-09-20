package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import CertifiedDihedrals.{Iv, V3}
import HoneycombAlphabet.CellType

/** The separation constant of the species search — the soundness of its identify-or-separate decisions.
  *
  * The spherical assembly identifies two tiling-vertices when their certified distance is below 1e-6 and
  * separates them when it is above 1e-3 (a gray zone between is flagged; none fires). Both decisions are
  * correct on any branch that is a subcomplex of a genuine edge-to-edge tiling by the corner figures, because
  * in such a tiling two DISTINCT tiling-vertices u, w are far apart: w lies outside the open star of u, whose
  * boundary consists of the sides of the corners at u not incident to u, so d(u, w) is at least the minimum,
  * over the 13 corner figures and over every (vertex, non-incident side) pair, of the vertex-to-side
  * distance. (Adjacent tiling-vertices are separated by an arc, i.e. by a face angle, at least 60°.) A
  * separation below 1e-3 is a certified interval miss from zero and is sound as it stands; an identification
  * below 1e-6 is sound because the alternative — two distinct vertices that close — is excluded by the
  * constant.
  *
  * The constant is certified with intervals. The distance from a point v to the arc (a, b) is the
  * great-circle distance asin(|n·v|/|n|), n = a × b, when the foot of the perpendicular lies on the arc, and
  * the nearer endpoint distance otherwise; the minimum of the two is a lower bound in every case. The minimum
  * is attained by the tetrahedral corner: the altitude of the equilateral spherical triangle of side 60°,
  * which is α = arctan √2 = 54.7356°.
  */
object SeparationConstant:

  private def dot(a: V3, b: V3): Iv   = a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  private def cross(a: V3, b: V3): V3 =
    (a._2 * b._3 - a._3 * b._2, a._3 * b._1 - a._1 * b._3, a._1 * b._2 - a._2 * b._1)

  /** Angle in degrees between two unit vectors. */
  private def angleDeg(a: V3, b: V3): Iv = Iv.toDeg(Iv.acosDec(dot(a, b)))

  /** A certified lower bound, in degrees, on the distance from the unit vector v to the arc (a, b). */
  def sideDistanceLB(v: V3, a: V3, b: V3): Double =
    val n    = cross(a, b)
    val sin2 = dot(n, v) * dot(n, v) / dot(n, n)
    val circ = Iv.toDeg(Iv.asinInc(sin2.sqrtIv))
    val ends = math.min(angleDeg(v, a).lo, angleDeg(v, b).lo)
    val s1   = dot(cross(a, v), n)
    val s2   = dot(cross(v, b), n)
    if s1.lo > 0 && s2.lo > 0 then circ.lo   // the foot certified on the arc
    else if s1.hi < 0 || s2.hi < 0 then ends // the foot certified off the arc
    else math.min(circ.lo, ends) // ambiguous: a bound either way

  /** Every (corner figure, vertex, non-incident side) with its certified lower bound, in degrees. */
  lazy val bounds: Vector[(CellType, Int, Int, Double)] =
    for
      (cell, vs) <- SpeciesEnumerator.cornerFigures.toVector.sortBy(_._1.ordinal)
      k           = vs.size
      j          <- (0 until k).toVector
      i          <- (0 until k).toVector
      if i != j && (i + 1) % k != j // the side (i, i+1) is not incident to vertex j
    yield (cell, j, i, sideDistanceLB(vs(j), vs(i), vs((i + 1) % k)))

  /** The separation constant: the corner attaining it and the certified bound in degrees. */
  lazy val constant: (CellType, Double) =
    val (cell, _, _, d) = bounds.minBy(_._4)
    (cell, d)
