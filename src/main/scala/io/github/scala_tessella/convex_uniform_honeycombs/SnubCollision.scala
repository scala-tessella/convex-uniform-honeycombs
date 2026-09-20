package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import MonoShell.{Flags, Glu, Vec}
import TransitivePatterns.{acceptedOf, searchPatterns, Iso, Mat, Pattern}

/** The developability gap made explicit (Example 6.4 of the paper): on the snub-trihexagonal prismatic lift,
  * a pattern satisfying (R1) and (R2) whose development COLLIDES — two words in the gluings reaching the same
  * position with point parts that differ by more than a stabilizer element. The enumeration of
  * [[TransitivePatterns]] rejects such patterns; this object re-develops the (R1)+(R2)-consistent patterns of
  * the species' skeletons with the words recorded, and reports the first collision found: the two words as
  * sequences of tiling-vertex indices (each step the gluing at that tiling-vertex, applied on the right), the
  * common position, and the two point parts.
  */
object SnubCollision:

  val speciesLabel = "{p3:8 p6:2}#2"

  final case class Collision(
      skeleton: Int,
      pattern: Vector[Glu],
      word1: Vector[Int],
      word2: Vector[Int],
      position: Vec,
      m1: Mat,
      m2: Mat
  )

  private def vNorm(v: Vec): Double = math.sqrt(v._1 * v._1 + v._2 * v._2 + v._3 * v._3)

  /** Develop `pat` breadth-first with words, out to `radius + 1.6`; the first stab-incompatible coincidence
    * of positions, if any.
    */
  private def firstCollision(
      pat: Pattern,
      stab: Vector[Mat],
      radius: Double
  ): Option[(Vector[Int], Vector[Int], Vec, Mat, Mat)] =
    val isos                                                      = pat.g.u.indices.map(pat.iso).toVector
    val slack                                                     = radius + 1.6
    val seen                                                      = collection.mutable.Map.empty[(Long, Long, Long), (Iso, Vector[Int])]
    var frontier                                                  = Vector((
      Iso(TransitivePatterns.Mat((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)), (0.0, 0.0, 0.0)),
      Vector.empty[Int]
    ))
    seen((0L, 0L, 0L)) = frontier.head
    var depth                                                     = 0
    val maxDepth                                                  = math.max(14, (3.0 * slack).toInt)
    var result: Option[(Vector[Int], Vector[Int], Vec, Mat, Mat)] = None
    while frontier.nonEmpty && depth < maxDepth && result.isEmpty do
      frontier = frontier.flatMap { (t, w) =>
        if result.isDefined then None
        else
          isos.indices.flatMap { x =>
            if result.isDefined then None
            else
              val t2 = t.compose(isos(x))
              val w2 = w :+ x
              if vNorm(t2.t) <= slack + 1e-6 then
                val k = TransitivePatterns.round4(t2.t)
                seen.get(k) match
                  case Some((s, ws)) =>
                    if s.m.dist(t2.m) > 1e-4 && !stab.exists(st => (s.m.t * t2.m).dist(st) < 1e-3) then
                      result = Some((ws, w2, t2.t, s.m, t2.m))
                    None
                  case None          =>
                    seen(k) = (t2, w2)
                    Some((t2, w2))
              else None
          }
      }
      depth += 1
    result

  /** The first colliding pattern of the species, skeleton by skeleton. */
  lazy val collision: Option[Collision] =
    val flags                    = Flags()
    val idx                      = (0 until 34).find(i => SpeciesCorona.label(i) == speciesLabel).get
    val acc                      = acceptedOf(idx, flags)
    var found: Option[Collision] = None
    for (domains, si) <- acc.skeletons.zipWithIndex if found.isEmpty do
      searchPatterns(
        acc.g,
        domains,
        acc.stab,
        40,
        glus =>
          if found.isEmpty then
            firstCollision(Pattern(acc.g, glus), acc.stab, 3.05).foreach { (w1, w2, p, m1, m2) =>
              found = Some(Collision(si, glus, w1, w2, p, m1, m2))
            }
      )
    found
