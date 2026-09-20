package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import PrismGrid.*

/** The prism grid: the exotic-prism vertical-edge equation over the core + prism world is solved completely
  * and its solutions are exactly the lifts of the six non-extendable 2D vertex species.
  */
class PrismGridSpec extends AnyFlatSpec with Matchers:

  "every solution" should "sum to 360 exactly with cancelling charges and 3..6 terms" in:
    solutions should not be empty
    solutions.foreach { s =>
      withClue(s.show) {
        s.size should (be >= 3 and be <= 6)
        s.exotic.foreach(p => corePrisms should not contain p)
        s.chargedVals.map(_._2).sum shouldBe 0
        val total = s.exotic.map(p => Frac(180L * p - 360L, p.toLong)).foldLeft(Frac(0, 1))(_ + _) +
          Frac(s.latticeVals.sum.toLong + s.chargedVals.map(_._1).sum.toLong, 1L)
        total shouldBe Frac(360, 1)
      }
    }

  "the surviving exotic prisms" should "be exactly the polygons of the six non-extendable 2D species" in:
    survivingExoticP shouldBe Set(5, 7, 9, 10, 15, 18, 20, 24, 42)

  "the solutions" should "all be pure lifts of the six 2D species — no charged participants survive" in:
    solutions.foreach(s => withClue(s.show)(s.chargedVals shouldBe empty))
    val speciesSet = solutions.flatMap(_.species).map(_.mkString(".")).toSet
    speciesSet shouldBe Set("3.7.42", "3.8.24", "3.9.18", "3.10.15", "4.5.20", "5.5.10")
    solutions.foreach(s => info(s.show))
