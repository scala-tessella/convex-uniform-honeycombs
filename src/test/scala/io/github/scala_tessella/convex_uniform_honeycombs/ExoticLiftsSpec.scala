package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ExoticLifts.*

/** The exotic lifts: the rco flank-propagation lemma verifies exactly, the ring realizations match
  * the six prism-grid shadows, the walk machinery has teeth, and the campaign kills all nine exotic families with
  * the pinned mechanisms — closing the Alphabet Theorem.
  */
class ExoticLiftsSpec extends AnyFlatSpec with Matchers:

  "lemma OPP (rco)" should "verify on the exact ℚ(√2) model" in:
    rcoOppositeEdgeLemma shouldBe true

  "the walk machinery" should "have teeth" in:
    val ab: Set[(String, String)] = Set("a" -> "b", "b" -> "a")
    closedWalkThrough(ab, 2, "a") shouldBe true
    closedWalkThrough(ab, 3, "a") shouldBe false
    closedWalkExists(ab, 4) shouldBe true
    closedWalkExists(ab, 5) shouldBe false

  "the round-1 ring realizations" should "match the six prism-grid shadows" in:
    val pool            = roundPool(exoticPs.toSet)
    def shadows(p: Int) =
      completions(pool, lateral(p), 4).map(_.map(_.label).sorted.mkString("+")).sorted
    shadows(42) shouldBe Vector("P7(4·4)+p3(4·4)")
    shadows(7) shouldBe Vector("P42(4·4)+p3(4·4)")
    shadows(9) shouldBe Vector("P18(4·4)+p3(4·4)")
    shadows(18) shouldBe Vector("P9(4·4)+p3(4·4)")
    shadows(15) shouldBe Vector("P10(4·4)+p3(4·4)")
    shadows(10) shouldBe Vector("P15(4·4)+p3(4·4)", "P5(4·4)+P5(4·4)")
    shadows(5) shouldBe Vector("P10(4·4)+P5(4·4)", "P20(4·4)+cube(4·4)")
    shadows(20) shouldBe Vector("P5(4·4)+cube(4·4)")
    shadows(24) shouldBe Vector("p3(4·4)+p8(4·4)", "p3(4·4)+rco(4·4)")

  "the P3-ring pair graph" should "contain the benign pairs and none of the fatal ones" in:
    val w1 = flankPairs(roundPool(exoticPs.toSet), Frac(60, 1), 5)
    w1 should contain("p3(4·4)" -> "p3(4·4)") // the all-P3 six-ring of the catalogue
    w1 should contain("P42(4·4)" -> "P7(4·4)") // the 3.7.42 shadow
    w1 should not contain ("P42(4·4)" -> "P42(4·4)")
    w1 should not contain ("P24(4·4)" -> "P24(4·4)")
    w1 should not contain ("p8(4·4)"  -> "rco(4·4)")

  "the campaign" should "kill all nine exotic families with the pinned mechanisms" in:
    val byP = campaign.map(k => k.p -> (k.round, k.mechanism.takeWhile(_ != ':'))).toMap
    byP(5) shouldBe (1, "self-walk")
    byP(7) shouldBe (1, "self-walk")
    byP(9) shouldBe (1, "self-walk")
    byP(15) shouldBe (1, "self-walk")
    byP(18) shouldBe (1, "P3-walk")
    byP(24) shouldBe (1, "P3-walk") // the sharp instance: [p3, X, X] closes at 330 ≠ 360
    byP(42) shouldBe (1, "P3-walk")
    byP(10) shouldBe (2, "corona")
    byP(20) shouldBe (2, "corona")
    alphabetClosed shouldBe true
