package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import TailExclusion.*

/** The tails: the antiprism closed forms agree with the interval reconstruction and the exact ℚ(√5)
  * values, Lemma E's machinery has its constants and interleaving certified, the finite prism sweep over the
  * FULL pool re-finds exactly the six prism-grid exotic families, and every tail target is excluded with all threats
  * refuted by named lemmas — no exact follow-up left.
  */
class TailExclusionSpec extends AnyFlatSpec with Matchers:

  "the closed forms" should "agree with the interval reconstruction" in:
    import CertifiedDihedrals.*
    for q <- List(4, 5, 6, 10, 50) do
      val ivs = edgeTypesOf(antiprismConfig(q))
      a3qDeg(q).intersects(ivs((3, q))) shouldBe true
      a33Deg(q).intersects(ivs((3, 3))) shouldBe true

  it should "reproduce the exact ℚ(√5) values at q = 5 (A5 base and the icosahedron)" in:
    val base = math.toDegrees(math.acos(-math.sqrt(IcosahedralIdentities.r2.toDouble)))
    val ico  = math.toDegrees(math.acos(-math.sqrt(5.0) / 3.0))
    a3qDeg(5).contains(base) shouldBe true
    a33Deg(5).contains(ico) shouldBe true

  "lemma E" should "have its explicit-constant chain verified" in:
    lemmaEConstants shouldBe true

  it should "interleave h(2q) < g(q) < h(2q−1) on the certified grid" in:
    (4 to 400).forall(interleaves) shouldBe true
    List(1000, 5000, 20000).forall(interleaves) shouldBe true

  "the finite prism sweep" should "re-find exactly the six prism-grid exotic families" in:
    val (survivors, _) = results
    survivors shouldBe Vector(5, 7, 9, 10, 15, 18, 20, 24, 42)
    // spot the classical shadows: 3.7.42 and the 3.8.24 lift
    val by             = prismSweep.toMap
    by(7).exists(_.chain.exists(_.label.startsWith("P42"))) shouldBe true
    by(24).exists(_.chain.exists(c => c.label == "p3(4·4)")) shouldBe true

  it should "exclude every other p ≤ 500 with all threats refuted" in:
    prismSweep.filterNot(t => Set(5, 7, 9, 10, 15, 18, 20, 24, 42)(t._1)).foreach { (_, threats) =>
      threats shouldBe empty // only unrefuted threats are kept by the sweep
    }

  "the tails" should "exclude every antiprism q ≥ 101 and the two tail families" in:
    val (_, verdicts) = results
    verdicts.filterNot(_._2) shouldBe empty
    verdicts.size shouldBe 202 // A101..A300, A>300, P>500

  it should "refute the prism-tail window with no exact follow-up" in:
    prismTailVerdict.filter(_.refutation == Refutation.Unrefuted) shouldBe empty

  it should "kill the designed threats by the designed lemmas" in:
    val tail = antiprismVerdict("A>300", a3qTail(301))
    val refs = tail.threats.map(t => t.chain.map(_.label).sorted.mkString("+") -> t.refutation).toMap
    refs("oct(3·3)+tet(3·3)") shouldBe Refutation.StrictLemmaA("exact 180")
    refs(s"A>300(3·3)") shouldBe Refutation.LemmaE
    tail.threats.count(_.refutation == Refutation.AboveLemmaA) should be >= 1
