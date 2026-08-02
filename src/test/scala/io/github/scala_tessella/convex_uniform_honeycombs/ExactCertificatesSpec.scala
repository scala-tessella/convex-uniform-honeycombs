package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ExactCertificates.*

/** The exactness escalation: the finitely many certified objects of the completeness
  * theorem — 34 star models, 28 class-representative periodization certificates, the two doubled-species
  * separations — hold as exact identities in ℚ(√2,√3). No floating-point tolerance decision remains on the
  * theorem's critical path: every equality the certificates assert is verified in the field, every inequality
  * is an exact sign or an interval certificate.
  */
class ExactCertificatesSpec extends AnyFlatSpec with Matchers:

  private val r = results

  "the quartic tower" should "multiply, invert and sign exactly" in:
    (Q23.sqrt2 * Q23.sqrt3) shouldBe Q23.sqrt6
    (Q23.sqrt2 * Q23.sqrt2) shouldBe Q23.ofInt(2)
    val x = Q23.one + Q23.sqrt2 + Q23.sqrt3 + Q23.sqrt6
    (x * x.inverse) shouldBe Q23.one
    (Q23.sqrt6 - Q23.sqrt2 - Q23.sqrt3 + Q23.one).signum shouldBe 1 // (√2−1)(√3−1) > 0
    (Q23.sqrt6 - Q23.sqrt2 - Q23.sqrt3).signum shouldBe -1          // 2.449… − 3.146… < 0
    (Q23.sqrt3 - Q23.sqrt2).signum shouldBe 1
    Q23.build(-3, 0, 0, 0, 2).signum shouldBe -1

  "recognition" should "identify the classical constants" in:
    recognize(math.sqrt(2) / 2) shouldBe Some(Q23.build(0, 1, 0, 0, 2))
    recognize(math.sqrt(3) / 2) shouldBe Some(Q23.build(0, 0, 1, 0, 2))
    recognize((math.sqrt(6) + math.sqrt(2)) / 4) shouldBe Some(Q23.build(0, 1, 0, 1, 4)) // cos 15°
    recognize(-1.0 / 3) shouldBe Some(Q23.build(-1, 0, 0, 0, 3))

  "the exact star models" should "cover all 34 species with certified Gram matrices" in:
    r.stars.size shouldBe 34
    r.stars.filterNot(_.ok) shouldBe empty

  it should "have tight interval enclosures around every recognized Gram entry" in:
    // the published bound: every recognized Gram entry sits inside an enclosure of width < 6e-9
    r.stars.foreach(s => s.maxIvWidth should be < 6e-9)

  "the exact certificates" should "raise no flags" in:
    r.flags shouldBe empty

  it should "cover all 28 classes" in:
    r.classes.size shouldBe 28
    r.classes.filterNot(_.ok) shouldBe empty

  it should "verify every gluing, R1, R2 and the translations exactly" in:
    r.classes.foreach { c =>
      withClue(s"${SpeciesCorona.label(c.idx)}#${c.classIdx}") {
        c.gluOk shouldBe true
        c.r1Ok shouldBe true
        c.r2Ok shouldBe true
        c.transOk shouldBe true
        c.indepOk shouldBe true
      }
    }

  it should "develop collision-free periodic balls with invariant lattices and covered domains" in:
    r.classes.foreach { c =>
      withClue(s"${SpeciesCorona.label(c.idx)}#${c.classIdx}") {
        c.collisionFree shouldBe true
        c.periodic shouldBe true
        c.latInv shouldBe true
        c.coverage shouldBe true
        c.ballVerts should be > 0
      }
    }
    // the published range of the certified exact balls
    r.classes.map(_.ballVerts).min shouldBe 515
    r.classes.map(_.ballVerts).max shouldBe 4521

  "exact separation" should "distinguish the two classes of both doubled species" in:
    r.separations.size shouldBe 2
    r.separations.map(s => SpeciesCorona.label(s.idx)).toSet shouldBe
      Set("{cube:4 p3:6}#1", "{tet:4 oct:3 p3:6}#1")
    r.separations.foreach(_.distinct shouldBe true)

  "the escalation" should "conclude: 28/28 exact, zero tolerance decisions on the critical path" in:
    r.allOk shouldBe true
