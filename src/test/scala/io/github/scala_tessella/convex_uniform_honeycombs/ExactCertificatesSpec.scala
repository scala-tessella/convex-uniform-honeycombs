package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ExactCertificates.*

/** The exactness escalation: the finitely many certified objects of the completeness theorem — 34 star
  * models, 28 class-representative periodization certificates, the coherence of every accepted pattern with
  * its class (opt-in, `-DexactCoherence`, always under `-Dcerts`), germ forcing on every skeleton, the two
  * doubled-species separations — hold as exact identities in ℚ(√2,√3). Every positive certificate on the
  * theorem's critical path is exact; what stays numeric is the enumerations' negative decisions and the
  * coherence of the exhaustion patterns.
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

  it should "develop collision-free periodic, generator-equivariant balls with covered domains" in:
    r.classes.foreach { c =>
      withClue(s"${SpeciesCorona.label(c.idx)}#${c.classIdx}") {
        c.collisionFree shouldBe true
        c.periodic shouldBe true
        c.latInv shouldBe true
        c.coverage shouldBe true
        c.genEquiv shouldBe true
        c.boxPeriodic shouldBe true
        c.closed shouldBe true
        c.ok shouldBe true
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

  "exact coherence" should "certify and align every accepted pattern with its class representative" in:
    // an exact ball per accepted pattern, about half an hour: opt-in, and always part of the certs run
    assume(sys.props.contains("exactCoherence") || sys.props.contains("certs"))
    val c = coherenceResults
    c.flags shouldBe empty
    c.coherences.size should be > r.classes.size // members, not only representatives
    c.coherences.filterNot(_.ok) shouldBe empty
    // every class is represented among the cohered patterns
    c.coherences.map(c => (c.idx, c.classIdx)).distinct.size shouldBe 28
    // the exhaustions: every accepted pattern beyond the caps cohered exactly, none capped; eight of the
    // ten skeletons carry no consistent pattern at all, the snub lift's second and third carry 256 each,
    // 128 of them accepted (the third is "dead" within the cap of 40: all 128 lie beyond it)
    // (the numeric audit exhausts ten; six of those are excluded exactly in (d), so four remain open)
    c.exhaustions.size shouldBe 4
    c.exhaustions.filterNot(_.ok) shouldBe empty
    c.exhaustions.map(e => (SpeciesCorona.label(e.idx), e.skeleton, e.patterns, e.accepted)).filter(_._3 >
      0) shouldBe
      Vector(("{p3:8 p6:2}#2", 1, 256, 128), ("{p3:8 p6:2}#2", 2, 256, 128))
    c.allOk shouldBe true

  "exact germ forcing" should
    "force, exclude or leave open every skeleton, refining the numeric audit's ten exhaustions" in:
      r.germs should not be empty
      val forced      = r.germs.filter(_.forced)
      val open        = r.germs.filter(_.open)
      val excluded    = r.germs.filter(_.excluded)
      forced.size should be > 0
      (forced.size + open.size + excluded.size) shouldBe r.germs.size
      // the numeric audit closes ten skeletons by exhaustion; exactly, six of them hold no genuine gluing
      // at some tiling-vertex (excluded) and four remain open (only the snub lift's second and third hold
      // patterns); four skeletons the numeric audit forces are excluded as well
      val numericOpen = CompletenessAudit.results._1.flatMap(a => a.exhaustedSkeletons.map(si => (a.idx, si)))
      open.map(g => (g.idx, g.skeleton)).foreach(numericOpen should contain(_))
      numericOpen.foreach(k => (open ++ excluded).map(g => (g.idx, g.skeleton)) should contain(k))
      open.map(g => (SpeciesCorona.label(g.idx), g.skeleton)) shouldBe Vector(
        ("{cube:4 p3:6}#2", 0),
        ("{p3:8 p6:2}#2", 0),
        ("{p3:8 p6:2}#2", 1),
        ("{p3:8 p6:2}#2", 2)
      )
      excluded.map(g => (SpeciesCorona.label(g.idx), g.skeleton)) shouldBe Vector(
        ("{cube:2 p8:4}#1", 1),
        ("{p3:2 p12:4}#1", 1),
        ("{p6:6}#1", 0),
        ("{p6:6}#1", 1),
        ("{p6:6}#1", 3),
        ("{p6:6}#1", 4),
        ("{p6:6}#1", 5),
        ("{p6:6}#1", 6),
        ("{p6:6}#1", 7),
        ("{p3:12}#2", 1)
      )
      (forced.size, excluded.size, open.size) shouldBe (27, 10, 4)
      info(s"skeletons ${r.germs.size}: forced ${forced.size}, excluded ${excluded.size}, open ${open.size}")

  "the exact atlas" should
    "keep at least one genuine gluing at every tiling-vertex and recognize every image" in:
      r.atlas.size shouldBe 26
      r.atlas.foreach { a =>
        withClue(SpeciesCorona.label(a.idx)) {
          a.genuine.zip(a.atlas).foreach((gn, at) => gn should (be > 0 and be <= at))
          a.unrecognized.sum shouldBe 0
        }
      }
      // the cubic star: all 48 gluings genuine at every tiling-vertex (the octahedral group)
      val cubic = r.atlas.find(a => SpeciesCorona.label(a.idx) == "{cube:8}#1").get
      cubic.genuine shouldBe Vector.fill(6)(48)
      info(r.atlas.map(a =>
        s"${SpeciesCorona.label(a.idx)}: ${a.atlas.mkString(",")} -> ${a.genuine.mkString(",")}"
      )
        .mkString("; "))

  "the escalation" should "conclude: 28/28 exact, every positive certificate on the critical path exact" in:
    r.allOk shouldBe true
