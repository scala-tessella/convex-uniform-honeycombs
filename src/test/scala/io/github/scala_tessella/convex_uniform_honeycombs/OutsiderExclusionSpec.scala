package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import CertifiedDihedrals.*
import OutsiderExclusion.*

/** The corona fixpoint: the vertex-configuration dihedral engine reproduces the exact alphabet table on all
  * 13 core cells (the validation anchor for every certified interval used in the exclusions), the icosahedral
  * edge identity that forces corona closure is real, and the fixpoint excludes ALL outsider targets — the
  * Alphabet Theorem within the stated participant scope.
  */
class OutsiderExclusionSpec extends AnyFlatSpec with Matchers:

  "the vertex-configuration engine" should "reproduce the exact alphabet dihedral table on the core cells" in:
    val byCell = HoneycombAlphabet.edgeTypes.groupBy(_.cell.label)
    coreConfigs.foreach { (name, cfg) =>
      val computed = edgeTypesOf(cfg)
      val exact    = byCell(name)
      withClue(s"$name:") {
        computed.keySet shouldBe
          exact.map(et => (et.faces._1 min et.faces._2, et.faces._1 max et.faces._2)).toSet
        exact.foreach { et =>
          val pair = (et.faces._1 min et.faces._2, et.faces._1 max et.faces._2)
          val iv   = computed(pair)
          withClue(s"$pair:") {
            iv.contains(et.angle.degrees) shouldBe true
            iv.width should be < 1e-7
          }
        }
      }
    }

  it should "confirm the icosahedral identities that make corona closure necessary" in:
    // A5 is the icosahedron minus two pentagonal pyramids: its lateral 3·3 dihedral IS the icosahedral one
    val a5   = edgeTypesOf(antiprismConfig(5))
    val icos = edgeTypesOf(outsiderConfigs("icos"))((3, 3))
    a5((3, 3)).intersects(icos) shouldBe true
    // the exact identity dodec(5·5) + A5(3·5) + icosidodec(3·5) = 360 — a genuine edge figure at value
    // level, killed only because A5 itself dies (its 3·3 edge is the icosahedral angle, no completion)
    val s    = edgeTypesOf(outsiderConfigs("dodec"))((5, 5)) + a5((3, 5)) +
      edgeTypesOf(outsiderConfigs("icosidodec"))((3, 5))
    s.intersects(Iv.point(360.0)) shouldBe true

  "the corona fixpoint" should "exclude every outsider target over the uncapped pool — no survivors" in:
    val fp = fixpoint
    fp.survivors shouldBe empty
    fp.excluded.size shouldBe 9 + (4 to maxTargetAntiprism).size // 9 named + A4..A100
    fp.excluded.size shouldBe 106
    // the published split over the uncapped pool: three rounds, 47 + 44 + 15; the icosahedron, A5 and
    // both snubs die in round one, the pentagon-family trio in round two, and every antiprism
    // 4 <= q <= 100 by the end
    fp.rounds.map(_.size) shouldBe Vector(47, 44, 15)
    fp.rounds(0) should contain allOf ("icos", "A5", "snubCube", "snubDodec")
    fp.rounds(1) should contain allOf ("dodec", "icosidodec", "truncDodec")
    (4 to 100).foreach(q => fp.excluded should contain(s"A$q"))
    info(s"rounds: ${fp.rounds.map(_.size).mkString(" + ")}")

  it should "die at the icosahedral 3·3 edge by a value-level miss, for the icosahedron and A5 alike" in:
    val fp   = fixpoint
    val icos = fp.kills.find(k => k.cell == "icos" && k.edgeType == (3, 3)).get
    val a5   = fp.kills.find(k => k.cell == "A5" && k.edgeType == (3, 3)).get
    icos.round shouldBe 1
    a5.round shouldBe 1
    // the nearest miss at that edge: p3(4·4) + tet(3·3) + A41(3·41), about 0.013° short of 360
    icos.margin should be > 0.01
    a5.margin should be > 0.01
    icos.near.head._1.map(_.label).toSet shouldBe Set("p3(4·4)", "tet(3·3)", "A41(3·41)")
    info(s"icosahedral edge: ${icos.show}")

  it should "decide every dead edge with a margin far above the interval widths" in:
    val fp = fixpoint
    fp.kills should not be empty
    fp.kills.sortBy(_.margin).take(6).foreach(k => info(k.show))
    // the tightest decision of the whole campaign: A56's lateral 3·3 edge against A115(3·115) + A16(3·16),
    // a genuine near-coincidence 1.7e-7° short of 360° (the closed forms agree in double precision), still
    // three orders of magnitude above the interval widths
    fp.kills.foreach(k => withClue(k.show)(k.margin should be > 1e-7))
    fp.minMargin should be > 1.7e-7
    fp.minMargin should be < 1.8e-7
    info(f"minimum margin over ${fp.kills.size} dead edges: ${fp.minMargin}%.3e°")
