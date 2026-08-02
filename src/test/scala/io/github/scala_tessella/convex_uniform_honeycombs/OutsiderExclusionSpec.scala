package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import CertifiedDihedrals.*
import OutsiderExclusion.*

/** The corona fixpoint: the vertex-configuration dihedral engine reproduces the exact alphabet table on all 13
  * core cells (the validation anchor for every certified interval used in the exclusions), the icosahedral
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

  "the corona fixpoint" should "exclude every outsider target — no survivors" in:
    val fp = fixpoint
    fp.survivors shouldBe empty
    fp.rounds.size shouldBe 2
    fp.excluded.size shouldBe 9 + (4 to maxTargetAntiprism).size // 9 named + A4..A100
    fp.rounds(1).toSet shouldBe Set("dodec", "icosidodec", "truncDodec")
    // the published split: 106 targets, 103 in round one — including the icosahedron, both snubs and
    // every antiprism 4 <= q <= 100 — and the pentagon-family trio in round two
    fp.excluded.size shouldBe 106
    fp.rounds(0).size shouldBe 103
    fp.rounds(0) should contain allOf ("icos", "snubCube", "snubDodec")
    (4 to 100).foreach(q => fp.rounds(0) should contain(s"A$q"))
    info(s"round 1: ${fp.rounds(0).size} excluded; round 2: ${fp.rounds(1).mkString(", ")}")
