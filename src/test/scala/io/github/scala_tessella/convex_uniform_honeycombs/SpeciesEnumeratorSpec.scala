package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import HoneycombAlphabet.CellType.*
import SpeciesEnumerator.*

/** The species table — completeness of the spherical assembly against the known
  * honeycombs' vertex stars, the Barlow dichotomy on the octet support, structural invariants (Euler,
  * catalogue closure, exact area), and the pinned catalogue counts.
  */
class SpeciesEnumeratorSpec extends AnyFlatSpec with Matchers:

  private val (all, flags)                                               = enumerated
  private def countOn(counts: Map[HoneycombAlphabet.CellType, Int]): Int =
    all.count(_.counts == counts)

  "the assembly" should "raise no ambiguity flags" in:
    flags shouldBe empty

  it should "find exactly 34 species over 25 of the 97 supports" in:
    all.size shouldBe 34
    all.map(_.counts).distinct.size shouldBe 25
    info(s"by cell count: ${all.groupBy(_.cells).view.mapValues(_.size).toMap.toList.sorted}")

  it should "distribute as 4/5/5/11/4/2/1/2 species over 4/5/6/8/10/12/13/14 cells" in:
    all.groupBy(_.cells).view.mapValues(_.size).toMap shouldBe
      Map(4 -> 4, 5 -> 5, 6 -> 5, 8 -> 11, 10 -> 4, 12 -> 2, 13 -> 1, 14 -> 2)

  "every species" should "be a genuine spherical complex (Euler, area, catalogue closure)" in:
    val figureKeys = HoneycombAlphabet.catalogue.map(_.key).toSet
    all.foreach { sp =>
      sp.vertices - sp.faces + sp.cells shouldBe 2 // V − E + F on S²
      sp.figures.map(_._2).sum shouldBe sp.vertices
      sp.figures.foreach((key, _) => figureKeys should contain(key))
      val total = sp.counts.toList.foldLeft(HoneycombAlphabet.CoreAngle.zero) { case (acc, (c, m)) =>
        HoneycombAlphabet.CoreAngle(
          acc.r + m * SpeciesSupports.cornerExcess(c).r,
          acc.n + m * SpeciesSupports.cornerExcess(c).n
        )
      }
      total shouldBe HoneycombAlphabet.CoreAngle(720, 0)
    }

  it should "sit on one of the 97 supports of the area equation" in:
    val supports = SpeciesSupports.supports.map(_.counts).toSet
    all.foreach(sp => supports should contain(sp.counts))

  "the octet support" should "carry EXACTLY the two Barlow species c and h" in:
    val octet = all.filter(_.counts == Map(Tet -> 8, Oct -> 6))
    octet.size shouldBe 2
    octet.foreach(_.vertices shouldBe 12)
    // c (fcc, cuboctahedral figure): all 12 edge figures alternate tet·oct·tet·oct;
    // h (hcp, orthobicupolar figure): 6 alternating + 6 paired tet·tet·oct·oct
    octet.map(_.figures.size).sorted shouldBe Vector(1, 2)

  it should "be the ONLY species a tetrahedron-octahedron honeycomb can carry" in:
    // what upgrades the dichotomy from a statement about one support to a statement about every
    // face-to-face unit tet-oct honeycomb, with no symmetry hypothesis: a vertex of such a honeycomb has
    // only tet and oct corners, and the octet support is the only one over that sub-alphabet
    // (SpeciesSupportsSpec), so its star is one of exactly these two
    val tetOct = all.filter(_.counts.keySet.subsetOf(Set(Tet, Oct)))
    tetOct.map(_.counts).distinct shouldBe Vector(Map(Tet -> 8, Oct -> 6))
    tetOct.size shouldBe 2

  "the known honeycombs" should "have their vertex species realized, uniquely where classical" in:
    countOn(Map(Cube -> 8)) shouldBe 1 // cubic
    countOn(Map(TruncOct -> 4)) shouldBe 1 // bitruncated cubic
    countOn(Map(Oct -> 1, TruncCube -> 4)) shouldBe 1 // truncated cubic
    countOn(Map(Oct -> 2, Cuboctahedron -> 4)) shouldBe 1 // rectified cubic
    countOn(Map(Cube -> 2, Cuboctahedron -> 1, Rhombicuboctahedron -> 2)) shouldBe 1 // cantellated
    countOn(Map(Cube -> 1, TruncOct -> 1, TruncCuboctahedron -> 2)) shouldBe 1 // cantitruncated
    countOn(Map(TruncCuboctahedron -> 2, P8 -> 2)) shouldBe 1 // omnitruncated
    countOn(Map(TruncTet -> 2, Cuboctahedron -> 1, TruncOct -> 2)) shouldBe 1 // cantic
    countOn(Map(Tet -> 1, Cube -> 1, Rhombicuboctahedron -> 3)) shouldBe 1 // runcinated alt.
    countOn(Map(Tet -> 4, Oct -> 3, P3 -> 6)) shouldBe 1 // elongated alternated cubic
    countOn(Map(P6 -> 6)) shouldBe 1 // hexagonal prismatic
    countOn(Map(Tet -> 2, TruncTet -> 6)) shouldBe 2 // quarter cubic + gyrated figure
    countOn(Map(P3 -> 12)) shouldBe 2 // triangular prismatic + gyrobifastigial

  "the prismatic lifts" should "realize every 2D species with faces in {3,4,6,8,12}" in:
    // 15 2D vertex species lift to prisms doubled; same-support pairs must stay distinct species
    countOn(Map(P3 -> 6, Cube -> 4)) shouldBe 2 // 3.3.3.4.4 and 3.3.4.3.4
    countOn(Map(P3 -> 4, Cube -> 2, P12 -> 2)) shouldBe 2 // 3.3.4.12 and 3.4.3.12
    countOn(Map(P3 -> 2, Cube -> 4, P6 -> 2)) shouldBe 2 // 3.4.4.6 and 3.4.6.4
    countOn(Map(P3 -> 4, P6 -> 4)) shouldBe 3 // 3.3.6.6, 3.6.3.6 + one novel
    countOn(Map(P3 -> 8, P6 -> 2)) shouldBe 2 // 3.3.3.3.6 + one novel
    countOn(Map(P3 -> 2, P12 -> 4)) shouldBe 1 // 3.12.12
    countOn(Map(Cube -> 2, P6 -> 2, P12 -> 2)) shouldBe 1 // 4.6.12
    countOn(Map(Cube -> 2, P8 -> 4)) shouldBe 1 // 4.8.8
