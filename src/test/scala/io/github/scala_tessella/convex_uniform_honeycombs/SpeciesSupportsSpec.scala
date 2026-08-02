package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import HoneycombAlphabet.CellType.*
import SpeciesSupports.*

/** The area equation: exact corner excesses, the split area equation, and the presence of the known
  * honeycombs' vertex supports among the solutions.
  */
class SpeciesSupportsSpec extends AnyFlatSpec with Matchers:

  "corner excesses" should "be exact lattice values matching the classical solid angles" in:
    cornerExcess(Tet) shouldBe HoneycombAlphabet.CoreAngle(360, -6)
    cornerExcess(Oct) shouldBe HoneycombAlphabet.CoreAngle(-360, 8)
    cornerExcess(Cube) shouldBe HoneycombAlphabet.CoreAngle(90, 0)
    cornerExcess(TruncOct) shouldBe HoneycombAlphabet.CoreAngle(180, 0)
    cornerExcess(P3) shouldBe HoneycombAlphabet.CoreAngle(60, 0)
    // numeric anchors: tet corner ≈ 31.586°, oct corner ≈ 77.885° (excess degrees)
    cornerExcess(Tet).degrees shouldBe 31.586 +- 1e-3
    cornerExcess(Oct).degrees shouldBe 77.885 +- 1e-3

  "the supports" should "contain the vertex multisets of the known honeycombs" in:
    val set = supports.map(_.counts).toSet
    set should contain(Map(Cube -> 8)) // cubic
    set should contain(Map(Tet -> 8, Oct -> 6)) // octet (fcc/hcp and all stackings)
    set should contain(Map(TruncOct -> 4)) // bitruncated cubic
    set should contain(Map(Tet -> 2, TruncTet -> 6)) // quarter cubic
    set should contain(Map(P3 -> 12)) // triangular prismatic
    set should contain(Map(P6 -> 6)) // hexagonal prismatic
    set should contain(Map(Oct -> 1, TruncCube -> 4)) // truncated cubic
    info(s"total supports: ${supports.size}")
    info(s"by cell count: ${supports.groupBy(_.cells).view.mapValues(_.size).toMap.toList.sorted}")

  it should "be exactly the published 97, with a unique 14-cell support" in:
    supports.size shouldBe 97
    supports.filter(_.cells == 14).map(_.counts) shouldBe Vector(Map(Tet -> 8, Oct -> 6))

  it should "satisfy the exact equations" in:
    supports.foreach { s =>
      val total = s.counts.toList.foldLeft(HoneycombAlphabet.CoreAngle(0, 0)) { case (acc, (c, m)) =>
        HoneycombAlphabet.CoreAngle(acc.r + m * cornerExcess(c).r, acc.n + m * cornerExcess(c).n)
      }
      total shouldBe HoneycombAlphabet.CoreAngle(720, 0)
    }
