package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import The28.*

/** The classical cross-check: the 28 certified classes are exactly the classical 28 — names, families and
  * vertex compositions pinned, the two doubled species resolved by exact lattice invariants on the certified
  * translation bases.
  */
class The28IdentifiedSpec extends AnyFlatSpec with Matchers:

  private val (rows, flags) = identified

  "the identification" should "raise no flags" in:
    flags shouldBe empty

  it should "name all 28, distinctly" in:
    rows.size shouldBe 28
    rows.map(_.name).distinct.size shouldBe 28

  it should "reproduce the classical family census 9 + 3 + 1 + 10 + 5" in:
    rows.groupBy(_.family).view.mapValues(_.size).toMap shouldBe Map(
      "cubic family"   -> 9,
      "B family"       -> 3,
      "quarter"        -> 1,
      "prismatic lift" -> 10,
      "gyro/elongated" -> 5
    )

  it should "match every vertex composition to the classical tables" in:
    rows.foreach(r => withClue(r.name)(classicalSupports(r.name) shouldBe r.support))

  "the doubled species" should "split into elongated and gyroelongated by the exact invariants" in:
    val tri = rows.filter(_.label == "{cube:4 p3:6}#1").map(_.name).toSet
    tri shouldBe Set("elongated triangular prismatic", "gyroelongated triangular prismatic")
    val alt = rows.filter(_.label == "{tet:4 oct:3 p3:6}#1").map(_.name).toSet
    alt shouldBe Set("elongated alternated cubic", "gyroelongated alternated cubic")

  "the anchors" should "sit where the classics put them" in:
    rows.find(_.name == "cubic").get.support shouldBe "{cube:8}"
    rows.find(_.name == "alternated cubic").get.support shouldBe "{tet:8 oct:6}"
    rows.find(_.name == "bitruncated cubic").get.support shouldBe "{truncOct:4}"
    rows.find(_.name == "quarter cubic").get.support shouldBe "{tet:2 truncTet:6}"
    rows.find(_.name == "gyrated alternated cubic").get.label shouldBe "{tet:8 oct:6}#1"
    rows.find(_.name == "snub trihexagonal prismatic").get.label shouldBe "{p3:8 p6:2}#2"
