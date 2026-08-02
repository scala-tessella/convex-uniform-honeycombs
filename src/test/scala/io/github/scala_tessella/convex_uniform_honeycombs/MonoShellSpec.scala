package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import MonoShell.*

/** The mono-species shell filter — 26 of the 34 species admit a consistent
  * same-species first shell and 8 are excluded from k = 1; the excluded set mirrors the 2D classification
  * (the four lifts of extendable-but-nonuniform 2D species die) and every star of a known uniform honeycomb
  * survives; no geometric decision was close (flags empty).
  */
class MonoShellSpec extends AnyFlatSpec with Matchers:

  private val (rs, flags)                   = results
  private def label(i: Int)                 = SpeciesCorona.label(i)
  private val verdict: Map[String, Boolean] =
    rs.map((i, r) => label(i) -> r.sat).toMap

  "the shell filter" should "raise no ambiguity flags" in:
    flags shouldBe empty

  it should "pass exactly 26 of the 34 species" in:
    rs.count(_._2.sat) shouldBe 26

  it should "exclude exactly the eight species, mirroring the 2D classification" in:
    val unsat = rs.filterNot(_._2.sat).map((i, _) => label(i)).toSet
    unsat shouldBe Set(
      "{cube:2 p3:4 p12:2}#1",          // 3.3.4.12 lift  — extendable 2D species, no uniform tiling
      "{cube:2 p3:4 p12:2}#2",          // 3.4.3.12 lift  —   "
      "{cube:4 p3:2 p6:2}#1",           // 3.4.4.6 lift   —   "
      "{p3:4 p6:4}#1",                  // 3.3.6.6 lift   —   "
      "{p3:4 p6:4}#2",                  // novel mixed-axis species
      "{tet:1 truncTet:3 p3:2 p6:2}#1", // novel (quarter-cubic/prism interface)
      "{tet:2 truncTet:6}#1",           // novel gyrated quarter figure
      "{p3:8 p6:2}#1"                   // novel
    )

  "the known uniform stars" should "all survive" in:
    // cubic family (8), tet-oct family (7), prismatic (11)
    Vector(
      "{cube:8}#1",
      "{oct:2 co:4}#1",
      "{oct:1 truncCube:4}#1",
      "{truncOct:4}#1",
      "{cube:2 co:1 rco:2}#1",
      "{cube:1 truncOct:1 tco:2}#1",
      "{cube:1 truncCube:1 rco:1 p8:2}#1",
      "{tco:2 p8:2}#1",
      "{tet:8 oct:6}#1",
      "{tet:8 oct:6}#2",
      "{tet:4 oct:3 p3:6}#1",
      "{truncTet:2 co:1 truncOct:2}#1",
      "{tet:2 truncTet:6}#2",
      "{truncTet:1 truncCube:1 tco:2}#1",
      "{tet:1 cube:1 rco:3}#1",
      "{p3:12}#1",
      "{p3:12}#2",
      "{p6:6}#1",
      "{p3:4 p6:4}#3",
      "{p3:2 p12:4}#1",
      "{cube:4 p3:2 p6:2}#2",
      "{p3:8 p6:2}#2",
      "{cube:2 p8:4}#1",
      "{cube:4 p3:6}#1",
      "{cube:4 p3:6}#2",
      "{cube:2 p6:2 p12:2}#1"
    ).foreach(l => withClue(l)(verdict(l) shouldBe true))

  "the gluing atlas" should "show the cubic star's 48 gluings per vertex (the octahedral group)" in:
    val cubic = rs.find((i, _) => label(i) == "{cube:8}#1").get._2
    cubic.counts shouldBe Vector.fill(6)(48)
