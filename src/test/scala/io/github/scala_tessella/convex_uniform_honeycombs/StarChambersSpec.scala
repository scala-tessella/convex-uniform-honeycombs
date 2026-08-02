package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import SpeciesEnumerator.species

/** The chamber complex of every species star must satisfy the flag laws —
  * fixed-point-free involutions, σ₁σ₃ commuting, (σ₁σ₂)-orbits the corner polygons, (σ₂σ₃)-orbits the edge
  * rings, chamber count 4·#arcs, invariant data preserved by the right involutions, and connectivity — all
  * checked against independent species and corona data (corner sizes, ring sizes), with the octet and cubic counts
  * pinned by hand.
  */
class StarChambersSpec extends AnyFlatSpec with Matchers:

  "every species' chamber complex" should "satisfy the flag laws against independent star data" in:
    for i <- species.indices do
      val sp = species(i)
      val cx = StarChambers.complexOf(sp)
      val n  = cx.chambers.size
      withClue(s"species ${SpeciesCorona.label(i)}: "):
        n shouldBe 4 * sp.state.arcs.size
        // fixed-point-free involutions
        for s <- Vector(cx.s1, cx.s2, cx.s3); c <- 0 until n do
          s(s(c)) shouldBe c
          s(c) should not be c
        // σ1 and σ3 commute (they act on independent flag coordinates)
        for c <- 0 until n do cx.s1(cx.s3(c)) shouldBe cx.s3(cx.s1(c))
        // the (σ1σ2)-COMPOSITION has order p around a p-corner (the m12 convention: each step rotates one
        // side, the 2p flags split into two p-cycles)
        for c <- 0 until n do
          StarChambers.orbitSize(cx.s1, cx.s2, c) shouldBe
            sp.state.corners(cx.chambers(c).corner).vids.size
        // the (σ2σ3)-composition has order r around an r-ring edge (ring sizes from the corona, independently)
        val ringSize = sp.state.positions.indices.map(x => x -> SpeciesCorona.ringAt(sp.state, x).size).toMap
        for c <- 0 until n do
          StarChambers.orbitSize(cx.s2, cx.s3, c) shouldBe ringSize(cx.endVertex(c))
        // the invariant data: σ1/σ3 fix the face size, σ2/σ3 fix the edge's end vertex
        for c <- 0 until n do
          cx.faceSize(cx.s1(c)) shouldBe cx.faceSize(c)
          cx.faceSize(cx.s3(c)) shouldBe cx.faceSize(c)
          cx.endVertex(cx.s2(c)) shouldBe cx.endVertex(c)
          cx.endVertex(cx.s3(c)) shouldBe cx.endVertex(c)
        // connected under {σ1, σ2, σ3}: one vertex's flags form a single complex
        val seen     = collection.mutable.Set(0)
        var front    = List(0)
        while front.nonEmpty do
          front = front.flatMap(c => List(cx.s1(c), cx.s2(c), cx.s3(c)).filter(seen.add))
        seen.size shouldBe n

  it should "have the hand-pinned octet and cubic chamber counts" in:
    def bySupport(sup: String): Vector[Int] =
      species.indices.toVector.filter(i => species(i).showSupport == sup)
    // octet: 8 tet corners (triangles) + 6 oct corners (squares): 2*(8*3 + 6*4) = 96 chambers, 24 arcs
    for i <- bySupport("{tet:8 oct:6}") do
      StarChambers.complexOf(species(i)).chambers.size shouldBe 96
    // cubic: 8 cube corners (triangles): 2*8*3 = 48 chambers, 12 arcs
    for i <- bySupport("{cube:8}") do
      StarChambers.complexOf(species(i)).chambers.size shouldBe 48
