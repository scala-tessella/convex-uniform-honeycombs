package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import HoneycombAlphabet.*
import HoneycombAlphabet.CellType.*

/** The core alphabet: the exact dihedral table matches the classical values, the α-charge decomposition is
  * faithful, and the edge-figure catalogue contains exactly the figures realized by the known honeycombs
  * (cubic, octet both ways, bitruncated cubic, truncated cubic, prismatic slabs).
  */
class HoneycombAlphabetSpec extends AnyFlatSpec with Matchers:

  /** Independent classical dihedral cosines, per (cell, face pair). */
  private val classicalCos: Map[(CellType, (Int, Int)), Double] = Map(
    (Tet, (3, 3))                 -> 1.0 / 3,
    (Cube, (4, 4))                -> 0.0,
    (Oct, (3, 3))                 -> -1.0 / 3,
    (TruncTet, (6, 6))            -> 1.0 / 3,
    (TruncTet, (3, 6))            -> -1.0 / 3,
    (Cuboctahedron, (3, 4))       -> -math.sqrt(3) / 3,
    (TruncOct, (4, 6))            -> -math.sqrt(3) / 3,
    (TruncOct, (6, 6))            -> -1.0 / 3,
    (TruncCube, (3, 8))           -> -math.sqrt(3) / 3,
    (TruncCube, (8, 8))           -> 0.0,
    (Rhombicuboctahedron, (3, 4)) -> -math.sqrt(6) / 3,
    (Rhombicuboctahedron, (4, 4)) -> -math.sqrt(2) / 2,
    (TruncCuboctahedron, (4, 6))  -> -math.sqrt(6) / 3,
    (TruncCuboctahedron, (4, 8))  -> -math.sqrt(2) / 2,
    (TruncCuboctahedron, (6, 8))  -> -math.sqrt(3) / 3,
    (P3, (4, 4))                  -> 0.5,
    (P3, (3, 4))                  -> 0.0,
    (P6, (4, 4))                  -> -0.5,
    (P6, (4, 6))                  -> 0.0,
    (P8, (4, 4))                  -> -math.sqrt(2) / 2,
    (P8, (4, 8))                  -> 0.0,
    (P12, (4, 4))                 -> -math.sqrt(3) / 2,
    (P12, (4, 12))                -> 0.0
  )

  "the dihedral table" should "match the classical values through the (r, n·α) decomposition" in:
    edgeTypes should have size 23
    edgeTypes.foreach { et =>
      val expected = math.toDegrees(math.acos(classicalCos(et.cell -> et.faces)))
      withClue(s"${et.cell} ${et.faces}:") {
        et.angle.degrees shouldBe expected +- 1e-9
        et.angle.r % 15 shouldBe 0
        math.abs(et.angle.n) should be <= 2
      }
    }

  it should "cover every (cell, face-pair) exactly once" in:
    edgeTypes.map(et => (et.cell, et.faces)).distinct should have size 23
    classicalCos.keySet shouldBe edgeTypes.map(et => (et.cell, et.faces)).toSet

  private def figure(entries: (CellType, Int, Int)*): CanonKey =
    val tokens = entries.toVector.map { (c, l, r) =>
      val et = edgeTypes.find(e => e.cell == c && (e.faces == (l, r) || e.faces == (r, l))).get
      Oriented(et, flipped = et.faces != (l, r))
    }
    // chain sanity of the hand-built figure itself
    tokens.indices.foreach { i =>
      tokens(i).right shouldBe tokens((i + 1) % tokens.size).left
    }
    canonical(tokens)

  private lazy val keys = catalogue.map(_.key).toSet

  "the catalogue" should "contain the edge figures of the known honeycombs" in:
    // cubic {4,3,4}: four cubes
    keys should contain(figure((Cube, 4, 4), (Cube, 4, 4), (Cube, 4, 4), (Cube, 4, 4)))
    // octet, alternating and non-alternating (the 2+2 edge lemma's two cyclic types)
    keys should contain(figure((Tet, 3, 3), (Oct, 3, 3), (Tet, 3, 3), (Oct, 3, 3)))
    keys should contain(figure((Tet, 3, 3), (Tet, 3, 3), (Oct, 3, 3), (Oct, 3, 3)))
    // bitruncated cubic: three truncated octahedra
    keys should contain(figure((TruncOct, 6, 6), (TruncOct, 6, 4), (TruncOct, 4, 6)))
    // truncated cubic: 8·8 edge (four truncated cubes) and 3·8 edge (two truncated cubes + octahedron)
    keys should contain(
      figure((TruncCube, 8, 8), (TruncCube, 8, 8), (TruncCube, 8, 8), (TruncCube, 8, 8))
    )
    keys should contain(figure((TruncCube, 3, 8), (TruncCube, 8, 3), (Oct, 3, 3)))
    // prismatic slabs: 6 triangular prisms; 3.12.12 lift (two P12 + P3)
    keys should contain(figure(Vector.fill(6)((P3, 4, 4))*))
    keys should contain(figure((P12, 4, 4), (P12, 4, 4), (P3, 4, 4)))

  it should "satisfy the exact closure invariants" in:
    catalogue should not be empty
    catalogue.foreach { f =>
      withClue(f.show) {
        f.size should (be >= 3 and be <= 6)
        f.tokens.map(_.et.angle).foldLeft(CoreAngle.zero)(_ + _) shouldBe CoreAngle.full
        f.tokens.indices.foreach { i =>
          f.tokens(i).right shouldBe f.tokens((i + 1) % f.size).left
        }
      }
    }
    info(s"catalogue size: ${catalogue.size} edge figures")
    info(s"by cell count: ${catalogue.groupBy(_.size).view.mapValues(_.size).toMap.toList.sorted}")

  it should "be the published catalogue: 69 figures over 62 cell multisets, 22/39/7/1 by ring size" in:
    catalogue.size shouldBe 69
    catalogue.groupBy(_.size).view.mapValues(_.size).toMap shouldBe Map(3 -> 22, 4 -> 39, 5 -> 7, 6 -> 1)
    catalogue.groupBy(_.cells.map(_.label).sorted).size shouldBe 62

  it should "pin the face-compatibility statements of the catalogue" in:
    // 12-gonal faces pair P12 only with itself; 8-gonal faces occur only among truncCube, tco and P8
    cellsWithFace(12).toSet shouldBe Set(P12)
    cellsWithFace(8).toSet shouldBe Set(TruncCube, TruncCuboctahedron, P8)
    // alpha-charged dihedrals appear in exactly 23 of the 69 figures
    catalogue.count(_.tokens.exists(_.et.angle.n != 0)) shouldBe 23

  "the Platonic sub-alphabet" should "reproduce the note's edge lemma exactly" in:
    val platonic = restrictedTo(Set(Tet, Cube, Oct))
    // {4,3,4}: 4 cubes; tet–oct: exactly the two 2+2 cyclic types — nothing else
    platonic.map(_.show).sorted.foreach(s => info(s))
    platonic should have size 3
    platonic.count(_.cells.toSet == Set(Cube)) shouldBe 1
    platonic.count(_.cells.toSet == Set(Tet, Oct)) shouldBe 2
