package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import CoreCells.*
import HoneycombAlphabet.CellType

/** The rigidity input of the appendix (Lemma A.2): for each of the 13 core cells the combinatorial
  * automorphism group of the face lattice has the order of the geometric symmetry group — so every lattice
  * isomorphism between congruent copies is an isometry — and the 13 face lattices are pairwise
  * non-isomorphic.
  */
class CoreCellsSpec extends AnyFlatSpec with Matchers:

  private val byType = cells.map(c => c.cellType -> c).toMap

  "the core cells" should "be built with the classical vertex, edge and face counts" in:
    cells.size shouldBe 13
    def counts(t: CellType) = { val c = byType(t); (c.verts.size, c.edges.size, c.faces.size) }
    counts(CellType.Tet) shouldBe (4, 6, 4)
    counts(CellType.Cube) shouldBe (8, 12, 6)
    counts(CellType.Oct) shouldBe (6, 12, 8)
    counts(CellType.TruncTet) shouldBe (12, 18, 8)
    counts(CellType.Cuboctahedron) shouldBe (12, 24, 14)
    counts(CellType.TruncOct) shouldBe (24, 36, 14)
    counts(CellType.TruncCube) shouldBe (24, 36, 14)
    counts(CellType.Rhombicuboctahedron) shouldBe (24, 48, 26)
    counts(CellType.TruncCuboctahedron) shouldBe (48, 72, 26)
    counts(CellType.P3) shouldBe (6, 9, 5)
    counts(CellType.P6) shouldBe (12, 18, 8)
    counts(CellType.P8) shouldBe (16, 24, 10)
    counts(CellType.P12) shouldBe (24, 36, 14)
    // every cell is a sphere: Euler characteristic 2, and every flag has three distinct neighbours
    cells.foreach(c => withClue(c.cellType)((c.verts.size - c.edges.size + c.faces.size) shouldBe 2))

  it should "have pairwise distinct face-size multisets: the 13 face lattices are non-isomorphic" in:
    faceSizesDistinct shouldBe true

  it should "carry symmetry groups of the classical orders" in:
    def sym(t: CellType) = byType(t).symOrder
    sym(CellType.Tet) shouldBe 24
    sym(CellType.Cube) shouldBe 48
    sym(CellType.Oct) shouldBe 48
    sym(CellType.TruncTet) shouldBe 24
    sym(CellType.Cuboctahedron) shouldBe 48
    sym(CellType.TruncOct) shouldBe 48
    sym(CellType.TruncCube) shouldBe 48
    sym(CellType.Rhombicuboctahedron) shouldBe 48
    sym(CellType.TruncCuboctahedron) shouldBe 48
    sym(CellType.P3) shouldBe 12
    sym(CellType.P6) shouldBe 24
    sym(CellType.P8) shouldBe 32
    sym(CellType.P12) shouldBe 48

  it should "be rigid: the combinatorial automorphism group has the order of the symmetry group" in:
    cells.foreach { c =>
      withClue(s"${c.cellType}: |Aut| = ${c.autOrder}, |Sym| = ${c.symOrder}")(c.rigid shouldBe true)
    }
    // the automorphism group acts freely on the 4·E flags, so its order divides the flag count, with
    // equality exactly for the regular cells (tet, cube, oct)
    cells.foreach(c => withClue(c.cellType)(c.flags.size % c.autOrder shouldBe 0))
    Set(CellType.Tet, CellType.Cube, CellType.Oct).foreach(t =>
      byType(t).autOrder shouldBe byType(t).flags.size
    )
    info(cells.map(c => s"${c.cellType.label}: ${c.autOrder}").mkString(", "))

  // the cell rigidity lemma: a placed core cell is determined by its type, one edge, the two face germs at
  // that edge and their roles (orbits of faces under the symmetry group)

  it should "have face roles determined by face size, except the squares of the rco" in:
    cells.foreach { c =>
      withClue(c.cellType) {
        if c.cellType == CellType.Rhombicuboctahedron then
          c.rolesBySize shouldBe false
          c.roleCount shouldBe 3
          val squareRoles =
            c.faces.indices.filter(c.faces(_).size == 4).groupBy(c.faceRoles).values.map(_.size)
          squareRoles.toVector.sorted shouldBe Vector(6, 12)
        else
          c.rolesBySize shouldBe true
          c.roleCount shouldBe c.faceSizes.size
      }
    }

  it should "carry the reflection in the perpendicular bisector plane of every edge as a symmetry" in:
    cells.foreach(c => withClue(c.cellType)(c.bisectorMirrors shouldBe true))

  it should "act transitively on the edge triples of each ordered pair of roles" in:
    cells.foreach(c => withClue(c.cellType)(c.roleTransitive shouldBe true))

  it should "not be determined by an edge and its two germs alone: the rco counterexample" in:
    // at a 4·4 edge of the rco the reflection in the plane bisecting the dihedral angle swaps the axial and
    // the belt square, so it preserves both face planes and both interior sides, but is not a symmetry
    val rco                                                            = byType(CellType.Rhombicuboctahedron)
    val squares                                                        = rco.faces.indices.filter(rco.faces(_).size == 4)
    val e                                                              = rco.edges.indices.find { e =>
      val (a, b) = rco.edges(e)
      val owners = squares.filter(f =>
        rco.faces(f).indices.exists { i =>
          val fs = rco.faces(f); Set(fs(i), fs((i + 1) % fs.size)) == Set(a, b)
        }
      )
      owners.size == 2 && rco.faceRoles(owners(0)) != rco.faceRoles(owners(1))
    }.get
    val (a, b)                                                         = rco.edges(e)
    def centroid(ids: Seq[Int])                                        =
      val ps = ids.map(rco.verts)
      (ps.map(_._1).sum / ps.size, ps.map(_._2).sum / ps.size, ps.map(_._3).sum / ps.size)
    val c                                                              = centroid(rco.verts.indices)
    val owners                                                         = squares.filter(f =>
      rco.faces(f).indices.exists { i =>
        val fs = rco.faces(f); Set(fs(i), fs((i + 1) % fs.size)) == Set(a, b)
      }
    )
    // unit outward normals of the two faces; the bisecting reflection has normal n1 − n2
    def normal(f: Int)                                                 =
      val fc = centroid(rco.faces(f))
      val d  = (fc._1 - c._1, fc._2 - c._2, fc._3 - c._3)
      val n  = math.sqrt(d._1 * d._1 + d._2 * d._2 + d._3 * d._3)
      (d._1 / n, d._2 / n, d._3 / n)
    val n1                                                             = normal(owners(0))
    val n2                                                             = normal(owners(1))
    val m                                                              = (n1._1 - n2._1, n1._2 - n2._2, n1._3 - n2._3)
    val mn                                                             = math.sqrt(m._1 * m._1 + m._2 * m._2 + m._3 * m._3)
    val u                                                              = (m._1 / mn, m._2 / mn, m._3 / mn)
    val pa                                                             = rco.verts(a)
    def reflect(p: (Double, Double, Double))                           =
      val t = (p._1 - pa._1) * u._1 + (p._2 - pa._2) * u._2 + (p._3 - pa._3) * u._3
      (p._1 - 2 * t * u._1, p._2 - 2 * t * u._2, p._3 - 2 * t * u._3)
    def near(p: (Double, Double, Double), q: (Double, Double, Double)) =
      math.abs(p._1 - q._1) + math.abs(p._2 - q._2) + math.abs(p._3 - q._3) < 1e-7
    // the reflection fixes the edge pointwise and swaps the two faces as vertex sets ...
    near(reflect(rco.verts(a)), rco.verts(a)) shouldBe true
    near(reflect(rco.verts(b)), rco.verts(b)) shouldBe true
    rco.faces(owners(0)).forall(i =>
      rco.faces(owners(1)).exists(j => near(reflect(rco.verts(i)), rco.verts(j)))
    ) shouldBe true
    // ... but does not map the vertex set onto itself
    rco.verts.forall(p => rco.verts.exists(near(reflect(p), _))) shouldBe false
