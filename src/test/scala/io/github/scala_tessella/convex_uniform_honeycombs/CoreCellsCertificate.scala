package io.github.scala_tessella.convex_uniform_honeycombs

import CoreCells.*

/** Certificate `core-cells.txt`. The rigidity table of the appendix: for each of the 13 core cells, the
  * vertex, edge, face and flag counts, the face-size multiset, and the orders of the combinatorial
  * automorphism group of the face lattice and of the geometric symmetry group — equal on every cell.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object CoreCellsCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val sb = new StringBuilder
    sb ++= "The 13 core cells: face lattices and rigidity (Lemma A.2 of the paper)\n"
    sb ++= "vertices from classical coordinates; faces as maximal coplanar supporting vertex sets; flags =\n"
    sb ++= "incident (vertex, edge, face) triples; |Aut| = combinatorial automorphisms of the face lattice\n"
    sb ++= "(flag images of a base flag whose flag-graph walk closes into a bijection); |Sym| = isometries\n"
    sb ++= "carrying the vertex set onto itself, counted as flag images with their parity. |Aut| = |Sym|\n"
    sb ++= "means every combinatorial automorphism is an isometry; the 13 face-size multisets are pairwise\n"
    sb ++= "distinct, so the 13 face lattices are pairwise non-isomorphic.\n\n"
    sb ++=
      f"${"cell"}%-22s ${"V"}%3s ${"E"}%3s ${"F"}%3s ${"flags"}%5s  ${"faces by size"}%-22s ${"|Aut|"}%5s ${"|Sym|"}%5s rigid\n"
    cells.foreach { c =>
      val sizes = c.faceSizes.toVector.sorted.map((k, n) => s"$n×$k-gon").mkString(" ")
      sb ++=
        f"${c.cellType.label}%-22s ${c.verts.size}%3d ${c.edges.size}%3d ${c.faces.size}%3d ${c.flags.size}%5d  $sizes%-22s ${c.autOrder}%5d ${c.symOrder}%5d ${c.rigid}\n"
    }
    sb ++=
      s"\nall rigid: ${cells.forall(_.rigid)}; face-size multisets pairwise distinct: $faceSizesDistinct\n"
    sb ++=
      "\n== The cell rigidity lemma: a placed core cell is determined by its type, one edge, the two face\n"
    sb ++= "germs at that edge and their ROLES (orbits of faces under the symmetry group) ==\n"
    sb ++= "roles = number of face orbits; by size = faces of equal size share an orbit (false only for the\n"
    sb ++= "rco, whose 18 squares split into 6 axial and 12 belt); bisector = the reflection in the\n"
    sb ++= "perpendicular bisector plane of every edge is a symmetry; transitive = the symmetry group is\n"
    sb ++= "transitive on the (edge, ordered pair of incident faces) triples of each ordered pair of roles.\n"
    sb ++=
      "Without the roles the lemma fails: at a 4·4 edge of the rco the reflection in the plane bisecting\n"
    sb ++=
      "the dihedral angle swaps the axial and the belt square, preserving both germs, and is no symmetry.\n\n"
    sb ++= f"${"cell"}%-22s ${"roles"}%5s ${"by size"}%7s ${"bisector"}%8s ${"transitive"}%10s\n"
    cells.foreach { c =>
      sb ++=
        f"${c.cellType.label}%-22s ${c.roleCount}%5d ${c.rolesBySize}%7s ${c.bisectorMirrors}%8s ${c.roleTransitive}%10s\n"
    }
    sb ++=
      s"\ncell rigidity lemma certified on all 13: ${cells.forall(c => c.bisectorMirrors && c.roleTransitive)}\n"
    java.nio.file.Files.writeString(dir.resolve("core-cells.txt"), sb.toString)
    println(s"core cells: ${cells.count(_.rigid)}/${cells.size} rigid -> $dir/core-cells.txt")
