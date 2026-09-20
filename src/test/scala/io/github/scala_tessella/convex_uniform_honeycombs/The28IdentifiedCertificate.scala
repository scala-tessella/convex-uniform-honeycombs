package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import The28.*

/** Certificate `the-28-identified.txt`. The certified classes against the classical list: names, families and
  * vertex compositions, with the two doubled species resolved by exact lattice invariants.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object The28IdentifiedCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val (rows, flags) = identified
    val sb            = new StringBuilder
    sb ++= "THE 28 IDENTIFIED — our fingerprint classes against the classical list\n"
    sb ++= "count 28: Gruenbaum (Geombinatorics 4, 1994, crediting Johnson); re-derived by\n"
    sb ++= "Deza-Shtogrin (arXiv:math/9906034: '28 uniform partitions of 3-space'); names follow\n"
    sb ++= "the standard (Johnson/Wikipedia) tables, against which the vertex compositions below\n"
    sb ++= "were cross-checked. The two doubled species are resolved by EXACT lattice invariants\n"
    sb ++= "on the certified translation bases (ExactCertificates): elongated triangular prismatic\n"
    sb ++= "has a translation of squared norm 2 + sqrt3 (the oblique in-plane period of the\n"
    sb ++= "elongated triangular tiling), unrepresentable in the gyroelongated lattice; elongated\n"
    sb ++= "alternated cubic has a translation of squared norm 2 + (2/3)sqrt6 (one octet slab +\n"
    sb ++= "one prism slab), unrepresentable in the gyroelongated lattice (its sqrt6-part 8k^2/3\n"
    sb ++= "never equals 2/3 for integer k).\n\n"
    val fams          = Vector("cubic family", "B family", "quarter", "prismatic lift", "gyro/elongated")
    for fam <- fams do
      sb ++= s"== $fam (${rows.count(_.family == fam)}) ==\n"
      sb ++= f"${"honeycomb"}%-36s ${"vertex star species"}%-24s ${"cl"}%2s ${"cells at a vertex"}%s\n"
      rows.filter(_.family == fam).foreach { r =>
        sb ++= f"${r.name}%-36s ${r.label}%-24s ${r.classIdx}%2d ${r.support}%s\n"
      }
      sb ++= "\n"
    sb ++= s"total: ${rows.size};  names distinct: ${rows.map(_.name).distinct.size == rows.size};  " +
      s"flags: ${flags.size}\n"
    flags.foreach(f => sb ++= s"  $f\n")
    java.nio.file.Files.writeString(dir.resolve("the-28-identified.txt"), sb.toString)
    println(s"rows ${rows.size}, flags ${flags.size} -> $dir/the-28-identified.txt")
