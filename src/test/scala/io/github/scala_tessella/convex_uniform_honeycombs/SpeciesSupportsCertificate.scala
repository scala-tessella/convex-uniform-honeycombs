package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import SpeciesSupports.*

/** Certificate `species-supports.txt`. Corner excesses and the complete catalogue of cell multisets admitted by the area equation.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object SpeciesSupportsCertificate:
  def write(dir: java.nio.file.Path): Unit =
    def num(v: Double) = String.format(java.util.Locale.ROOT, "%9.5f", v)
    val sb             = new StringBuilder
    sb ++= "Corner excesses and the complete species-support catalogue\n"
    sb ++= "area equation: sum of corner excesses = (720, 0) in the (15-degree, alpha) lattice;\n"
    sb ++= "parity: each face size occurs an even number of times over the multiset\n\n"
    sb ++= "== Corner excess table (excess-degrees, exact) ==\n"
    cornerExcess.toList.sortBy(_._1.ordinal).foreach { (c, e) =>
      sb ++= f"${c.label}%-10s r=${e.r}%4d n=${e.n}%3d  ${num(e.degrees)}\n"
    }
    sb ++= s"\n== Supports: ${supports.size} ==\n"
    supports.groupBy(_.cells).toList.sortBy(_._1).foreach { (m, ss) =>
      sb ++= s"\n-- $m cells: ${ss.size} --\n"
      ss.foreach(s => sb ++= s.show + "\n")
    }
    java.nio.file.Files.writeString(dir.resolve("species-supports.txt"), sb.toString)
    println(s"${supports.size} supports -> $dir/species-supports.txt")
