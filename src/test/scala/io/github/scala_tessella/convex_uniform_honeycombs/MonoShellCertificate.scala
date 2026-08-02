package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import MonoShell.*

/** Certificate `mono-shell.txt`. The star-gluing atlas and the mono-species shell verdict of every species.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object MonoShellCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val (rs, flags) = results
    val sb          = new StringBuilder
    sb ++= "The mono-species shell filter over the 34 species\n"
    sb ++= "gluings per tiling-vertex = verified rigid star-to-star identifications along the edge\n"
    sb ++= "(descriptor-verified: a cell is determined by an edge, its two face germs and its type);\n"
    sb ++= "shell SAT = all neighbors can carry same-species stars with mutual edges + agreeing rings\n\n"
    sb ++= s"== Verdicts: ${rs.count(_._2.sat)} of ${rs.size} species admit a mono-species shell ==\n"
    rs.foreach { (i, r) =>
      val l = SpeciesCorona.label(i)
      sb ++= f"$l%-34s ${if r.sat then "SAT  " else "UNSAT"}  gluings/vertex: ${r.counts.mkString(",")}\n"
    }
    sb ++= "\n== The eight k=1 exclusions ==\n"
    sb ++= "the four lifts of extendable-but-nonuniform 2D species (3.3.4.12, 3.4.3.12, 3.4.4.6, 3.3.6.6)\n"
    sb ++= "die in the shell exactly as their shadows die in the plane; the four other exclusions are novel\n"
    sb ++= "species of the species table — none of them can be the species of ANY mono-species honeycomb, uniform or not.\n"
    sb ++= s"\n== Ambiguity flags: ${flags.size} ==\n"
    flags.foreach(f => sb ++= f + "\n")
    java.nio.file.Files.writeString(dir.resolve("mono-shell.txt"), sb.toString)
    println(s"${rs.count(_._2.sat)}/${rs.size} SAT, ${flags.size} flags -> $dir/mono-shell.txt")
