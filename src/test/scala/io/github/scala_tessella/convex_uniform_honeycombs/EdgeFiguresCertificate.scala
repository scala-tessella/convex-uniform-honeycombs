package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import HoneycombAlphabet.*

/** Certificate `edge-figures.txt`. The exact dihedral table, the face-compatibility lists and the complete edge-figure catalogue of the
  * 13-cell core alphabet.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object EdgeFiguresCertificate:
  def write(dir: java.nio.file.Path): Unit =
    def num(v: Double) = String.format(java.util.Locale.ROOT, "%10.5f", v)
    val sb             = new StringBuilder
    sb ++= "Core alphabet, exact dihedrals, edge-figure catalogue\n"
    sb ++= s"alpha = arctan(sqrt 2) = ${num(CoreAngle.alphaDeg).trim} deg; every angle = r + n*alpha\n\n"
    sb ++= "== Dihedral table ==\n"
    edgeTypes.foreach { et =>
      sb ++=
        f"${et.cell.label}%-10s ${et.faces._1}%2d·${et.faces._2}%-2d  r=${et.angle.r}%3d n=${et.angle.n}%2d  ${num(et.angle.degrees)}\n"
    }
    sb ++= "\n== Face compatibility (a p-face glues only these cells) ==\n"
    cellsWithFace.toList.sortBy(_._1).foreach { (p, cells) =>
      sb ++= f"$p%2d-gon: ${cells.map(_.label).mkString(", ")}\n"
    }
    sb ++= s"\n== Edge-figure catalogue: ${catalogue.size} figures ==\n"
    catalogue.groupBy(_.size).toList.sortBy(_._1).foreach { (m, figs) =>
      sb ++= s"\n-- $m cells: ${figs.size} figures --\n"
      figs.foreach(f => sb ++= f.show + "\n")
    }
    val bySupport      = catalogue.groupBy(_.cells.map(_.label).sorted).size
    sb ++= s"\ncell-multiset supports: $bySupport\n"
    java.nio.file.Files.writeString(dir.resolve("edge-figures.txt"), sb.toString)
    println(s"${catalogue.size} edge figures -> $dir/edge-figures.txt")
