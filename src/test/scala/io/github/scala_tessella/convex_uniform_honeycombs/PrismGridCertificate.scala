package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import PrismGrid.*

/** Certificate `prism-grid.txt`. The exotic-prism vertical-edge equation, solved completely by exact
  * Egyptian-fraction enumeration.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object PrismGridCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val sb = new StringBuilder
    sb ++= "The prism grid: exotic-prism vertical-edge equation, solved completely\n"
    sb ++= "participants: rational lattice {60,90,120,135,150}, alpha-charged core values (charges must\n"
    sb ++= "cancel), prism verticals 180-360/p; equation sum = 360, 3..6 terms, >= 1 exotic prism\n\n"
    sb ++= s"solutions: ${solutions.size}\n"
    solutions.foreach(s => sb ++= s.show + "\n")
    sb ++= s"\nsurviving exotic p: ${survivingExoticP.toList.sorted.mkString(", ")}\n"
    sb ++= "every solution is the lift of a non-extendable 2D vertex species; no charged participants\n"
    java.nio.file.Files.writeString(dir.resolve("prism-grid.txt"), sb.toString)
    println(s"${solutions.size} solutions -> $dir/prism-grid.txt")
