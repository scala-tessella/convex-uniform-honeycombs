package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import TransitivePatterns.*

/** Certificate `transitive-patterns.txt`. The transitive enumeration table: per species, cosets, forcing,
  * skeletons, patterns and honeycomb count.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object TransitivePatternsCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val (reports, flags) = results
    val sb               = new StringBuilder
    sb ++= "Transitive gluing patterns + rigid development over the 26 species\n"
    sb ++= "pattern = one atlas gluing per tiling-vertex; acceptance = R1 (reverse pairs in Stab) +\n"
    sb ++= "R2 (face-cycle words in Stab) + collision-free development to radius 3.05 (+1.6 slack);\n"
    sb ++= "single-coset species are resolved by the FORCING THEOREM (unique mono-species honeycomb);\n"
    sb ++= "dedup by canonical development fingerprints over Stab(S), radii 2.05 and 3.05\n\n"
    sb ++= f"${"species"}%-34s ${"stab"}%4s ${"forced"}%6s ${"skel"}%4s ${"pat"}%4s ${"rej"}%4s ${"H"}%2s\n"
    reports.foreach { r =>
      sb ++= f"${SpeciesCorona.label(r.idx)}%-34s ${r.stabOrder}%4d ${r.forced}%6s ${r.skeletons}%4d " +
        f"${r.patternsFound}%4d ${r.patternsRejected}%4d ${r.honeycombs}%2d\n"
    }
    sb ++= f"\nTOTAL UNIFORM HONEYCOMBS: ${reports.map(_.honeycombs).sum}\n\n"
    sb ++= "== The classical identification ==\n"
    sb ++= "9 cubic family + 3 B-family (cantic, runcic, runcicantic) + 1 quarter cubic +\n"
    sb ++= "10 prismatic lifts (the 11 uniform plane tilings minus the square tiling = cubic) +\n"
    sb ++= "5 gyro/elongated forms: gyrated alternated cubic ({tet:8 oct:6}#1), elongated +\n"
    sb ++= "gyroelongated alternated cubic (both {tet:4 oct:3 p3:6}#1), gyrated triangular prismatic\n"
    sb ++= "({p3:12}#1), gyroelongated triangular prismatic (second honeycomb of {cube:4 p3:6}#1)\n\n"
    sb ++= "== Notes ==\n"
    sb ++= "the snub-trihexagonal lift ({p3:8 p6:2}#2) has a full skeleton of patterns passing the local\n"
    sb ++= "conditions but failing collision-free development — without the development check the count\n"
    sb ++= "would be a spurious 29; fingerprints are stable from radius 2.05 to 3.05 for every species\n"
    sb ++= s"\nflags: ${flags.size}\n"
    java.nio.file.Files.writeString(dir.resolve("transitive-patterns.txt"), sb.toString)
    println(s"TOTAL = ${reports.map(_.honeycombs).sum}, flags ${flags.size} -> $dir/transitive-patterns.txt")
