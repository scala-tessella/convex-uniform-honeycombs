package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import CertifiedDihedrals.*
import OutsiderExclusion.*

/** Certificate `outsider-exclusion.txt`. The certified outsider dihedral table and the corona-fixpoint exclusion verdicts, round by round.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object OutsiderExclusionCertificate:
  def write(dir: java.nio.file.Path): Unit =
    def num(v: Double) = String.format(java.util.Locale.ROOT, "%.6f", v)
    val sb             = new StringBuilder
    sb ++= "Certified outsider dihedrals + corona-fixpoint edge-completion verdicts\n"
    sb ++=
      s"participants: core, prisms p<=$maxPoolPrism, pentagon/decagon family, snubs, antiprisms q<=$maxPoolAntiprism\n"
    sb ++= s"targets: the nine named outsiders and antiprisms q<=$maxTargetAntiprism\n\n"
    sb ++= "== Outsider dihedral table (certified intervals, degrees) ==\n"
    (outsiderConfigs.toVector.sortBy(_._1) ++ Vector("A4" -> antiprismConfig(4), "A5" -> antiprismConfig(5)))
      .foreach { (name, cfg) =>
        edgeTypesOf(cfg).toVector.sortBy(_._1).foreach { (pair, iv) =>
          sb ++= f"$name%-16s ${pair._1}%2d·${pair._2}%-2d  [${num(iv.lo)}, ${num(iv.hi)}]\n"
        }
      }
    val fp             = fixpoint
    sb ++= s"\n== Fixpoint: ${fp.rounds.size} rounds ==\n"
    fp.rounds.zipWithIndex.foreach { (out, i) =>
      sb ++= s"round ${i + 1}: excluded ${out.size}: ${out.mkString(", ")}\n"
    }
    sb ++= s"\n== Survivors (${fp.survivors.size}) and their completions ==\n"
    fp.survivors.foreach((_, ts) => ts.foreach(t => sb ++= t.show + "\n"))
    sb ++= s"\ntotal excluded: ${fp.excluded.size}; survivors: ${
        if fp.survivors.isEmpty then "NONE — alphabet theorem holds within the stated scope"
        else fp.survivors.map(_._1).mkString(", ")
      }\n"
    java.nio.file.Files.writeString(dir.resolve("outsider-exclusion.txt"), sb.toString)
    println(
      s"rounds: ${fp.rounds.size}; excluded ${fp.excluded.size}; survivors: ${fp.survivors.map(_._1).mkString(", ")}"
    )
    println(s"-> $dir/outsider-exclusion.txt")
