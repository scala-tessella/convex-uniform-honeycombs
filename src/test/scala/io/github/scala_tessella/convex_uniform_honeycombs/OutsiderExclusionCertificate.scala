package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import CertifiedDihedrals.*
import OutsiderExclusion.*

/** Certificate `outsider-exclusion.txt`. The certified outsider dihedral table and the corona-fixpoint
  * exclusion verdicts, round by round.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object OutsiderExclusionCertificate:
  def write(dir: java.nio.file.Path): Unit =
    def num(v: Double) = String.format(java.util.Locale.ROOT, "%.6f", v)
    val sb             = new StringBuilder
    sb ++= "Certified outsider dihedrals + corona-fixpoint edge-completion verdicts\n"
    sb ++=
      s"participants (no cap): core; every prism (p<=$maxPoolPrism one by one, p>$maxPoolPrism as two tail\n" +
        s"classes); pentagon/decagon family; snubs; every antiprism (q<=$maxPoolAntiprism one by one, q>$maxPoolAntiprism\n" +
        "as two tail corridors)\n"
    sb ++= s"targets: the nine named outsiders and antiprisms q<=$maxTargetAntiprism\n"
    sb ++= "tail classes: " +
      tailValues.map(v => s"${v.label} [${num(v.iv.lo)}, ${num(v.iv.hi)}]").mkString(", ") +
      "\n\n"
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
    sb ++= s"\n== Dead edges and their nearest misses (multisets summing within 0.1° of 360°) ==\n"
    sb ++= "margin = the closest miss at the interval midpoints; interval widths are ~1e-10\n"
    fp.kills.foreach(k => sb ++= k.show + "\n")
    sb ++= f"\nminimum margin over all dead edges: ${fp.minMargin}%.3e°\n"
    sb ++= f"minimum face-compatible margin over all dead edges: ${fp.minMarginCompatible}%.3e°\n"
    sb ++= f"minimum face-compatible margin beyond Lemma E's family: ${fp.minMarginCompatibleBeyondE}%.3e°\n"
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
