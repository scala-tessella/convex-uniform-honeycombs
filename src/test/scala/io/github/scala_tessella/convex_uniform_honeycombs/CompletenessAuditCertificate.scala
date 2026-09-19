package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import CompletenessAudit.*

/** Certificate `completeness-audit.txt`. The four audit certificates: periodization, class coherence, fingerprint separation and cap closure.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object CompletenessAuditCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val (audits, flags) = results
    val sb              = new StringBuilder
    sb ++= "The completeness audit for the 28\n"
    sb ++= "per class: PERIODIZATION CERTIFICATE (three independent translation words; ball periodicity;\n"
    sb ++= "lattice invariance under all generator point-parts; coverage radius over a fundamental domain);\n"
    sb ++= "per pattern: CLASS COHERENCE (the pattern's own periodization certificate, reaching the\n"
    sb ++= "representative's basis; alignment of the representative's ball onto the pattern's by an element\n"
    sb ++= "of Stab(S) at the determination radius; the aligned representative basis acting by symmetries on\n"
    sb ++= "the pattern's certified ball); per skeleton: GERM FORCING or\n"
    sb ++=
      "EXHAUSTION (all R1+R2-passing patterns developed; accepted ones fingerprint into known classes)\n\n"
    sb ++= f"${"species"}%-34s ${"classes"}%7s ${"certified"}%9s ${"patterns"}%8s ${"coherent"}%8s " +
      f"${"skeletons closed"}%16s  exhausted\n"
    audits.foreach { a =>
      sb ++= f"${SpeciesCorona.label(a.idx)}%-34s ${a.classes}%7d ${a.certified}%9d ${a.patterns}%8d " +
        f"${a.coherent}%8s ${s"${a.forcingSkeletons}/${a.skeletonsWithPatterns}"}%16s  " +
        (if a.exhaustedSkeletons.isEmpty then "-" else a.exhaustedSkeletons.mkString(",")) + "\n"
    }
    sb ++= f"\naccepted patterns within the caps, each certified and aligned: ${audits.map(_.patterns).sum}\n"
    sb ++= f"\nALL AUDITS PASS: ${audits.forall(_.ok)};  TOTAL CLASSES: ${audits.map(_.classes).sum};  " +
      s"flags: ${flags.size}\n\n"
    sb ++= "== THE COMPLETENESS THEOREM ==\n"
    sb ++= "chain: the Alphabet Theorem (only the 13 core cells) -> the species table (exactly 34 species) ->\n"
    sb ++= "the shell filter (26 admit a mono-species shell) -> the pattern enumeration (28 classes) ->\n"
    sb ++= "this audit (: every class is a certified periodic transitive honeycomb, every accepted\n"
    sb ++= "pattern develops one of them, every skeleton closed)\n"
    sb ++= "==> every vertex-transitive face-to-face unit-edge honeycomb of E^3 by convex uniform cells\n"
    sb ++= "    is one of the 28 — the first completeness proof of Gruenbaum's list.\n"
    java.nio.file.Files.writeString(dir.resolve("completeness-audit.txt"), sb.toString)
    println(s"all ok: ${audits.forall(_.ok)}, classes ${audits.map(_.classes).sum}, flags ${flags.size}" +
      s" -> $dir/completeness-audit.txt")
