package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import TailExclusion.*

/** Certificate `tail-exclusion.txt`. The antiprism closed forms, Lemmas A, B and E, and the prism and antiprism tails excluded over the full
  * pool with no cap left anywhere.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object TailExclusionCertificate:
  def write(dir: java.nio.file.Path): Unit =
    def ivs(v: CertifiedDihedrals.Iv) =
      String.format(java.util.Locale.ROOT, "[%.6f, %.6f]", v.lo, v.hi)
    val sb                            = new StringBuilder
    sb ++= "Tail exclusion over the FULL pool (no caps left)\n\n"
    sb ++= "== Antiprism closed forms ==\n"
    sb ++= "cos a3q(q) = -tan(pi/2q)/sqrt3;  cos a33(q) = (1 - 4 cos(pi/q))/3\n"
    for q <- List(4, 5, 6, 10, 50, 300) do
      sb ++= s"A$q: a3q ${ivs(a3qDeg(q))}  a33 ${ivs(a33Deg(q))}\n"
    sb ++= s"tail corridors (q > $maxPoolAntiprism): a3q ${ivs(a3qTail(maxPoolAntiprism + 1))}, " +
      s"a33 ${ivs(a33Tail(maxPoolAntiprism + 1))}\n\n"
    sb ++= "== Lemmas ==\n"
    sb ++= "A (strict): a3q > 90 for all q >= 4 (tan > 0);  B (strict): a33 < 180 (cos(pi/q) < 1)\n"
    sb ++= "E: 2 a3q(q) + a33(r) != 360 for ALL q, r >= 4 — reduces to tan(pi/2q) = 2 sin(pi/2r);\n"
    sb ++= "   interleaving h(2q) < g(q) < h(2q-1) pins the real solution strictly between integers.\n"
    sb ++= s"   constant chain verified: $lemmaEConstants; grid q=4..400 + {1000, 5000, 20000}: " +
      s"${((4 to 400) ++ List(1000, 5000, 20000)).forall(interleaves)}\n\n"
    val (survivors, verdicts)         = results
    sb ++= s"== Finite prism sweep, 5 <= p <= $maxPoolPrism non-core, full pool ==\n"
    sb ++= s"surviving p (lateral edge completable): ${survivors.mkString(", ")}\n"
    sb ++= "(exactly the six exotic families of the prism grid; their completions:)\n"
    prismSweep.filter(_._2.nonEmpty).foreach { (p, ts) =>
      sb ++= s"P$p:\n"
      ts.take(6).foreach(t => sb ++= s"   ${t.chain.map(_.label).mkString(" + ")}\n")
    }
    sb ++= s"\n== Antiprism sweep (101..$maxPoolAntiprism) + tails ==\n"
    sb ++= s"verdicts: ${verdicts.size} targets, all excluded: ${verdicts.forall(_._2)}\n"
    val tail                          = antiprismVerdict(s"A>$maxPoolAntiprism", a3qTail(maxPoolAntiprism + 1))
    sb ++= s"threats at the antiprism tail (all refuted):\n"
    tail.threats.foreach(t => sb ++= s"   ${t.show}\n")
    val ptRefs                        = prismTailVerdict.groupBy(_.refutation.toString).view.mapValues(_.size).toMap
    sb ++= s"\n== Prism tail p > $maxPoolPrism ==\n"
    sb ++= s"threat census by refutation: ${ptRefs.toList.sorted.mkString(", ")}\n"
    sb ++= s"unrefuted: ${prismTailVerdict.count(_.refutation == Refutation.Unrefuted)}\n"
    java.nio.file.Files.writeString(dir.resolve("tail-exclusion.txt"), sb.toString)
    println(s"survivors ${survivors.mkString(",")}; verdicts ok=${verdicts.forall(_._2)} -> " +
      s"$dir/tail-exclusion.txt")
