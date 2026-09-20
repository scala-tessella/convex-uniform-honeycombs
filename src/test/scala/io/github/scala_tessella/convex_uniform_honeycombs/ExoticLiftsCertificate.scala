package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import ExoticLifts.*

/** Certificate `exotic-lifts.txt`. The nine exotic prism families excluded by lifted odd-face walks,
  * completing the Alphabet Theorem.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object ExoticLiftsCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val sb   = new StringBuilder
    sb ++= "The six planar-species lifts: all nine exotic prism families excluded\n"
    sb ++= "pool per round: 13 core cells + alive exotic laterals and horizontals (all else excluded by\n"
    sb ++=
      "the corona fixpoint and the tails); rings enumerated exactly; walks use LEMMA OPP (prisms structural, cube trivial,\n"
    sb ++= s"rco machine-verified over Q(sqrt2): $rcoOppositeEdgeLemma)\n\n"
    sb ++= "== Round-1 ring realizations (the six prism-grid shadows, cell-resolved) ==\n"
    val pool = roundPool(exoticPs.toSet)
    exoticPs.foreach { p =>
      val cs = completions(pool, lateral(p), 4).map(_.map(_.label).mkString(" + "))
      sb ++= s"P$p lateral: ${cs.mkString("  |  ")}\n"
    }
    sb ++= "\n== The campaign ==\n"
    campaign.foreach { k =>
      sb ++= s"round ${k.round}: P${k.p} EXCLUDED [${k.mechanism}]\n"
    }
    sb ++= s"\nALL NINE EXOTIC FAMILIES DEAD: $alphabetClosed\n\n"
    sb ++= "== THE ALPHABET THEOREM ==\n"
    sb ++=
      "chain: the corona fixpoint (pentagon/decagon family, snubs, antiprisms q <= 100 — interval corona fixpoint)\n"
    sb ++= "  + the tails (all antiprisms q >= 101 via closed forms + Lemma E; all prisms p > 500;\n"
    sb ++= "    non-exotic prisms p <= 500 over the full pool)\n"
    sb ++= "  + the exotic lifts (this campaign: the nine exotic families via lifted odd-face walks)\n"
    sb ++= "==> every face-to-face unit-edge honeycomb by convex uniform cells uses only the 13 core cells.\n"
    java.nio.file.Files.writeString(dir.resolve("exotic-lifts.txt"), sb.toString)
    println(s"campaign: ${campaign.size} kills in ${campaign.map(_.round).max} rounds, " +
      s"alphabet closed: $alphabetClosed -> $dir/exotic-lifts.txt")
