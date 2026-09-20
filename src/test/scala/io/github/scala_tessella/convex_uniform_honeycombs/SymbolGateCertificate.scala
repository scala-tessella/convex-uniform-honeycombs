package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import SpeciesEnumerator.species
import SymbolCatalog.{canonicalKey, k1Of}
import SymbolRealization.derivedSymbolsOf

/** Certificate `symbol-gate.txt`. The identification of the appendix: per species, the canonical keys derived
  * from the certified honeycombs against the keys of the combinatorial census, with the key-set equality
  * verdict.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object SymbolGateCertificate:
  def write(dir: java.nio.file.Path): Unit =
    def say(s: String): Unit = { println(s); System.out.flush() }
    val flags                = MonoShell.Flags()
    val sat                  = MonoShell.results._1.filter(_._2.sat).map(_._1)
    val t0                   = System.currentTimeMillis
    val rows                 = sat.map { i =>
      val derived = derivedSymbolsOf(i, flags).map(canonicalKey).toSet
      val census  = k1Of(i, sigma0Cap = 100000)._1.map(canonicalKey).toSet
      val ok      = derived == census
      say(
        s"${SpeciesCorona.label(i)}: derived ${derived.size}, census ${census.size}, " +
          s"${if ok then "MATCH" else "MISMATCH"} [${(System.currentTimeMillis - t0) / 1000}s]"
      )
      (i, derived.size, census.size, ok)
    }
    val sb                   = new StringBuilder
    sb ++= "The k = 1 symbol gate certificate\n"
    sb ++= "per mono-shell-passing species: canonical keys of the minimal symbols DERIVED from the\n"
    sb ++= "certified honeycombs (vertex stabilizer from the certified development ball, sigma0 by\n"
    sb ++= "descriptor-rigidity pullback through the certified pattern) vs the combinatorial census;\n"
    sb ++= "key-set equality certifies both directions: every census symbol realized, every certified\n"
    sb ++= "honeycomb's minimal symbol in the census, identification exact and radius-free\n\n"
    sb ++= f"${"species"}%-34s ${"derived"}%7s ${"census"}%7s  verdict\n"
    for (i, d, c, ok) <- rows do
      sb ++= f"${SpeciesCorona.label(i)}%-34s $d%7d $c%7d  ${if ok then "MATCH" else "MISMATCH"}\n"
    sb ++= f"\nspecies gated: ${rows.size}; total symbols: ${rows.map(_._2).sum}; " +
      f"all match: ${rows.forall(_._4)}\n"
    sb ++= s"gray-zone flags: ${flags.items.distinct.size}\n"
    java.nio.file.Files.writeString(dir.resolve("symbol-gate.txt"), sb.toString)
    say(
      s"\n${rows.size} species, ${rows.map(_._2).sum} symbols, all match: ${rows.forall(_._4)} " +
        s"-> $dir/symbol-gate.txt"
    )
