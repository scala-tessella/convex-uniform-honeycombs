package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import SpeciesEnumerator.species
import StarFoldings.{subgroupsOfSpecies, symmetryOf}
import SymbolCatalog.*

/** Certificate `symbols-k1.txt`. The k = 1 combinatorial symbol census of the appendix: every folding of every star swept through the
  * sigma0 enumerator, minimal symbols deduplicated by canonical key.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object SymbolCensusCertificate:
  def write(dir: java.nio.file.Path): Unit =
    def say(s: String): Unit = { println(s); System.out.flush() }
    val targets              =
      species.indices.toVector
    val t0                   = System.currentTimeMillis
    def dt                   = (System.currentTimeMillis - t0) / 1000
    val rows                 = targets.map { i =>
      val sym           = symmetryOf(i)
      val subs          = subgroupsOfSpecies(i)
      say(
        s"== ${SpeciesCorona.label(i)}: chambers ${sym.cx.chambers.size}, stab ${sym.perms.size}, " +
          s"foldings ${subs.size} [${dt}s] =="
      )
      val (syms, stats) = k1Of(i, sigma0Cap = 100000, log = m => say(s"  [${dt}s]$m"))
      say(s"== ${SpeciesCorona.label(i)}: ${syms.size} minimal symbols [${dt}s] ==")
      (
        i,
        sym.cx.chambers.size,
        sym.perms.size,
        subs.size,
        stats.map(_.solutions).sum,
        syms,
        stats.exists(_.capped)
      )
    }
    val sb                   = new StringBuilder
    sb ++= "The k = 1 combinatorial symbol census (pre-realization)\n"
    sb ++= "per species: full folding lattice x exhaustive sigma0 assembly, minimal symbols deduped by\n"
    sb ++= "canonical traversal keys; realizability (rigid development) is the symbol gate's filter\n\n"
    sb ++=
      f"${"species"}%-34s ${"chambers"}%8s ${"stab"}%4s ${"foldings"}%8s ${"sigma0"}%7s ${"minimal"}%7s\n"
    for (i, ch, st, fo, so, syms, capd) <- rows do
      sb ++= f"${SpeciesCorona.label(i)}%-34s $ch%8d $st%4d $fo%8d $so%7d ${syms.size}%7d${
          if capd then "  CAPPED" else ""
        }\n"
    sb ++= f"\nTOTAL distinct minimal k = 1 symbols: ${rows.map(_._6.size).sum}\n"
    java.nio.file.Files.writeString(dir.resolve("symbols-k1.txt"), sb.toString)
    say(
      s"\nTOTAL ${rows.map(_._6.size).sum} minimal symbols over ${targets.size} species -> $dir/symbols-k1.txt"
    )
