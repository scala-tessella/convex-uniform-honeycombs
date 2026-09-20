package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import HoneycombAlphabet.catalogue
import SpeciesEnumerator.*

/** Certificate `species-table.txt`. The species table: every support with its realized vertex species,
  * assembled on the sphere of directions and deduplicated by canonical combinatorial-map key.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object SpeciesTableCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val (all, flags)     = enumerated
    val figShow          = catalogue.map(f => f.key -> f.show).toMap
    val sb               = new StringBuilder
    sb ++= "The species table: edge-to-edge tilings of S^2 by core corner figures\n"
    sb ++= "assembly: geometric interval DFS, exact (15-degree, alpha) bookkeeping, canonical-map dedup;\n"
    sb ++= "a species lists its tiling-vertex count (= honeycomb edges at the vertex), arc count (= faces),\n"
    sb ++= "and its vertex-figure census over the 69-figure catalogue of the alphabet\n\n"
    sb ++= s"== Species: ${all.size} over ${all.map(_.counts).distinct.size} realized supports"
    sb ++= s" (of ${SpeciesSupports.supports.size}) ==\n"
    val realized         = table.filter(_._2.nonEmpty)
    val unrealized       = table.filter(_._2.isEmpty)
    realized.groupBy(_._1.cells).toList.sortBy(_._1).foreach { (m, rows) =>
      sb ++= s"\n-- $m cells --\n"
      rows.foreach { (sup, specs) =>
        sb ++= s"${sup.show}  species: ${specs.size}\n"
        specs.zipWithIndex.foreach { (sp, i) =>
          sb ++= s"  #${i + 1}  vertices=${sp.vertices} faces=${sp.faces}\n"
          sp.figures.foreach((key, mult) => sb ++= s"       ${mult} x ${figShow(key)}\n")
        }
      }
    }
    val (sepCell, sepLB) = SeparationConstant.constant
    sb ++= "\n== Separation constant (soundness of the identify-or-separate decisions) ==\n"
    sb ++= "distinct tiling-vertices of any genuine tiling are at least " +
      String.format(java.util.Locale.ROOT, "%.4f", sepLB) +
      s" degrees apart (attained by the ${sepCell.label} corner); the search identifies below 1e-6 and\n" +
      "separates above 1e-3\n"
    sb ++= s"\n== Unrealized supports: ${unrealized.size} ==\n"
    unrealized.foreach((sup, _) => sb ++= sup.show + "\n")
    sb ++= s"\n== Ambiguity flags: ${flags.size} ==\n"
    flags.foreach(f => sb ++= f + "\n")
    java.nio.file.Files.writeString(dir.resolve("species-table.txt"), sb.toString)
    println(s"${all.size} species on ${realized.size}/${table.size} supports, ${flags.size} flags -> " +
      s"$dir/species-table.txt")
