package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import HoneycombAlphabet.catalogue
import SpeciesCorona.*

/** Certificate `species-corona.txt`. Corona structure over the vertex species: figure hosting, the dead letters, and the face-cycle filter.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object SpeciesCoronaCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val a       = analysis
    val sp      = SpeciesEnumerator.species
    val figShow = catalogue.map(f => f.key -> f.show).toMap
    val sb      = new StringBuilder
    sb ++= "Corona structure over the 34 vertex species\n"
    sb ++= "pair level: provably vacuous (every species self-hosts its figures up to reflection);\n"
    sb ++= "face level: the p-gon walk (marked-ring forms, exact p-1-step path), iterated to a fixpoint\n\n"
    sb ++= s"== Face-cycle fixpoint: ${a.survivors.size} of ${sp.size} species survive ==\n"
    if a.killedByRound.isEmpty then sb ++= "no species dies: all 34 are corona-consistent\n"
    else
      a.killedByRound.zipWithIndex.foreach { (dead, r) =>
        sb ++= s"round ${r + 1} kills ${dead.size}: ${dead.map(label).mkString(", ")}\n"
      }
    sb ++= s"\n== Live edge figures: ${a.figuresUsed.size} of ${catalogue.size} ==\n"
    sb ++= "an edge figure not occurring in any species cannot occur in any honeycomb;\n"
    sb ++= "the following catalogued figures are DEAD LETTERS:\n"
    catalogue.filterNot(f => a.figuresUsed(f.key)).foreach(f => sb ++= s"  ${f.show}\n")
    sb ++= s"\n== Hosting table (figure -> species) ==\n"
    catalogue.filter(f => a.figuresUsed(f.key)).foreach { f =>
      sb ++= s"${f.show}\n    ${a.hosting(f.key).map(label).mkString(", ")}\n"
    }
    sb ++= s"\n== Species adjacency (shared-figure neighbors) ==\n"
    sp.indices.foreach { i =>
      sb ++= s"${label(i)}  ~  ${a.adjacency(i).filter(_ != i).map(label).mkString(", ")}\n"
    }
    java.nio.file.Files.writeString(dir.resolve("species-corona.txt"), sb.toString)
    println(
      s"survivors ${a.survivors.size}/${sp.size}, live figures ${a.figuresUsed.size}/${catalogue.size}" +
        s" -> $dir/species-corona.txt"
    )
