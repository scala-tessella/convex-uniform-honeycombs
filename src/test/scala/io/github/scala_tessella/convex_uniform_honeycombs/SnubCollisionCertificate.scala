package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.TransitivePatterns
import SnubCollision.*

/** Certificate `snub-collision.txt`. The developability gap witnessed: an (R1)+(R2)-consistent pattern of the
  * snub-trihexagonal lift with two explicit words reaching the same position with stabilizer-inequivalent
  * point parts.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object SnubCollisionCertificate:
  def write(dir: java.nio.file.Path): Unit =
    val sb = new StringBuilder
    sb ++= s"The developability gap on $speciesLabel (Example 6.4 of the paper)\n"
    sb ++=
      "A pattern satisfying (R1) and (R2) whose breadth-first development (to radius 3.05 + 1.6) collides:\n"
    sb ++=
      "two words in the gluings — each step the gluing at the named tiling-vertex, applied on the right —\n"
    sb ++= "reach the same position with point parts that are not equal modulo Stab(S).\n\n"
    collision match
      case None    => sb ++= "no colliding pattern found\n"
      case Some(c) =>
        sb ++= s"skeleton ${c.skeleton}; pattern: gluing at tiling-vertex x -> back-vertex y(x)\n"
        c.pattern.zipWithIndex.foreach((g, x) => sb ++= s"  x = $x -> y = ${g.y}\n")
        sb ++= s"\nword 1 (${c.word1.size} steps): ${c.word1.mkString(" ")}\n"
        sb ++= s"word 2 (${c.word2.size} steps): ${c.word2.mkString(" ")}\n"
        sb ++= f"common position: (${c.position._1}%.6f, ${c.position._2}%.6f, ${c.position._3}%.6f)\n"
        def row(m: TransitivePatterns.Mat, r: Int): String =
          val (a, b, cc) = r match
            case 0 => m.r1
            case 1 => m.r2
            case _ => m.r3
          f"[$a%8.5f $b%8.5f $cc%8.5f]"
        sb ++= "point part of word 1:\n" + (0 to 2).map(r => "  " + row(c.m1, r)).mkString("\n") + "\n"
        sb ++= "point part of word 2:\n" + (0 to 2).map(r => "  " + row(c.m2, r)).mkString("\n") + "\n"
        sb ++=
          f"\ndistance between the point parts: ${c.m1.dist(c.m2)}%.6f; not stabilizer-equivalent: true\n"
    java.nio.file.Files.writeString(dir.resolve("snub-collision.txt"), sb.toString)
    println(s"snub collision: ${collision.isDefined} -> $dir/snub-collision.txt")
