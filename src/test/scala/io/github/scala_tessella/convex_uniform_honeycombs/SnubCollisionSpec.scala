package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import SnubCollision.*

/** The developability gap, witnessed (Example 6.4): the snub-trihexagonal lift carries an
  * (R1)+(R2)-consistent pattern whose development collides — two explicit words reach the same position with
  * point parts that are not stabilizer-equivalent.
  */
class SnubCollisionSpec extends AnyFlatSpec with Matchers:

  "the snub-trihexagonal lift" should "carry a locally consistent pattern whose development collides" in:
    val c   = collision.getOrElse(fail("no colliding pattern found"))
    c.word1 should not be c.word2
    c.word1 should not be empty
    c.word2 should not be empty
    // the two words reach the same position ...
    val pos = c.position
    math.sqrt(pos._1 * pos._1 + pos._2 * pos._2 + pos._3 * pos._3) should be <= 3.05 + 1.6 + 1e-6
    // ... with point parts that differ by more than a stabilizer element
    c.m1.dist(c.m2) should be > 1e-4
    info(s"skeleton ${c.skeleton}: words ${c.word1.mkString("·")} and ${c.word2.mkString("·")} " +
      f"meet at (${pos._1}%.4f, ${pos._2}%.4f, ${pos._3}%.4f)")
