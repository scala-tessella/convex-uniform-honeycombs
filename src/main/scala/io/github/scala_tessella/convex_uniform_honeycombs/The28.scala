package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import ExactCertificates.{ExactStar, Q23, Rat, VQ}

/** The classical cross-check: the per-honeycomb identification of the 28 — our (species, class) pairs against
  * the classical names (Grünbaum 1994 / Johnson; Deza–Shtogrin arXiv:math/9906034 review the same 28) and the
  * classical vertex-figure compositions.
  *
  * 24 species carry exactly one honeycomb, so their identification is a static table keyed by the species
  * label; the vertex cell counts equal the species support by construction and are pinned against the
  * classical compositions (cubic + B families checked against the standard tables; prismatic lifts carry
  * twice the 2D face counts of their plane tiling's vertex configuration, which is definitional for a lift).
  * The two DOUBLED species are resolved by an exact lattice invariant on the certified translation bases of
  * `ExactCertificates`:
  *
  *   - {cube:4 p3:6}#1 — elongated vs gyroelongated triangular prismatic: the elongated honeycomb (the 2D
  *     lift) has the oblique in-plane period of the elongated triangular tiling, squared norm 2 + √3
  *     (brickwork offset ½ across a square row plus a triangle row). The gyroelongated honeycomb — prism
  *     layers gyrated between cube layers — has a lattice whose vectors have squared norm with √3-part a
  *     multiple of 4k² (from the doubled gyration period), never 1: the norm² 2 + √3 is unrepresentable. (An
  *     axis-parallel unit translation does NOT separate the two — both honeycombs have one.)
  *   - {tet:4 oct:3 p3:6}#1 — elongated vs gyroelongated alternated cubic: the elongated honeycomb's lattice
  *     contains a vector of squared norm 2 + (2/3)√6 (one octet slab + one prism slab, horizontal shift
  *     included: 1/3 + (1+η)² with η = √6/3); in the gyroelongated lattice every vector has squared norm |h|²
  *     + 4k²(1+η)² with |h|² rational, whose √6-part 8k²/3 can never equal 2/3 — the norm² is
  *     unrepresentable, no axis test needed.
  *
  * The invariants are exact field arithmetic on certified data; the spec pins that exactly one class of each
  * doubled species tests elongated.
  */
object The28:

  final case class Row(
      family: String,
      name: String,
      idx: Int,       // species index
      classIdx: Int,  // class within the species (discovery order, stable)
      label: String,  // species label, e.g. {tet:8 oct:6}#2
      support: String // vertex cell counts = the species support, e.g. {tet:8 oct:6}
  )

  /** The 24 single-class species: label → (classical name, family). */
  val singles: Map[String, (String, String)] = Map(
    "{cube:8}#1"                        -> ("cubic", "cubic family"),
    "{oct:2 co:4}#1"                    -> ("rectified cubic", "cubic family"),
    "{oct:1 truncCube:4}#1"             -> ("truncated cubic", "cubic family"),
    "{cube:2 co:1 rco:2}#1"             -> ("cantellated cubic", "cubic family"),
    "{truncOct:4}#1"                    -> ("bitruncated cubic", "cubic family"),
    "{cube:1 truncOct:1 tco:2}#1"       -> ("cantitruncated cubic", "cubic family"),
    "{cube:1 truncCube:1 rco:1 p8:2}#1" -> ("runcitruncated cubic", "cubic family"),
    "{tco:2 p8:2}#1"                    -> ("omnitruncated cubic", "cubic family"),
    "{tet:8 oct:6}#2"                   -> ("alternated cubic", "cubic family"),
    "{truncTet:2 co:1 truncOct:2}#1"    -> ("cantic cubic", "B family"),
    "{tet:1 cube:1 rco:3}#1"            -> ("runcic cubic", "B family"),
    "{truncTet:1 truncCube:1 tco:2}#1"  -> ("runcicantic cubic", "B family"),
    "{tet:2 truncTet:6}#2"              -> ("quarter cubic", "quarter"),
    "{p3:12}#2"                         -> ("triangular prismatic", "prismatic lift"),
    "{p6:6}#1"                          -> ("hexagonal prismatic", "prismatic lift"),
    "{p3:4 p6:4}#3"                     -> ("trihexagonal prismatic", "prismatic lift"),
    "{cube:2 p8:4}#1"                   -> ("truncated square prismatic", "prismatic lift"),
    "{p3:2 p12:4}#1"                    -> ("truncated hexagonal prismatic", "prismatic lift"),
    "{cube:4 p3:2 p6:2}#2"              -> ("rhombitrihexagonal prismatic", "prismatic lift"),
    "{cube:2 p6:2 p12:2}#1"             -> ("truncated trihexagonal prismatic", "prismatic lift"),
    "{cube:4 p3:6}#2"                   -> ("snub square prismatic", "prismatic lift"),
    "{p3:8 p6:2}#2"                     -> ("snub trihexagonal prismatic", "prismatic lift"),
    "{tet:8 oct:6}#1"                   -> ("gyrated alternated cubic", "gyro/elongated"),
    "{p3:12}#1"                         -> ("gyrated triangular prismatic", "gyro/elongated")
  )

  /** The classical vertex compositions, name → support (cross-checked against the standard tables). */
  val classicalSupports: Map[String, String] = Map(
    "cubic"                              -> "{cube:8}",
    "rectified cubic"                    -> "{oct:2 co:4}",
    "truncated cubic"                    -> "{oct:1 truncCube:4}",
    "cantellated cubic"                  -> "{cube:2 co:1 rco:2}",
    "bitruncated cubic"                  -> "{truncOct:4}",
    "cantitruncated cubic"               -> "{cube:1 truncOct:1 tco:2}",
    "runcitruncated cubic"               -> "{cube:1 truncCube:1 rco:1 p8:2}",
    "omnitruncated cubic"                -> "{tco:2 p8:2}",
    "alternated cubic"                   -> "{tet:8 oct:6}",
    "cantic cubic"                       -> "{truncTet:2 co:1 truncOct:2}",
    "runcic cubic"                       -> "{tet:1 cube:1 rco:3}",
    "runcicantic cubic"                  -> "{truncTet:1 truncCube:1 tco:2}",
    "quarter cubic"                      -> "{tet:2 truncTet:6}",
    "triangular prismatic"               -> "{p3:12}",
    "hexagonal prismatic"                -> "{p6:6}",
    "trihexagonal prismatic"             -> "{p3:4 p6:4}",
    "truncated square prismatic"         -> "{cube:2 p8:4}",
    "truncated hexagonal prismatic"      -> "{p3:2 p12:4}",
    "rhombitrihexagonal prismatic"       -> "{cube:4 p3:2 p6:2}",
    "truncated trihexagonal prismatic"   -> "{cube:2 p6:2 p12:2}",
    "snub square prismatic"              -> "{cube:4 p3:6}",
    "snub trihexagonal prismatic"        -> "{p3:8 p6:2}",
    "elongated triangular prismatic"     -> "{cube:4 p3:6}",
    "gyroelongated triangular prismatic" -> "{cube:4 p3:6}",
    "gyrated triangular prismatic"       -> "{p3:12}",
    "gyrated alternated cubic"           -> "{tet:8 oct:6}",
    "elongated alternated cubic"         -> "{tet:4 oct:3 p3:6}",
    "gyroelongated alternated cubic"     -> "{tet:4 oct:3 p3:6}"
  )

  // ---------- small exact vector helpers on internal coordinates ----------

  private def vqScale(u: VQ, s: Q23): VQ = u.map(_ * s)
  private def vqAdd(u: VQ, v: VQ): VQ    = Vector(u(0) + v(0), u(1) + v(1), u(2) + v(2))
  private def vqIsZero(u: VQ): Boolean   = u.forall(_.isZero)

  /** Does the lattice spanned by the certified τ-basis contain a vector of squared norm `target`? Searched
    * exactly over small integer combinations.
    */
  private def latticeRepresents(star: ExactStar, taus: Vector[VQ], target: Q23): Boolean =
    val range = -2 to 2
    range.exists { m =>
      range.exists { n =>
        range.exists { k =>
          (m, n, k) != (0, 0, 0) && {
            val v = vqAdd(
              vqAdd(vqScale(taus(0), Q23.ofInt(m)), vqScale(taus(1), Q23.ofInt(n))),
              vqScale(taus(2), Q23.ofInt(k))
            )
            !vqIsZero(v) && (star.norm2(v) - target).isZero
          }
        }
      }
    }

  /** |τ|² = 2 + √3: the oblique in-plane period of the elongated triangular tiling (brickwork offset ½
    * across a square row plus a triangle row: ¼ + (1 + √3/2)²).
    */
  private val elongTriNorm2: Q23 =
    Q23(Rat.frac(2, 1), Rat.zero, Rat.one, Rat.zero)

  /** |τ|² = 2 + (2/3)√6: one octet slab plus one prism slab of the elongated alternated cubic
    * (horizontal shift included: 1/3 + (1+η)² with η = √6/3).
    */
  private val elongAltNorm2: Q23 =
    Q23(Rat.frac(2, 1), Rat.zero, Rat.zero, Rat.frac(2, 3))

  // ---------- the identification ----------

  /** The 28 rows in certificate order, plus consistency flags (expect none). */
  lazy val identified: (Vector[Row], Vector[String]) =
    val flags = collection.mutable.ListBuffer.empty[String]
    val exact = ExactCertificates.results
    val rows  = exact.classes.map { c =>
      val label   = SpeciesCorona.label(c.idx)
      val support = species(c.idx)
      singles.get(label) match
        case Some((name, family)) =>
          if c.classIdx != 0 then flags += s"$label: unexpected extra class ${c.classIdx}"
          Row(family, name, c.idx, c.classIdx, label, support)
        case None                 =>
          exact.starModels.get(c.idx) match
            case None       =>
              flags += s"$label: no exact star model"
              Row("?", "?", c.idx, c.classIdx, label, support)
            case Some(star) =>
              if c.taus.size != 3 then flags += s"$label#${c.classIdx}: no certified τ-basis"
              val elong          =
                c.taus.size == 3 && {
                  if label == "{cube:4 p3:6}#1" then
                    latticeRepresents(star, c.taus, elongTriNorm2)
                  else if label == "{tet:4 oct:3 p3:6}#1" then
                    latticeRepresents(star, c.taus, elongAltNorm2)
                  else
                    flags += s"$label: unidentified multi-class species"
                    false
                }
              val (name, family) = label match
                case "{cube:4 p3:6}#1"      =>
                  if elong then ("elongated triangular prismatic", "prismatic lift")
                  else ("gyroelongated triangular prismatic", "gyro/elongated")
                case "{tet:4 oct:3 p3:6}#1" =>
                  if elong then ("elongated alternated cubic", "gyro/elongated")
                  else ("gyroelongated alternated cubic", "gyro/elongated")
                case _                      => ("?", "?")
              Row(family, name, c.idx, c.classIdx, label, support)
    }
    // consistency: 28 distinct names, classical supports match, family counts 9+3+1+10+5
    if rows.size != 28 then flags += s"expected 28 rows, got ${rows.size}"
    if rows.map(_.name).distinct.size != rows.size then flags += "names not distinct"
    rows.foreach { r =>
      classicalSupports.get(r.name) match
        case Some(exp) if exp != r.support => flags += s"${r.name}: support ${r.support} != classical $exp"
        case None                          => flags += s"${r.name}: no classical support recorded"
        case _                             => ()
    }
    val fam   = rows.groupBy(_.family).view.mapValues(_.size).toMap
    val famOk = fam == Map(
      "cubic family"   -> 9,
      "B family"       -> 3,
      "quarter"        -> 1,
      "prismatic lift" -> 10,
      "gyro/elongated" -> 5
    )
    if !famOk then flags += s"family counts off: $fam"
    (rows, flags.distinct.toVector)

  private def species(idx: Int): String = SpeciesEnumerator.species(idx).showSupport
