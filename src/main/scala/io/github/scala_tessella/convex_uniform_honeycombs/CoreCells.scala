package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

import HoneycombAlphabet.CellType

/** The rigidity input of the appendix (Lemma A.2): for each of the 13 core cells, every combinatorial
  * automorphism of its face lattice is induced by an isometry. The finite assertion behind it is that the
  * combinatorial automorphism group and the geometric symmetry group have the same order (the latter embeds
  * in the former, since an isometry of a convex polytope permutes its faces), which is what this object
  * certifies cell by cell; and that the 13 face lattices are pairwise non-isomorphic, which the face-size
  * multisets already show.
  *
  * Each cell is built from classical coordinates (all permutations and sign changes of a seed, or the regular
  * polygon of a prism), its faces recovered as the maximal coplanar vertex sets supporting the polytope, its
  * edges as the pairs of vertices sharing two faces, and its FLAGS as the incident triples (vertex, edge,
  * face). A combinatorial automorphism is determined by the image of one flag and exists for that image
  * exactly when walking the three flag involutions from both flags in step yields a consistent bijection
  * (abstract polytopes are flag-connected); so |Aut| is the number of flags reachable as images of the base
  * flag. A geometric symmetry is determined by the image of one flag too: the orthogonal map carrying the
  * base flag's frame (vertex direction, edge direction, face normal, all from the centroid) onto the image
  * flag's frame, which is a symmetry exactly when it maps the vertex set onto itself; |Sym| is the number of
  * flags for which it does. Vertex matching uses doubles with tolerance 1e-9 against vertex separations of at
  * least 1, so the counts are decided with margin.
  *
  * The same symmetry groups certify the CELL RIGIDITY LEMMA of the paper — a placed core cell is determined
  * by its type, one edge, the two face germs at that edge and the ROLES of the two faces (their orbits under
  * the cell's symmetry group) — through three finite facts per cell: faces of equal size have equal roles
  * except the squares of the rhombicuboctahedron (six axial, twelve belt); the reflection in the
  * perpendicular bisector plane of every edge is a symmetry; and the symmetry group is transitive on the
  * (edge, ordered pair of incident faces) triples of each ordered pair of roles. Without the roles the
  * statement is false: at a 4·4 edge of the rhombicuboctahedron the reflection in the bisector plane of the
  * dihedral angle swaps the axial and the belt square, preserves both germs, and is not a symmetry — two
  * distinct placed rhombicuboctahedra share the edge, both face planes and both interior sides.
  */
object CoreCells:

  type V = (Double, Double, Double)

  private def sub(a: V, b: V): V      = (a._1 - b._1, a._2 - b._2, a._3 - b._3)
  private def dot(a: V, b: V): Double = a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  private def cross(a: V, b: V): V    =
    (a._2 * b._3 - a._3 * b._2, a._3 * b._1 - a._1 * b._3, a._1 * b._2 - a._2 * b._1)
  private def norm(a: V): Double      = math.sqrt(dot(a, a))
  private def unit(a: V): V           =
    val n = norm(a)
    (a._1 / n, a._2 / n, a._3 / n)
  private def scale(a: V, s: Double)  = (a._1 * s, a._2 * s, a._3 * s)
  private def dist(a: V, b: V)        = norm(sub(a, b))

  private val Eps = 1e-9

  /** All permutations of the coordinates and all sign changes of a seed, deduplicated. */
  private def orbit(seed: V, evenSignsOnly: Boolean = false): Vector[V] =
    val (x, y, z) = seed
    val perms     = Vector(x, y, z).permutations.toVector
    val signs     =
      for
        s1 <- Vector(1.0, -1.0)
        s2 <- Vector(1.0, -1.0)
        s3 <- Vector(1.0, -1.0)
        if !evenSignsOnly || s1 * s2 * s3 > 0
      yield (s1, s2, s3)
    val all       = for p <- perms; (s1, s2, s3) <- signs yield (s1 * p(0), s2 * p(1), s3 * p(2))
    all.foldLeft(Vector.empty[V])((acc, v) => if acc.exists(dist(_, v) < Eps) then acc else acc :+ v)

  private def prism(p: Int): Vector[V] =
    val r = 1.0 / (2.0 * math.sin(math.Pi / p))
    (0 until p).toVector.flatMap { k =>
      val a = 2 * math.Pi * k / p
      Vector((r * math.cos(a), r * math.sin(a), 0.5), (r * math.cos(a), r * math.sin(a), -0.5))
    }

  private val s2 = math.sqrt(2.0)

  /** Classical coordinates of the 13 core cells (edge lengths differ between cells; only shape matters). */
  val vertices: Map[CellType, Vector[V]] = Map(
    CellType.Tet                 -> orbit((1, 1, 1), evenSignsOnly = true),
    CellType.Cube                -> orbit((1, 1, 1)),
    CellType.Oct                 -> orbit((1, 0, 0)),
    CellType.TruncTet            -> orbit((1, 1, 3), evenSignsOnly = true),
    CellType.Cuboctahedron       -> orbit((1, 1, 0)),
    CellType.TruncOct            -> orbit((0, 1, 2)),
    CellType.TruncCube           -> orbit((s2 - 1, 1, 1)),
    CellType.Rhombicuboctahedron -> orbit((1, 1, 1 + s2)),
    CellType.TruncCuboctahedron  -> orbit((1, 1 + s2, 1 + 2 * s2)),
    CellType.P3                  -> prism(3),
    CellType.P6                  -> prism(6),
    CellType.P8                  -> prism(8),
    CellType.P12                 -> prism(12)
  )

  final case class Cell(
      cellType: CellType,
      verts: Vector[V],
      faces: Vector[Vector[Int]],     // vertex indices, cyclically ordered
      edges: Vector[(Int, Int)],
      flags: Vector[(Int, Int, Int)], // (vertex, edge, face) indices
      autOrder: Int,                  // combinatorial automorphisms of the face lattice
      symmetries: Vector[
        Vector[Int]
      ]                               // the isometries mapping the vertex set onto itself, as vertex permutations
  ):
    def symOrder: Int            = symmetries.size
    def faceSizes: Map[Int, Int] = faces.groupBy(_.size).view.mapValues(_.size).toMap
    def rigid: Boolean           = autOrder == symOrder

    private val faceIndex: Map[Set[Int], Int]          = faces.zipWithIndex.map((f, i) => f.toSet -> i).toMap
    private def faceImage(p: Vector[Int], f: Int): Int = faceIndex(faces(f).map(p).toSet)
    private def edgeImage(p: Vector[Int], e: Int): Int =
      val (a, b) = edges(e)
      edges.indexOf((p(a) min p(b), p(a) max p(b)))

    private def orbitsOf[A](items: Vector[A], image: (Vector[Int], A) => A): Vector[Int] =
      val idx               = items.zipWithIndex.toMap
      val parent            = Array.tabulate(items.size)(identity)
      def find(i: Int): Int = if parent(i) == i then i else { parent(i) = find(parent(i)); parent(i) }
      for (it, i) <- items.zipWithIndex; p <- symmetries do
        val j = idx(image(p, it))
        parent(find(i)) = find(j)
      val roots             = items.indices.map(find).distinct
      items.indices.toVector.map(i => roots.indexOf(find(i)))

    /** The ROLE of each face: the index of its orbit under the symmetry group. */
    lazy val faceRoles: Vector[Int] = orbitsOf(faces.indices.toVector, faceImage)
    def roleCount: Int              = faceRoles.distinct.size

    /** Faces of equal size have equal roles, except the squares of the rhombicuboctahedron. */
    def rolesBySize: Boolean = faces.indices.forall(i =>
      faces.indices.forall(j => faces(i).size != faces(j).size || faceRoles(i) == faceRoles(j))
    )

    /** Is the reflection in the perpendicular bisector plane of every edge a symmetry? */
    lazy val bisectorMirrors: Boolean = edges.forall { (a, b) =>
      val m = scale((verts(a)._1 + verts(b)._1, verts(a)._2 + verts(b)._2, verts(a)._3 + verts(b)._3), 0.5)
      val d = unit(sub(verts(b), verts(a)))
      verts.forall { p =>
        val q = sub(p, scale(d, 2 * dot(sub(p, m), d)))
        verts.exists(dist(_, q) < 1e-7)
      }
    }

    /** The (edge, first face, second face) triples with both faces at the edge, both orders. */
    private lazy val edgeFlags: Vector[(Int, Int, Int)] =
      edges.indices.toVector.flatMap { e =>
        val (a, b) = edges(e)
        val owners = faces.indices.filter { f =>
          val fs = faces(f)
          fs.indices.exists(i => Set(fs(i), fs((i + 1) % fs.size)) == Set(a, b))
        }
        Vector((e, owners(0), owners(1)), (e, owners(1), owners(0)))
      }

    /** Is the symmetry group transitive on the edge triples of each ordered pair of roles? Then a placed copy
      * of the cell is determined by an edge, its two face germs there, and their roles.
      */
    lazy val roleTransitive: Boolean =
      val orbits = orbitsOf(edgeFlags, (p, t) => (edgeImage(p, t._1), faceImage(p, t._2), faceImage(p, t._3)))
      val pairs  = edgeFlags.map(t => (faceRoles(t._2), faceRoles(t._3)))
      pairs.zip(orbits).distinct.size == pairs.distinct.size

  /** Faces as maximal coplanar vertex sets on the boundary, each cyclically ordered around its centroid. */
  private def facesOf(vs: Vector[V]): Vector[Vector[Int]] =
    val c     = scale(vs.reduce((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3)), 1.0 / vs.size)
    val found = collection.mutable.ArrayBuffer.empty[Set[Int]]
    for
      i <- vs.indices; j <- vs.indices if j > i; k <- vs.indices if k > j
      n  = cross(sub(vs(j), vs(i)), sub(vs(k), vs(i))) if norm(n) > Eps
    do
      val nn  = unit(n)
      val d   = dot(nn, vs(i))
      val out = if dot(nn, c) > d then scale(nn, -1.0) else nn // outward normal
      val dd  = dot(out, vs(i))
      if vs.forall(v => dot(out, v) <= dd + Eps) then
        val members = vs.indices.filter(m => math.abs(dot(out, vs(m)) - dd) < Eps).toSet
        if members.size >= 3 && !found.contains(members) then found += members
    found.toVector.map { members =>
      val ms = members.toVector
      val fc = scale(ms.map(vs).reduce((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3)), 1.0 / ms.size)
      val n  = unit(sub(fc, c))
      val e1 = unit(sub(vs(ms.head), fc))
      val e2 = cross(n, e1)
      ms.sortBy { m =>
        val d = sub(vs(m), fc)
        math.atan2(dot(d, e2), dot(d, e1))
      }
    }

  private def edgesOf(faces: Vector[Vector[Int]]): Vector[(Int, Int)] =
    faces.flatMap { f =>
      f.indices.map(i => (f(i), f((i + 1) % f.size))).map((a, b) => (a min b, a max b))
    }.distinct.sorted

  /** The flag graph: for a flag (v, e, f), the three neighbours σ₀ (other vertex of e), σ₁ (other edge of f
    * at v), σ₂ (other face at e).
    */
  final private class Flags(faces: Vector[Vector[Int]], edges: Vector[(Int, Int)]):
    val edgeIndex: Map[(Int, Int), Int]                     = edges.zipWithIndex.toMap
    val all: Vector[(Int, Int, Int)]                        =
      (for
        (f, fi) <- faces.zipWithIndex
        i       <- f.indices
        e        = edgeIndex((f(i) min f((i + 1) % f.size), f(i) max f((i + 1) % f.size)))
        v       <- Vector(f(i), f((i + 1) % f.size))
      yield (v, e, fi)).distinct
    val index: Map[(Int, Int, Int), Int]                    = all.zipWithIndex.toMap
    def sigma(k: Int, fl: (Int, Int, Int)): (Int, Int, Int) =
      val (v, e, f) = fl
      k match
        case 0 => val (a, b) = edges(e); (if v == a then b else a, e, f)
        case 1 =>
          val face = faces(f)
          val es   = face.indices.map(i =>
            edgeIndex((face(i) min face((i + 1) % face.size), face(i) max face((i + 1) % face.size)))
          )
          val mine = es.filter { ei =>
            val (a, b) = edges(ei); a == v || b == v
          }
          (v, if mine(0) == e then mine(1) else mine(0), f)
        case _ =>
          val owners = faces.indices.filter(fi =>
            faces(fi).indices.exists(i =>
              edgeIndex((
                faces(fi)(i) min faces(fi)((i + 1) % faces(fi).size),
                faces(fi)(i) max faces(fi)((i + 1) % faces(fi).size)
              )) == e
            )
          )
          (v, e, if owners(0) == f then owners(1) else owners(0))

  /** Does mapping the base flag to `target` extend to a combinatorial automorphism? Walk the flag graph from
    * both flags in step; the map must stay a bijection.
    */
  private def extendsToAutomorphism(fl: Flags, base: (Int, Int, Int), target: (Int, Int, Int)): Boolean =
    val map   = collection.mutable.HashMap[(Int, Int, Int), (Int, Int, Int)](base -> target)
    val image = collection.mutable.HashSet[(Int, Int, Int)](target)
    val queue = collection.mutable.Queue(base)
    var ok    = true
    while queue.nonEmpty && ok do
      val a = queue.dequeue()
      val b = map(a)
      for k <- 0 to 2 if ok do
        val a2 = fl.sigma(k, a)
        val b2 = fl.sigma(k, b)
        map.get(a2) match
          case Some(c) => if c != b2 then ok = false
          case None    =>
            if image.contains(b2) then ok = false
            else
              map(a2) = b2; image += b2; queue.enqueue(a2)
    ok && map.size == fl.all.size

  /** The orthonormal frame of a flag: vertex direction, edge direction (orthogonalized), their cross. */
  private def frame(vs: Vector[V], c: V, edges: Vector[(Int, Int)], fl: (Int, Int, Int)): (V, V, V) =
    val (v, e, _) = fl
    val (a, b)    = edges(e)
    val w         = if a == v then b else a
    val u1        = unit(sub(vs(v), c))
    val d         = sub(vs(w), vs(v))
    val u2        = unit(sub(d, scale(u1, dot(d, u1))))
    (u1, u2, cross(u1, u2))

  /** The orthogonal map carrying frame F onto frame G (proper, or composed with the reflection in the plane
    * of G's first two vectors), as the permutation of the vertex set it induces, when it is a symmetry of the
    * vertex set about the centroid that carries the base face onto the target face. A flag's frame does not
    * see its face, so the face condition is what makes the map an isometry of FLAGS, counted once per flag
    * image.
    */
  private def flagSymmetry(
      vs: Vector[V],
      c: V,
      f: (V, V, V),
      g: (V, V, V),
      improper: Boolean,
      baseFace: Vector[Int],
      targetFace: Vector[Int]
  ): Option[Vector[Int]] =
    val (f1, f2, f3)   = f
    val (g1, g2, g3)   = g
    val g3s            = if improper then scale(g3, -1.0) else g3
    def apply(p: V): V =
      val d         = sub(p, c)
      val (x, y, z) = (dot(d, f1), dot(d, f2), dot(d, f3))
      (
        c._1 + x * g1._1 + y * g2._1 + z * g3s._1,
        c._2 + x * g1._2 + y * g2._2 + z * g3s._2,
        c._3 + x * g1._3 + y * g2._3 + z * g3s._3
      )
    val perm           = vs.map(p => vs.indexWhere(q => dist(apply(p), q) < 1e-7))
    val faceOk         = baseFace.forall(i => targetFace.exists(j => dist(apply(vs(i)), vs(j)) < 1e-7))
    if perm.forall(_ >= 0) && faceOk then Some(perm) else None

  def build(t: CellType): Cell =
    val vs    = vertices(t)
    val faces = facesOf(vs)
    val edges = edgesOf(faces)
    val fl    = Flags(faces, edges)
    val base  = fl.all.head
    val aut   = fl.all.count(target => extendsToAutomorphism(fl, base, target))
    val c     = scale(vs.reduce((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3)), 1.0 / vs.size)
    val fb    = frame(vs, c, edges, base)
    val sym   = fl.all.flatMap { target =>
      val ft = frame(vs, c, edges, target)
      Vector(false, true).flatMap(imp => flagSymmetry(vs, c, fb, ft, imp, faces(base._3), faces(target._3)))
    }
    Cell(t, vs, faces, edges, fl.all, aut, sym)

  /** All 13 core cells, in the alphabet's order. */
  lazy val cells: Vector[Cell] = CellType.values.toVector.map(build)

  /** The 13 face-size multisets are pairwise distinct: the face lattices are pairwise non-isomorphic. */
  lazy val faceSizesDistinct: Boolean = cells.map(_.faceSizes).distinct.size == cells.size
