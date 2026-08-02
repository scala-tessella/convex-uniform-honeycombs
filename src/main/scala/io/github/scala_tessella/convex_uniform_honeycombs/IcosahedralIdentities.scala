package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*

/** The two icosahedral identities that the corona fixpoint observed at interval
  * precision, now proven EXACTLY over ℚ(√5).
  *
  * (I1) The lateral 3·3 dihedral of the unit pentagonal antiprism A5 IS the icosahedral dihedral
  * arccos(−√5/3): the unit-edge icosahedron minus two antipodal caps is a unit pentagonal antiprism whose
  * lateral faces are icosahedron faces, so the lateral edges carry literally the same face pair. Verified
  * here on the exact model (0, ±1, ±φ)-cyclic (edge 2): the ten vertices off one antipodal pair split into
  * two parallel pentagon rings, all lateral faces are icosahedron faces, and cos² = 5/9 with negative sign.
  *
  * (I2) θ_dodec(5·5) + θ_A5(3·5) + θ_icosidodec(3·5) = 360° EXACTLY — the identity that let the trio {dodec,
  * icosidodec, truncDodec} survive round 1 of the corona fixpoint. The three cosines are −√r₁, −√r₂, −√r₃
  * with r₁ = 1/5, r₂ = (5−2√5)/15, r₃ = (5+2√5)/15 (certified on exact models below), and the nested radicals
  * cancel PAIRWISE into rationals: r₂r₃ = 1/45, (1−r₂)(1−r₃) = 16/45, so cos(θ₂+θ₃) = √(1/45) − √(16/45) =
  * −√(1/5) = cos θ₁, and with A = (1−r₂)r₃, B = r₂(1−r₃): A + B = 28/45, AB = (4/45)², so sin(θ₂+θ₃) =
  * −(√A+√B) with (√A+√B)² = 28/45 + 8/45 = 4/5 = sin²θ₁, i.e. sin(θ₂+θ₃) = −sin θ₁. Hence θ₂+θ₃ ≡ −θ₁ (mod
  * 360°); the certified intervals put θ₁+θ₂+θ₃ in (358°, 362°), so the sum is exactly 360°. Every step is
  * arithmetic in ℚ(√5) plus comparisons of square roots of positive rationals.
  */
object IcosahedralIdentities:

  /** Exact ℚ(√5): a + b·√5. */
  final case class Q5(a: Frac, b: Frac):
    def +(o: Q5): Q5    = Q5(a + o.a, b + o.b)
    def -(o: Q5): Q5    = Q5(a - o.a, b - o.b)
    def *(o: Q5): Q5    = Q5(a * o.a + Frac(5, 1) * (b * o.b), a * o.b + b * o.a)
    def /(o: Q5): Q5    = // multiply by the conjugate
      val d = o.a * o.a - Frac(5, 1) * (o.b * o.b)
      require(!d.isZero, s"Q5 division by zero-norm $o")
      val n = this * Q5(o.a / d, Frac(0, 1) - o.b / d)
      n
    def unary_- : Q5    = Q5(Frac(0, 1) - a, Frac(0, 1) - b)
    def isZero: Boolean = a.isZero && b.isZero

    /** Exact sign of a + b√5. */
    def signum: Int =
      (a.signum, b.signum) match
        case (0, sb)              => sb
        case (sa, 0)              => sa
        case (sa, sb) if sa == sb => sa
        case (sa, _)              => sa * (a * a - Frac(5, 1) * (b * b)).signum

    def toDouble: Double = a.toDouble + b.toDouble * math.sqrt(5.0)

  object Q5:
    val zero: Q5                  = Q5(Frac(0, 1), Frac(0, 1))
    val one: Q5                   = Q5(Frac(1, 1), Frac(0, 1))
    val phi: Q5                   = Q5(Frac(1, 2), Frac(1, 2))
    def ofInt(n: Int): Q5         = Q5(Frac(n, 1), Frac(0, 1))
    def rat(n: Long, d: Long): Q5 = Q5(Frac.make(n, d), Frac(0, 1))

  type V5 = (Q5, Q5, Q5)
  private def vSub(u: V5, v: V5): V5  = (u._1 - v._1, u._2 - v._2, u._3 - v._3)
  private def vAdd(u: V5, v: V5): V5  = (u._1 + v._1, u._2 + v._2, u._3 + v._3)
  private def vNeg(u: V5): V5         = (-u._1, -u._2, -u._3)
  private def dot(u: V5, v: V5): Q5   = u._1 * v._1 + u._2 * v._2 + u._3 * v._3
  private def cross(u: V5, v: V5): V5 =
    (u._2 * v._3 - u._3 * v._2, u._3 * v._1 - u._1 * v._3, u._1 * v._2 - u._2 * v._1)
  private def dist2(u: V5, v: V5): Q5 = { val d = vSub(u, v); dot(d, d) }

  import Q5.{phi, one, zero}

  // ---------- the exact icosahedron, edge 2 ----------

  val icosaVertices: Vector[V5] =
    val pm = Vector((one, phi), (one, -phi), (-one, phi), (-one, -phi))
    pm.map((a, b) => (zero, a, b)) ++ pm.map((a, b) => (a, b, zero)) ++ pm.map((a, b) => (b, zero, a))

  private val four = Q5.ofInt(4)

  lazy val icosaEdges: Vector[(Int, Int)] =
    for
      i <- icosaVertices.indices.toVector
      j <- i + 1 until icosaVertices.size
      if dist2(icosaVertices(i), icosaVertices(j)) == four
    yield (i, j)

  lazy val icosaFaces: Vector[(Int, Int, Int)] =
    val adj =
      icosaEdges.flatMap((i, j) => Vector(i -> j, j -> i)).groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    for
      i <- icosaVertices.indices.toVector
      j <- adj(i).toVector.sorted if j > i
      k <- (adj(i) & adj(j)).toVector.sorted if k > j
    yield (i, j, k)

  /** Outward normal of a triangle on vertices of an origin-centered solid. */
  private def outwardNormal(a: V5, b: V5, c: V5): V5 =
    val n = cross(vSub(b, a), vSub(c, a))
    if dot(n, vAdd(vAdd(a, b), c)).signum > 0 then n else vNeg(n)

  /** Certified dihedral cosine across two adjacent faces with outward normals n1, n2: cos(dihedral) =
    * −(n1·n2)/(|n1||n2|), returned as (sign of the cosine, cos² exact).
    */
  def dihedralCos2(n1: V5, n2: V5): (Int, Q5) =
    val d = dot(n1, n2)
    (-d.signum, d * d / (dot(n1, n1) * dot(n2, n2)))

  /** cos²(icosahedron dihedral) = 5/9, sign −. */
  lazy val icosaDihedral: (Int, Q5) =
    val (i, j)                    = icosaEdges.head
    val Vector(f1, f2)            =
      icosaFaces.filter(f => Set(f._1, f._2, f._3).intersect(Set(i, j)).size == 2).take(2): @unchecked
    def n(f: (Int, Int, Int)): V5 =
      outwardNormal(icosaVertices(f._1), icosaVertices(f._2), icosaVertices(f._3))
    dihedralCos2(n(f1), n(f2))

  // ---------- A5 = icosahedron minus two antipodal caps ----------

  final case class A5Model(
      axis: V5,                              // the removed antipodal pair ±axis
      ring1: Vector[Int],                    // dot(v, axis) = +φ, a regular pentagon (verified)
      ring2: Vector[Int],                    // dot(v, axis) = −φ
      lateralFaces: Vector[(Int, Int, Int)], // exactly the 10 icosa faces avoiding ±axis
      ringsArePentagons: Boolean,            // each ring: 5 vertices, each with exactly 2 ring-neighbors at edge distance
      lateralEdgesAreIcosaEdges: Boolean
  )

  lazy val a5: A5Model =
    val axis                                 = icosaVertices(0)
    val negIdx                               = icosaVertices.indexWhere(v => vSub(v, vNeg(axis)) == ((zero, zero, zero)))
    val ring1                                = icosaVertices.indices.filter(i => (dot(icosaVertices(i), axis) - phi).isZero).toVector
    val ring2                                = icosaVertices.indices.filter(i => (dot(icosaVertices(i), axis) + phi).isZero).toVector
    val lateral                              = icosaFaces.filter(f => !Set(f._1, f._2, f._3).exists(i => i == 0 || i == negIdx))
    def pentagon(ring: Vector[Int]): Boolean =
      ring.size == 5 && ring.forall { i =>
        ring.count(j => j != i && dist2(icosaVertices(i), icosaVertices(j)) == four) == 2
      }
    val latEdges                             = lateral.flatMap(f => Vector((f._1, f._2), (f._1, f._3), (f._2, f._3)))
    A5Model(
      axis,
      ring1,
      ring2,
      lateral,
      pentagon(ring1) && pentagon(ring2) && ring1.size + ring2.size + 2 == icosaVertices.size,
      latEdges.forall((i, j) => dist2(icosaVertices(i), icosaVertices(j)) == four)
    )

  /** (I1): the A5 lateral 3·3 dihedral IS the icosahedral dihedral — a lateral 3·3 edge of the embedded A5 is
    * an icosahedron edge between two icosahedron faces, so the dihedral is the same angle by identity of the
    * faces. Certified: the shared-lateral-face pairs exist and their dihedral cos² = 5/9, sign −.
    */
  lazy val a5LateralDihedral: (Int, Q5) =
    val fs                        = a5.lateralFaces
    def vs(f: (Int, Int, Int))    = Set(f._1, f._2, f._3)
    val Vector(f1, f2)            =
      (for
        i <- fs.indices.toVector
        j <- i + 1 until fs.size
        if (vs(fs(i)) & vs(fs(j))).size == 2
      yield Vector(fs(i), fs(j))).head: @unchecked
    def n(f: (Int, Int, Int)): V5 =
      outwardNormal(icosaVertices(f._1), icosaVertices(f._2), icosaVertices(f._3))
    dihedralCos2(n(f1), n(f2))

  /** cos²(A5 base·lateral 3·5 dihedral) = (5−2√5)/15, sign −. The base pentagon of the embedded A5 lies in
    * the plane dot(v, axis) = φ with outward normal +axis.
    */
  lazy val a5BaseLateralDihedral: (Int, Q5) =
    val f  = a5.lateralFaces
      .find(f => Vector(f._1, f._2, f._3).count(a5.ring1.contains) == 2)
      .get
    val n1 = outwardNormal(icosaVertices(f._1), icosaVertices(f._2), icosaVertices(f._3))
    dihedralCos2(n1, a5.axis)

  // ---------- the exact dodecahedron ----------

  /** Dodecahedron: (±1,±1,±1) ∪ cyclic (0, ±1/φ, ±φ); face planes are the icosahedron vertex directions. */
  val dodecVertices: Vector[V5] =
    val invPhi = phi - one // 1/φ = φ − 1
    val cube   =
      for
        sx <- Vector(one, -one)
        sy <- Vector(one, -one)
        sz <- Vector(one, -one)
      yield (sx, sy, sz)
    val pm     = Vector((invPhi, phi), (invPhi, -phi), (-invPhi, phi), (-invPhi, -phi))
    cube ++ pm.map((a, b) => (zero, a, b)) ++ pm.map((a, b) => (a, b, zero)) ++ pm.map((a, b) => (b, zero, a))

  /** Dodec edge²: (2/φ)² = 4(2−φ) = 6 − 2√5. */
  private val dodecEdge2 = Q5(Frac(6, 1), Frac(-2, 1))

  /** The 12 pentagon faces, built intrinsically: coplanar 5-cycles walked in the exact edge graph. */
  lazy val dodecFaces: Vector[Vector[Int]] =
    val n     = dodecVertices.size
    val adj   = (0 until n).map { i =>
      (0 until n).filter(j => j != i && dist2(dodecVertices(i), dodecVertices(j)) == dodecEdge2).toVector
    }
    val found = collection.mutable.Map.empty[Set[Int], Vector[Int]]
    for
      a <- 0 until n
      b <- adj(a)
      c <- adj(b)
      if c != a
    do
      val nrm   = cross(vSub(dodecVertices(b), dodecVertices(a)), vSub(dodecVertices(c), dodecVertices(a)))
      var cycle = Vector(a, b, c)
      var ok    = true
      while ok && cycle.size < 5 do
        val prev = cycle(cycle.size - 2)
        val cur  = cycle.last
        adj(cur).find(d => d != prev && dot(vSub(dodecVertices(d), dodecVertices(a)), nrm).isZero) match
          case Some(d) => cycle :+= d
          case None    => ok = false
      if ok && adj(cycle.last).contains(a) then found.getOrElseUpdate(cycle.toSet, cycle)
    found.values.toVector

  /** cos²(dodecahedron dihedral) = 1/5, sign −, from two intrinsically-built adjacent pentagon faces. */
  lazy val dodecDihedral: (Int, Q5) =
    val fs                      = dodecFaces
    require(fs.size == 12, s"dodecahedron must have 12 faces, got ${fs.size}")
    val Vector(i, j)            =
      (for
        i <- fs.indices.toVector
        j <- i + 1 until fs.size
        if fs(i).toSet.intersect(fs(j).toSet).size == 2
      yield Vector(i, j)).head: @unchecked
    def nrm(f: Vector[Int]): V5 =
      outwardNormal(dodecVertices(f(0)), dodecVertices(f(1)), dodecVertices(f(2)))
    dihedralCos2(nrm(fs(i)), nrm(fs(j)))

  // ---------- the exact icosidodecahedron (rectified icosahedron) ----------

  /** cos²(icosidodecahedron 3·5 dihedral) = (5+2√5)/15, sign −. Vertices are icosa edge midpoints (doubled to
    * stay integral: v_i + v_j); a triangle face lies in an icosa face plane (normal = the face normal), a
    * pentagon face lies in the plane orthogonal to an icosa vertex (normal = that vertex); the two faces of
    * icosa face f at its vertex i share the icosidodec edge {mid(e1), mid(e2)} of the two f-edges at i.
    */
  lazy val icosidodecDihedral: (Int, Q5) =
    val f  = icosaFaces.head
    val i  = f._1
    val nF = outwardNormal(icosaVertices(f._1), icosaVertices(f._2), icosaVertices(f._3))
    dihedralCos2(nF, icosaVertices(i))

  // ---------- the exact radicands and the 360° identity ----------

  val r1: Q5 = Q5.rat(1, 5)                 // cos² θ_dodec
  val r2: Q5 = Q5(Frac(1, 3), Frac(-2, 15)) // cos² θ_A5(3·5)  = (5−2√5)/15
  val r3: Q5 = Q5(Frac(1, 3), Frac(2, 15))  // cos² θ_icosidodec = (5+2√5)/15

  /** The models certify exactly these radicands (all with negative cosine sign). */
  lazy val radicandsCertified: Boolean =
    dodecDihedral == (-1, r1) && a5BaseLateralDihedral == (-1, r2) && icosidodecDihedral == (-1, r3) &&
      icosaDihedral == (-1, Q5.rat(5, 9)) && a5LateralDihedral == (-1, Q5.rat(5, 9))

  /** (I2), the exact ledger: every product of the nested radicals is a square root of a RATIONAL, so the
    * angle-sum identity reduces to ℚ(√5) arithmetic. Returns true iff every exact step verifies.
    */
  lazy val edgeIdentityExact: Boolean =
    val c2c3sq = r2 * r3                       // (cos θ₂ cos θ₃)², both cosines negative ⇒ product +√
    val s2s3sq = (Q5.one - r2) * (Q5.one - r3) // (sin θ₂ sin θ₃)², both sines positive
    val aa     = (Q5.one - r2) * r3            // (sin θ₂ |cos θ₃|)²
    val bb     = r2 * (Q5.one - r3)            // (|cos θ₂| sin θ₃)²
    val abProd = aa * bb
    // cos(θ₂+θ₃) = √(1/45) − √(16/45) = −3√(1/45) = −√(9/45) = −√(1/5) = −√r1 = cos θ₁
    val cosOk  = c2c3sq == Q5.rat(1, 45) && s2s3sq == Q5.rat(16, 45) && Q5.rat(9, 45) == r1
    // sin(θ₂+θ₃) = −(√aa + √bb); (√aa+√bb)² = aa + bb + 2√(aa·bb) with √(aa·bb) = 4/45 rational
    val sinOk  = abProd == Q5.rat(16, 2025) && (aa + bb) == Q5.rat(28, 45) &&
      (Q5.rat(28, 45) + Q5.rat(8, 45)) == (Q5.one - r1) // (√aa+√bb)² = 4/5 = sin²θ₁
    cosOk && sinOk

  /** Interval location: θ₁+θ₂+θ₃ ∈ (358°, 362°) certified, so ≡ 0 (mod 360°) pins the sum to exactly 360°. */
  lazy val sumInterval: CertifiedDihedrals.Iv =
    import CertifiedDihedrals.*
    val t1 = edgeTypesOf(List(5, 5, 5))((5, 5))
    val t2 = edgeTypesOf(antiprismConfig(5))((3, 5))
    val t3 = edgeTypesOf(List(3, 5, 3, 5))((3, 5))
    t1 + t2 + t3

  /** The full identity (I2): θ_dodec + θ_A5(3·5) + θ_icosidodec = 360° exactly. */
  lazy val edgeIdentity: Boolean =
    radicandsCertified && edgeIdentityExact &&
      sumInterval.lo > 358.0 && sumInterval.hi < 362.0
