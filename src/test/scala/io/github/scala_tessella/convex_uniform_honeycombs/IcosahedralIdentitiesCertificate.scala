package io.github.scala_tessella.convex_uniform_honeycombs

import io.github.scala_tessella.research_core.*
import IcosahedralIdentities.*

/** Certificate `icosahedral-identities.txt`. The two icosahedral identities, proved exactly over Q(sqrt 5) on
  * intrinsic models.
  *
  * Emitted by `CertificatesSpec` under `-Dcerts`; the paper cites it by that file name.
  */
object IcosahedralIdentitiesCertificate:
  def write(dir: java.nio.file.Path): Unit =
    def q5(v: Q5): String = s"${v.a.num}/${v.a.den} + (${v.b.num}/${v.b.den})·√5"
    val sb                = new StringBuilder
    sb ++=
      "Exactness upgrades: the two icosahedral identities of the corona fixpoint, proven over Q(sqrt5)\n\n"
    sb ++= "== Exact models (edge-2 icosahedron (0,±1,±φ)-cyclic and its derived solids) ==\n"
    sb ++=
      s"icosahedron: ${icosaVertices.size} vertices, ${icosaEdges.size} edges, ${icosaFaces.size} faces\n"
    sb ++= s"A5 embedding: rings are pentagons = ${a5.ringsArePentagons}, " +
      s"laterals = ${a5.lateralFaces.size} icosa faces, lateral edges icosa edges = ${a5.lateralEdgesAreIcosaEdges}\n\n"
    sb ++= "== Certified dihedral radicands (cos = sign · sqrt(cos²), all signs negative) ==\n"
    sb ++= s"icosahedron 3·3   : cos² = ${q5(icosaDihedral._2)}\n"
    sb ++= s"A5 lateral 3·3    : cos² = ${q5(a5LateralDihedral._2)}   (I1: same faces as the icosahedron)\n"
    sb ++= s"dodecahedron 5·5  : cos² = ${q5(dodecDihedral._2)}\n"
    sb ++= s"A5 base 3·5       : cos² = ${q5(a5BaseLateralDihedral._2)}\n"
    sb ++= s"icosidodec 3·5    : cos² = ${q5(icosidodecDihedral._2)}\n\n"
    sb ++= "== (I1) A5 lateral = icosahedral dihedral ==\n"
    sb ++=
      s"exact, by embedding (icosahedron minus two antipodal caps): ${a5LateralDihedral == icosaDihedral}\n\n"
    sb ++= "== (I2) dodec(5·5) + A5(3·5) + icosidodec(3·5) = 360 exactly ==\n"
    sb ++= "ledger over Q(sqrt5): r2·r3 = 1/45, (1-r2)(1-r3) = 16/45  =>  cos(θ2+θ3) = -sqrt(1/5) = cos θ1;\n"
    sb ++=
      "A=(1-r2)r3, B=r2(1-r3): A+B = 28/45, AB = (4/45)²  =>  sin(θ2+θ3)² = 4/5, sign -  =>  = -sin θ1;\n"
    sb ++= s"ledger verified: $edgeIdentityExact\n"
    sb ++= f"interval location: sum ∈ [${sumInterval.lo}%.9f, ${sumInterval.hi}%.9f] ∋ 360 (mod-360 pin)\n"
    sb ++= s"IDENTITY EXACT: $edgeIdentity\n"
    java.nio.file.Files.writeString(dir.resolve("icosahedral-identities.txt"), sb.toString)
    println(
      s"identities: I1=${a5LateralDihedral == icosaDihedral} I2=$edgeIdentity -> $dir/icosahedral-identities.txt"
    )
