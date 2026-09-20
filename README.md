# convex-uniform-honeycombs — verification artifact

[![CI](https://github.com/scala-tessella/convex-uniform-honeycombs/actions/workflows/ci.yml/badge.svg)](https://github.com/scala-tessella/convex-uniform-honeycombs/actions/workflows/ci.yml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22849635.svg)](https://doi.org/10.5281/zenodo.22849635)

Machine-checked companion to the paper

> **The 28 convex uniform honeycombs: a completeness theorem.**

Every vertex-transitive face-to-face honeycomb of Euclidean 3-space by unit-edge convex uniform polyhedra
is one of the 28 convex uniform honeycombs. The list has been known since Johnson assembled it in 1991 and
Grünbaum published it in 1994, and three independent enumerations agree on it — but none of them is a
completeness proof, and a 2006 survey reviewed by Johnson still records the list as "considered complete
(but not yet proved so)". This repository is the machine-checked half of the proof.

It contains the paper's **claim specs** and the modules that carry the paper's **own** results — the
Alphabet Theorem's exclusion campaigns, the exact $\mathbb{Q}(\sqrt2,\sqrt3)$ certificates, and the
identification against the classical list. Every shared engine — the cell alphabet, certified dihedrals,
the species assembly on the sphere of directions, the shell filter, the pattern/development engine, the
audit certificates and the Delaney–Dress symbol side of a vertex star — is the pinned
[`research-core`](https://github.com/scala-tessella/research-core) `0.11.0` library.

## Reproduce

```bash
sbt test                    # the whole paper, appendix included — a couple of minutes
sbt -DexactCoherence test   # + the exact coherence of every accepted pattern (about half an hour)
sbt -Dcerts test            # + regenerate the sixteen cited certificates into certs/ (includes the above)
```

The entire proof re-derives from scratch on every run — no fixtures, no published input, no external tools,
every positive certificate on the critical path exact — in a couple of minutes on a laptop. That is a
property of the proof, not of the harness: the objects are finite and small (34 vertex species, 69 edge
figures, 28 certified development balls of 515 to 4521 vertices), and the expensive part of the programme
is the *design* of the finite certificates, not their checking. The one long tier is the exact replay of
class coherence pattern by pattern (an exact ball per accepted pattern), opt-in and always part of the
certificate run.

`certs/` is committed, so the certificates the paper cites are readable without running anything. They are
an **output** of this repository and never an input: no spec reads them back, so a stale or hand-edited
certificate cannot make anything pass.

## Claim → check

Run one with `sbt "testOnly *<SpecName>"`.

| Paper result | Spec | Certifies |
|---|---|---|
| The two-tier split and the edge-figure catalogue | `HoneycombAlphabetSpec` | The 23 core dihedrals against the classical cosines, the $\alpha$-charge decomposition, and the **69 edge figures** over 62 cell multisets — 22 with 3 cells, 39 with 4, 7 with 5, 1 with 6. Certificate: `edge-figures.txt`. |
| **The Alphabet Theorem** — the corona fixpoint | `OutsiderExclusionSpec` | Dihedrals rebuilt from the vertex configuration alone (no coordinate tables), reproducing all 23 core values within $10^{-7}$; the fixpoint, over an **uncapped pool** (every prism and antiprism, the tails as corridors), kills all **106 outsider targets in three rounds**. An interval *miss* certifies non-equality, so each exclusion is a proof; every dead edge records its nearest misses, the closest of all $1.7\cdot10^{-7}$ degrees, a genuine near-coincidence at $A_{56}$ still three orders above the interval widths; read for face compatibility, the closest miss is Lemma E's own family at $9.3\cdot10^{-4}$ degrees and, beyond it, $0.049$ degrees at the icosahedral edge. Certificate: `outsider-exclusion.txt`. |
| The two exact identities | `IcosahedralIdentitiesSpec` | The two exact coincidences interval arithmetic cannot decide, settled over $\mathbb{Q}(\sqrt5)$ on intrinsic models: the $A_5$ lateral dihedral **is** the icosahedral one, and the pentagon-family edge figure closes at exactly $360°$. Certificate: `icosahedral-identities.txt`. |
| Lemmas A, B, E and the tails | `TailExclusionSpec` | The antiprism closed forms against the independent interval reconstruction and against the exact $\mathbb{Q}(\sqrt5)$ values at $q=5$; **Lemma E** (no antiprism pair closes) by the interleaving $h(2q) < g(q) < h(2q-1)$, checked on a grid to $q = 20000$; every prism $p>500$ and every antiprism $q\ge101$ excluded with **no cap left anywhere**. Certificate: `tail-exclusion.txt`. |
| The prism grid | `PrismGridSpec` | The exotic-prism vertical-edge equation solved completely by exact Egyptian-fraction enumeration: **exactly six solutions**, precisely the prismatic lifts of the six non-extendable planar species $3.7.42$, $3.8.24$, $3.9.18$, $3.10.15$, $4.5.20$, $5.5.10$. Certificate: `prism-grid.txt`. |
| **The Alphabet Theorem completed** — Lemma OPP and the six lifts | `ExoticLiftsSpec` | The rco flank-propagation lemma on the exact $\mathbb{Q}(\sqrt2)$ model, and the walk campaign killing **all nine exotic prism families in two rounds** (seven by their walks in round one, P10 and P20 by corona in round two) — the planar non-extension proofs, lifted. Certificate: `exotic-lifts.txt`. |
| The area equation | `SpeciesSupportsSpec` | Corner excesses in the two-tier lattice, the split $\sum r = 720 \wedge \sum n = 0$ with the parity condition, leaving **exactly 97 cell multisets**; the unique 14-cell support is $\{\mathrm{tet}{:}8,\ \mathrm{oct}{:}6\}$, and it is also the **only** support over the tet/oct sub-alphabet — which is what makes the Barlow statement unconditional. Certificate: `species-supports.txt`. |
| **The species table** | `SpeciesEnumeratorSpec` | **Exactly 34 vertex species** on 25 of the 97 supports, assembled as edge-to-edge tilings of the sphere of directions with exact acceptance (closed complex, excess exactly $(720,0)$, every vertex sum exactly $(360,0)$) and deduplicated by canonical combinatorial-map key. Includes the **Barlow dichotomy** (the octet support carries exactly the fcc and hcp stars) and its unconditional form: those two are the **only** species any face-to-face unit tetrahedron–octahedron honeycomb can carry, with no symmetry hypothesis — the local half of the close-packing classification. Certificate: `species-table.txt`. |
| Corona structure | `SpeciesCoronaSpec` | Only **58 of the 69** edge figures are hosted by any species — 11 are dead letters, locally consistent at an edge but hosted by no vertex — and the face-cycle filter leaves all 34 species intact, with a teeth check proving the filter is not vacuous. Certificate: `species-corona.txt`. |
| The mono-species shell filter | `MonoShellSpec` | **26 of the 34 species survive**; the 8 exclusions mirror the planar classification exactly. Ring agreement is decided by cell descriptors, so no case analysis enters. Anchor: the cubic star has exactly 48 gluings per tiling-vertex — the octahedral group. Certificate: `mono-shell.txt`. |
| **The pattern enumeration** | `TransitivePatternsSpec` | Over the 26 species, exactly **28 fingerprint classes**: 24 species carry one, two carry two. Sixteen species are settled by the forcing theorem with no search at all. The developability gap is pinned: the snub-trihexagonal lift has a family of (R1)+(R2)-consistent patterns whose developments collide — without the collision check the count would be a spurious 29. Certificate: `transitive-patterns.txt`. |
| **The completeness audit** | `CompletenessAuditSpec` | All **28 classes** carry periodization certificates (three exact translation words, ball periodicity, lattice invariance, coverage), every one of the **496 accepted patterns** coheres with its class representative (its own periodization certificate, the representative's ball aligned onto it by an explicit stabilizer element, the transported lattice checked on its ball — the paper's coherence lemma), and **every skeleton is closed** by germ forcing or by uncapped exhaustion — ten skeletons of three species go to exhaustion, eight of them empty of consistent patterns, the snub lift's second and third carrying 128 accepted patterns each, the third entirely behind the cap. Certificate: `completeness-audit.txt`. |
| **The exact certificates** | `ExactCertificatesSpec` | All 34 star models and all 28 periodization certificates re-verified in **exact arithmetic over $\mathbb{Q}(\sqrt2,\sqrt3)$** via the internal basis (nothing is ever normalized), with recognized Gram entries confined inside interval enclosures of width $<6\cdot10^{-9}$, collision-free periodic balls of **515 to 4521** vertices that are box-periodic and closed, on which every generator is checked to act as an exact symmetry, and the two doubled species separated by **exact** canonical fingerprints; germ forcing replayed exactly on every skeleton (forcing exactly the skeletons the audit forces); and, opt-in (`-DexactCoherence`, always under `-Dcerts`), the **coherence of every accepted pattern** with its class — own exact certificate, exact alignment by a stabilizer element, the representative's lattice transported and checked — within the caps and, on the ten skeletons germ forcing leaves open, beyond them: every exhaustion re-enumerated uncapped and every accepted pattern cohered exactly. Every positive certificate on the critical path is exact; what stays numeric is the enumerations' negative decisions. Certificate: `exact-certificates.txt`. |
| **The 28, identified** | `The28IdentifiedSpec` | The 28 certified classes against the classical names, with the family census **9 + 3 + 1 + 10 + 5** and every vertex composition matched to the classical tables; the two doubled species resolved by exact lattice invariants ($\lvert\tau\rvert^2 = 2+\sqrt3$ and $2+\tfrac23\sqrt6$), with the natural false discriminant explicitly refuted. Certificate: `the-28-identified.txt`. |
| Appendix, the star complex | `StarChambersSpec` | The flag laws on every species: three fixed-point-free involutions, $\sigma_1\sigma_3 = \sigma_3\sigma_1$, $(\sigma_1\sigma_2)$ of order $p$ at a $p$-corner and $(\sigma_2\sigma_3)$ of order $r$ at an $r$-ring, chamber count four times the arc count — each checked against independently derived ring data. |
| Appendix, the foldings | `StarFoldingsSpec` | The chamber action is a faithful automorphism group; the subgroup enumeration agrees with brute force and satisfies Lagrange and conjugation-closure on the cubic star's order-48 group; the classical anchors fold as they must — the cubic star to the 1-chamber regular $\{4,3,4\}$ symbol, the fcc octet star to the 2-chamber quasiregular tetrahedral–octahedral symbol. |
| Appendix, the $\sigma_0$ assembly | `Sigma0AssemblySpec` | The propagating enumerator equals an **independently written brute-force oracle** — all involutions with fixed points, filtered by the axioms (M), (K), (F), (C) written out directly — on small foldings of several shapes, and the connectivity teeth bite. |
| **The combinatorial census** (appendix) | `SymbolCatalogSpec` | Canonical keys relabeling-invariant and separating; minimality by congruence closure on hand fixtures and on an assembled instance; and the headline — **exactly 28 minimal $k=1$ symbols over the 34 species**, with count 0 on precisely the eight shell-excluded species and 2 on precisely the two doubled ones. The shell exclusions are geometric and the census is combinatorial: their agreement cross-validates both. Certificate: `symbols-k1.txt`. |
| **The symbol identification** (appendix) | `SymbolGateSpec` | Key-for-key over all 26 shell-passing species: the minimal symbols **derived** from the certified honeycombs are exactly the symbols of the combinatorial census, totalling 28 — so the count is reproduced by an enumeration sharing only the species table with the proof. Certificate: `symbol-gate.txt`. |
| The certificates | `CertificatesSpec` | Not a claim: regenerates the sixteen cited certificate files into `certs/`. *(opt-in: `-Dcerts`)* |

## What this artifact does and does not settle

The appendix is a **consistency theorem between two methods**, not a second self-contained proof, and this
repository is explicit about the same boundary. Completeness of the census over honeycombs is
unconditional: every vertex-transitive honeycomb by the core cells has its minimal symbol among the 28,
using no shell filter, no patterns, no development and no audit. Realization of the census symbols,
however, is certified by matching against the 28 honeycombs already classified — `SymbolGateSpec` consumes
the certified patterns and balls. A standalone symbol-based proof would replace that matching by a rigid
development driven by the symbol alone.

## Trust base

| step | rests on | checked by |
|---|---|---|
| the two-tier arithmetic | Niven's theorem ($\alpha = \arctan\sqrt2$ irrational in degrees) | `HoneycombAlphabetSpec` |
| outsider exclusion | certified intervals; an interval miss proves non-equality | `OutsiderExclusionSpec` |
| the two exact coincidences | exact arithmetic in $\mathbb{Q}(\sqrt5)$ | `IcosahedralIdentitiesSpec` |
| the tails | closed forms + the interleaving lemma; no cap anywhere | `TailExclusionSpec` |
| the species table | the degree-1 spherical cover lemma; exact acceptance; the separation constant (54.7356°) behind the identify-or-separate decisions | `SpeciesEnumeratorSpec` |
| the enumeration | pattern necessity; collision-free development | `TransitivePatternsSpec` |
| the classification | periodization, coherence, separation, cap closure | `CompletenessAuditSpec` |
| the final theorem | exact $\mathbb{Q}(\sqrt2,\sqrt3)$ — every positive certificate exact | `ExactCertificatesSpec` |
| the independent count | Delaney–Dress symbols, sharing only the species table | `SymbolCatalogSpec`, `SymbolGateSpec` |

## Archival

Deposited on Zenodo as a supplement to the paper record. **Cite the version DOI of the release you
checked**, not the all-versions concept DOI — the latter always resolves to whatever is newest:

| Version | DOI |
|---|---|
| 0.4.0 | [10.5281/zenodo.22859902](https://doi.org/10.5281/zenodo.22859902) |
| 0.3.0 | [10.5281/zenodo.22857000](https://doi.org/10.5281/zenodo.22857000) |
| 0.2.0 | [10.5281/zenodo.22849635](https://doi.org/10.5281/zenodo.22849635) |
| 0.1.0 | [10.5281/zenodo.21762729](https://doi.org/10.5281/zenodo.21762729) |

Release-by-release changes are in [CHANGELOG.md](CHANGELOG.md), which states for each release whether the
claims, the specs or only the packaging moved — what a referee who checked an earlier version needs.

Zenodo assigns a release's version DOI at the moment that release is published, so it cannot be present in
the tree that release archives: the `CITATION.cff` inside a deposit carries no version DOI. The version DOI
is recorded in this table, and in `CITATION.cff` on the main branch, in the first commit after the tag; the
concept DOI (all versions) is [10.5281/zenodo.21762728](https://doi.org/10.5281/zenodo.21762728).

Pinned to `research-core 0.11.0` (an immutable Central release), archived as
[10.5281/zenodo.22859955](https://doi.org/10.5281/zenodo.22859955). That archived snapshot — not the
`research-core` repository's main branch, which may since have moved on — is the authoritative source for
what this artifact depends on. The pin plus the snapshot make this a closed, reproducible artifact
independent of any moving repository. The certificates under `certs/` are committed rather than archived
separately: they total 92 KB, regenerate byte for byte by `sbt -Dcerts test`, and are outputs only, never
read back by any spec.
