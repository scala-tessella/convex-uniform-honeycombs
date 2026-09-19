# Changelog

All notable changes to this verification artifact are documented here.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to
[early-semver](https://www.scala-sbt.org/1.x/docs/Publishing.html#Version+scheme). Because the artifact backs
a paper, entries state what a re-check would find different from the previous release — a referee who checked
an earlier version should be able to tell from here whether the claims, the specs, or only the packaging moved.

## [0.2.0] — 2026-09-19

**Class coherence proved, and the exactness claim made precise.** Two claims of 0.1.0 were stronger than
what was checked. The audit's step (B) — same fingerprint, same honeycomb — compared two developed balls
at the determination radius, which does not make a honeycomb periodic under the representative's lattice;
and "no floating-point tolerance decision on the critical path" overlooked that class coherence and germ
forcing were numeric decisions with only the 28 representatives' certificates replayed exactly. A referee
who re-checks will find both closed: the pinned `research-core` 0.9.0 certifies every accepted pattern on
its own, aligns the class representative's ball onto it by an explicit element of Stab±(S) and checks the
transported lattice on its ball (the three hypotheses of the paper's coherence lemma); the exact layer
replays that coherence for every accepted pattern — within the caps and, on the ten skeletons germ
forcing leaves open, beyond them, every exhaustion re-enumerated uncapped — and germ forcing on every
skeleton. The claim is now: every *positive* certificate on the critical path is exact in ℚ(√2,√3); what
stays numeric is the enumerations' negative decisions. The species table, the pattern classes and the 28
are unchanged.

### Changed

- `research-core` pinned to **0.9.0** (from 0.5.0 at release, 0.6.0 on main): `CompletenessAudit`'s
  coherence step is the strengthened check, and `Audit` carries the accepted-pattern count (496 over the
  26 species) and the exhausted skeletons — ten, of three species: six of the hexagonal prismatic star,
  one of the snub square lift, three of the snub-trihexagonal lift. `CompletenessAuditSpec` pins both.
- `ExactCertificatesSpec`: exact coherence of every accepted pattern and the exact replay of every
  exhaustion (`coherenceResults`; an exact ball per pattern — opt-in with `-DexactCoherence`, always part
  of the `-Dcerts` run), and exact germ forcing on every skeleton in the default suite, cross-checked
  against the audit's exhausted skeletons. The default suite grows to about two minutes.
- `TailExclusion`: Lemma E's tangent bound is the elementary `tan x ≤ x/(1 - x²/2)`; the certified
  constant chain is `(4/7)³/3 < 0.0623`, `1/(2 - (π/8)²) < 0.5418`, sum `< 0.6041`,
  `0.6041·π/8 < 0.2373 < 1/π`, each interval-checked separately.
- `exact-certificates.txt` gains sections (c) exact class coherence, (d) exact germ forcing and (f) the
  exhaustions replayed; `completeness-audit.txt` gains the pattern count and the exhausted skeletons per
  species; both re-emitted.
- `CertificatesSpec` pins the run's locale to ROOT before writing: the writers' f-interpolators formatted
  through the default locale, so a certificate regenerated on a machine with a decimal comma did not
  reproduce byte for byte.

### Added

- `CITATION.cff`: the release version and date, the version DOI as the one to cite, both the concept and
  version DOIs as identifiers, and the archive DOI of the pinned `research-core` release. Zenodo mints a
  version DOI when the release is published, so it cannot be present in the tree that release archives;
  it is recorded here in the first commit after the tag.
- README: the archival table, the note on citing the version DOI rather than the all-versions concept DOI,
  and CI and DOI badges.

## [0.1.0] — 2026-08-02

Initial release: the complete verification surface for the paper, pinned to `research-core 0.5.0`. Archived
as [doi:10.5281/zenodo.21762729](https://doi.org/10.5281/zenodo.21762729) — the version DOI to cite.

The whole paper, appendix included, re-derives from scratch under `sbt test` in about a minute: **20 suites,
112 tests**, no fixtures, no published input, no external tools, and no floating-point tolerance decision on
the critical path. There is no long tier to opt into; the single opt-in run writes the certificate files.

### Added

- **The Alphabet Theorem's exclusion campaigns**, the paper's own modules and their claim specs:
  `HoneycombAlphabetSpec` (the 23 core dihedrals against the classical cosines; the catalogue is exactly
  69 edge figures over 62 cell multisets, 22/39/7/1 by ring size, 23 of them α-charged),
  `OutsiderExclusionSpec` (dihedrals rebuilt from the vertex configuration alone — no coordinate table is
  trusted — and all 106 outsider targets excluded in two rounds, 103 in the first),
  `IcosahedralIdentitiesSpec` (the two exact coincidences interval arithmetic cannot decide, settled over
  ℚ(√5) on intrinsic models), `TailExclusionSpec` (the antiprism closed forms, Lemmas A, B and E, and the
  interleaving on grids to q = 20,000 — the tails close with **no cap left anywhere**),
  `PrismGridSpec` (the vertical-edge equation solved completely by exact Egyptian-fraction enumeration:
  exactly six solutions) and `ExoticLiftsSpec` (Lemma OPP on the exact ℚ(√2) rco model, and all nine
  exotic prism families dead in three rounds).
- **The species table and the enumeration**: `SpeciesSupportsSpec` (exactly 97 cell multisets, a unique
  14-cell support, and a unique support over the tetrahedron/octahedron sub-alphabet),
  `SpeciesEnumeratorSpec` (exactly 34 species on 25 supports, each a genuine spherical complex, with the
  Barlow dichotomy and its unconditional form), `SpeciesCoronaSpec` (58 of the 69 figures live, 11 dead
  letters, the face-cycle filter shown to have teeth), `MonoShellSpec` (26 of the 34 survive) and
  `TransitivePatternsSpec` (exactly 28 fingerprint classes, 16 species settled by forcing alone, and the
  snub collision that would otherwise make the count a spurious 29).
- **The audit and the exact certificates**: `CompletenessAuditSpec` (28 certified classes, coherence at the
  determination radius, every skeleton closed by germ forcing or uncapped exhaustion),
  `ExactCertificatesSpec` (34 exact star models and 28 exact periodization certificates over ℚ(√2,√3), Gram
  entries inside enclosures of width < 6·10⁻⁹, collision-free periodic balls of 515 to 4521 vertices, and
  the two doubled species separated exactly) and `The28IdentifiedSpec` (the classical names, the family
  census 9 + 3 + 1 + 10 + 5, and the two exact lattice invariants).
- **The appendix's independent Delaney–Dress count**: `StarChambersSpec` (the flag laws, against
  independently derived ring data), `StarFoldingsSpec` (the subgroup lattice against brute force, Lagrange
  and conjugation-closure on the cubic star's order-48 group, and the two classical foldings),
  `Sigma0AssemblySpec` (the propagating σ₀ enumerator against an independently written brute-force oracle
  over the axioms (M), (K), (F), (C)), `SymbolCatalogSpec` (canonical keys, minimality, and the census of
  exactly 28 minimal symbols whose zeros and twos match the shell filter and the doubled species) and
  `SymbolGateSpec` (the key-for-key identification over all 26 shell-passing species).
- `certs/` — the sixteen plain-text certificates the paper cites, committed so a reader need not run
  anything. They regenerate byte for byte, and they are **outputs only**: no spec reads one back, so a
  stale or hand-edited certificate cannot make a claim pass. `CertificatesSpec` (`-Dcerts`) rewrites them.
- Continuous integration (`.github/workflows/ci.yml`): `sbt test` on every push and pull request, on JDK
  17 **and** 21 — the enumeration is exact and combinatorial, so the counts must not depend on the runtime,
  and the matrix asserts that. A manual-dispatch job regenerates the certificates and fails if the
  committed set does not come back byte for byte, which is what keeps the committed copy honest.

### Notes for a referee

- Every number the paper publishes is an **assertion**, not a printed line. Several were reported rather
  than asserted in the development tree and were promoted during extraction: the catalogue shape, the
  α-charged count, the 106/103 outsider split, the 97 supports, the 515–4521 ball range, and the
  6·10⁻⁹ enclosure bound (the development tree asserted only 10⁻⁵).
- The repository holds the paper's **own** results; every shared engine is the pinned `research-core`
  library, archived at [doi:10.5281/zenodo.21762821](https://doi.org/10.5281/zenodo.21762821). That
  archived snapshot, not the library's moving main branch, is the authoritative source for what this
  artifact depends on.
- The appendix is a consistency theorem between two methods, not a second self-contained proof:
  completeness of the symbol census over honeycombs is unconditional, while realization of the census
  symbols is certified by matching against the 28 already classified. `SymbolGateSpec` consumes the
  certified patterns and balls, and the README says so in the same words as the paper.

[Unreleased]: https://github.com/scala-tessella/convex-uniform-honeycombs/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/scala-tessella/convex-uniform-honeycombs/releases/tag/v0.2.0
[0.1.0]: https://github.com/scala-tessella/convex-uniform-honeycombs/releases/tag/v0.1.0
