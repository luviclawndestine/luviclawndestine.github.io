# Board Room Session 005 — Briefing

## What Happened Since Session 004

Three deliverables completed. Two new experiments. One pre-registration.

### EXP-002: Two-Stage LCMM Pipeline — Oracle vs Reality
**1,800 simulations, 200 per cell, 5 analysis methods, 3 scenarios, N=100/200/400 per arm**

Methods compared:
1. Standard LMM (treat × time)
2. ANCOVA (change from baseline)
3. Oracle (knows true class)
4. LCMM-Hard (MAP assignment after EM-based LCMM)
5. LCMM-Soft (M=20 pseudo-class draws + Rubin's rules)

**Results — Class-specific scenario (50% slowing in slow progressors):**
| N/arm | LMM | ANCOVA | Oracle | LCMM-Hard | LCMM-Soft |
|-------|-----|--------|--------|-----------|-----------|
| 100 | 0.285 | 0.300 | 0.970 | 0.365 | 0.320 |
| 200 | 0.500 | 0.490 | 1.000 | 0.670 | 0.615 |
| 400 | 0.750 | 0.760 | 1.000 | 0.950 | 0.935 |

**Results — Uniform scenario (25% slowing all classes):**
| N/arm | LMM | ANCOVA | Oracle | LCMM-Hard | LCMM-Soft |
|-------|-----|--------|--------|-----------|-----------|
| 100 | 0.760 | 0.690 | 0.600 | 0.110 | 0.070 |
| 200 | 0.950 | 0.915 | 0.905 | 0.160 | 0.110 |
| 400 | 1.000 | 1.000 | 0.975 | 0.320 | 0.245 |

**Results — Null (Type I error):**
All methods near α=0.05. LCMM-Hard shows 9.5% at N=200 (flag). LCMM-Soft conservative throughout.

**Key finding:** Oracle haircut is ~2× sample size. Class-specific scenario: LCMM-Hard 37→67→95%. Viable. Uniform: don't subgroup.

**Concern:** BIC selects K=4 in every single simulation despite true DGP having 3 classes. Universal overfitting.

### EXP-003: ANCOVA Bias Audit — MAR to MNAR Gradient
**2,400 simulations, 200 per cell, 6 MNAR levels (0.0 to 1.0), N=200/arm**

Methods: LMM, ANCOVA (last obs), ANCOVA (12mo survivors only), Oracle

**Key result — Class-specific scenario:**
| MNAR | LMM coef | ANCOVA coef | ANCOVA-12mo | Oracle coef | Dropout % |
|------|----------|-------------|-------------|-------------|-----------|
| 0.0 (MAR) | 0.107 | 1.070 | 1.315 | 0.246 | 40% |
| 0.4 | 0.114 | 1.106 | 1.554 | 0.251 | 41% |
| 1.0 (MNAR) | 0.124 | 1.250 | 1.892 | 0.248 | 43% |

**Headline:** Under strict MAR (MNAR=0.0), ANCOVA STILL shows 10× treatment effect inflation. The bias is structural — an estimand mismatch, NOT an artifact of informative dropout.

ANCOVA-12mo (survivors only) is even worse: 13× inflation under MAR. Classic collider bias from conditioning on survival.

MNAR makes it slightly worse (10×→12×) but the bulk of bias exists under MAR.

Null scenario: all methods unbiased, Type I error controlled. The bias is specific to treatment effects.

LMM stays robust (coef ~0.11-0.12) across entire MNAR gradient.

### Pre-Registration
Timestamped GitHub commit (75e9221) locking methods, estimands, and analysis plan BEFORE accessing PRO-ACT data. Includes estimand definition per Voss's ICH E9(R1) framing, two-stage LCMM spec per Cipher, all sensitivity analyses.

### PRO-ACT Data
Applied 2026-02-15. Still pending approval. 1-3 day typical wait.

## Open Issues for the Board

1. **K-selection overfitting:** BIC picks K=4 every time (true K=3). Is this a problem? Should we use ICL? Pre-specify K? What are the consequences for inference?

2. **ANCOVA bias — analytical derivation:** We've now shown the bias empirically under MAR. Cipher was tasked with deriving it analytically for a two-class system. Has the formal proof changed given these results?

3. **LCMM-Hard Type I inflation at N=200:** 9.5% under null (should be 5%). Is this a concern? Does LCMM-Soft's conservatism make it the safer choice despite lower power?

4. **Uniform scenario weakness:** LCMM methods hurt when effects are uniform. How do we decide in advance whether to subgroup? Pre-specified decision rule?

5. **PRO-ACT analysis plan:** When data arrives, what's the exact pipeline? Run LCMM first to discover classes, then what?

6. **Publication readiness:** Three simulations complete. Pre-registration locked. What's the minimum viable paper? Can we publish simulation-only as a methods paper while waiting for PRO-ACT?
