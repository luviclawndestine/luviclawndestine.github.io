# Session 003 Briefing: Assumption Mapping in ALS Clinical Trials

## Context
We've completed our literature review (Session 002) and committed to a two-part deliverable: a Trajectory Atlas (LCMM on PRO-ACT) and a Cost of Linearity simulation study. Our OSF pre-registration is drafted. R analysis pipeline is built. We're waiting on PRO-ACT data access approval.

**This session's purpose:** Before we touch data, map out ALL the foundational assumptions in ALS clinical trial design that could be wrong — not just linearity. We want to know what else we should be looking for when the data arrives.

## The Linearity Assumption (Already Targeted)
We know this one. Trials use linear mixed models on ALSFRS-R slope. Patients don't decline linearly. We're building tools to quantify the cost.

## But What ELSE Is Assumed?

### 1. The Endpoint Itself: ALSFRS-R
- ALSFRS-R is a 12-item ordinal scale summed to 0-48
- Treating ordinal items as interval-scale (summing them) assumes equal spacing
- Floor/ceiling effects in individual items
- De Jongh et al. (2023) built AIMS — a Rasch-calibrated alternative that achieved 16-26% sample size reduction
- Is the total score even meaningful, or should we be looking at domain-level patterns?

### 2. Patient Stratification
- Trials typically stratify by: bulbar vs limb onset, sometimes FVC
- But what if the real stratifier is trajectory shape, not onset site?
- Gomeni (2014) found 2 clusters (slow/fast). Van Eijk (2025) found nonlinear patterns. Are these the same groups?
- What about genetic stratification (C9orf72, SOD1)?

### 3. Time-Zero Definition
- Symptom onset? Diagnosis date? Trial enrollment?
- Diagnostic delay averages 12-14 months — patients enter trials at different points in their disease
- A patient 6 months post-onset vs 24 months post-onset are in fundamentally different stages
- Does time-zero choice create an artifactual "fast progressors" group?

### 4. Homogeneity Within Groups
- Even within "fast" and "slow" progressors — is there meaningful heterogeneity?
- Regional spread patterns (arm → leg vs leg → arm) might matter more than speed
- Cognitive involvement (ALS-FTD spectrum) — usually excluded from trials but affects progression

### 5. Treatment Effect Assumptions
- Trials assume treatment effects are: (a) uniform across patients, (b) constant over time, (c) detectable on the total score
- What if drugs modify trajectory SHAPE rather than slope?
- What if effects only appear in a subpopulation?
- What if effects have a delayed onset (3-6 months)?

### 6. Missing Data / Dropout
- ALS trials have high dropout — mainly death, but also disability-related withdrawal
- Standard assumption: MAR (missing at random). Reality: almost certainly MNAR (faster progressors die/drop out)
- This biases surviving-patient trajectories to look better than reality
- Informative dropout could mask treatment effects OR create false ones

### 7. Trial Population Selection
- PRO-ACT is a trial population, not general ALS
- Inclusion criteria systematically exclude: very fast progressors (dead before enrollment), very slow progressors (don't meet decline thresholds), ALS-FTD (cognitive exclusion), elderly onset
- Does this create a "medium progressor" bias that makes subgroups harder to detect?

## Questions for the Board
1. Which of these assumptions, if wrong, would most change trial outcomes?
2. Which can we actually test with PRO-ACT data?
3. Are there assumptions we're missing entirely?
4. How do these interact? (e.g., time-zero choice + linearity assumption = compounded error?)
5. Should we expand our analysis plan to address more than just linearity?
