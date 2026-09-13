# West Bengal reservation persistence and follow-up officeholding

Written before estimating the models below. This is an exploratory extension
of quota_raj, not a registered confirmatory study. Source counts, label/ID
contradictions and published 1998 first-stage results were known at planning.

## Estimand and limits

Unit: named Birbhum GP. Population: GPs with explicit 1998 and 2003 reservation
history, no detected 1998 source conflict, consistent current-survey GP IDs,
and a 2006 follow-up interview. Exposure: women-reserved Pradhan office in
1998. Outcome: explicitly recorded female current Pradhan at follow-up.
Contrast: prior-reserved versus prior-unreserved among offices not reserved
for women in 2003 (q03==0), equally weighted. SC/ST reservations may remain;
"open" here means open with respect to the women's quota, not all categories.
This is follow-up officeholding in the 2003 term, not a verified indicator of
the original 2003 election winner. No gender is inferred from names, honorifics,
reservation categories or the spouse's gender.

Use the current-Pradhan questionnaire check A8_13_chk; contradictory A0_5 or
nonmissing A1_2 causes the outcome to remain missing. Retain each raw field.
The merged survey's pradhan_spouse field and officeholder-survival selection
require source review. If the questionnaire cannot establish the respondent
as the current officeholder, describe the result as a respondent-level proxy
and do not present a causal winner effect.

Prior and current assignment may be mechanically related by rotation and
changing caste strata. Conditioning on q03==0 may select GPs differently by
prior treatment. Block fixed effects do not fix that. The estimates therefore
remain descriptive associations; a non-rejection of transition independence
is not evidence of random assignment and will not select an analytic sample.

## Fixed analyses

- Reproduce the 1998 reservation-to-female-officeholder first stage from Part A;
  retain the 161-row archive sample and show its difference from the paper.
- Tabulate 1998-to-2003 reservation transitions for women, SC and ST, separately,
  using explicit-year history. Preserve missing/ambiguous years, not as No.
- Main follow-up model: female_followup ~ q98 among q03==0.
- Adjustment: add block fixed effects and sc98/st98/sc03/st03; same primary
  outcome. Report the unadjusted model on this model's complete-case sample.
- Sensitivities: allow within-1998-term inferred histories; allow the Gonpur
  interview-ID linkage; restrict to exact normalized GP spellings. Report each
  as a different sample, with treated/control counts and support warnings.
- Report the survey's missingness and sex/ID conflicts by reservation group.
- No 10/15-year persistence claim: only one prior-to-current transition is held.

## Inference and diagnostics

One record per independently observed GP: HC2 intervals from
estimatr::lm_robust, with classical/HC1/HC3 SE comparisons. No district-cluster
SE with one district. Report 95% intervals in probability and percentage-point
units and approximate MDE/normal-model Type S/Type M for 5 and 10 percentage
point scenarios, explicitly not a recovered randomization design. For the
small open-office sample, wide intervals do not establish no effect.

Expectation: exposure could increase later female officeholding by 0 to
15 percentage points; this is an exploratory design scenario, not a bound.
No sign restriction, name-based gender classification, or significance-based
sample selection. Retain all tests/variants; label every extension exploratory.

## Pre-estimation review response

The adjusted open-office model has sparse within-block support. It remains
secondary; add block counts, design-matrix leverage, HC3 and leave-one-block-out
checks. Primary reporting uses the unadjusted contrast and interval. Report
ambiguous-history and exclusion counts separately from transition cells.
