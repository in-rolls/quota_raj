# Electoral gender quotas in rural India

Research code and manuscript for *The Limits of Electoral Gender Quotas in Rural Local Bodies*, by Varun Karekurve-Ramachandra and Gaurav Sood. The analysis follows local elections in Rajasthan (2005–2020) and Uttar Pradesh (2005–2021).

## Reproduce

Restore the R packages pinned in `renv.lock`, then run from the repository root:

```sh
Rscript -e 'renv::restore()'
Rscript scripts/99_run_all.R
bash ms/compile.sh
```

The runner rebuilds the panels, tables, figures and numerical manuscript inputs, then runs `scripts/98_validate.R`. Validation checks join cardinality, unresolved matches, outcome missingness, phone denominators, independently estimated cumulative contrasts, and seeded bootstrap reproduction. To rerun those checks alone:

```sh
Rscript scripts/98_validate.R
```

R 4.6 and XeLaTeX were used for this revision. `fwildclusterboot` uses its R engine; Julia is not required.

## Data and analysis

Rajasthan source records are in `data/raj/source/`. External UP election files and SHRUG 2.0 inputs are pinned by SHA-256 in `data/manifest.yaml`. UP files resolve from a sibling `local_elections_up` checkout or the versioned cache. Obtain licensed SHRUG inputs from [Development Data Lab](https://www.devdatalab.org/shrug_download/), retaining their attribution files. The default external cache is `~/data`; set `INDIA_DATA_HOME` to use another location. The SHRUG–LGD crosswalk is in `data/shrug_gp_xwalk/` and LGD source files are in `data/lgd/`.

Scripts `01–03` construct election-specific links and analysis panels. Candidate identity and candidate-name uniqueness are separate: different candidates may share a name. Ambiguous links remain unavailable. Primary reservation classifications follow the original GP source; exclusions of conflicts with candidate records are sensitivity analyses. Native UP identifiers survive transliteration.

Village counts are summed. GP distance is the unweighted mean of observed constituent village-to-town distances, with the minimum recorded distance as a sensitivity analysis. A facility is available if observed in any constituent village; absence requires observed absence in every village. Coverage accompanies these aggregates.

Scripts `04–07` produce descriptive comparisons and regressions. Main inference clusters by district–samiti in Rajasthan and district–block in UP, with explicit small-sample adjustments and retained singleton groups. HC1 results are supplied as a sensitivity analysis. Primary fixed-effects coefficients and cumulative contrasts also receive 9,999 null-imposed Rademacher wild cluster bootstrap draws with inverted confidence intervals. `tabs/model_inference.csv` records the sample, contrast, cluster counts, comparison-cell counts, covariance choice and bootstrap seed. The history-support table reports how many GPs and clusters contribute to each history. Restricted cumulative models use the same final election as the full model; passing an independence test is a sample restriction, not proof of random assignment.

Script `08a` uses the completed interview records. The archived sampler `08b` is deliberately outside the runner: rerunning an analysis must not redraw a completed survey. The archived initial selections contain 578 quota-seat and 593 open-seat records; the completed call files contain 500 and 507. The reason for that reduction is not documented in the retained files. Officeholder identity conflicts in seven female-held open-seat calls remain unresolved.

The Weaver appendix uses the supplied source panel, with its final wave labeled 2021 in the exhibits (coded 2020 in the source). Its models use complete 2011 Census district–block identifiers; the original election-specific 2015 block fields are blank throughout the matched short-run sample.

## Versions

`pre-corrections-20260912` preserves the manuscript and code before these revisions; it is not a certification that every earlier exhibit reproduces. Current scripts overwrite the current derived outputs. Git and release tags preserve earlier versions. The final release tag awaits review of the cumulative-effect and mechanism interpretations.
