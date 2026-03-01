# Replication Assessment: Appel, Pan & Roberts (2023)

## Paper
**"Partisan Conflict over Content Moderation Is More Than Disagreement about Facts"**
- **Authors:** Ruth E. Appel, Jennifer Pan, Margaret E. Roberts
- **Journal:** *Science Advances*, Vol. 9(44), November 2023
- **DOI:** [10.1126/sciadv.adg6799](https://doi.org/10.1126/sciadv.adg6799)
- **Full text (open access):** [PMC10624338](https://pmc.ncbi.nlm.nih.gov/articles/PMC10624338/)

---

## Replication Materials
| Resource | Link |
|----------|------|
| Data & Code | [Harvard Dataverse (DVN/UM4VAJ)](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UM4VAJ) |
| Preregistration | [OSF (osf.io/3q4yn)](https://doi.org/10.17605/osf.io/3q4yn) |
| Supplementary (28 tables, 21 figures) | [PDF (12 MB)](https://pmc.ncbi.nlm.nih.gov/articles/instance/10624338/bin/sciadv.adg6799_sm.pdf) |

---

## Research Design Summary

**Question:** Why do Democrats and Republicans disagree about what social media companies should remove?

**Theory — Three sources of partisan disagreement:**
1. **Fact gap** — Partisans disagree about *what is true* (existing explanation)
2. **Party promotion** — Partisans want to keep misinformation online when it benefits their own side (new)
3. **Preference gap** — Partisans differ in baseline preferences for content removal, regardless of content alignment (new)

**Design:** Survey experiment embedded in a national survey (Ipsos KnowledgePanel, summer 2021).
- **N = 1,120** English-speaking Democrats & Republicans
- Each respondent sees **2 false headlines** (one aligned with their party, one misaligned)
- Headlines explicitly labeled as **"established as false by third-party fact checkers"**
- 18 headline bank (9 pro-Democrat, 9 pro-Republican), sourced from Snopes
- Randomized: headline order, which specific headlines shown

**Three binary outcomes:**
1. Should the headline be **removed** by the social media company?
2. Would the respondent **report** the headline as harmful?
3. Is removal of the headline **censorship**?

---

## Statistical Methods

### Core Analysis: OLS with Interactions (Equation 1)
```
Y_ia = β_D * D_i * H_da + β_R * R_i * H_ra + γ_D * D_i + γ_R * R_i + ε_ia
```
Where:
- `Y_ia` = binary outcome for individual *i*, headline *a*
- `D_i`, `R_i` = Democrat/Republican indicators
- `H_da`, `H_ra` = headline aligned for Democrats/Republicans
- `γ_D - γ_R` = **preference gap** (overall D-R difference controlling for alignment)
- `β_D`, `β_R` = **party promotion** (effect of alignment within each party)

### Additional Analyses
| Method | Purpose |
|--------|---------|
| OLS on "inaccurate" subgroup | Subset to respondents who agree headlines are false — tests if results hold when fact gap is removed |
| Consensus headlines analysis | Restrict to headlines where D & R agree on inaccuracy — further controls for fact gap |
| Causal mediation analysis | Decomposes party promotion into direct effect vs. effect mediated through accuracy beliefs |
| Sensitivity analysis | Tests robustness of mediation to violations of sequential ignorability |
| OLS with controls | Age, gender, education, race, ethnicity, income, political interest, social media news use, prior flagging experience |
| Multiple imputation | Amelia II (R package) for missing data |

### Software
- **R** (confirmed by references to `mediation` R package and `Amelia II`)
- Possibly Stata as well (common in Dataverse packages)

---

## What You Would Replicate

### Tables (from main text + supplementary)
The paper has **28 supplementary tables** plus the main-text Table 1. Key ones:

| Table | Content | Method |
|-------|---------|--------|
| Table 1 | Mediation analysis for party promotion (ACME, ADE) | Causal mediation |
| Tables S12–S14 | Main OLS regressions for removal, harm, censorship outcomes | OLS with interactions |
| Tables S15–S17 | Same with demographic controls | OLS with controls |
| Tables S18–S20 | Inaccurate subgroup regressions | OLS subset |
| Tables S22–S24 | Accuracy order randomization check | OLS |
| Tables S25–S26 | Alternative mediation specifications | Mediation |
| Tables S3–S4 | Balance/descriptive statistics | Summary stats |
| Table S1 | Headline characteristics | Descriptive |

### Figures (from main text + supplementary)
The paper has **3 main figures** and **21 supplementary figures**:

| Figure | Content | Type |
|--------|---------|------|
| Fig 1 (A–F) | Coefficient plots: preference gap & party promotion, all respondents | Coefficient plot with CIs |
| Fig 2 | Accuracy ratings by party × headline alignment | Coefficient plot |
| Fig 3 (A–F) | Coefficient plots: inaccurate subgroup | Coefficient plot with CIs |
| Figs S2–S7 | With control variables versions of main figures | Coefficient plots |
| Figs S8–S13 | Consensus headlines versions | Coefficient plots |
| Fig S1 | Experimental flow diagram | Diagram |
| Figs S14–S21 | Additional robustness checks, sensitivity plots | Various |

---

## Replication Difficulty Assessment

### ✅ Strengths for replication
- **Clean experimental design** — straightforward randomization, clear treatments
- **Standard methods** — OLS with interaction terms is bread-and-butter regression
- **Binary outcomes** — easy to interpret, easy to code
- **Moderate sample** — 1,120 respondents × 2 headlines = ~2,240 observations (manageable)
- **Complete replication package** on Dataverse with code and data
- **Open access** paper (CC BY 4.0)
- **Preregistered** — you can compare what was planned vs. executed
- **Self-contained** — no external APIs, no web scraping, no massive computation
- **Great topic** for a class presentation (content moderation is widely understood)

### ⚠️ Moderate challenges
- **Mediation analysis** requires the `mediation` R package (Tingley et al.) — a bit more advanced than plain OLS, but well-documented
- **Multiple imputation** with Amelia II — adds a step, but you could start with complete-case analysis
- **Clustered standard errors** (observations clustered within respondents) — easy in R (`sandwich`/`lmtest`) or Python (`statsmodels`)
- **Survey weights** — Ipsos provides weights; main results are weighted, robustness checks unweighted
- **28 supplementary tables** — you don't need to replicate *all* of them; focus on the core

### Recommended scope for a class project
**Core replication (minimum):**
1. Descriptive statistics (Table S3–S4)
2. Main OLS regressions — Equation 1 for all 3 outcomes (Tables S12–S14, Figures 1 & 2)
3. Inaccurate subgroup analysis (Tables S18–S20, Figure 3)

**Extended (if time allows):**
4. OLS with controls (Tables S15–S17)
5. Mediation analysis (Table 1)
6. Consensus headlines robustness check

---

## Python Re-implementation Notes

When you do your parallel Python track, here's the mapping:

| R Package/Function | Python Equivalent |
|-------------------|-------------------|
| `lm()` | `statsmodels.formula.api.ols()` |
| `coeftest()` + `vcovCL()` (clustered SEs) | `statsmodels` `.fit(cov_type='cluster', cov_kwds={'groups': ...})` |
| `mediation::mediate()` | No direct equivalent; use `statsmodels` for Baron-Kenny steps, or `pingouin.mediation_analysis()` |
| `Amelia::amelia()` | `sklearn.impute.IterativeImputer` or `miceforest` |
| `ggplot2` (coefficient plots) | `matplotlib` + `seaborn`, or `plotnine` |
| Survey weights in `lm()` | `statsmodels` WLS or `weights` parameter |
| `.dta` file reading | `pandas.read_stata()` |
| `.RData` file reading | `pyreadr.read_r()` |

---

## Key Findings (for reference when checking your replication)

| Outcome | Democrats | Republicans | Gap |
|---------|-----------|-------------|-----|
| Pr(Remove) | 0.69 | 0.34 | 0.35 |
| Pr(Report as harmful) | 0.49 | 0.27 | 0.22 |
| Pr(See removal as censorship) | 0.29 | 0.65 | −0.36 |

- **Preference gap** dominates across all three outcomes
- **Party promotion** observed among Democrats (less willing to remove aligned misinformation) but *not* Republicans
- Results persist when subsetting to respondents who agree headlines are inaccurate
- Mediation analysis: accuracy explains ~62% of Democrats' party promotion for removal, but direct effect remains significant

---

## Verdict

**Highly recommended for your stats class replication.** The paper hits every criterion you specified:

- ✅ Technology × politics intersection (content moderation, misinformation)
- ✅ A few tables (core: ~6 regression tables, plus descriptive stats)
- ✅ A few plots (3 main coefficient plots, each with 6 panels)
- ✅ OLS regressions with interactions as the primary method
- ✅ Clean data + code on Dataverse
- ✅ Manageable scope — you can replicate the core in R/Stata, then re-implement in Python
- ✅ Top-tier journal (*Science Advances*)
- ✅ Timely, engaging topic for presentation
