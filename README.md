# The Effect of Gender Quotas in Local Bodies on Women's Representation in Rural India

Using a novel dataset of over 67,000 rural local governance bodies (Gram Panchayats) spanning 20 years across Rajasthan and Uttar Pradesh, we find that randomly implemented gender quotas for women do not substantially increase the chances of women winning elections once the seat is unreserved. Even fifteen years of exposure to local women leaders has limited effects.

**Authors**: Varun, K. R. and Gaurav Sood

## Quick Start

```bash
# 1. Clone and setup
git clone https://github.com/soodoku/quota_raj.git
cd quota_raj

# 2. Install R dependencies
R -e "renv::restore()"

# 3. Obtain external data (see Data Dependencies below)

# 4. Run full pipeline
Rscript scripts/99_run_all.R

# 5. Compile manuscript
cd ms && latexmk -xelatex main.tex
```

## Data Dependencies

### Included Data
- **Rajasthan**: GP election data 2005-2020 in `data/raj/source/`

### External Data (must be obtained separately)

#### 1. SHRUG (Development Data Lab)
Source: https://www.devdatalab.org/shrug_download/

Download and place in `data/shrug/`:
- `shrug-pca01-csv` - Population Census Abstracts 2001
- `shrug-pca11-csv` - Population Census Abstracts 2011
- `shrug-vd01-csv` - Village Directory 2001
- `shrug-vd11-csv` - Village Directory 2011
- `shrug-ec05-csv` - Economic Census 2005
- `shrug-ec13-csv` - Economic Census 2013
- `shrug-secc-mord-rural-csv` - SECC Rural
- `shrug-pc-keys-csv` - PC Keys for linking

#### 2. SHRUG-LGD Crosswalk
Source: https://www.devdatalab.org/shrug_download/ (User Contributed Modules section)

Download "SHRUG GP to shrids" by Pratik Mahajan, extract to `data/shrug_gp_xwalk/`

#### 3. LGD (Local Government Directory)
Source: https://lgdirectory.gov.in/

Download for Rajasthan and UP, place in `data/lgd/`:
- Village-GP mapping files
- Block/Panchayat Samiti files

#### 4. UP Election Data
Source: [in-rolls/local_elections_up](https://github.com/in-rolls/local_elections_up)

## Code Organization

```
scripts/
├── 00_*.R          # Configuration and utilities
├── 01_*.R          # Data extraction, standardization, crosswalks
├── 02_*.R          # Panel creation and recoding
├── 03_*.R          # SHRUG linkage at block level
├── 04_*.R          # Balance tests
├── 05_*.R          # Transition matrices
├── 07_*.R          # Short-term effects
├── 08_*.R          # Long-term effects
├── 10-11_*.R       # Candidate analysis
├── 12_*.R          # Phone survey tables
├── 13_*.R          # Weaver replication
├── 14_*.R          # Power analysis
└── 99_run_all.R    # Master pipeline
```

## Notes on Analysis

### Random Rotation Subsample (08b)

The random rotation analysis restricts to districts where chi-square tests fail to reject independence of quota assignment across transitions (p > 0.05).

**10-year analysis (2005→2015)**: Requires independence for 05→10 AND 10→15
- Rajasthan: 9/32 districts (592 open seats)
- UP: 10/44 districts (1,772 open seats)
- Results reported in `tabs/long_term_random_rotation.tex`

**15-year analysis (2005→2020/21)**: Requires independence for ALL THREE transitions
- Rajasthan: 3/32 districts (208 open seats in Churu, Jaisalmer, Sikar)
- UP: 0/44 districts pass

We do not report 15-year random rotation results because the sample is too small. With only 3 districts and 208 observations, the 7-coefficient interaction model cannot be reliably estimated, and any findings would likely reflect district-specific patterns rather than generalizable effects.

## Outputs

**Tables** (`tabs/`): Balance tests, treatment effects, candidate characteristics (~25 LaTeX tables)

**Figures** (`figs/`): Coefficient plots, treatment persistence, maps

**Manuscript** (`ms/main.pdf`)

## Requirements

- R 4.5+
- XeLaTeX (for manuscript compilation)

## Related Repositories

- [in-rolls/local_elections_up](https://github.com/in-rolls/local_elections_up) - UP Local Election Data
- [in-rolls/quota](https://github.com/in-rolls/quota) - Effects of Reservations on Allocation and Development Outcomes
- [in-rolls/local_elections_kerala](https://github.com/in-rolls/local_elections_kerala) - Kerala Local Government Data
