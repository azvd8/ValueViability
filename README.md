# Value-Viability Framework: Reproducible Synthetic Illustration

This repository contains the R code and synthetic data generation procedures used to reproduce the illustrative application of the Value-Viability Framework.

The script reproduces:

1. the synthetic BCV panelist responses;
2. the BCV endorsement summaries and validation decision;
3. the synthetic project ratings on validated criteria;
4. the Value and Viability composite scores;
5. the 2x2 median-based Value-Viability map;
6. the 3x3 tertile-based Value-Viability map;
7. vector graphics in PDF and SVG format.

## Repository structure

```text
ValueViability/
├── R/
│   └── value_viability_framework.R
├── data/
│   └── README.md
├── outputs/
│   └── README.md
├── CITATION.cff
├── LICENSE
├── LICENSE-DATA.md
├── CHANGELOG.md
├── .gitignore
└── README.md
```

## Requirements

The code was written in R and uses the following packages:

- tidyverse
- ggrepel
- gridExtra
- svglite

The script checks whether these packages are installed and installs missing packages when needed.

## How to reproduce the results

1. Download or clone this repository.
2. Open the file `R/value_viability_framework.R`.
3. Adjust the working directory in the script, if necessary:

```r
setwd("e:/datasets/")
```

4. Run the full script in R or RStudio.
5. The following files will be generated:

```text
bcv_panel_responses_synthetic.csv
bcv_summary_synthetic.csv
project_classification_results.csv
BCV.pdf
BCV.svg
2x2_matrix.pdf
2x2_matrix.svg
3x3_matrix.pdf
3x3_matrix.svg
```

## Important note on the data

All data used in this repository are synthetic and were created only for methodological illustration. They do not represent real participants, real institutions, or real project evaluations.

## Suggested citation

Please cite the archived Zenodo version associated with the article release:

> TODO: Insert final Zenodo citation after creating the DOI.

## License

Code is distributed under the MIT License. Synthetic data and documentation are distributed under the Creative Commons Attribution 4.0 International License (CC BY 4.0).
