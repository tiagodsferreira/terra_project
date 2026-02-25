DOI: https://doi.org/10.5281/zenodo.18773549
Version: 1.0
Date: 25 February 2026

# #TERRA.: Paths to Sustainability in Adolescence (COMPETE2030-FEDER-00903800)
Structured research repository for the TERRA project, providing reproducible analysis workflows, documentation, and curated outputs.
---

## Licensing
[![Code License: MIT](https://img.shields.io/badge/Code%20License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Docs License: CC%20BY%204.0](https://img.shields.io/badge/Docs%20License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

- Source code in this repository is licensed under the MIT License.
- Documentation, figures, and other non-code materials are licensed under CC BY 4.0.

See the LICENSE and LICENSE-docs files for details.

## Repository Role

This repository functions as the **canonical, version-controlled research archive** of the TERRA project.

It serves as the authoritative source of truth for:

- Analysis scripts  
- Structured documentation  
- Curated figures and tables  
- Manuscript materials  
- Approved processed datasets (if applicable)

The repository is not intended to mirror the operational OneDrive workspace. Instead, it contains stable, reproducible, and archival-quality materials.

---

## Working Model

- **OneDrive** → Active collaboration, drafts, raw data  
- **GitHub (this repository)** → Curated, reproducible research archive  

Only validated and structured materials are committed to this repository.

---

## Repository Structure
```
terra_project/
├── LICENSE  # MIT
├── LICENSE-docs  # CC BY 4.0 
├── README.md
├── .gitignore
├── docs/ # Management documents
├── data/ # Processed data (raw excluded)
├── scripts/ # Reproducible analysis code
├── results/ # Final figures and tables
├── paper/ # Manuscript materials
├── presentations/ # Conference and workshop slides
└── reports/ # Technical documentation
```


---

## Reproducibility Principles

All analytical outputs included in this repository should:

1. Be generated from version-controlled scripts.
2. Be reproducible from documented inputs.
3. Specify software and package versions where relevant.
4. Avoid manual or undocumented processing steps.

---

## Data Policy

- Raw data is not stored in this repository.
- Sensitive or identifiable information must never be committed.
- Inclusion of processed data requires ethical and institutional approval.

---

## Versioning and Releases

Major analytical milestones (e.g., manuscript submission) should be:

- Tagged as formal releases.
- Linked to the exact scripts that generated submitted results.
- Preserved to ensure traceability.

---

## Licensing

No license has currently been applied.

Until a license is added, all materials are considered **all rights reserved**.

A formal licensing strategy (code, documentation, and potential data) will be defined prior to any public release.

---

## Long-Term Vision

This repository is intended to evolve into:

- A reproducible analytical archive
- A structured scientific record
- A transparent companion to publications
- A compliant research output aligned with funder expectations



