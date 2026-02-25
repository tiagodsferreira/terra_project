# 📘 Data Management Plan

## Project Title
**#TERRA: Paths to Sustainability in Adolescence**

## Document
Data Management Plan (DMP)

## Institution
Center for Psychology at the University of Porto (CPUP)  
Faculty of Psychology and Education Sciences – University of Porto  

---

# 1. Repository Purpose

This repository contains the LaTeX source files for the Data Management Plan (DMP) of the #TERRA project.

The DMP describes the procedures for:

- Data collection and processing  
- Documentation and metadata standards  
- Storage and security protocols  
- Personal data protection and GDPR compliance  
- Data sharing and licensing  
- Long-term preservation strategy  

This repository ensures transparency, version control, and reproducibility of the project’s data governance framework.

---

# 2. Repository Structure
```

├── main.tex # Master LaTeX file
├── titlepage.tex # Title page (separate file)
├── body.tex # Main DMP content
├── images/ # Logos used in header/footer
├── README.md # This file
```


---

# 3. Compilation Instructions

The document is compiled using **XeLaTeX**.

To compile locally:
```
xelatex main.tex
```


Recommended editors:
- TeXmaker
- Overleaf (with XeLaTeX selected as compiler)

---

# 4. Data Governance Summary

The #TERRA project generates:

- Cross-sectional survey data  
- Longitudinal data (multi-informant)  
- Daily diary data  
- Bibliographic review data  

Key governance principles:

- Compliance with FAIR principles (Findable, Accessible, Interoperable, Reusable)  
- Separation of personal data from research datasets  
- Encrypted institutional storage (University server + NAS)  
- Controlled access by the Data Management Team  
- Public release of fully anonymized processed datasets  
- Persistent identifiers (DOI) assigned upon repository deposit  
- Licensing under CC BY-SA 4.0  

---

# 5. Version Control

This repository uses Git for:

- Tracking modifications to the DMP  
- Maintaining revision transparency  
- Supporting auditability for funding compliance  

Each update should include a concise commit message describing:

- Section modified  
- Nature of revision (clarification, update, compliance adjustment, etc.)

---

# 6. Licensing
Unless otherwise specified, the DMP text in this repository is released under:
**Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)**
Logos and institutional graphics are excluded from reuse unless explicitly permitted by their respective owners.
