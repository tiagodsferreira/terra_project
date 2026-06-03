# 📘 Data Management Plan (DMP) & Contingency Plan — **#TERRA: Paths to Sustainability in Adolescence**

## Authors, Institution
Tiago Ferreira, Center for Psychology at the University of Porto  
Filipa Nunes, Center for Psychology at the University of Porto  
Marília Montenegro, Center for Psychology at the University of Porto  

---

# 1. Repository Purpose

This repository contains the LaTeX source files for the **Data Management Plan (DMP)** and the **Contingency Plan** of the #TERRA project (COMPETE2030-FEDER-00903800), funded by COMPETE 2030 and Portugal 2030 programmes and co-financed by the European Union.

The DMP describes the procedures for:
- Data collection and processing
- Documentation and metadata standards
- Storage and security protocols
- Personal data protection and GDPR compliance
- Data sharing and licensing
- Long-term preservation strategy

The Contingency Plan identifies potential risks to data management activities across the project and each individual study, assesses their likelihood, and describes the mitigation strategies in place to ensure continuity, data integrity, and compliance with GDPR and FAIR principles.

---

# 2. Repository Structure

```
├── main_contingency.tex        # Master LaTeX file (Contingency Plan)
├── contingency_titlepage.tex   # Contingency Plan title page
├── contingency_plan_body.tex   # Contingency Plan content
├── images/                     # Logos used in header/footer
└── README.md                   # This file
```

---

# 3. Compilation Instructions

Both documents are compiled using **XeLaTeX**. To compile locally:

```bash
# Compile the DMP
xelatex main.tex

# Compile the Contingency Plan (run twice for correct table numbering)
xelatex main_contingency.tex
xelatex main_contingency.tex
```

Recommended editors:
- TeXmaker
- Overleaf (with XeLaTeX selected as compiler)

### Additional packages required

The Contingency Plan uses the following packages beyond the standard DMP preamble:

| Package    | Purpose                              |
|------------|--------------------------------------|
| `array`    | Extended column types in tables      |
| `longtable`| Multi-page tables                    |
| `xcolor`   | Colour definitions                   |
| `colortbl` | Coloured table rows and cells        |
| `booktabs` | Professional table rules             |
| `caption`  | Custom table caption formatting      |

All packages are included in standard TeX Live and MiKTeX distributions.

---

# 4. Document Notes

- The `images/` folder must be present and contain all logo files referenced in the headers and footers of both documents.
- The Contingency Plan shares the same page style (headers, footers, fonts) as the DMP for visual consistency.
- Personal data handling and GDPR compliance procedures are described in detail in Section 5 of the DMP.
