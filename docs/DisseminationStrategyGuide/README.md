# 📢 Dissemination Strategy Guide — **#TERRA: Paths to Sustainability in Adolescence**

## Authors, Institution
Tiago Ferreira, Center for Psychology at the University of Porto  
Filipa Nunes, Center for Psychology at the University of Porto  
Marília Montenegro, Center for Psychology at the University of Porto  

---

# 1. Repository Purpose

This repository contains the LaTeX source files for the **Dissemination Strategy Guide (DSG)** of the #TERRA project (COMPETE2030-FEDER-00903800), funded by COMPETE 2030 and Portugal 2030 programmes and co-financed by the European Union.

The DSG describes the communication and dissemination approach for the project, covering:
- Project visual identity and logo usage guidelines
- Funding organisation acknowledgement requirements
- Internal communication mechanisms
- External communication channels (website, social media, OSF, scientific events)
- Planned dissemination activities and timelines
- Monitoring indicators for dissemination effectiveness

---

# 2. Repository Structure

```
├── main_dissemination.tex        # Master LaTeX file (Dissemination Strategy Guide)
├── dissemination_titlepage.tex   # DSG title page
├── dissemination_body.tex        # DSG main content
├── images/                       # Logos, screenshots, and figures used in the document
│   ├── Prancheta 2.png           # Project logo – gradient/green version
│   ├── Prancheta 3.png           # Project logo – black-and-white version
│   ├── Prancheta 6.png           # Project logo – gradient with tagline
│   ├── Prancheta 7.png           # Project logo – black-and-white with tagline
│   ├── emailfooter.png           # Standard project email footer
│   ├── websitefooter.png         # Project website footer
│   └── website.png               # Screenshot of the project website homepage
└── README.md                     # This file
```

---

# 3. Compilation Instructions

The document is compiled using **XeLaTeX**. To compile locally:

```bash
# Compile the Dissemination Strategy Guide (run twice for correct figure numbering)
xelatex main_dissemination.tex
xelatex main_dissemination.tex
```

Recommended editors:
- TeXmaker
- Overleaf (with XeLaTeX selected as compiler)

### Packages required

| Package    | Purpose                                        |
|------------|------------------------------------------------|
| `fontspec` | Custom font loading (XeLaTeX)                  |
| `geometry` | Page margins                                   |
| `hyperref` | Hyperlinks and clickable URLs                  |
| `xurl`     | Line-breaking of long URLs within margins      |
| `enumitem` | Custom list formatting                         |
| `parskip`  | Paragraph spacing                              |
| `array`    | Extended column types in tables                |
| `ragged2e` | Ragged-right alignment in table columns        |
| `caption`  | Figure caption formatting                      |
| `graphicx` | Image inclusion                                |
| `fancyhdr` | Custom headers and footers                     |
| `lastpage` | Total page count in footer                     |

All packages are included in standard TeX Live and MiKTeX distributions.

---

# 4. Document Notes

- The `images/` folder must be present and contain all files listed in Section 2 above. The document will fail to compile if any image file is missing.
- The DSG shares the same page style (headers, footers, fonts, and funder logos) as the DMP and Contingency Plan for visual consistency across all project documents.
- The four logo variants (Figures 1a–d) are displayed side by side in a single `figure` environment at 22% text width each.
- The `\graphicspath{{images/}}` declaration in `main_dissemination.tex` means all image filenames in the body should be given without a path prefix.
- Long URLs (e.g., the project website) use `\href{<full URL>}{<short label>}` combined with the `xurl` package to prevent overflow beyond the right margin.
