# Pressure Cooker: Building Analytics for 'the school'

BDS Toolbox pressure cooker project for the data department of the school.

> **Privacy note:** the name of the school has been removed from this repository for privacy reasons. The client is referred to as **"the school"** throughout. Please do the same in code, comments, commit messages, file names, plots and slides.

## Client question

Which aspects of early years education should the school prioritise to maximise pupils' development in kindergarten (groups 1–2) and their success in the rest of primary school?

- **Sub-question 1, extended time in kindergarten:** what are the effects of a longer kindergarten period on cognitive, social-emotional and executive development, and on functioning in groups 3 and 4?
- **Sub-question 2, teaching focus aligned with groups 3 and 4:** what are the effects of teaching towards group 3/4 learning goals on achievement and later school pathways?

## Deliverables

| Deliverable | Format | Where | Due |
| --- | --- | --- | --- |
| Proposal | PowerPoint | `proposal/` (outline in `proposal-outline.md`) | 15 Oct 2026 @ 23:59 |
| Presentation | Short PowerPoint for the talk: the report with >50% removed | `presentation/` | 20 Oct 2026 @ 12:00 |
| Report | PowerPoint (from `templates/report-template.potx`), exported to PDF; readable without a presenter | `report/` | 20 Oct 2026 @ 23:59 |
| R code | Readable, commented `.R` or `.Rmd` | `scripts/` | 20 Oct 2026 @ 23:59 |

## Team (group 1)

* Matheus Alves Cunha Nacif
* Robin Lux
* Natasha Medved 
* Alice Školoudíková

## Planning

| When | Checkpoint | Owner |
| --- | --- | --- |
| 13 Oct 2026 | Kick-off and intake meeting | |
| 14 Oct 2026 | Data is clear (cleaned, understood, codebook checked) | Alice |
| 15 Oct 2026 | Modelling approach is clear | |
| **15 Oct 2026** | Proposal submitted (see `/proposal.md`) | |
|  | Core analysis finished | |
| **20 Oct 2026** | Submit slides for pitch | |
| **20 Oct 2026** | Submit all deliverables on Canvas | |

## Repository structure

```
pressure-cooker.Rproj
proposal/        proposal slides
report/          report (PowerPoint) and exported PDF
presentation/    short deck for the pitch
scripts/         pressure_cooker_analysis.R (the full analysis, one script)
docs/            intake notes, meeting notes
data/
  raw/           data from the school (NOT on GitHub, see Data privacy)
  processed/     cleaned data written by the script (NOT on GitHub)
output/          tables and plots written by the script (NOT on GitHub)
  table_1/       time in kindergarten by birth period
  table_2/       transition goals achieved at the start of group 3
  question_1/    staying longer in kindergarten vs Cito and extra support
  question_2/    kindergarten skills (transition goals) vs Cito
```

`data/processed/` and `output/` are created by the script, so they don't need to exist beforehand.

## Getting started

**Requirements:** R 4.1 or newer and RStudio. Install the packages once:

```r
install.packages(c("tidyverse", "readxl", "here", "writexl",
                   "broom", "sandwich", "lmtest"))
```

(tidyverse 2.0+ / dplyr 1.1+ needed.)

1. Clone the repo: `git clone https://github.com/NatashaMedved/pressure-cooker-2026.git`
2. Open `pressure-cooker.Rproj` in RStudio. The file paths only work from the project (they use `here()`).
3. Get the data from the agreed shared location and put these files in `data/raw/`, with exactly these names:

   | File | Used for |
   |---|---|
   | `Pupils.xlsx` | pupil list, birth and start dates |
   | `Cito_UvA_EN.xlsx` | Cito results (groups 3–4) |
   | `Support interventions_care team.xlsx` | extra support interventions |
   | `pupil questionnaire.xlsx` | well-being and engagement in group 3 |
   | `Selected learning goals.xlsx` | the 34 transition goals |
   | `Observaties_groep_1_2.xlsx` | teacher observations in groups 1–2 |

   The other files from the school (`CLASS.xlsx`, `Extracurricular.xlsx`, `teacher survey.docx`, `Translated_pupil questionnaire.docx`, `UvA_testresults_math.xlsx`) are not used.
4. Restart R (Session > Restart R) and run the whole script `scripts/pressure_cooker_analysis.R`.

### What the script does

| Part | Content | Output |
|---|---|---|
| 0 | Setup: packages, folders, shared settings | |
| 1 | Data cleaning and linking all files | `data/processed/final_dataset.xlsx` (codebook on sheet 2) |
| 2 | Table 1: time in kindergarten by birth period | `output/table_1/` |
| 3 | Table 2: transition goals achieved at the start of group 3 | `output/table_2/` |
| 4 | Question 1: staying longer in kindergarten, Cito and extra support | `output/question_1/` |
| 5 | Question 2: kindergarten skills and Cito | `output/question_2/` |

## Data privacy

- The raw data contains personal pupil data (names, dates of birth, test results, support). It is **never** committed to GitHub; `data/raw/`, `data/processed/` and `output/` are listed in `.gitignore`.
- Share data only via the agreed shared location, never via GitHub, chat or email.
- Before every commit, run `git status` and check that no `.xlsx`, `.csv` or `.docx` files are listed.
- Use no pupil names in code, comments, plots or slides. Report only aggregated results.
- Codebook / data definitions: generated by the script on the `codebook` sheet of `data/processed/final_dataset.xlsx`.

