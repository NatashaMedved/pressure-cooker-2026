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
| **15 Oct 2026** | Proposal submitted (see `docs/proposal.md`) | |
|  | Core analysis finished | |
| **20 Oct 2026** | Submit slides for pitch | |
| **20 Oct 2026** | Submit all deliverables on Canvas | |

## Repository structure

```
proposal/       proposal outline and slides
report/         report (PowerPoint) and exported PDF
presentation/   short deck for the pitch
scripts/        R scripts, numbered in run order
output/
  figures/      plots for the report and presentation
  tables/       aggregated result tables (no pupil-level rows)
data/
  raw/          data from the school
  processed/    cleaned data written by scripts
docs/           intake notes, meeting notes
```

## Getting started

1. Clone the repo: `git clone <repo-url>`
2. Open `pressure-cooker.Rproj` in RStudio.
3. Get the data from the agreed shared location and put it in `data/raw/`.
4. Run the scripts in `scripts/` in numbered order.

## Data privacy

- Codebook / data definition:

## How we work

- Pull before you start: `git pull`
- One branch per task: `git checkout -b clean-groep3-scores`
- Small commits with clear messages: `git commit -m "Recode birth month into autumn/other"`
- Push, open a pull request, and let one teammate review before merging into `main`.
- Keep `main` working: scripts there should run start to finish.
- Plan B: get a simple complete analysis working first, then try anything more complex.
