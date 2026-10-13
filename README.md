# Pressure Cooker: Het Epos early years

BDS Toolbox pressure cooker project for the Data & Knowledge department of Het Epos primary school (Rotterdam-Zuid).

## Client question

Which aspects of early years education should Het Epos prioritise to maximise pupils' development in kindergarten (groups 1–2) and their success in the rest of primary school?

- **Sub-question 1, extended time in kindergarten:** what are the effects of a longer kindergarten period on cognitive, social-emotional and executive development, and on functioning in groups 3 and 4?
- **Sub-question 2, teaching focus aligned with groups 3 and 4:** what are the effects of teaching towards group 3/4 learning goals on achievement and later school pathways?

## Deliverables

| Deliverable | Where | Due |
| --- | --- | --- |
| Report (PowerPoint) | `presentation/` |  |
| Presentation | `presentation/` |  |
| R code | `scripts/` |  |

## Team

| Name | GitHub | Focus |
| --- | --- | --- |
| Natasha |  |  |
|  |  |  |
|  |  |  |
|  |  |  |

## Planning (work backwards from the presentation)

| When | Checkpoint |
| --- | --- |
|  | Data is clear (cleaned, understood, codebook checked) |
|  | Modelling approach is clear |
|  | Core analysis finished, final check with supervisor |
|  | Presentation to Het Epos |

## Repository structure

```
data/
  raw/          data from Het Epos (stays local, never committed)
  processed/    cleaned data written by scripts (stays local)
scripts/        R scripts, numbered in run order
output/
  figures/      plots for the slides
  tables/       aggregated result tables (no pupil-level rows)
docs/           intake notes, meeting notes, client brief
presentation/   report and presentation slides
```

## Getting started

1. Clone the repo: `git clone <repo-url>`
2. Open `pressure-cooker.Rproj` in RStudio.
3. Get the data from the agreed shared location and put it in `data/raw/`.
4. Run the scripts in `scripts/` in numbered order.

## Data privacy

The data is about young pupils, so it never goes on GitHub, even in a private repo. `.gitignore` blocks `data/raw/`, `data/processed/` and common data files (`.csv`, `.xlsx`, `.sav`). Only aggregated tables in `output/tables/` may be committed. Before every commit, check `git status` for anything that looks like pupil data.

- Data location:
- Codebook / data definition:

## How we work

- Pull before you start: `git pull`
- One branch per task: `git checkout -b clean-groep3-scores`
- Small commits with clear messages: `git commit -m "Recode birth month into autumn/other"`
- Push, open a pull request, and let one teammate review before merging into `main`.
- Keep `main` working: scripts there should run start to finish.
- Plan B: get a simple complete analysis working first, then try anything more complex.
