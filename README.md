# Pressure Cooker: early years

BDS Toolbox pressure cooker project for the data department of the school.

> **Privacy note:** the name of the school has been removed from this repository for privacy reasons. The client is referred to as **"the school"** throughout. Please do the same in code, comments, commit messages, file names, plots and slides.

## Client question

Which aspects of early years education should the school prioritise to maximise pupils' development in kindergarten (groups 1–2) and their success in the rest of primary school?

- **Sub-question 1, extended time in kindergarten:** what are the effects of a longer kindergarten period on cognitive, social-emotional and executive development, and on functioning in groups 3 and 4?
- **Sub-question 2, teaching focus aligned with groups 3 and 4:** what are the effects of teaching towards group 3/4 learning goals on achievement and later school pathways?

## Deliverables

| Deliverable | Where | Due |
| --- | --- | --- |
| Proposal (PowerPoint) | outline in `docs/proposal.md`, slides in `presentation/` | 15 Oct 2026 |
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
| 13 Oct 2026 | Kick-off and intake meeting |
| 14 Oct 2026 | Data is clear (cleaned, understood, codebook checked) 
| 15 Oct 2026 | Proposal submitted (see `docs/proposal.md`) |
|  | Modelling approach is clear |
|  | Core analysis finished, final check with supervisor |
|  | Presentation to the school |

## Repository structure

```
data/
  raw/          data from the school (decide is this needs to stays local, or we okay to commit??)
  processed/    cleaned data written by scripts (decide is this needs to stays local, or we okay to commit??)
scripts/        R scripts, numbered in run order
output/
  figures/      plots for the slides
  tables/       aggregated result tables (no pupil-level rows)
docs/           proposal outline, intake notes, meeting notes
presentation/   report and presentation slides
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
