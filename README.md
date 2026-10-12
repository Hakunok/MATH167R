# MATH167R
Data analysis projects from R Programming

## Project 1: Monte Carlo Investigation of the Central Limit Theorem

Uses reproducible Monte Carlo simulations in R to investigate how the sampling
distribution of the mean changes with sample size for five populations: standard
normal, six-sided die, exponential, small binomial, and Cauchy.

The analysis compares simulated means and standard errors with theory, examines
histograms and Q-Q plots, and uses skewness and excess kurtosis to assess normal
approximations. It explores why there is no universal sample-size cutoff and why
the ordinary CLT does not apply to Cauchy data.

- [R analysis](Project1/project1.R)
- [Quarto report source](Project1/project1_report.qmd)
- [Rendered HTML report](Project1/project1_report.html)
- [Results, plots, and executive summary](Project1/results/)
- [AI assistance log](Project1/ai_prompt_log.txt)

Run the analysis from the repository root in R:

```r
source("Project1/project1.R")
```

Render the report from the repository root in a terminal:

```sh
quarto render Project1/project1_report.qmd
```
