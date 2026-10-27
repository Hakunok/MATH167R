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

## Homework 3: World Map and Personal Places

Uses `sf`, `rnaturalearth`, and `ggplot2` to map estimated GDP per country in
millions of US dollars. The GDP map includes a logarithmic color scale, legend,
title, and explanatory subtitle. GDP estimate years vary by country.

A second map adds three places to visit: Lapland in Finland (represented by
Rovaniemi), Uyuni in Bolivia, and Seljalandsfoss in Iceland. Each location has
a distinct point color and shape, a name label, and a shared dashed connecting
line. The line illustrates the connection between locations.

- [Homework instructions](hw3/HW%203-%20world_map_homework.pdf)
- [R script](hw3/hw_3.R)
- [GDP world map](hw3/hw3_gdp_map.png)
- [GDP map with selected locations](hw3/hw3_places_map.png)
- [AI prompt log](hw3/ai_prompt_log.txt)

From an R session at the repository root, switch to the homework directory
before running the script so both PNG files are saved inside `hw3/`:

```r
setwd("hw3")
source("hw_3.R")
```

Required packages: `sf`, `ggplot2`, `rnaturalearth`, `rnaturalearthdata`, and
`scales`.
