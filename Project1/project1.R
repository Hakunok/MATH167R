# CLT Project 1: a reproducible Monte Carlo investigation
# Run source("Project1/project1.R") from the repository root, or source("project1.R")
# from Project1. Rscript Project1/project1.R also works. Only base R is needed.
# Results and PNG plots are written to Project1/results.

# 1. Settings ---------------------------------------------------------------
# Seed based on the last four digits supplied by the student.
student_seed <- 5100L
sample_size_n <- c(1L, 2L, 5L, 10L, 20L, 30L, 50L, 100L, 200L, 500L) # 10 sample sizes
number_of_simulations <- 10000L 
population_plot_size <- 10000L
batch_size <- 1000L
plot_sample_sizes <- c(1L, 10L, 30L, 100L, 500L)

# These are transparent project choices, not universal definitions of normality.
skewness_limit <- 0.20
excess_kurtosis_limit <- 0.50
se_relative_error_limit <- 0.05

stopifnot(length(student_seed) == 1L, is.finite(student_seed),
          student_seed >= 0, student_seed <= 9999,
          student_seed == floor(student_seed),
          all(sample_size_n > 0), !anyDuplicated(sample_size_n),
          number_of_simulations > 1, batch_size > 0)

find_project_directory <- function() {
  # Locate this script when sourced, including from the Quarto report.
  # sys.frames() returns variables belonging to the called functions
  # one of which contains the path to the file, in this case Project1/project1.R, using the frame$ofile, and returns NULL for anything that doesn't fit. 
  # lapply() then compiles these variables into a list; then unlist() drops all NULL values, and keep the file path.
  source_files <- unlist(lapply(sys.frames(), function(frame) frame$ofile))         
  command_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  candidates <- c(source_files, command_file, "Project1/project1.R", "project1.R")
  candidates <- candidates[basename(candidates) == "project1.R"]
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates)) stop("Cannot locate project1.R; run from the repository or Project1.")
  dirname(normalizePath(candidates[1L], winslash = "/"))
}

project_directory <- find_project_directory()
output_directory <- file.path(project_directory, "results")
dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)
# Specify the RNG algorithms so the seed's meaning is recorded explicitly.
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

# 2. Population definitions and predictions ---------------------------------
# For Binomial Small, size = 5 describes ONE observation; sample n describes
# how many independent observations are averaged in one simulated sample.
populations <- list(
  normal = list(name = "Standard Normal", draw = function(k) rnorm(k),
                mu = 0, sigma = 1, skewness = 0, excess_kurtosis = 0,
                prediction = "Sample means are exactly normal at every n."),
  die = list(name = "Six-Sided Die", draw = function(k) sample.int(6L, k, replace = TRUE),
             mu = 3.5, sigma = sqrt(35 / 12), skewness = 0,
             excess_kurtosis = -222 / 175,
             prediction = "Symmetric but discrete; the normal approximation improves with n."),
  exponential = list(name = "Exponential", draw = function(k) rexp(k, rate = 1),
                     mu = 1, sigma = 1, skewness = 2, excess_kurtosis = 6,
                     prediction = "Right skew decreases slowly; mean skewness is 2/sqrt(n)."),
  binomial = list(name = "Binomial Small", draw = function(k) rbinom(k, size = 5, prob = 0.10),
                  mu = 0.5, sigma = sqrt(0.45), skewness = 0.8 / sqrt(0.45),
                  excess_kurtosis = (1 - 6 * 0.1 * 0.9) / 0.45,
                  prediction = "Initially skewed and discrete; both effects decrease with n."),
  cauchy = list(name = "Cauchy", draw = function(k) rcauchy(k, location = 0, scale = 1),
                mu = NA_real_, sigma = NA_real_, skewness = NA_real_,
                excess_kurtosis = NA_real_,
                prediction = "Sample means remain standard Cauchy; the ordinary CLT fails.")
)

population_parameters <- do.call(rbind, lapply(populations, function(population) {
  data.frame(distribution = population$name, theoretical_mean = population$mu,
             theoretical_sd = population$sigma,
             population_skewness = population$skewness,
             population_excess_kurtosis = population$excess_kurtosis,
             prediction = population$prediction, row.names = NULL)
}))

# 3. Reusable simulation and numerical summaries -----------------------------
simulate_sample_means <- function(draw, n, repetitions, batch_size = 1000L) {
  # Each row is a fresh independent sample of n observations.
  # Batching avoids allocating a very large repetitions-by-n matrix.
  sample_means <- numeric(repetitions)
  for (first in seq.int(1L, repetitions, by = batch_size)) {
    last <- min(first + batch_size - 1L, repetitions)
    rows <- last - first + 1L
    observations <- matrix(draw(rows * n), nrow = rows, ncol = n)
    sample_means[first:last] <- rowMeans(observations)
  }
  sample_means
}

sample_shape <- function(x) {
  centered <- x - mean(x)
  second_moment <- mean(centered^2)
  if (second_moment == 0) return(c(skewness = NA_real_, excess_kurtosis = NA_real_))
  # Moment estimates: normal kurtosis is 3, so normal EXCESS kurtosis is 0.
  c(skewness = mean(centered^3) / second_moment^1.5,
    excess_kurtosis = mean(centered^4) / second_moment^2 - 3)
}

summarize_sample_means <- function(x, population, n) {
  finite_variance <- is.finite(population$sigma)
  theoretical_se <- if (finite_variance) population$sigma / sqrt(n) else NA_real_
  empirical_sd <- sd(x)
  shape <- if (finite_variance) sample_shape(x) else
    c(skewness = NA_real_, excess_kurtosis = NA_real_)
  se_ratio <- if (finite_variance) empirical_sd / theoretical_se else NA_real_
  mean_error <- if (finite_variance) mean(x) - population$mu else NA_real_
  # This is uncertainty in our Monte Carlo estimate of E[Xbar], not SE(Xbar).
  mc_se_of_mean <- if (finite_variance) theoretical_se / sqrt(length(x)) else NA_real_
  normal_coverage <- if (finite_variance)
    mean(abs((x - population$mu) / theoretical_se) <= qnorm(0.975)) else NA_real_
  shape_pass <- finite_variance && abs(shape["skewness"]) <= skewness_limit &&
    abs(shape["excess_kurtosis"]) <= excess_kurtosis_limit
  se_pass <- finite_variance && abs(se_ratio - 1) <= se_relative_error_limit
  data.frame(
    distribution = population$name, n = n, repetitions = length(x),
    theoretical_mean = population$mu, theoretical_sd = population$sigma,
    simulated_mean = mean(x), empirical_sd = empirical_sd,
    theoretical_se = theoretical_se, se_ratio = se_ratio,
    mean_error = mean_error, mc_se_of_mean = mc_se_of_mean,
    mean_error_in_mc_se = mean_error / mc_se_of_mean,
    skewness = unname(shape["skewness"]),
    excess_kurtosis = unname(shape["excess_kurtosis"]),
    theoretical_skewness = population$skewness / sqrt(n),
    theoretical_excess_kurtosis = population$excess_kurtosis / n,
    median = median(x), iqr = IQR(x),
    q025 = unname(quantile(x, 0.025)), q975 = unname(quantile(x, 0.975)),
    normal_95_coverage = normal_coverage,
    normal_95_coverage_mc_se = if (finite_variance)
      sqrt(normal_coverage * (1 - normal_coverage) / length(x)) else NA_real_,
    shape_screen_pass = shape_pass, se_check_pass = se_pass,
    numeric_screen_pass = shape_pass && se_pass, row.names = NULL
  )
}

simulation_means <- list()
population_samples <- list()
summary_rows <- list()
for (population_index in seq_along(populations)) {
  key <- names(populations)[population_index]
  population <- populations[[key]]
  # Each population and n has its own reproducible stream. Changing the grid
  # or drawing plotting data does not change results for existing scenarios.
  set.seed(student_seed + 100000L + population_index * 1000L)
  population_samples[[key]] <- population$draw(population_plot_size)
  simulation_means[[key]] <- list()
  for (n in sort(sample_size_n)) {
    set.seed(student_seed + population_index * 1000L + n)
    x <- simulate_sample_means(population$draw, n, number_of_simulations, batch_size)
    stopifnot(length(x) == number_of_simulations, all(is.finite(x)))
    simulation_means[[key]][[as.character(n)]] <- x
    summary_rows[[length(summary_rows) + 1L]] <- summarize_sample_means(x, population, n)
  }
}
simulation_summary <- do.call(rbind, summary_rows)
rownames(simulation_summary) <- NULL

# 4. Final comparison table --------------------------------------------------
final_summary <- do.call(rbind, lapply(populations, function(population) {
  rows <- simulation_summary[simulation_summary$distribution == population$name, ]
  passing <- rows$n[rows$numeric_screen_pass]
  # Also report the first tested n for which all larger tested sizes pass.
  # Neither value establishes an exact minimum, and plots still need review.
  sustained <- rows$n[vapply(seq_len(nrow(rows)), function(i) {
    all(rows$numeric_screen_pass[i:nrow(rows)])
  }, logical(1))]
  at_30 <- rows[rows$n == 30L, ]
  data.frame(
    distribution = population$name, theoretical_mean = population$mu,
    theoretical_sd = population$sigma,
    first_tested_n_passing_screen = if (length(passing)) min(passing) else NA_integer_,
    first_tested_n_all_larger_pass = if (length(sustained)) min(sustained) else NA_integer_,
    n30_screen_pass = if (nrow(at_30) && is.finite(population$sigma))
      at_30$numeric_screen_pass else NA,
    n30_skewness = if (nrow(at_30)) at_30$skewness else NA_real_,
    n30_excess_kurtosis = if (nrow(at_30)) at_30$excess_kurtosis else NA_real_,
    n30_se_ratio = if (nrow(at_30)) at_30$se_ratio else NA_real_,
    n30_normal_95_coverage = if (nrow(at_30)) at_30$normal_95_coverage else NA_real_,
    interpretation = population$prediction, row.names = NULL
  )
}))
rownames(final_summary) <- NULL

# 5. Plots ------------------------------------------------------------------
save_png <- function(filename, draw_plot, width = 1800, height = 1000) {
  png(file.path(output_directory, filename), width = width, height = height, res = 150)
  on.exit(dev.off(), add = TRUE)
  draw_plot()
}

plot_cauchy_histogram <- function(x, title) {
  # Show [-10, 10] but keep density normalized by ALL observations.
  # No observations are removed from the simulation or numerical summaries.
  inside <- x >= -10 & x <= 10
  histogram <- hist(x[inside], breaks = seq(-10, 10, by = 0.5), plot = FALSE)
  histogram$density <- histogram$counts / (length(x) * diff(histogram$breaks))
  plot(histogram, freq = FALSE, main = "", xlab = "Value (display: -10 to 10)",
       col = "#D7E8F5", border = "white", ylim = c(0, max(0.35, histogram$density)))
  title(main = title, line = 2.5, cex.main = 0.9)
  curve(dcauchy(x), from = -10, to = 10, add = TRUE, col = "#C44536", lwd = 2)
  mtext(sprintf("%.1f%% outside window; red = Cauchy density", 100 * mean(!inside)),
        side = 3, line = 0.6, cex = 0.6)
}

save_png("population_distributions.png", function() {
  par(mfrow = c(2, 3), mar = c(4, 4, 4, 1))
  for (key in names(populations)) {
    population <- populations[[key]]
    x <- population_samples[[key]]
    if (key == "cauchy") {
      plot_cauchy_histogram(x, population$name)
    } else {
      breaks <- if (key == "die") seq(0.5, 6.5, by = 1) else
        if (key == "binomial") seq(-0.5, 5.5, by = 1) else 40
      hist(x, breaks = breaks, probability = TRUE, main = population$name,
           xlab = "One population observation", col = "#D7E8F5", border = "white")
      abline(v = population$mu, col = "#C44536", lwd = 2)
    }
  }
  plot.new()
  text(0.5, 0.65, "Population shapes before averaging", cex = 1.1)
  text(0.5, 0.45, "Red lines: theoretical means\nCauchy: theoretical density", cex = 0.9)
})

plot_sample_sizes <- intersect(plot_sample_sizes, sample_size_n)
for (key in names(populations)) {
  population <- populations[[key]]
  # Show the first passing size and its predecessor so the claimed crossing
  # can be checked visually, alongside n = 1, n = 30, and the largest size.
  first_pass <- final_summary$first_tested_n_passing_screen[
    final_summary$distribution == population$name]
  predecessor <- if (is.finite(first_pass) && any(sample_size_n < first_pass))
    max(sample_size_n[sample_size_n < first_pass]) else integer(0)
  shown_sizes <- intersect(c(1L, 30L, max(sample_size_n), first_pass, predecessor), sample_size_n)
  shown_sizes <- sort(head(unique(c(shown_sizes, plot_sample_sizes)), 5L))
  save_png(paste0("sampling_", key, ".png"), function() {
    par(mfrow = c(2, length(shown_sizes)), mar = c(4, 4, 4, 1), oma = c(0, 0, 2, 0))
    for (n in shown_sizes) {
      x <- simulation_means[[key]][[as.character(n)]]
      if (key == "cauchy") {
        plot_cauchy_histogram(x, paste("n =", n))
      } else {
        hist(x, breaks = 40, probability = TRUE, main = paste("n =", n),
             xlab = "Sample mean", col = "#D7E8F5", border = "white")
        curve(dnorm(x, mean = population$mu, sd = population$sigma / sqrt(n)),
              add = TRUE, col = "#C44536", lwd = 2)
      }
    }
    for (n in shown_sizes) {
      x <- simulation_means[[key]][[as.character(n)]]
      if (key == "cauchy") {
        # Use all values to compute Q-Q positions; only crop the display.
        qqnorm(x, main = "", pch = 16, cex = 0.3,
               ylim = quantile(x, c(0.01, 0.99)))
        title(main = paste("Normal Q-Q: n =", n), line = 2.5, cex.main = 0.9)
        mtext("Display: empirical 1st-99th percentiles", side = 3, line = 0.6, cex = 0.6)
      } else {
        qqnorm(x, main = paste("Normal Q-Q: n =", n), pch = 16, cex = 0.3)
      }
      qqline(x, col = "#C44536", lwd = 2)
    }
    mtext(paste(population$name, "- sampling distributions and normal Q-Q plots"),
          outer = TRUE, cex = 1.1)
  }, width = 2400, height = 1100)
}

finite_keys <- names(populations)[names(populations) != "cauchy"]
plot_colors <- c("#2463A0", "#25855A", "#C44536", "#8055A5")
save_png("convergence_diagnostics.png", function() {
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  metrics <- c("skewness", "excess_kurtosis", "se_ratio", "normal_95_coverage")
  labels <- c("Skewness", "Excess kurtosis", "Empirical SD / theoretical SE",
              "Probability within normal 95% limits")
  for (j in seq_along(metrics)) {
    values <- simulation_summary[simulation_summary$distribution != "Cauchy", metrics[j]]
    reference <- switch(metrics[j], skewness = 0, excess_kurtosis = 0,
                        se_ratio = 1, normal_95_coverage = 0.95)
    bounds <- switch(metrics[j], skewness = c(-skewness_limit, skewness_limit),
                     excess_kurtosis = c(-excess_kurtosis_limit, excess_kurtosis_limit),
                     se_ratio = c(1 - se_relative_error_limit, 1 + se_relative_error_limit),
                     normal_95_coverage = 0.95)
    plot(range(sample_size_n), range(c(values, bounds)), type = "n", log = "x",
         xlab = "Sample size n (log scale)", ylab = labels[j], main = labels[j])
    abline(h = reference, lty = 2, col = "grey40")
    if (j <= 3) abline(h = bounds, lty = 3, col = "grey65")
    for (i in seq_along(finite_keys)) {
      rows <- simulation_summary[
        simulation_summary$distribution == populations[[finite_keys[i]]]$name, ]
      lines(rows$n, rows[[metrics[j]]], type = "b", pch = i, col = plot_colors[i])
      if (j <= 2) {
        theory <- if (j == 1) rows$theoretical_skewness else rows$theoretical_excess_kurtosis
        lines(rows$n, theory, col = plot_colors[i], lty = 2)
      }
    }
    if (j == 1) legend("topright", legend = population_parameters$distribution[1:4],
                       col = plot_colors, pch = 1:4, lty = 1, bty = "n", cex = 0.7)
  }
}, width = 1800, height = 1400)

save_png("cauchy_stability.png", function() {
  rows <- simulation_summary[simulation_summary$distribution == "Cauchy", ]
  par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  plot(rows$n, rows$median, type = "b", log = "x", pch = 16, col = "#2463A0",
       xlab = "Sample size n (log scale)", ylab = "Median of sample means",
       main = "Location remains near zero", ylim = range(c(rows$median, 0)))
  abline(h = 0, col = "#C44536", lty = 2)
  plot(rows$n, rows$iqr, type = "b", log = "x", pch = 16, col = "#2463A0",
       xlab = "Sample size n (log scale)", ylab = "IQR of sample means",
       main = "Spread does not shrink", ylim = range(c(rows$iqr, 2)))
  abline(h = 2, col = "#C44536", lty = 2)
})

# 6. Save reproducible deliverables ------------------------------------------
write.csv(population_parameters, file.path(output_directory, "population_parameters.csv"), row.names = FALSE)
write.csv(simulation_summary, file.path(output_directory, "simulation_summary.csv"), row.names = FALSE)
write.csv(final_summary, file.path(output_directory, "final_summary.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(output_directory, "session_info.txt"))
writeLines(c(
  paste("Seed:", student_seed),
  paste("Sample sizes:", paste(sample_size_n, collapse = ", ")),
  paste("Independent samples per scenario:", number_of_simulations),
  paste("Batch size:", batch_size),
  paste("Shape screen: absolute skewness <=", skewness_limit,
        "and absolute excess kurtosis <=", excess_kurtosis_limit),
  paste("SE check: absolute empirical/theoretical SE ratio error <=", se_relative_error_limit),
  "Numeric screening is provisional: inspect histograms and Q-Q plots before drawing conclusions.",
  "Cauchy sample means and SDs are descriptive only; population mean and variance are undefined.",
  "Cauchy skewness, kurtosis, theoretical SE and normal coverage are intentionally NA.",
  "Cauchy plot limits crop the display only; all generated means remain in the summaries.",
  "RNG algorithms: Mersenne-Twister / Inversion / Rejection."
), file.path(output_directory, "analysis_settings.txt"))

screen_descriptions <- vapply(seq_len(nrow(final_summary)), function(i) {
  row <- final_summary[i, ]
  first <- if (is.na(row$first_tested_n_passing_screen)) "none" else
    as.character(row$first_tested_n_passing_screen)
  at30 <- if (row$distribution == "Cauchy") "inapplicable (undefined variance)" else
    if (isTRUE(row$n30_screen_pass)) "passes" else "does not pass"
  paste0(row$distribution, ": first tested n passing numeric screen = ", first,
         "; n = 30 ", at30, ". ", row$interpretation)
}, character(1))
executive_summary <- c(
  "CLT Project 1 - Executive Summary",
  sprintf("Simulated %s independent samples at each of %s sizes for five populations (seed %s).",
          format(number_of_simulations, big.mark = ","), length(sample_size_n), student_seed),
  "Predictions were specified before simulation; results are compared with exact population moments.",
  "Numeric screening combines small skewness, small excess kurtosis and agreement with theoretical SE.",
  screen_descriptions,
  "There is no universal n = 30 rule. The needed size depends on population shape and the chosen accuracy criteria.",
  "For independent finite-variance populations, the sampling SD follows sigma/sqrt(n).",
  "Cauchy sample means remain Cauchy, with theoretical median 0 and IQR 2, at every n.",
  "First passing sizes are provisional tested-grid results: confirm with histograms and Q-Q plots.",
  "Monte Carlo error can change threshold crossings; the grid does not identify an exact minimum.",
  "Normal-window coverage is a separate diagnostic; one central probability cannot establish normality.",
  "Scope: the five required populations. Optional dependent, non-identical and mixture cases are not included."
)
writeLines(executive_summary, file.path(output_directory, "executive_summary.txt"))
cat(paste(executive_summary, collapse = "\n"), "\n\n")
print(final_summary[, c("distribution", "first_tested_n_passing_screen", "n30_screen_pass",
                        "n30_skewness", "n30_se_ratio")], row.names = FALSE)
cat("\nResults saved to:", output_directory, "\n")
