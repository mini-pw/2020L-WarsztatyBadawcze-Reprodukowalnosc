

# library ####
library(atable)
library(multgee)
library(survival)

# Set classes of dataset arthritis ####
data(arthritis)

arthritis = within(arthritis, {
  score = ordered(y)
  baselinescore = ordered(baseline)
  time = paste0("Month ", time)
  sex = factor(sex, levels = c(1,2), labels = c("female", "male"))
  trt = factor(trt, levels = c(1,2), labels = c("placebo", "drug"))})


# Apply atable: Demographics of dataset arthritis ####
the_table <- atable::atable(subset(arthritis, time=="Month 1"),
                            target_cols = c("age", "sex", "baselinescore"),
                            group_col = "trt")[1:3]

Hmisc::latex(the_table,
             file = "",
             title = "",
             label = "tab:arthritisDemographics",
             caption = "Demographics of dataset arthritis.",
             caption.lot = "Demographics of dataset arthritis",
             rowname = NULL,
             where = "!htbp",
             booktabs = TRUE,
             multicol = FALSE )


# Apply atable: Hypothesis tests of dataset arthritis ####
the_table <- atable::atable(score ~ trt | time, arthritis)


Hmisc::latex(the_table,
             file = "",
             title = "",
             label = "tab:arthritisHypothesisTests",
             caption = "Hypothesis tests of dataset arthritis.",
             caption.lot = "Hypothesis tests of dataset arthritis",
             rowname = NULL,
             where = "!htbp",
             booktabs = TRUE,
             multicol = FALSE)



# Define replacement functions ####

new_two_sample_htest_numeric <- function(value, group, ...){

  d <- data.frame(value = value, group = group)
  group_levels <- levels(group)
  x <- subset(d, group %in% group_levels[1], select = "value", drop = TRUE)
  y <- subset(d, group %in% group_levels[2], select = "value", drop = TRUE)

  ks_test_out <- stats::ks.test(x, y)
  t_test_out <- stats::t.test(x, y)

  out <- list(p_ks = ks_test_out$p.value,
              p_t = t_test_out$p.value )

  return(out)
}



new_statistics_numeric <- function(x, ...){

  statistics_out <- list(Median = median(x, na.rm = TRUE),
                         MAD = mad(x, na.rm = TRUE),
                         Mean = mean(x, na.rm = TRUE),
                         SD = sd(x, na.rm = TRUE))

  class(statistics_out) <- c("statistics_numeric", class(statistics_out))
  # We will need this new class later to specify the format
  return(statistics_out)
}

new_format_statistics_numeric <- function(x, ...){

  Median_MAD <- paste(round(c(x$Median, x$MAD), digits = 1), collapse = "; ")
  Mean_SD <- paste(round(c(x$Mean, x$SD), digits = 1), collapse = "; ")

  out <- data.frame(
    tag = factor(c("Median; MAD", "Mean; SD"), levels = c("Median; MAD", "Mean; SD")),
    # the factor needs levels for the non-alphabetical order
    value = c(Median_MAD, Mean_SD),
    stringsAsFactors = FALSE)
  return(out)
}

# Replace two_sample_htest.numeric in atable's namespace
utils::assignInNamespace(x = "two_sample_htest.numeric",
                         value = new_two_sample_htest_numeric,
                         ns = "atable")

# set statistics.numeric in atable_options
atable_options('statistics.numeric' = new_statistics_numeric)


# Apply atable with replaced methods ####
the_table <- atable(age ~ trt, arthritis,
                    format_statistics.statistics_numeric = new_format_statistics_numeric)


Hmisc::latex(the_table,
             file = "",
             title = "",
             label = "tab:modifynumeric",
             caption = "Modified atable now calculates the median, MAD, t-test and KS-test for numeric variables.
               The median is greater than the mean in both the drug and placebo group, which indicates a skewed distribution of age.
              Additionally the KS-test is significant, while the t-test is not.",
             caption.lot = "Modified atable",
             rowname = NULL,
             where  ="!htbp",
             booktabs = TRUE,
             multicol = FALSE)

# atable on empty data frames with placeholders ####

# create empty data.frame with non-empty column names
E <- atable::test_data[FALSE, ]

stats_placeholder <- function(x, ...){

  return(list(Mean = "X.xx",
              SD = "X.xx"))
}

the_table <- atable::atable(E, target_cols = c("Numeric", "Factor"),
                            statistics.numeric = stats_placeholder)

Hmisc::latex(the_table,
             file = "",
             title = "",
             label = "tab:empty df and placeholder",
             caption = "atable applied to an empty data frame with placeholder statistics for numeric variables. The placeholder-function is applied to the numeric variable, printing X.xx in the table. The empty factor variable is summarized in the same way as non-empty factors: by returning percentages and counts; in this case yielding 0/0 = NaN percent and counts of 0 in every category, as expected. Note, that the empty data frame still needs non-empty column names.",
             caption.lot = "atable applied to an empty data frame with placeholder statistics",
             rowname = NULL,
             where = "!htbp",
             booktabs = TRUE,
             multicol = FALSE)
# Add new methods for class surv ####


statistics.Surv <- function(x, ...){

  survfit_object <- survival::survfit(x ~ 1)

  # copy from survival:::print.survfit:
  out <- survival:::survmean(survfit_object, rmean = "common")

  return(list(mean_survival_time = out$matrix["*rmean"],
              SE = out$matrix["*se(rmean)"]))
}

two_sample_htest.Surv <- function(value, group, ...){


  survdiff_result <- survival::survdiff(value~group, rho=0)

  # copy from survival:::print.survdiff:
  etmp <- survdiff_result$exp
  df <- (sum(1 * (etmp > 0))) - 1
  p <- 1 - stats::pchisq(survdiff_result$chisq, df)

  return(list(p = p,
              stat = survdiff_result$chisq))
}




# Set classes of dataset ovarian ####

ovarian <- within(survival::ovarian, {
  time_to_event = survival::Surv(futime, fustat)})

# Apply atable with new methods ####
the_table <- atable(ovarian, target_cols = c("time_to_event"), group_col = "rx")

Hmisc::latex(the_table,
             file = "",
             title = "",
             label = "tab:ovarianHypothesisTests",
             caption = "Hypothesis tests of dataset ovarian.",
             caption.lot = "Hypothesis tests of dataset ovarian",
             rowname = NULL,
             where = "!htbp",
             booktabs = TRUE,
             multicol = FALSE)

