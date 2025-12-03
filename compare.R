library(tidyverse)
library(readr)

ruakura_file <- "ruakura.csv"
wayne_file   <- "AgPasture Model Horitou Soil 1.csv"
scott_farm <- "scott farm pasture growth curves.csv"

out_monthly_stats_calendar <- "monthly_stats_calendar.csv"
out_monthly_means_prod     <- "monthly_means_production.csv"
out_annual_means_prod      <- "annual_mean_production.csv"
out_comp_table             <- "comparison_wayne_vs_ruakura.csv"
out_plot_box               <- "boxplot_monthly_means_by_prodmonth.png"
out_plot_scatter           <- "scatter_wayne_vs_ruakura.png"

# Date windows
start_date <- as.Date("2000-06-01")
end_date   <- as.Date("2024-05-31")
comp_start <- as.Date("2010-06-01")
comp_end   <- as.Date("2020-05-31")

# read ruakura ####
rua_raw <- read_csv(ruakura_file, skip = 21, col_names = FALSE, show_col_types = FALSE)

rua <- rua_raw %>%
  rename(Date_chr = X1, PGR = X2) %>%
  mutate(Date = suppressWarnings(as.POSIXct(Date_chr, format = "%d/%m/%Y %H:%M", tz = "UTC"))) %>%
  filter(!is.na(Date)) %>%
  filter(Date >= start_date, Date <= end_date) %>%
  mutate(
    Year  = year(Date),
    Month = month(Date),
    # Production-year (Jun..May)
    ProdYear  = if_else(Month >= 6, Year, Year - 1),
    ProdLabel = paste0(ProdYear, "/", str_sub(ProdYear + 1, 3, 4)),
    ProdMonth = if_else(Month >= 6, Month - 5, Month + 7),  # Jun=1 … May=12
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Summer",
      Month %in% c(3, 4, 5)  ~ "Autumn",
      Month %in% c(6, 7, 8)  ~ "Winter",
      Month %in% c(9, 10, 11)~ "Spring"
    )
  )

# calculation ####
calculation <- rua %>%
  group_by(Month) %>%
  summarise(
    n      = n(),
    mean   = mean(PGR, na.rm = TRUE),
    median = median(PGR, na.rm = TRUE),
    sd     = sd(PGR, na.rm = TRUE),
    cv     = ifelse(mean == 0, NA_real_, sd / mean),
    .groups = "drop"
  ) %>%
  mutate(MonthName = month.abb[Month]) %>%
  arrange(Month)

write_csv(calculation, out_monthly_stats_calendar)

# production year/month ####
month_prod <- rua %>%
  group_by(ProdLabel, ProdMonth) %>%
  summarise(OurMonthMean = mean(PGR, na.rm = TRUE), .groups = "drop") %>%
  arrange(ProdLabel, ProdMonth)

write_csv(month_prod, out_monthly_means_prod)

annual_prod <- rua %>%
  group_by(ProdLabel) %>%
  summarise(AnnualGrowth = mean(PGR, na.rm = TRUE), .groups = "drop") %>%
  arrange(ProdLabel)

write_csv(annual_prod, out_annual_means_prod)

# wrangle wayne data using long/wide ####
wayne_raw <- read_csv(wayne_file, col_names = FALSE, skip = 142, show_col_types = FALSE)

# Use first retained row as column names; fix blanks and duplicates to avoid errors
new_names <- wayne_raw %>% slice(1) %>% unlist() %>% as.character()
new_names <- ifelse(is.na(new_names) | new_names == "", paste0("V", seq_along(new_names)), new_names)
names(wayne_raw) <- make.unique(new_names)

wayne_tbl <- wayne_raw %>%
  slice(-1) %>%
  mutate(across(everything(), as.character))  # important: character for safe pivot

wayne_long <- wayne_tbl %>%
  pivot_longer(cols = everything(), names_to = "key", values_to = "value") %>%
  # Keep only month columns like '1.Jun 2010' (drop 'Total ...' etc.)
  filter(str_detect(key, "^1\\.(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s+\\d{4}$")) %>%
  mutate(
    Mon   = str_sub(key, 3, 5),
    Yr    = as.integer(str_sub(key, -4)),
    Month = match(Mon, month.abb),
    Date  = as.Date(sprintf("%04d-%02d-01", Yr, Month)),
    WayneMonthMean = suppressWarnings(as.numeric(str_replace(value, ",", ""))),
    ProdYear  = if_else(Month >= 6, Yr, Yr - 1),
    ProdLabel = paste0(ProdYear, "/", str_sub(ProdYear + 1, 3, 4)),
    ProdMonth = if_else(Month >= 6, Month - 5, Month + 7)
  ) %>%
  filter(Date >= comp_start, Date <= comp_end) %>%
  # KEEP Yr, Month, Date so we can label with calendar month/year in the final table
  select(ProdLabel, ProdMonth, WayneMonthMean, Yr, Month, Date)

# compare vcs (wayne) and ruakura ####
comp <- month_prod %>%
  inner_join(wayne_long, by = c("ProdLabel","ProdMonth")) %>%
  mutate(
    Diff      = WayneMonthMean - OurMonthMean,
    MonthYear = paste0(month.abb[Month], " ", Yr),  # e.g., "Jun 2010"
    MonthDate = Date                                 # first day of that calendar month (YYYY-MM-01)
  ) %>%
  arrange(MonthDate) %>%
  select(MonthYear, MonthDate, OurMonthMean, WayneMonthMean, Diff)

monthLabel <- c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May")

# box plot ####
p_box <- ggplot(month_prod,
                aes(x = factor(ProdMonth, levels = 1:12, labels = monthLabel),
                    y = OurMonthMean)) +
  geom_boxplot(outlier.alpha = 0.4, fill = "lightsteelblue") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "navy") +
  labs(title = "Monthly PGR across years",
       x = "Month (Production Year Order)",
       y = "Monthly mean PGR (kg DM/ha)") +
  theme_minimal(base_size = 12)

# scatter plot ####
ggplot(comp, aes(x = OurMonthMean, y = WayneMonthMean)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  labs(title = "VCS vs Ruakura monthly mean PGR (Jun 2010 - May 2020)",
       x = "Ruakura (monthly mean, kg DM/ha)",
       y = "VCS (monthly mean, kg DM/ha)") +
  theme_minimal(base_size = 12)
