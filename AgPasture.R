
library(tidyverse)
library(lubridate)
library(glue)
library(broom)
library(stringr)
library(purrr)
library(scales)

# User-configurable settings
folder         <- "Dec2025"           # Input CSV folder
skip_header    <- 20                  # Header rows to skip
focus_station  <- "Ruakura"           # Station spotlight for models
out_dir        <- file.path("outputs", format(Sys.time(), "%Y%m%d-%H%M%S"))

files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
if (length(files) == 0) stop(glue("No CSV files found in: {folder}"))

process_file <- function(path) {
  df <- readr::read_csv(path, skip = skip_header, show_col_types = FALSE) %>%
    select(-contains("Events")) %>%
    # Adjust if paddock columns are not exactly positions 2:7
    rename_with(~ paste0("P", seq_along(.)), .cols = 2:7)
  
  date_col <- names(df) %>% keep(~ .x %in% c("Parameter:", "Parameter", "Date")) %>% first()
  if (is.na(date_col)) stop(glue("No date/Parameter column found in: {basename(path)}"))
  
  df %>%
    rename(Date = !!sym(date_col)) %>%
    pivot_longer(
      cols = matches("^P\\d+$"),
      names_to = "Paddock",
      values_to = "PGR"
    ) %>%
    mutate(
      PGR      = suppressWarnings(as.numeric(PGR)),
      DateTime = suppressWarnings(lubridate::dmy_hm(Date)),
      DateTime = if_else(is.na(DateTime), lubridate::dmy(Date), DateTime),
      Date     = as_date(DateTime),
      Station  = str_remove(basename(path), "\\.csv$")
    ) %>%
    drop_na(PGR, Date) %>%
    select(Date, Station, Paddock, PGR)
}

all <- map_df(files, process_file)

# ---- Paddock → soil/fertility ---- ####
paddock <- tibble(
  Paddock   = c("P1","P2","P3","P4","P5","P6"),
  Soil      = c("Horotiu","Lismore","Tokomaru","Horotiu","Lismore","Tokomaru"),
  Fertility = c(1.5,1.5,1.5,1,1,1)
)

all1 <- all %>%
  left_join(paddock, by = "Paddock")

# ---- Production year fields (Jun → May) ---- ####
min_year   <- min(year(all1$Date), na.rm = TRUE)
max_year   <- max(year(all1$Date), na.rm = TRUE)
start_date <- as.Date(paste0(min_year, "-06-01"))
end_date   <- as.Date(paste0(max_year + 1, "-06-01"))

prod_month <- as.character(
  lubridate::month(
    seq.Date(as.Date("2000-06-01"), by = "month", length.out = 12),
    label = TRUE, abbr = TRUE, locale = "C"
  )
)

all_prodYear <- all1 %>%
  filter(Date >= start_date, Date < end_date) %>%
  mutate(
    Year       = year(Date),
    Month      = month(Date),
    anchorYear = year(Date %m-% months(5)),                      # June-based production year
    prodYear   = paste0(anchorYear, "/", substr(anchorYear + 1, 3, 4)),
    prodMonth  = ((Month - 6) %% 12) + 1L,                       # 1..12 for Jun..May
    monthLabel = factor(prod_month[prodMonth], levels = prod_month, ordered = TRUE),
    Season     = case_when(
      Month %in% c(12, 1, 2) ~ "Summer",
      Month %in% c(3, 4, 5)  ~ "Autumn",
      Month %in% c(6, 7, 8)  ~ "Winter",
      Month %in% c(9, 10, 11)~ "Spring",
      TRUE ~ NA_character_
    )
  )

# Keep tidy dataset for plotting
data_use <- all_prodYear %>%
  mutate(monthLabel = factor(monthLabel, levels = prod_month, ordered = TRUE))

# ---- Aggregations (monthly/seasonal/annual) ---- ####
monthlyMean <- data_use %>%
  group_by(Station, prodYear, anchorYear, monthLabel, Season) %>%
  summarise(mean_month = mean(PGR, na.rm = TRUE), .groups = "drop") %>%
  arrange(anchorYear) %>%
  mutate(
    # Short production year label, e.g., "23/24", ordered chronologically
    short_prodYear = paste0(substr(anchorYear, 3, 4), "/", substr(anchorYear + 1, 3, 4)),
    short_prodYear = factor(short_prodYear, levels = unique(short_prodYear), ordered = TRUE)
  )

seasonalMean <- data_use %>%
  group_by(Station, prodYear, anchorYear, Season) %>%
  summarise(mean_season = mean(PGR, na.rm = TRUE), .groups = "drop") %>%
  arrange(anchorYear) %>%
  mutate(
    short_prodYear = paste0(substr(anchorYear, 3, 4), "/", substr(anchorYear + 1, 3, 4)),
    short_prodYear = factor(short_prodYear, levels = unique(short_prodYear), ordered = TRUE)
  )

seasonalSum <- data_use %>%
  group_by(Station, prodYear, anchorYear, Season) %>%
  summarise(seasonal_sum = sum(PGR, na.rm = TRUE), .groups = "drop") %>%
  arrange(anchorYear) %>%
  mutate(
    short_prodYear = paste0(substr(anchorYear, 3, 4), "/", substr(anchorYear + 1, 3, 4)),
    short_prodYear = factor(short_prodYear, levels = unique(short_prodYear), ordered = TRUE)
  )

annualStation <- data_use %>%
  group_by(Station, prodYear, anchorYear) %>%
  summarise(
    annual_mean = mean(PGR, na.rm = TRUE),
    annual_sum  = sum(PGR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(anchorYear) %>%
  mutate(
    short_prodYear = paste0(substr(anchorYear, 3, 4), "/", substr(anchorYear + 1, 3, 4)),
    short_prodYear = factor(short_prodYear, levels = unique(short_prodYear), ordered = TRUE)
  )


# ---- SD by year + models ---- ####
sd_by_year <- monthlyMean %>%
  group_by(Station, prodYear, anchorYear) %>%
  summarise(sd_month = sd(mean_month, na.rm = TRUE), .groups = "drop")

# Model: SD ~ anchorYear (per station)
sd_year_tidy <- sd_by_year %>%
  group_by(Station) %>%
  group_modify(~ broom::tidy(lm(sd_month ~ anchorYear, data = .x))) %>%
  ungroup()

sd_year_glance <- sd_by_year %>%
  group_by(Station) %>%
  group_modify(~ broom::glance(lm(sd_month ~ anchorYear, data = .x))) %>%
  ungroup()

# Seasonal average growth per prodYear (mean of seasonal means)
seasonal_avg_prodYear <- seasonalMean %>%
  group_by(Station, prodYear, anchorYear) %>%
  summarise(mean_seasonal_growth = mean(mean_season, na.rm = TRUE), .groups = "drop")

# Model: SD ~ seasonal average growth
sd_seasonal_df <- sd_by_year %>%
  left_join(seasonal_avg_prodYear, by = c("Station","prodYear","anchorYear"))

sd_seasonal_tidy <- sd_seasonal_df %>%
  group_by(Station) %>%
  group_modify(~ broom::tidy(lm(sd_month ~ mean_seasonal_growth, data = .x))) %>%
  ungroup()

sd_seasonal_glance <- sd_seasonal_df %>%
  group_by(Station) %>%
  group_modify(~ broom::glance(lm(sd_month ~ mean_seasonal_growth, data = .x))) %>%
  ungroup()

# Annual yield ~ year (all stations + spotlight Ruakura)
annual_models_by_station <- annualStation %>%
  group_by(Station) %>%
  group_modify(~ broom::tidy(lm(annual_sum ~ anchorYear, data = .x))) %>%
  ungroup() %>%
  arrange(Station)

annual_model_ruakura <- annualStation %>%
  filter(Station == focus_station) %>%
  lm(annual_sum ~ anchorYear, data = .) %>%
  list(tidy = broom::tidy(.), glance = broom::glance(.))

# Seasonal yield ~ year (spotlight Ruakura by season)
seasonal_models_ruakura <- seasonalSum %>%
  filter(Station == focus_station) %>%
  group_by(Season) %>%
  group_map(~ list(
    Season = .y$Season,
    tidy   = broom::tidy(lm(seasonal_sum ~ anchorYear, data = .x)),
    glance = broom::glance(lm(seasonal_sum ~ anchorYear, data = .x))
  ))

# ---- plots ---- ####

# (A/B) Box plot: y = monthly average growth, x = short production year, facet by station
p_box_monthProdGrowth <- ggplot(monthlyMean, aes(x = short_prodYear, y = mean_month)) +
  geom_boxplot(outlier.alpha = 0.5) +
  facet_wrap(~ Station, scales = "free_y") +
  labs(
    title = "Monthly Production Growth by Production Year",
    x = "Prod. Year", y = "Mean Monthly PGR"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# SD vs year (points) facet by station
p_sd_points_only <- ggplot(sd_by_year, aes(x = anchorYear, y = sd_month)) +
  geom_point(size = 2.5, alpha = 0.9, colour = "steelblue") +
  facet_wrap(~ Station, scales = "free_y") +
  labs(
    title = "SD of Monthly Average Growth within Production Year",
    x = "Anchor Year (June start)", y = "SD of monthly mean PGR"
  ) +
  theme_minimal()

# SD vs year + LM line
p_sd_points_lm <- p_sd_points_only + geom_smooth(method = "lm", se = FALSE, colour = "grey30")

# SD vs seasonal average growth (scatter + LM), facet by station
p_sd_vs_seasonal <- ggplot(sd_seasonal_df, aes(mean_seasonal_growth, sd_month)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~ Station, scales = "free") +
  labs(
    title = "Within-year Variability vs Seasonal Average Growth",
    x = "Mean Seasonal Growth (Production Year)",
    y = "SD of Monthly Mean Growth"
  ) +
  theme_minimal()

# (C) Box: y = mean_month, x = Station, facet by production month
p_box_y_month_x_station <- ggplot(monthlyMean, aes(x = Station, y = mean_month)) +
  geom_boxplot(outlier.alpha = 0.5) +
  facet_wrap(~ monthLabel, nrow = 3) +
  labs(
    title = "Monthly Average Growth by Station (faceted by Production Month)",
    x = "Station", y = "Mean Monthly PGR"
  ) +
  theme_minimal() +
  coord_flip()

# (D) Box: y = mean_season, x = Station, facet by Season
p_box_y_avgSeason_x_station <- ggplot(seasonalMean, aes(x = Station, y = mean_season)) +
  geom_boxplot(outlier.alpha = 0.5) +
  facet_wrap(~ Season) +
  labs(
    title = "Seasonal Average Growth by Station",
    x = "Station", y = "Mean Seasonal PGR"
  ) +
  theme_minimal() +
  coord_flip()

# (E) Box: monthly mean by Station, facet by Season
p_box_monthYearAvg_stationX_facetSeason <- ggplot(monthlyMean, aes(x = Station, y = mean_month)) +
  geom_boxplot(outlier.alpha = 0.5) +
  facet_wrap(~ Season) +
  labs(
    title = "Monthly Mean Growth by Station (faceted by Season)",
    x = "Station", y = "Mean Monthly PGR"
  ) +
  theme_minimal() +
  coord_flip()

# Peak & lowest month per year per station — table + labeled plot
peaks_troughs <- monthlyMean %>%
  group_by(Station, prodYear, anchorYear) %>%
  summarise(
    peak_month = monthLabel[which.max(mean_month)],
    peak_value = max(mean_month, na.rm = TRUE),
    low_month  = monthLabel[which.min(mean_month)],
    low_value  = min(mean_month, na.rm = TRUE),
    .groups = "drop"
  )


# Use the peaks_troughs table ####
p_lollipop_peak_low <- ggplot(peaks_troughs, aes(x = anchorYear)) +
  # segment (from low to peak per year) for context
  geom_segment(aes(xend = anchorYear, y = low_value, yend = peak_value), colour = "grey70") +
  # points
  geom_point(aes(y = peak_value), colour = "darkgreen", size = 2) +
  geom_point(aes(y = low_value),  colour = "firebrick", size = 2) +
  # concise labels (nudged slightly)
  geom_text(
    aes(y = peak_value, label = peak_month), colour = "darkgreen",
    vjust = -0.8, size = 3, check_overlap = TRUE
  ) +
  geom_text(
    aes(y = low_value, label = low_month), colour = "firebrick",
    vjust = 1.6, size = 3, check_overlap = TRUE
  ) +
  facet_wrap(~ Station, scales = "free_y") +
  scale_x_continuous(
    breaks = sort(unique(peaks_troughs$anchorYear)),
    labels = paste0(
      substr(sort(unique(peaks_troughs$anchorYear)), 3, 4), "/",
      substr(sort(unique(peaks_troughs$anchorYear)) + 1, 3, 4)
    )
  ) +
  labs(
    title = "Peak and Trough Monthly Mean Growth (Lollipop)",
    x = "Prod. Year", y = "Mean Monthly PGR"
  ) 

# ---- ECDFs (cumulative distribution) — annual & seasonal ---- ####
p_ecdf_annual_allStations <- ggplot(annualStation, aes(annual_sum, colour = Station)) +
  stat_ecdf(geom = "step", linewidth = 1) +
  labs(
    title = "ECDF: Annual Yield (sum of PGR) — all stations",
    x = "Annual yield (sum PGR)", y = "Cumulative probability"
  ) +
  theme_minimal()

p_ecdf_annual_facetedStation <- ggplot(annualStation, aes(annual_sum)) +
  stat_ecdf(geom = "step", colour = "steelblue", linewidth = 0.9) +
  facet_wrap(~ Station, scales = "free_x") +
  labs(
    title = "ECDF: Annual Yield (sum of PGR) — faceted by station",
    x = "Annual yield (sum PGR)", y = "Cumulative probability"
  ) +
  theme_minimal()

p_ecdf_seasonal_faceted <- ggplot(seasonalSum, aes(seasonal_sum)) +
  stat_ecdf(geom = "step", colour = "steelblue", linewidth = 0.8) +
  facet_grid(Station ~ Season, scales = "free_x") +
  labs(
    title = "ECDF: Seasonal Yield (sum of PGR) — Station × Season",
    x = "Seasonal yield (sum PGR)", y = "Cumulative probability"
  ) +
  theme_minimal()

# Violin plot — vertical, stations on x, colours by Season (side-by-side)
season_cols <- c("Summer"="#F2C14E","Autumn"="#D96C06","Winter"="#3772A1","Spring"="#2E8B57")
p_violin_dodged <- ggplot(data_use, aes(x = Station, y = PGR, fill = Season)) +
  geom_violin(
    alpha = 0.6, trim = TRUE, colour = "grey30",
    position = position_dodge(width = 0.85),
    width = 0.85
  ) +
  scale_fill_manual(values = season_cols) +
  labs(
    title = "PGR distribution by Station (vertical violins), colours = Season",
    x = "Station", y = "PGR"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ---- Violin plot ---- ####
p_violin <- ggplot(data_use, aes(x = Station, y = PGR, fill = Season)) +
  geom_violin(trim = TRUE, alpha = 0.5) +
  facet_wrap(~ Season) +
  labs(
    title = "Distribution of PGR by Station and Season",
    x = "Station", y = "PGR"
  ) +
  theme_minimal() +
  coord_flip()

# -----------------------------
p_violin_overlay <- ggplot(monthlyMean, aes(x = Station, y = mean_month, fill = Season)) +
  geom_violin(
    alpha = 0.35,                  # transparency so overlaps are visible
    trim = TRUE,
    colour = NA,                   # no outline to reduce clutter
    position = "identity",         # overlay at the same x position
    width = 0.95
  ) +
  labs(
    title = "PGR distribution by Station (overlayed vertical violins), colours = Season",
    x = "Station", y = "PGR"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 30, hjust = 1))

# ---- Summary tables (mean, median, SD, CV) --- ####
annual_summary_station <- annualStation %>%
  group_by(Station) %>%
  summarise(
    mean_annual   = mean(annual_sum, na.rm = TRUE),
    median_annual = median(annual_sum, na.rm = TRUE),
    sd_annual     = sd(annual_sum, na.rm = TRUE),
    cv_annual     = sd_annual / mean_annual,
    .groups = "drop"
  )

seasonal_summary_station <- seasonalSum %>%
  group_by(Station, Season) %>%
  summarise(
    mean_seasonal   = mean(seasonal_sum, na.rm = TRUE),
    median_seasonal = median(seasonal_sum, na.rm = TRUE),
    sd_seasonal     = sd(seasonal_sum, na.rm = TRUE),
    cv_seasonal     = sd_seasonal / mean_seasonal,
    .groups = "drop"
  )

# -----------------------------
# 8) Optional: Horotiu (Fertility 1.5) profiles
# -----------------------------
horotiu_focus <- all_prodYear %>% 
  filter(Soil == "Horotiu", Fertility == 1.5)

p_horotiu_profile <- horotiu_focus %>%
  group_by(Station, monthLabel) %>%
  summarise(PGR_mean = mean(PGR, na.rm = TRUE), .groups = "drop") %>%
  mutate(monthLabel = factor(monthLabel, levels = prod_month, ordered = TRUE)) %>%
  ggplot(aes(monthLabel, PGR_mean, group = Station, colour = Station)) +
  geom_line() + geom_point() +
  scale_x_discrete(limits = prod_month, drop = FALSE) +
  labs(
    title = "Seasonal Profile of PGR by Production Month (Horotiu, Fertility 1.5)",
    x = "Production Month (Jun → May)", y = "Mean PGR"
  ) +
  theme_minimal()

p_horotiu_by_year <- ggplot(
  horotiu_focus %>% filter(!is.na(monthLabel)),
  aes(monthLabel, PGR, colour = Station, group = interaction(Station, prodYear))
) +
  geom_line(na.rm = TRUE) +
  scale_x_discrete(limits = prod_month, drop = FALSE) +
  facet_wrap(~ prodYear) +
  labs(
    title = "PGR by Production Month - Horotiu, Fertility 1.5",
    x = "Production month (Jun → May)", y = "PGR"
  ) +
  scale_color_brewer(palette = "PuOr")

