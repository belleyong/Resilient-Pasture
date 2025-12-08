
library(tidyverse)
library(lubridate)
library(readr)
library(stringr)
library(purrr)
library(ggplot2) 

# ---- Set correct folder ----
folder <- "Dec2025"   # <- adjust if needed
files  <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)

# Guard against empty list
if (length(files) == 0) stop(glue::glue("No CSV files found in: {folder}"))

process_file <- function(path) {
  df <- read_csv(path, skip = 20) %>%
    # drop "Events" columns if present
    select(-contains("Events")) %>%
    # temporarily name paddock columns 2:7 as P1..P6
    rename_with(~ paste0("P", seq_along(.)), .cols = 2:7)
  
  # find a usable date column name
  date_col <- names(df) %>% keep(~ .x %in% c("Parameter:", "Parameter", "Date")) %>% first()
  if (is.na(date_col)) {
    stop(glue::glue("No date/Parameter column found in: {basename(path)}"))
  }
  
  df %>%
    rename(Date = !!sym(date_col)) %>%
    pivot_longer(
      cols = matches("^P\\d+$"),
      names_to = "Paddock",
      values_to = "PGR"
    ) %>%
    mutate(
      PGR = as.numeric(PGR),
      # parse dd/mm/YYYY HH:MM safely; fall back to just date if needed
      DateTime = suppressWarnings(lubridate::dmy_hm(Date)),
      DateTime = if_else(is.na(DateTime), lubridate::dmy(Date), DateTime),
      Date     = as_date(DateTime),
      Station  = str_remove(basename(path), "\\.csv$")  # <- set Station here
    ) %>%
    drop_na(PGR)
}

# Apply to all files
all <- map_df(files, process_file)

# ---- Paddock info ----
paddock <- tibble(
  Paddock  = c("P1","P2","P3","P4","P5","P6"),
  Soil     = c("Horotiu","Lismore","Tokomaru","Horotiu","Lismore","Tokomaru"),
  Fertility= c(1.5,1.5,1.5,1,1,1)
)

all1 <- all %>%
  left_join(paddock, by = "Paddock")

# ---- Add production year fields (generic across all soils/stations) ----
all_prodYear <- all1 %>%
  mutate(
    # If date is in format like "dd/mm/YYYY HH:MM", parse once 
    # If it is already Date, then code will keep date 
    Date = if (inherits(Date, "Date")) Date else as.Date(Date, format = "%d/%m/%Y %H:%M"),
    Year = as.integer(format(Date, "%Y")),
    Month = as.integer(format(Date, "%m")),
    anchorYear = if_else(Month >= 6, Year, Year - 1),
    prodYear = paste0(anchorYear, "/", substr(anchorYear + 1, 3, 4)), # e.g. 2024/25
    prodMonth = ifelse(Month >= 6, Month - 5, Month + 7),             # Jun = 1 ... May = 12 
    monthLabel = factor(format(Date, "%b"),
                        levels = c("Jun", "Jul"
                                   ,"Aug","Sep","Oct","Nov","Dec",
                                   "Jan","Feb","Mar","Apr","May"),
                        ordered = TRUE),
    Season    = case_when(
      Month %in% c(12, 1, 2) ~ "Summer",
      Month %in% c(3, 4, 5)  ~ "Autumn",
      Month %in% c(6, 7, 8)  ~ "Winter",
      Month %in% c(9, 10, 11)~ "Spring"
    )
  ) %>%
  filter(!is.na(Date)) # keep only valid dates 

# ---- Filter to Horotiu Soil + 1.5 fertility ----
horotiu_focus <- all_prodYear %>% 
  filter(Soil == "Horotiu", Fertility == 1.5)

# ---- Check stations and data range ----
horotiu_focus %>%
  summarise(
    n_rows = n(),
    date_min = min(Date, na.rm = TRUE),
    date_max = max(Date, na.rm = TRUE),
    n_stations = n_distinct(Station),
    stations = paste(sort(unique(Station)), collapse = ",")
  )

# ---- Time series: PGR over time by station ----
ggplot(horotiu_focus, aes(Date, PGR, colour = Station)) + 
  geom_line(alpha = 0.8) +
  labs(
    title = "Pasture Growth Rate (PGR) - Horotiu, Fertility 1.5",
    x = "Date",
    y = "PGR",
    colour = "Station"
  ) +
  theme_minimal()

# ---- Seasonal Pattern: average PGR by month (across years), per station ----
horotiu_focus %>%
  mutate(Month = lubridate::month(Date, label = TRUE)) %>% 
  group_by(Station, Month) %>%
  summarise(PGR_mean = mean(PGR, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(Month, PGR_mean, group = Station, colour = Station)) +
  geom_line() + geom_point() + 
  labs(
    title = "Seasonal profile of PGR (Horotiu, Fertility 1.5)",
    x = "Month", y = "Mean PGR") +
  theme_minimal()

# ---- production-month progression, faceted by production year ----
ggplot(horotiu_focus %>% filter(prodYear != "2024/25"), aes(monthLabel, PGR, colour = Station, 
                          group = interaction(Station, prodYear))) +
  geom_line() + geom_point(size = 0.8) + 
  facet_wrap(~ prodYear) + 
  labs(title = "PGR by Production Month - Horotiu, Fertility 1.5",
       x = "Production month (Jun -> May)", y = "PGR") + 
  scale_color_brewer(palette = "PuOr") +
  theme(plot.title = element_text(hjust = 0.5)) # nake title middle 
  
