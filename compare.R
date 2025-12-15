library(tidyverse)
library(ggplot2)
library(readr)

ruakura_file <- "ruakura.csv"
wayne_file <- "AgPasture Model Horitou Soil 1.csv"
scott_farm <- "scott farm pasture growth curves.csv"

# map uppercase calendar month to seasons ####
month_to_season <- function(month_upper) {
  case_when(
    month_upper %in% c("DECEMBER","JANUARY","FEBRUARY") ~ "Summer",
    month_upper %in% c("MARCH","APRIL","MAY")           ~ "Autumn",
    month_upper %in% c("JUNE","JULY","AUGUST")          ~ "Winter",
    month_upper %in% c("SEPTEMBER","OCTOBER","NOVEMBER")~ "Spring",
    TRUE ~ NA_character_
  )
}

prodMonths <- c("JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER","JANUARY","FEBRUARY","MARCH","APRIL","MAY")

# read & wrangle Ruakura Data ####
rua_raw <- read_csv(ruakura_file, skip = 21, col_names = FALSE, show_col_types = FALSE)

ruakura <- rua_raw %>%
  rename(Date = X1, PGR = X2) %>%
  select(-X3) %>%                                
  # Drop column
  mutate(Date = as.Date(Date, format = "%d/%m/%Y %H:%M")) %>%
  filter(!is.na(Date)) %>%
  mutate(
    Year      = year(Date),
    Month     = month(Date),
    anchorYear  = if_else(Month >= 6, Year, Year - 1),
    prodYear  = paste0(anchorYear, "/", str_sub(anchorYear + 1, 3, 4)),     # Label 2000/01 for display 
    prodMonth = if_else(Month >= 6, Month - 5, Month + 7),
    Season    = case_when(
      Month %in% c(12, 1, 2) ~ "Summer",
      Month %in% c(3, 4, 5)  ~ "Autumn",
      Month %in% c(6, 7, 8)  ~ "Winter",
      Month %in% c(9, 10, 11)~ "Spring"
    )
  )

# ---- Ruakura Monthly & Annually Mean ---- ####
rua_monthly <- ruakura %>%
  group_by(prodYear, anchorYear, prodMonth) %>%                         # Group by production year (string + numeric) and prod month 
  summarise(RuakuraMean = mean(PGR, na.rm = TRUE), .groups = "drop") %>%
  mutate(RuakuraMean = round(RuakuraMean, 2)) %>%
  arrange(anchorYear, prodMonth)                                        # Sort by production year then month 

rua_annual_prod <- ruakura %>%
  group_by(prodYear, anchorYear) %>%                                    # Group by production year
  summarise(AnnualGrowth = mean(PGR, na.rm = TRUE), .groups = "drop") %>%
  arrange(anchorYear)

# read & wrangle wayne's data ####
# compute production year/month, filter by production year 
wayne_raw <- read_csv(wayne_file, col_names = FALSE, skip = 142, show_col_types = FALSE)

# make a coloumn of dates
wayne_header <- wayne_raw %>%
  slice(1) %>%
  unlist() %>%
  as.character() # Keep everything character for safe pivoting             

wayne_header <- ifelse(is.na(wayne_header) | wayne_header == "",        # replace blank/missing names 
                       paste0("V", seq_along(wayne_header)), wayne_header)

names(wayne_raw) <- make.unique(wayne_header) # ensure names are unique to avoid pivot errors 

wayne_table <- wayne_raw %>% 
  slice(-1) %>%    # remove the header row
  mutate(across(everything(), as.character))

wayne_long <- wayne_table %>%
  pivot_longer(cols = everything(), names_to = "key", values_to = "value") %>% # wide -> long 
  filter(str_detect(key, "^1\\.(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s+\\d{4}$")) %>%
  mutate(
    Mon   = str_sub(key, 3, 5),                                           # Extract 3-letter month from key
    Yr    = as.integer(str_sub(key, -4)),                                 # Extract year from end of key
    Month = match(Mon, month.abb),                                        # Map to numeric month (1..12)
    WayneMonthMean = suppressWarnings(as.numeric(str_replace(value, ",", ""))), # Parse numeric, strip commas
    anchorYear  = if_else(Month >= 6, Yr, Yr - 1),                          # Compute production-year
    prodYear  = paste0(anchorYear, "/", str_sub(anchorYear + 1, 3, 4)),       # String label for display
    prodMonth = if_else(Month >= 6, Month - 5, Month + 7) 
  ) %>%
  filter(anchorYear >= 2010, anchorYear <= 2019) %>%
  select(prodYear, anchorYear, prodMonth, WayneMonthMean, Yr, Month)  # keep relevant columns 

# ---- Wayne (VCS) vs RUAKURA by production-year/month ---- ####
vscVSrua <- rua_monthly %>%
  inner_join(wayne_long, by = c("prodYear", "anchorYear", "prodMonth")) %>% # exact match on production calendar 
  mutate(Diff = WayneMonthMean - RuakuraMean) %>%
  select(prodYear, anchorYear, prodMonth, RuakuraMean, WayneMonthMean, Diff)

# ---- Box Plot of Ruakura Monthly means ---- ####
ruaBox <- ggplot(rua_monthly, 
                 aes(x = factor(prodMonth, levels = 1:12, labels = prodMonths),
                     y = RuakuraMean)) +
  geom_boxplot(outlier.alpha = 0.4, fill = "lightsteelblue") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "navy") +
  labs(title = "Ruakura: Monthly Mean PGR across Production Years",                   
       x = "Month (Production Year Order: Jun…May)",                                   
       y = "Monthly mean PGR (kg DM/ha)") + 
  theme_minimal(base_size = 12)

# ---- Scatter of Wayne VCS vs Ruakura ---- ####
ruaScatter <- ggplot(vscVSrua, aes(x = RuakuraMean, y = WayneMonthMean)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  labs(title = "VCS vs Ruakura Monthly Mean PGR (Production-Year matched)",           
       x = "Ruakura (monthly mean, kg DM/ha)",                                        
       y = "VCS (monthly mean, kg DM/ha)"  ) + 
  theme_minimal(base_size = 12)
  
