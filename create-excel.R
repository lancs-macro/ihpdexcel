
# Dependencies are managed by rv (rproject.toml) - run `rv sync` to install.

# Load libraries ----------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(exuber)
  library(ihpdr)
})

# "remote" queries the ihpdr package's online source directly; "local" uses
# the newest hp*.xlsx dropped in data/ instead, for when a release file
# arrives before ihpdr's online source is updated.
DOWNLOAD_TYPE <- "remote"

ihpd_get_local <- function(path) {
  list(
    ihpdr:::format_excel_raw(path, 2, "hpi"),
    ihpdr:::format_excel_raw(path, 3, "rhpi"),
    ihpdr:::format_excel_raw(path, 4, "pdi"),
    ihpdr:::format_excel_raw(path, 5, "rpdi")) %>%
    reduce(full_join, by = c("Date", "country")) %>%
    drop_na()
}

if (DOWNLOAD_TYPE == "local") {
  local_file <- fs::dir_ls("data", regexp = "hp[0-9]{4}\\.xlsx$") %>% sort() %>% tail(1)
  full_data <- ihpd_get_local(local_file)
} else {
  full_data <- ihpdr::ihpd_get()
}
full_data <- full_data %>%
  mutate_at(vars(-Date, -country), as.numeric)

price <-
  full_data %>% 
  select(Date, country, rhpi) %>% 
  spread(country, rhpi) %>% 
  select(-starts_with("Aggregate"))

income <-
  full_data %>% 
  mutate(ratio = rhpi/rpdi) %>% 
  select(Date, country, ratio) %>% 
  spread(country, ratio) %>% 
  select(-starts_with("Aggregate"))

# Estimate ----------------------------------------------------------------

minw <- psy_minw(price)

suppressMessages({
  radf_price1 <- radf(price, lag = 1, minw = minw)
  radf_price4 <- radf(price, lag = 4, minw = minw)
  radf_income1 <- radf(income, lag = 1, minw = minw)
  radf_income4 <- radf(income, lag = 4, minw = minw)
})

n <- nrow(price)
# exuber 2.0.0 removed the bundled `radf_crit` lookup table (lag-0, n<=600);
# radf_mc_cv() is its documented offline replacement (own Monte Carlo sim)
cv <- radf_mc_cv(n, minw = minw, seed = 42)

# Custom ordering assing target -------------------------------------------

target <- 
  c("Australia", "Belgium", "Canada", "Switzerland", "Germany", "Denmark", 
    "Spain", "Finland", "France", "UK", "Ireland", "Italy", "Japan", "S. Korea",
    "Luxembourg", "Netherlands", "Norway", "New Zealand", "Sweden", "US", 
    "S. Africa", "Croatia", "Israel", "Slovenia", "Colombia", "Portugal")

# Tidy cv -----------------------------------------------------------------

xdata_cv <- tidy(cv) %>% 
  select(-sig, -adf) %>% 
  # mutate(sig = paste0(sig, "%")) %>% 
  set_names(c("SADF", "GSADF")) 

idx <- index(radf_price1) %>% 
  zoo::as.yearqtr() %>% 
  format("%Y:Q%q") %>% 
  enframe("key", "true_date") %>% 
  bind_cols(index = index(radf_price1))

cv_seq <- augment(cv) %>%
  filter(sig == 95) %>% 
  right_join(idx,  by = "key") %>% 
  select(bsadf)

# tidy price --------------------------------------------------------------

xdata_price1 <- tidy(radf_price1) %>% 
  select(-adf,-id) %>% 
  set_names(c("SADF", "GSADF"))

xdata_price4 <- tidy(radf_price4) %>% 
  select(-adf,-id) %>% 
  set_names(c("SADF", "GSADF"))


xdata_price_seq1 <- augment_join(radf_price1, cv) %>% 
  filter(stat == "bsadf", sig == 95) %>% 
  full_join(idx, by = "index") %>% 
  select(true_date, id, tstat, crit) %>% 
  arrange(true_date) %>% 
  pivot_wider(names_from = id, values_from = tstat) %>% 
  select(Date = true_date, crit, all_of(target)) %>% 
  drop_na(Date)

xdata_price_seq4 <-augment_join(radf_price4, cv) %>% 
  filter(stat == "bsadf", sig == 95) %>% 
  full_join(idx, by = "index") %>% 
  select(true_date, id, tstat, crit) %>% 
  arrange(true_date) %>% 
  pivot_wider(names_from = id, values_from = tstat) %>% 
  select(Date = true_date, crit, all_of(target)) %>% 
  drop_na(Date)


# tidy income -------------------------------------------------------------

xdata_income1 <- tidy(radf_income1) %>% 
  mutate(id = as.factor(id)) %>% 
  mutate(id = forcats::fct_relevel(id, target)) %>% 
  arrange(id) %>% 
  select(-adf,-id) %>% 
  set_names(c("SADF", "GSADF"))

xdata_income4 <- tidy(radf_income4) %>% 
  mutate(id = as.factor(id)) %>% 
  mutate(id = forcats::fct_relevel(id, target)) %>% 
  arrange(id) %>% 
  select(-adf,-id) %>% 
  set_names(c("SADF", "GSADF"))

# xdata_income_seq1 <- augment(radf_income1) %>% 
#   select(key, id, bsadf) %>% 
#   spread(id, bsadf) %>% 
#   right_join(idx,  by = "key") %>% 
#   bind_cols(cv_seq) %>% 
#   arrange(key) %>% 
#   select(Date = true_date, bsadf, all_of(target)) 

# xdata_income_seq4 <-
#   augment(radf_income4) %>% 
#   select(key, id, bsadf) %>%
#   spread(id, bsadf) %>% 
#   right_join(idx,  by = "key") %>% 
#   bind_cols(cv_seq) %>% 
#   arrange(key) %>% 
#   select(Date = true_date, bsadf, all_of(target))


xdata_income_seq1 <- augment_join(radf_income1, cv) %>% 
  filter(stat == "bsadf", sig == 95) %>% 
  full_join(idx, by = "index") %>% 
  select(true_date, id, tstat, crit) %>% 
  arrange(true_date) %>% 
  pivot_wider(names_from = id, values_from = tstat) %>% 
  select(Date = true_date, crit, all_of(target)) %>% 
  drop_na(Date)
  
xdata_income_seq4 <-augment_join(radf_income4, cv) %>% 
  filter(stat == "bsadf", sig == 95) %>% 
  full_join(idx, by = "index") %>% 
  select(true_date, id, tstat, crit) %>% 
  arrange(true_date) %>% 
  pivot_wider(names_from = id, values_from = tstat) %>% 
  select(Date = true_date, crit, all_of(target)) %>% 
  drop_na(Date)

# start writing -----------------------------------------------------------

library(openxlsx)

# ihpdr::ihpd_versions() was removed upstream; derive the release tag
# ("2602" etc.) from the data's own last date instead.
vers <- pull(price, Date)[n] %>%
  zoo::as.yearqtr() %>%
  format("%y0%q")

old_version <- fs::dir_ls("versions", regexp = "^versions/hpta[0-9]{4}\\.xlsx$") %>%
  fs::path_file() %>%
  gsub("\\.xlsx$", "", .) %>%
  sort() %>%
  tail(1)

file_name <- paste0("hpta", vers, ".xlsx")
if (fs::file_exists(here::here("versions", file_name))) {
  if (interactive()) {
    answer <- yesno::yesno2(sprintf("Whould you like to overwrite `%s`", file_name))
  }
}

second_table_num <- cellranger::letter_to_num("G") + ncol(xdata_price_seq1) + 1
second_table_start <- cellranger::num_to_letter(second_table_num)

# load template -----------------------------------------------------------

wb <- loadWorkbook(here::here("template", "full_post2304.xlsx"))
modifyBaseFont(wb, fontSize = 11, fontName = "Calibri")
options("openxlsx.numFmt" = "0.00")

# Sheet 2: LAG=1 ----------------------------------------------------------

writeData(wb, sheet = 2, xdata_cv, startCol = "B", 
          startRow = 5, colNames = FALSE)
writeData(wb, sheet = 2, xdata_price1, startCol = "B", 
          startRow = 10, colNames = FALSE)
writeData(wb, sheet = 2, xdata_income1, startCol = "D", 
          startRow = 10, colNames = FALSE)

writeData(wb, sheet = 2, xdata_price_seq1, startCol = "G", startRow = 4, 
          keepNA = TRUE, colNames = FALSE)
writeData(wb, sheet = 2, xdata_income_seq1, startCol = second_table_start, 
          startRow = 4,  keepNA = TRUE, colNames = FALSE)

# Sheet 2: LAG=4 ----------------------------------------------------------

writeData(wb, sheet = 3, xdata_cv, startCol = "B", 
          startRow = 5, colNames = FALSE)
writeData(wb, sheet = 3, xdata_price4, startCol = "B", 
          startRow = 10, colNames = FALSE)
writeData(wb, sheet = 3, xdata_income4, startCol = "D", 
          startRow = 10, colNames = FALSE)

writeData(wb, sheet = 3, xdata_price_seq4, startCol = "G", startRow = 4, 
          keepNA = TRUE, colNames = FALSE)
writeData(wb, sheet = 3, xdata_income_seq4, startCol = second_table_start, 
          startRow = 4,  keepNA = TRUE, colNames = FALSE)

# Save Final Output -------------------------------------------------------

suppressMessages(saveWorkbook(wb, here::here("versions", file_name), overwrite = TRUE))
message(sprintf("Saving `%s` to `versions/%s`", file_name, file_name))


# comparison -------------------------------------------------------------

source("functions.R")

new_version <- paste0("hpta", vers)

compare(v1 = old_version, v2 = new_version)


