library(tidyverse)
library(exuber)
# devtools::install_github("kvasilopoulos/ihpdr")

full_data <- ihpdr::download_raw()

price <-
  full_data %>% 
  select(Date, country, rhpi) %>% 
  spread(country, rhpi)

income <-
  full_data %>% 
  mutate(ratio = rhpi/rpdi) %>% 
  select(Date, country, ratio) %>% 
  spread(country, ratio)

# Estimate ----------------------------------------------------------------

radf_price1 <- radf(price, lag = 1)

radf_price4 <- radf(price, lag = 4)

n <- nrow(price)
cv <- crit[[n]]

radf_income1 <- radf(income, lag = 1)

radf_income4 <- radf(income, lag = 4)


# Custom ordering assing target -------------------------------------------

target <- 
  c("Australia", "Belgium", "Canada", "Switzerland", "Germany", "Denmark", 
    "Spain", "Finland", "France", "UK", "Ireland", "Italy", "Japan", "S. Korea",
    "Luxembourg", "Netherlands", "Norway", "New Zealand", "Sweden", "US", 
    "S. Africa", "Croatia", "Israel")

# Tidy cv -----------------------------------------------------------------

xdata_cv <- tidy(cv) %>% 
  select(-sig, -adf) %>% 
  # mutate(sig = paste0(sig, "%")) %>% 
  set_names(c("SADF", "GSADF")) 

idx <- index(radf_price1) %>% 
  zoo::as.yearqtr() %>% 
  format("%Y:Q%q") %>% 
  enframe("key", "true_date")

cv_seq <- augment(cv) %>%
  filter(sig == 0.95) %>% 
  right_join(idx,  by = "key") %>% 
  select(bsadf)

# tidy price --------------------------------------------------------------

xdata_price1 <- tidy(radf_price1) %>% 
  slice(-1) %>% 
  mutate(id = as.factor(id)) %>% 
  mutate(id = forcats::fct_relevel(id, target)) %>% 
  arrange(id) %>% 
  select(-adf,-id) %>% 
  set_names(c("SADF", "GSADF"))

xdata_price4 <- tidy(radf_price4) %>% 
  slice(-1) %>% 
  mutate(id = as.factor(id)) %>% 
  mutate(id = forcats::fct_relevel(id, target)) %>% 
  arrange(id) %>% 
  select(-adf,-id) %>% 
  set_names(c("SADF", "GSADF"))

xdata_price_seq1 <- augment(radf_price1) %>% 
  select(key, id, bsadf) %>% 
  spread(id, bsadf) %>% 
  right_join(idx,  by = "key") %>% 
  bind_cols(cv_seq) %>% 
  select(Date = true_date, bsadf, target) 

xdata_price_seq4 <- augment(radf_price4) %>% 
  select(key, id, bsadf) %>% 
  spread(id, bsadf) %>% 
  right_join(idx,  by = "key") %>% 
  bind_cols(cv_seq) %>% 
  select(Date = true_date, bsadf, target) 


# tidy income -------------------------------------------------------------

xdata_income1 <- tidy(radf_income1) %>% 
  slice(-1) %>% 
  mutate(id = as.factor(id)) %>% 
  mutate(id = forcats::fct_relevel(id, target)) %>% 
  arrange(id) %>% 
  select(-adf,-id) %>% 
  set_names(c("SADF", "GSADF"))

xdata_income4 <- tidy(radf_income4) %>% 
  slice(-1) %>% 
  mutate(id = as.factor(id)) %>% 
  mutate(id = forcats::fct_relevel(id, target)) %>% 
  arrange(id) %>% 
  select(-adf,-id) %>% 
  set_names(c("SADF", "GSADF"))

xdata_income_seq1 <- augment(radf_income1) %>% 
  select(key, id, bsadf) %>% 
  spread(id, bsadf) %>% 
  right_join(idx,  by = "key") %>% 
  bind_cols(cv_seq) %>% 
  select(Date = true_date, bsadf, target) 

xdata_income_seq4 <- augment(radf_income4) %>% 
  select(key, id, bsadf) %>% 
  spread(id, bsadf) %>% 
  right_join(idx,  by = "key") %>% 
  bind_cols(cv_seq) %>% 
  select(Date = true_date, bsadf, target) 


# start writing -----------------------------------------------------------

library(openxlsx)

vers <- pull(price, Date)[n] %>% 
  zoo::as.yearqtr() %>% 
  format("%y0%q")

file_name <- paste0("hpta", vers, ".xlsx")
if (fs::file_exists(file_name)) {
  fs::file_delete(file_name)
}
fs::file_copy("template/full.xlsx", file_name)


# load template -----------------------------------------------------------

wb <- loadWorkbook(file_name)
modifyBaseFont(wb, fontSize = 11, fontName = "Calibri")
options("openxlsx.numFmt" = "0.00")

# Sheet 2: LAG=1 ----------------------------------------------------------

writeData(wb, sheet = 2, xdata_cv, startCol = "B", startRow = 5, colNames = FALSE)
writeData(wb, sheet = 2, xdata_price1, startCol = "B", startRow = 10, colNames = FALSE)
writeData(wb, sheet = 2, xdata_income1, startCol = "D", startRow = 10, colNames = FALSE)

writeData(wb, sheet = 2, xdata_price_seq1, startCol = "G", startRow = 4, 
          keepNA = TRUE, colNames = FALSE)
writeData(wb, sheet = 2, xdata_income_seq1, startCol = "AG", startRow = 4, 
          keepNA = TRUE, colNames = FALSE)

# Sheet 2: LAG=4 ----------------------------------------------------------

writeData(wb, sheet = 3, xdata_cv, startCol = "B", startRow = 5, colNames = FALSE)
writeData(wb, sheet = 3, xdata_price4, startCol = "B", startRow = 10, colNames = FALSE)
writeData(wb, sheet = 2, xdata_income4, startCol = "D", startRow = 10, colNames = FALSE)


writeData(wb, sheet = 3, xdata_price_seq4, startCol = "G", startRow = 4, 
          keepNA = TRUE, colNames = FALSE)
writeData(wb, sheet = 3, xdata_income_seq4, startCol = "AG", startRow = 4, 
          keepNA = TRUE, colNames = FALSE)

saveWorkbook(wb, "hpta1901.xlsx", overwrite = TRUE)




  

  


