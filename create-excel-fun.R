
download_version <- function(vers, tmplt_xlsx, compare = FALSE) {
  # Package Management ------------------------------------------------------
  
  pkgs <- c("dplyr", "tidyr", "exuber", "zoo", "openxlsx", "fs", "ihpdr", 
            "forcats", "here", "devtools")
  
  miss_pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if (length(miss_pkgs)) {
    install.packages(miss_pkgs)
  }
  # devtools::install_github("kvasilopoulos/ihpdr", quiet = TRUE)
  
  # Load libraries ----------------------------------------------------------
  
  suppressPackageStartupMessages({
    library(tidyverse)
    library(exuber)
    library(ihpdr)
  })
  
  full_data <- ihpdr::ihpd_get(version = vers) %>% 
    mutate_at(vars(hpi, rhpi, pdi, rpdi), as.numeric) %>% 
    filter(country != "Aggregate") %>% 
    filter(!country %in% c("Aggregate - 2005 Fixed Weights", "Aggregate - Dynamic Weights" )) %>% 
    drop_na()
  
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
  
  minw <- psy_minw(price)
  
  suppressMessages({
    radf_price1 <- radf(price, lag = 1, minw = minw)
    radf_price4 <- radf(price, lag = 4, minw = minw)
    radf_income1 <- radf(income, lag = 1, minw = minw)
    radf_income4 <- radf(income, lag = 4, minw = minw)
  })
  
  n <- nrow(price)
  if(minw != psy_minw(price)){
    cv <- radf_mc_cv(n, minw = 25)
  }else{
    cv <- radf_crit[[n]]
  }
  
  # Custom ordering assing target -------------------------------------------
  
  # target <- 
  #   c("Australia", "Belgium", "Canada", "Switzerland", "Germany", "Denmark", 
  #     "Spain", "Finland", "France", "UK", "Ireland", "Italy", "Japan", "S. Korea",
  #     "Luxembourg", "Netherlands", "Norway", "New Zealand", "Sweden", "US", 
  #     "S. Africa", "Croatia", "Israel", "Slovenia", "Colombia")
  target <-  unique(full_data$country)
  
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
    select(Date = true_date, crit, all_of(target))
  
  xdata_price_seq4 <-augment_join(radf_price4, cv) %>% 
    filter(stat == "bsadf", sig == 95) %>% 
    full_join(idx, by = "index") %>% 
    select(true_date, id, tstat, crit) %>% 
    arrange(true_date) %>% 
    pivot_wider(names_from = id, values_from = tstat) %>% 
    select(Date = true_date, crit, all_of(target))
  
  
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
  
  xdata_income_seq1 <-augment_join(radf_income1, cv) %>% 
    filter(stat == "bsadf", sig == 95) %>% 
    full_join(idx, by = "index") %>% 
    select(true_date, id, tstat, crit) %>% 
    arrange(true_date) %>% 
    pivot_wider(names_from = id, values_from = tstat) %>% 
    select(Date = true_date, crit, all_of(target))
  
  xdata_income_seq4 <-augment_join(radf_income4, cv) %>% 
    filter(stat == "bsadf", sig == 95) %>% 
    full_join(idx, by = "index") %>% 
    select(true_date, id, tstat, crit) %>% 
    arrange(true_date) %>% 
    pivot_wider(names_from = id, values_from = tstat) %>% 
    select(Date = true_date, crit, all_of(target))
  
  # start writing -----------------------------------------------------------
  
  library(openxlsx)
  
  # vers <- pull(price, Date)[n] %>%
  #   zoo::as.yearqtr() %>%
  #   format("%y0%q")
  
  all_versions <- ihpdr::ihpd_versions()
  # vers <- 1904 #all_versions[1]
  
  file_name <- paste0("hpta", vers, ".xlsx")
  if (fs::file_exists(here::here("versions", file_name))) {
    if (interactive()) {
      answer <- yesno::yesno2(sprintf("Whould you like to overwrite `%s`", file_name))
    }
  }
  
  
  # TODO automating filling columns without template - cellranger
  second_part <- 6 + ncol(xdata_price_seq1) + 2
  
  # cellranger::letter_to_num("G")
  
  # load template -----------------------------------------------------------
  
  wb <- loadWorkbook(here::here("template", tmplt_xlsx))
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
  writeData(wb, sheet = 2, xdata_income_seq1, startCol = cellranger::num_to_letter(second_part), 
            startRow = 4, keepNA = TRUE, colNames = FALSE)
  
  # Sheet 2: LAG=4 ----------------------------------------------------------
  
  writeData(wb, sheet = 3, xdata_cv, startCol = "B", 
            startRow = 5, colNames = FALSE)
  writeData(wb, sheet = 3, xdata_price4, startCol = "B", 
            startRow = 10, colNames = FALSE)
  writeData(wb, sheet = 3, xdata_income4, startCol = "D", 
            startRow = 10, colNames = FALSE)
  
  writeData(wb, sheet = 3, xdata_price_seq4, startCol = "G", startRow = 4, 
            keepNA = TRUE, colNames = FALSE)
  writeData(wb, sheet = 3, xdata_income_seq4, startCol = cellranger::num_to_letter(second_part), 
            startRow = 4,  keepNA = TRUE, colNames = FALSE)
  
  # Save Final Output -------------------------------------------------------
  
  suppressMessages(saveWorkbook(wb, here::here("versions", file_name), overwrite = TRUE))
  message(sprintf("Saving `%s` to `versions/%s`", file_name, file_name))
  
  
  # comparison -------------------------------------------------------------
  
  if(compare) {
    source("functions.R")
    # Write the names of the versions you want to compare
    
    old_version <- paste0("hpta", all_versions[2])
    new_version <- paste0("hpta", vers)
    
    compare(v1 = old_version, v2 = new_version)  
  }
  

}
