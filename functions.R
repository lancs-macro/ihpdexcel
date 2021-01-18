library(readxl)
library(tidyverse)
library(glue)

format_excel_bsadf <- function(x, nm, nms, ...) {
  x %>% slice(-1) %>% set_names(nms) %>%
    dplyr::rename(Date = 1) %>%
    dplyr::mutate(Date = Date %>% lubridate::yq()) %>%
    tidyr::gather(country, !!enquo(nm), -Date, -crit, ...)
}

ihpd_get_bsadf_local <- function(tf = NULL) {
  nms <- read_excel(tf, sheet = 2, range = "G2:BQ2", .name_repair = "minimal") %>%
    names() %>% unique()
  nms[1:2] <- c("Date", "crit")
  nms <- nms[nms != ""]
  
  suppressMessages({
    lag1 <- readxl::read_excel(tf, sheet = 2, skip = 1, na = c("", "NA"))[,-c(1:6)]
    lag4 <- readxl::read_excel(tf, sheet = 3, skip = 1, na = c("", "NA"))[,-c(1:6)]
  })
  
  split_at <- which(colSums(is.na(lag1)) == nrow(lag1))
  
  tbl_lag1 <-
    list(
      format_excel_bsadf(lag1[, 1:(split_at - 1)], nm = "rhpi", nms),
      format_excel_bsadf(lag1[, (split_at + 1):ncol(lag1)], nm = "ratio", nms)
    ) %>%
    reduce(full_join, by = c("Date", "crit", "country")) %>%
    mutate(lag = 1) %>%
    gather(type, value, -Date, -country, -crit, -lag)
  
  tbl_lag4 <-
    list(
      format_excel_bsadf(lag4[, 1:(split_at - 1)], nm = "rhpi", nms),
      format_excel_bsadf(lag4[, (split_at + 1):ncol(lag1)], nm = "ratio", nms)
    ) %>%
    reduce(full_join, by = c("Date", "crit", "country")) %>%
    mutate(lag = 4) %>%
    gather(type, value, -Date, -country, -crit, -lag)
  
  full_join(tbl_lag1, tbl_lag4,
            by = c("country", "Date", "crit", "type", "lag", "value")) %>%
    select(Date, country, type, lag, value, crit) 
  
}


# comparison --------------------------------------------------------------

compare_versions <- function(versions = c(old_version, new_version), #c("hpta2002.xlsx", "hpta2003.xlsx"), 
                             lags = 1, types = "rhpi") {
  suppressWarnings({
    vrs <- gsub(".xlsx", "", versions)
    hpta <- list()
    for(i in 1:length(versions)) {
      hpta[[i]] <- ihpd_get_bsadf_local(paste0("versions/", versions[i])) %>%
        filter(type == types, lag == lags) %>%
        select(-crit) %>%
        drop_na() %>% 
        set_names(c("Date", "country", "type", "lag", vrs[i]))
    }
    hpta %>% 
      reduce(full_join, by = c("Date", "country", "type", "lag")) %>% 
      select(-type, -lag) 
  })
}

mse <- function(e1, e2) {
  ds <- (e1 - e2)^2
  mean(ds)
}

compare <- function(v1, v2) {
 
  i_lags <- c(1,4)
  j_types <- c("rhpi", "ratio")
  
  dir_name <- glue("comparisons/{v2}")
  fs::dir_create(dir_name)
  message(glue("Creating {dir_name} directory."))
  
  for(i in i_lags) {
    for(j in j_types) {
      compare_versions(paste0(c(v1, v2), ".xlsx"), lags = i, types = j) %>% 
        drop_na() %>% 
        group_by(country) %>% 
        summarize(
          mse = mse(!!rlang::sym(v1),!!rlang::sym(v2)),
          .groups = "drop"
        ) %>% 
        ggplot(aes(country, mse)) +
        geom_col(width = 0.8) +
        geom_hline(yintercept = 0.4, linetype = "dashed", color = "red") +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 90)
        )
      ggsave(glue("comparisons/{v2}/{j}-lag{i}.png"))
    }
  }
}
