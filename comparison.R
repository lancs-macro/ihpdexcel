source("functions.R")

compare_versions <- function(versions = c("hpta2001.xlsx", "hpta2002.xlsx"), 
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

compare <- function(ver1, ver2) {
  
  v1 <- enquo(ver1) 
  v2 <- enquo(ver2)
  vs1 <- rlang::as_name(v1)
  vs2 <- rlang::as_name(v2)
  
  i_lags <- c(1,4)
  j_types <- c("rhpi", "ratio")
  
  for(i in i_lags) {
    for(j in j_types) {
      compare_versions(paste0(c(vs1, vs2), ".xlsx"), lags = i, types = j) %>% 
        drop_na() %>% 
        group_by(country) %>% 
        summarize(
          mse = mse(!!v1,!!v2)
        ) %>% 
        ggplot(aes(country, mse)) +
        geom_col(width = 0.8) +
        geom_hline(yintercept = 0.4, linetype = "dashed", color = "red") +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 90)
        )
      ggsave(glue("comparisons/hpta2002/{j}-lag{i}.png"))
    }
  }
}

compare(hpta2001, hpta2002)


compare_versions() %>% 
  drop_na() %>% 
  group_by(country) %>% 
  summarize(
    mse = mse(hpta2001,hpta2002)
  ) %>% 
  ggplot(aes(country, mse)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0.4, linetype = "dashed", color = "red") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggsave("comparisons/hpta2002/rhpi.png", width = 10, height = 4)  


