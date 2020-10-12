source("functions.R")

hpta1903 <- ihpd_get_bsadf_local("versions/hpta1903.xlsx")
hpta1904 <- ihpd_get_bsadf_local("versions/hpta1904.xlsx")
hpta2001 <- ihpd_get_bsadf_local("versions/hpta2001.xlsx")


hpta1903 <- hpta1903 %>% 
  filter(type == "ratio", lag == 4) %>% 
  select(-crit) %>% 
  drop_na() %>% 
  rename("hpta1903" = value)
hpta1904 <- hpta1904 %>% 
  filter(type == "ratio", lag == 4) %>% 
  select(-crit) %>% 
  drop_na() %>% 
  rename("hpta1904" = value)
hpta2001 <- hpta2001 %>% 
  filter(type == "ratio", lag == 4) %>% 
  select(-crit) %>% 
  drop_na() %>% 
  rename("hpta2001" = value)


compare_versions <- function(versions = c("hpta1903.xlsx", "hpta1904.xlsx", "hpta2001.xlsx"), 
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

compare_versions(types = "ratio") %>% 
  drop_na() %>% 
  group_by(country) %>% 
  summarise(
    mse_1904_1903 = mse(hpta1903, hpta1904), 
    mse_2001_1904 = mse(hpta2001, hpta1904)
  ) %>% 
  pivot_longer(cols = starts_with("mse")) %>% 
  ggplot(aes(country, value, fill = name)) +
  geom_col(width = 1)
  

list(hpta1903, hpta1904, hpta2001) %>% 
  reduce(full_join, by = c("Date", "country", "type", "lag")) %>% 
  select(-type, -lag) %>% 
  mutate(diff_1904_1903 = hpta1904 - hpta1903, diff_2001_1904 = hpta2001 - hpta1904) %>% 
  # mutate(mse_1904_1903 = mean((hpta1904 - hpta1903)^2), mse_2001_1904 = mean((hpta2001 - hpta1904)^2)) %>% 
  pivot_longer(cols = starts_with("hpta"), names_to = "version", values_to = "hpta") %>%
  pivot_longer(cols = starts_with("diff"), names_to = "diff_version", values_to = "diff") %>%
  # pivot_longer(cols = starts_with("mse"), names_to = "mse_version", values_to = "mse") %>%
  # ggplot(aes(Date, hpta, col = version)) +
  ggplot(aes(Date, diff, col = diff_version)) +
  # ggplot(aes(Date, mse, col = diff_version)) +
  geom_line() +
  facet_wrap(~country, scales = "free_y") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    axis.title = element_blank()
  ) +
  ggtitle("Price-to-Income Ratio (lag = 1)")


ggsave("price-to-income-diff.png")
