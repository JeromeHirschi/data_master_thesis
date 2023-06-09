# rm(list = ls())

path_gt <- paste0(here::here(),"/_data/google_trend_data")


csv_files_gt <- list.files(path = path_gt) %>% 
  as_tibble() %>% 
  filter(str_detect(value,".csv")) %>%
  mutate(value = substr(value,start = 1, stop = nchar(value)-4))


# weather
for(ifile in pull(csv_files_gt)) {
  if (str_detect(ifile, "weather")) {
    df_name <- ifile %>% tolower()
    df <- read_delim(paste0(path_gt, "/", ifile, ".csv"),
                     col_names = T, delim = ",") %>%
      select(-`...1`) %>% 
      mutate(category = tolower(category),
             variable = tolower(variable),
             canton = tolower(canton))
    assign(df_name, df)
  }
}

# absolut
for(ifile in pull(csv_files_gt)) {
  if (str_detect(ifile, "abs")) {
    df_name <- ifile %>% tolower()
    df <- read_delim(paste0(path_gt, "/", ifile, ".csv"),
                     col_names = T, delim = ",") %>% 
      select(-`...1`) %>% 
      mutate(variable = tolower(variable),
             canton = tolower(canton))
    assign(df_name, df)
  }
}

# geoMap
for(ifile in pull(csv_files_gt)) {
  if (str_detect(ifile, "geoMap")) {
    df_name <- ifile %>% tolower()
    df <- read_delim(paste0(path_gt, "/", ifile, ".csv"),
                     col_names = T, delim = ",", skip = 2) %>% 
      rename(canton_name = 1,
             value = 2)
    assign(df_name, df)
  }
}

geomap_m_1 <- geomap_m_1 %>% 
  rename(m_1 = 2)
geomap_m_2 <- geomap_m_2 %>% 
  rename(m_2 = 2)
geomap_m_3 <- geomap_m_3 %>% 
  rename(m_3 = 2)


gtrend_keyword_weather <- gtrend_keyword_weather_1 %>% 
  bind_rows(gtrend_keyword_weather_2) %>% 
  bind_rows(gtrend_keyword_weather_3) %>% 
  bind_rows(gtrend_keyword_weather_4) %>% 
  bind_rows(gtrend_keyword_weather_5) %>% 
  bind_rows(gtrend_keyword_weather_6) %>% 
  bind_rows(gtrend_keyword_weather_7) %>% 
  bind_rows(gtrend_keyword_weather_8)


gtrend_keyword_abs <- gtrend_keyword_abs_1 %>% 
  bind_rows(gtrend_keyword_abs_2) %>% 
  bind_rows(gtrend_keyword_abs_3) %>% 
  bind_rows(gtrend_keyword_abs_4) %>% 
  bind_rows(gtrend_keyword_abs_5) %>% 
  bind_rows(gtrend_keyword_abs_6) %>% 
  rename(absolut = value)

canton_code <- tibble(
  canton_name = c(
    "Geneva", "Basel-Stadt", "Canton of Zug", "Zurich", "Valais", "Vaud",
    "Canton of Obwalden", "Nidwalden", "Grisons", "Canton of Bern",
    "Canton of Schwyz", "Aargau", "Basel-Landschaft", "Canton of Uri",
    "Lucerne", "Canton of Schaffhausen", "Ticino", "Appenzell Outer Rhodes",
    "St. Gallen", "Appenzell Innerrhoden", "Canton of Fribourg",
    "Canton of Solothurn", "Canton of Glarus", "Canton of Neuchâtel",
    "Thurgau", "Canton of Jura"),
  canton = c(
    "GE", "BS", "ZG", "ZH", "VS", "VD", "OW", "NW", "GR", "BE", "SZ",
    "AG", "BL", "UR", "LU", "SH", "TI", "AR", "SG", "AI", "FR", "SO",
    "GL", "NE", "TG", "JU") ) %>% 
  mutate(canton = tolower(canton))

geo_map_canton <- geomap_m_1 %>% 
  left_join(geomap_m_2, by = "canton_name") %>% 
  left_join(geomap_m_3, by = "canton_name") %>% 
  mutate(m_sum = m_1+m_2+m_3) %>% 
  left_join(canton_code) %>% 
  select(canton, m_sum)

geo_map <- geo_map_canton %>% 
  mutate(total_sum = sum(m_sum),
         canton_weight = m_sum / total_sum) %>% 
  select(canton, canton_weight)


# geo_map %>%  
#   print(n=26)


  

gtrend <- left_join(gtrend_keyword_weather, gtrend_keyword_abs,
                    by = c("date", "variable", "canton"))


gtrend %>% filter(is.na(absolut)) %>% 
  group_by(canton) %>% 
  distinct(variable)

# gtrend %>% distinct(category) %>% arrange() %>%  View()

# gtrend_keyword_weather %>% 
#   group_by(canton) %>% 
#   filter(variable == "war") %>% 
#   filter(year(date) == 2020) %>% 
#   filter(canton == 'ag') %>% 
#   filter(date == min(date)) %>% 
#   print(n = 26)

gtrend_cat_size <- gtrend %>%
  group_by(canton, category) %>%
  summarise(cat_size = n_distinct(variable))

google_trends <- gtrend %>% 
  mutate(weight_var = value/100 * absolut) %>% 
  group_by(date, canton, category) %>% 
  summarise(weight_cat = sum(weight_var, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(gtrend_cat_size) %>% 
  mutate(cat_imp = weight_cat/cat_size) %>% 
  left_join(geo_map) %>% 
  mutate(value = cat_imp*canton_weight) %>% 
  rename(time = date) %>% 
  select(c(time, canton, category, value)) %>% 
  arrange(canton, category, time) %>% 
  mutate(category = gsub(" ", "_", category)) %>%
  mutate(category = factor(category, labels = unique(category)) ) ; google_trends



# -------------------------------------------------------------------------

google_trends %>% 
  distinct(category)

gtrend_sub_0 <- google_trends %>% 
  mutate(category = factor(category, labels = unique(category)) ) %>% 
  mutate(category = paste0("gt_", as.numeric(category), "_", str_sub(category, start = 1, end = 3), "_t")) %>% 
  pivot_wider(names_from = category, values_from = value)




gtrend_sub_1 <- google_trends %>% 
  mutate(category = factor(category, labels = unique(category)) ) %>% 
  group_by(canton, category) %>% 
  mutate(gtrend_1m = (dplyr::lag(value,1)
                      + dplyr::lag(value,2)
                      + dplyr::lag(value,3)
                      + dplyr::lag(value,4) /4 ) ,
         gtrend_2m = dplyr::lag(gtrend_1m,4) ) %>%
  drop_na() %>% 
  ungroup() %>% 
  select(time, canton, category, gtrend_1m) %>% 
  mutate(category = paste0("gt_", as.numeric(category), "_", str_sub(category, start = 1, end = 3), "_m1")) %>% 
  pivot_wider(names_from = category, values_from = gtrend_1m)


gtrend_sub_2 <- google_trends %>% 
  mutate(category = factor(category, labels = unique(category)) ) %>% 
  group_by(canton, category) %>% 
  mutate(gtrend_1m = (dplyr::lag(value,1)
                      + dplyr::lag(value,2)
                      + dplyr::lag(value,3)
                      + dplyr::lag(value,4) /4 ) ,
         gtrend_2m = dplyr::lag(gtrend_1m,4) ) %>%
  drop_na() %>% 
  ungroup() %>% 
  select(time, canton, category, gtrend_2m) %>% 
  mutate(category = paste0("gt_", as.numeric(category), "_", str_sub(category, start = 1, end = 3), "_m2")) %>% 
  pivot_wider(names_from = category, values_from = gtrend_2m)


gtrend_sub_m <- gtrend_sub_0 %>% 
  left_join(gtrend_sub_1, by = c("time", "canton")) %>% 
  left_join(gtrend_sub_2, by = c("time", "canton")) %>% 
  drop_na(); gtrend_sub_m


# -------------------------------------------------------------------------




# 
# 
# gtrend %>% 
#   group_by(canton, variable) %>% 
#   summarise(unique_vars = n_distinct(date)) %>% 
#   pivot_wider(names_from = variable, values_from = unique_vars) %>% 
#   ungroup() %>% 
#   select(-canton) %>%
#   summarise(across(everything(), ~all(. == first(.)))) %>%
#   t() %>%
#   as_tibble() %>%
#   rename(value = V1)


# gtrend %>% 
#   mutate(rel_imp = value/weather*absolut) %>% 
#   group_by(canton, category) %>% 
#   summarise(cat_w = )
# 
# 
# gtrend %>% 
#   filter(variable == "war" & canton == "zh") %>% 
#   mutate(rel_val = value/sum(value)*100,
#          rel_abs = absolut/sum(absolut)*100) %>% 
#   arrange(rel_val)
# 
# gtrend %>% 
#   filter(variable == "war" & canton == "zh") %>% 
#   mutate(rel_val = value/sum(value)*100,
#          rel_abs = absolut/sum(absolut)*100) %>% 
#   arrange(rel_abs)
# 
# 
# gtrend %>% 
#   filter(variable == "war" & canton == "zh") %>% 
#   mutate(rel_val = value/max(value)*100,
#          rel_abs = absolut/max(absolut)*100) %>% 
#   arrange(rel_val)
# 
# gtrend %>% 
#   filter(variable == "war" & canton == "zh") %>% 
#   mutate(rel_val = value/max(value)*100,
#          rel_abs = absolut/max(absolut)*100) %>% 
#   arrange(rel_abs)
# 
# gtrend %>% 
#   filter(variable == "war" & canton == "zh") %>% 
#   mutate(rel_val = value/sum(value)*100,
#          rel_abs = absolut/sum(absolut)*100) %>% 
#   View()


# -------------------------------------------------------------------------


# df_master %>% 
#   filter(agglo == "commuting zone") %>% 
#   filter(merch == "other") %>% 
#   filter(year(time)==2020)
#   
# 
# for (ds in csv_files_gt) {
#   rm(ls = as.character(ds))
# }  


