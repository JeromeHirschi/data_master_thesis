# rm(list = ls())




# google trends: cantons to 3 clusters ------------------------------------

cluster_for_canton


gtrend_cluster <- gtrend_keyword_weather %>% 
  left_join(gtrend_keyword_abs,
            by = c("date", "variable", "canton")) %>% 
  left_join(geo_map_canton,
            by = c("canton")) %>% 
  left_join(cluster_for_canton,
            by = c("canton"))


gtrend_cat_size_cluster <- gtrend_cluster %>%
  group_by(cluster, category) %>%
  summarise(cat_size = n_distinct(variable))

geo_map_cluster <- geo_map_canton %>% 
  left_join(cluster_for_canton,
            by = c("canton")) %>% 
  select(-canton) %>% 
  group_by(cluster) %>% 
  summarise(m_sum = sum(m_sum)) %>% 
  ungroup() %>% 
  mutate(total_sum = sum(m_sum),
         cluster_weight = m_sum / total_sum) %>% 
  select(1,4)
  


google_trends_cluster2 <- gtrend_cluster %>% 
  mutate(weight_var = value/100 * absolut) %>% 
  group_by(date, cluster, category) %>% 
  summarise(weight_cat = sum(weight_var, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(gtrend_cat_size_cluster, by = c("cluster", "category")) %>% 
  mutate(cat_imp = weight_cat/cat_size) %>% 
  left_join(geo_map_cluster) %>% 
  mutate(value = cat_imp*cluster_weight) %>% 
  rename(time = date) %>% 
  select(c(time, cluster, category, value)) %>% 
  arrange(cluster, category, time); google_trends_cluster2
  

# google_trends_cluster3 <- google_trends_cluster2 %>% 
#   mutate(id = paste0(cluster,"YY", category), .before=1) %>% 
#   select(-c(cluster, category)) %>%
#   mutate(lvalue = dplyr::lag(value)) %>%
#   drop_na(lvalue) %>% 
#   mutate(value = log(value) - log(lvalue)) %>% 
#   select(-lvalue) %>% 
#   separate(id, into = c("cluster", "category"), sep = "YY") ; google_trends_cluster3


# google_trends_cluster2 %>%
#   mutate(id = paste0(cluster,"YY", category), .before=1) %>%
#   select(-c(cluster, category)) %>%
#   group_by(id) %>%
#   ts_pc() %>%
#   drop_na(value)
#   separate(id, into = c("cluster", "category"), sep = "YY")



cluster_factors_canton <- cluster_for_canton %>% 
  distinct(cluster) %>% 
  mutate(cluster_name = as.character(cluster))


google_trends_cluster <- google_trends_cluster2 %>% 
  rename(cluster_name = cluster) %>% 
  left_join(cluster_factors_canton) %>% 
  select(time, cluster, category, value) %>% 
  mutate(category = gsub(" ", "_", category)); google_trends_cluster


# -------------------------------------------------------------------------


gtrend_sub_0 <- google_trends_cluster %>% 
  mutate(category = factor(category, labels = unique(category)) ) %>% 
  mutate(category = paste0("gt_", as.numeric(category), "_", str_sub(category, start = 1, end = 3), "_t")) %>% 
  pivot_wider(names_from = category, values_from = value)




gtrend_sub_1 <- google_trends_cluster %>% 
  mutate(category = factor(category, labels = unique(category)) ) %>% 
  group_by(cluster, category) %>% 
  mutate(gtrend_1m = (dplyr::lag(value,1)
                      + dplyr::lag(value,2)
                      + dplyr::lag(value,3)
                      + dplyr::lag(value,4) /4 ) ,
         gtrend_2m = dplyr::lag(gtrend_1m,4) ) %>%
  drop_na() %>% 
  ungroup() %>% 
  select(time, cluster, category, gtrend_1m) %>% 
  mutate(category = paste0("gt_", as.numeric(category), "_", str_sub(category, start = 1, end = 3), "_m1")) %>% 
  pivot_wider(names_from = category, values_from = gtrend_1m)


gtrend_sub_2 <- google_trends_cluster %>% 
  mutate(category = factor(category, labels = unique(category)) ) %>% 
  group_by(cluster, category) %>% 
  mutate(gtrend_1m = (dplyr::lag(value,1)
                      + dplyr::lag(value,2)
                      + dplyr::lag(value,3)
                      + dplyr::lag(value,4) /4 ) ,
         gtrend_2m = dplyr::lag(gtrend_1m,4) ) %>%
  drop_na() %>% 
  ungroup() %>% 
  select(time, cluster, category, gtrend_2m) %>% 
  mutate(category = paste0("gt_", as.numeric(category), "_", str_sub(category, start = 1, end = 3), "_m2")) %>% 
  pivot_wider(names_from = category, values_from = gtrend_2m)


gtrend_sub_m <- gtrend_sub_0 %>% 
  left_join(gtrend_sub_1, by = c("time", "cluster")) %>% 
  left_join(gtrend_sub_2, by = c("time", "cluster")) %>% 
  drop_na(); gtrend_sub_m



# noga: agglo to 3 clusters -----------------------------------------------


cluster_for_agglo


noga_cluster1 <- noga_agglo %>% 
  filter(agglomeration_type != "9999") %>% 
  mutate(geo_fac = agglomeration_type %>% as.character() %>% as.factor(), .after = 1) %>% 
  select(-agglomeration_type)

noga_cluster2 <- noga_cluster1 %>% 
  left_join(cluster_for_agglo) %>% 
  group_by(time, cluster, merchant_category) %>% 
  summarise(
    scl_value = sum(scl_value),
    scl_n_trans = sum(scl_n_trans)
  ) %>% 
  ungroup()


noga_cluster_week <- noga_cluster2 %>% 
  mutate(id = paste0(cluster,"XX",merchant_category), .before=1) %>% 
  select(-c(cluster, merchant_category)) %>% 
  pivot_longer(cols = c(scl_value, scl_n_trans)) %>% 
  mutate(id = paste0(name,"YY",id)) %>% 
  select(-name) %>% 
  arrange(id, time) %>% 
  ts_frequency(to = "week")

# expenditure cluster
expenditure_cluster <- noga_cluster_week %>% 
  separate(id, into = c("var", "cluster_merch"), sep = "YY") %>% 
  pivot_wider(values_from = value, names_from = var) %>% 
  rename(scl_n = scl_n_trans,
         scl_v = scl_value) %>% 
  separate(cluster_merch, into = c("cluster", "merch"), sep = "XX") %>% 
  filter(year(time)==2019) %>% 
  group_by(cluster) %>% 
  summarise(expenditure = mean(scl_v))



noga_cluster3 <- noga_cluster_week %>% 
  group_by(id) %>% 
  mutate(lvalue = dplyr::lag(value)) %>%
  drop_na(lvalue) %>% 
  mutate(value = log(value) - log(lvalue)) %>% 
  select(-lvalue) %>% 
  separate(id, into = c("var", "cluster_merch"), sep = "YY") %>% 
  pivot_wider(values_from = value, names_from = var) %>% 
  rename(scl_n = scl_n_trans,
         scl_v = scl_value) %>% 
  mutate(scl_r = scl_v - scl_n) %>% 
  separate(cluster_merch, into = c("cluster", "merch"), sep = "XX") %>% 
  select(time, cluster, merch, scl_v, scl_n, scl_r)




noga_cluster4 <- noga_cluster3 %>% 
  left_join(expenditure_cluster, by = "cluster")


cluster_factors_agglo <- cluster_for_agglo %>% 
  distinct(cluster) %>% 
  mutate(cluster_name = as.character(cluster))


noga_cluster5 <- noga_cluster4 %>% 
  rename(cluster_name = cluster) %>% 
  left_join(cluster_factors_agglo) %>% 
  select(time, cluster, merch, scl_v, scl_n, scl_r, expenditure)


noga_cluster5 %>% distinct(time)


# gas price: append -------------------------------------------------------

fit_df_sp95_tmp <- fit_df_sp95 %>%
  filter(id != "sp95") %>%
  mutate(id = if_else(id=="fitted values", "price_fitted", "price_resid")) %>%
  ts_wide() 


fit_df_sp95_tmp_m <- fit_df_sp95_tmp  %>% 
  mutate(price_fitted_1m = dplyr::lag(price_fitted,1)
         + dplyr::lag(price_fitted,2)
         + dplyr::lag(price_fitted,3)
         + dplyr::lag(price_fitted,4),
         price_fitted_2m = dplyr::lag(price_fitted_1m,4)) %>%
  mutate(price_resid_1m = dplyr::lag(price_resid,1)
         + dplyr::lag(price_resid,2)
         + dplyr::lag(price_resid,3)
         + dplyr::lag(price_resid,4),
         price_resid_2m = dplyr::lag(price_resid_1m,4)) %>%
  drop_na()



# create master DF --------------------------------------------------------


df_master <- fit_df_sp95_tmp %>% 
  right_join(noga_cluster5, by = c("time")) %>% 
  left_join(gtrend_sub_0, by = c("time", "cluster")) %>% 
  mutate(merch = factor(merch, labels = unique(merch)) ); df_master


df_master_m <- fit_df_sp95_tmp_m %>% 
  right_join(noga_cluster5, by = c("time")) %>% 
  left_join(gtrend_sub_m, by = c("time", "cluster")) %>% 
  mutate(merch = factor(merch, labels = unique(merch)) ); df_master



