# rm(list = ls())

tmp <- df_master_m %>% 
  filter(merch == "retail_fuel") %>% 
  as.data.frame(); tmp


plm(scl_v ~ price_fitted     + price_fitted:cluster + 
      price_fitted_1m + price_fitted_1m:cluster + 
      price_fitted_2m + price_fitted_2m:cluster +
      # price_fitted:expenditure + price_fitted_1m:expenditure + price_fitted_2m:expenditure +
      
      gt_1_car_t + gt_2_com_t + gt_3_eco_t + 
      gt_4_ene_t + gt_5_gro_t + gt_6_hea_t +
      gt_7_rea_t + gt_8_res_t + gt_9_war_t +
      
      gt_1_car_m1 + gt_2_com_m1 + gt_3_eco_m1 + 
      gt_4_ene_m1 + gt_5_gro_m1 + gt_6_hea_m1 +
      gt_7_rea_m1 + gt_8_res_m1 + gt_9_war_m1 +
      
      gt_1_car_m2 + gt_2_com_m2 + gt_3_eco_m2 + 
      gt_4_ene_m2 + gt_5_gro_m2 + gt_6_hea_m2 +
      gt_7_rea_m2 + gt_8_res_m2 + gt_9_war_m2 +
      
      price_resid + price_resid:cluster + 
      price_resid_1m + price_resid_1m:cluster + 
      price_resid_2m + price_resid_2m:cluster
      # price_resid:expenditure + price_resid_1m:expenditure + price_resid_2m:expenditure
      , 
    model = "within",
    data = tmp,
    index = c("cluster", "time")) %>% 
  summary()




for (unit in c("_v")) {
  df_value <- df_master_m %>% 
    select(time, cluster, merch, paste0("scl", unit)) %>% 
    rename(value = 4)
  tmp2 <- df_value %>% 
    left_join(df_master_m, by = c("time", "cluster", "merch"))
  for(mercher in as.character(merchants)) {
    tmp1 <- tmp2 %>% 
      filter(merch == mercher) %>% 
      as.data.frame()
    
    print("Below")
    print(paste0("merch type: ", mercher))
    print(paste0("value type: ", unit))
    
    
    fit <- plm(scl_v ~ price_fitted     + price_fitted:cluster + 
                 price_fitted_1m + price_fitted_1m:cluster + 
                 price_fitted_2m + price_fitted_2m:cluster +
                 # price_fitted:expenditure + price_fitted_1m:expenditure + price_fitted_2m:expenditure +
                 
                 gt_1_car_t + gt_2_com_t + gt_3_eco_t + 
                 gt_4_ene_t + gt_5_gro_t + gt_6_hea_t +
                 gt_7_rea_t + gt_8_res_t + gt_9_war_t +
                 
                 gt_1_car_m1 + gt_2_com_m1 + gt_3_eco_m1 + 
                 gt_4_ene_m1 + gt_5_gro_m1 + gt_6_hea_m1 +
                 gt_7_rea_m1 + gt_8_res_m1 + gt_9_war_m1 +
                 
                 gt_1_car_m2 + gt_2_com_m2 + gt_3_eco_m2 + 
                 gt_4_ene_m2 + gt_5_gro_m2 + gt_6_hea_m2 +
                 gt_7_rea_m2 + gt_8_res_m2 + gt_9_war_m2 +
                 
                 price_resid + price_resid:cluster + 
                 price_resid_1m + price_resid_1m:cluster + 
                 price_resid_2m + price_resid_2m:cluster
               # price_resid:expenditure + price_resid_1m:expenditure + price_resid_2m:expenditure
               , 
               model = "within",
               data = tmp1,
               index = c("cluster", "time")) 
    
    print(summary(fit))
  }
}



for (unit in c("_n")) {
  df_value <- df_master_m %>% 
    select(time, cluster, merch, paste0("scl", unit)) %>% 
    rename(value = 4)
  tmp2 <- df_value %>% 
    left_join(df_master_m, by = c("time", "cluster", "merch"))
  for(mercher in as.character(merchants)) {
    tmp1 <- tmp2 %>% 
      filter(merch == mercher) %>% 
      as.data.frame()
    
    print("Below")
    print(paste0("merch type: ", mercher))
    print(paste0("value type: ", unit))
    
    
    fit <- plm(scl_v ~ price_fitted     + price_fitted:cluster + 
                 price_fitted_1m + price_fitted_1m:cluster + 
                 price_fitted_2m + price_fitted_2m:cluster +
                 # price_fitted:expenditure + price_fitted_1m:expenditure + price_fitted_2m:expenditure +
                 
                 gt_1_car_t + gt_2_com_t + gt_3_eco_t + 
                 gt_4_ene_t + gt_5_gro_t + gt_6_hea_t +
                 gt_7_rea_t + gt_8_res_t + gt_9_war_t +
                 
                 gt_1_car_m1 + gt_2_com_m1 + gt_3_eco_m1 + 
                 gt_4_ene_m1 + gt_5_gro_m1 + gt_6_hea_m1 +
                 gt_7_rea_m1 + gt_8_res_m1 + gt_9_war_m1 +
                 
                 gt_1_car_m2 + gt_2_com_m2 + gt_3_eco_m2 + 
                 gt_4_ene_m2 + gt_5_gro_m2 + gt_6_hea_m2 +
                 gt_7_rea_m2 + gt_8_res_m2 + gt_9_war_m2 +
                 
                 price_resid + price_resid:cluster + 
                 price_resid_1m + price_resid_1m:cluster + 
                 price_resid_2m + price_resid_2m:cluster
               # price_resid:expenditure + price_resid_1m:expenditure + price_resid_2m:expenditure
               , 
               model = "within",
               data = tmp1,
               index = c("cluster", "time")) 
    
    print(summary(fit))
  }
}




for (unit in c("_r")) {
  df_value <- df_master_m %>% 
    select(time, cluster, merch, paste0("scl", unit)) %>% 
    rename(value = 4)
  tmp2 <- df_value %>% 
    left_join(df_master_m, by = c("time", "cluster", "merch"))
  for(mercher in as.character(merchants)) {
    tmp1 <- tmp2 %>% 
      filter(merch == mercher) %>% 
      as.data.frame()
    
    print("Below")
    print(paste0("merch type: ", mercher))
    print(paste0("value type: ", unit))
    
    
    fit <- plm(scl_v ~ price_fitted     + price_fitted:cluster + 
                 price_fitted_1m + price_fitted_1m:cluster + 
                 price_fitted_2m + price_fitted_2m:cluster +
                 # price_fitted:expenditure + price_fitted_1m:expenditure + price_fitted_2m:expenditure +
                 
                 gt_1_car_t + gt_2_com_t + gt_3_eco_t + 
                 gt_4_ene_t + gt_5_gro_t + gt_6_hea_t +
                 gt_7_rea_t + gt_8_res_t + gt_9_war_t +
                 
                 gt_1_car_m1 + gt_2_com_m1 + gt_3_eco_m1 + 
                 gt_4_ene_m1 + gt_5_gro_m1 + gt_6_hea_m1 +
                 gt_7_rea_m1 + gt_8_res_m1 + gt_9_war_m1 +
                 
                 gt_1_car_m2 + gt_2_com_m2 + gt_3_eco_m2 + 
                 gt_4_ene_m2 + gt_5_gro_m2 + gt_6_hea_m2 +
                 gt_7_rea_m2 + gt_8_res_m2 + gt_9_war_m2 +
                 
                 price_resid + price_resid:cluster + 
                 price_resid_1m + price_resid_1m:cluster + 
                 price_resid_2m + price_resid_2m:cluster
               # price_resid:expenditure + price_resid_1m:expenditure + price_resid_2m:expenditure
               , 
               model = "within",
               data = tmp1,
               index = c("cluster", "time")) 
    
    print(summary(fit))
  }
}


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------




for (unit in c("_v")) {
  df_value <- df_master_m %>% 
    select(time, cluster, merch, paste0("scl", unit)) %>% 
    rename(value = 4)
  tmp2 <- df_value %>% 
    left_join(df_master_m, by = c("time", "cluster", "merch"))
  for(mercher in as.character(merchants)) {
    tmp1 <- tmp2 %>% 
      filter(merch == mercher) %>% 
      as.data.frame()
    
    print("Below")
    print(paste0("merch type: ", mercher))
    print(paste0("value type: ", unit))
    
    
    fit <- plm(scl_v ~ price_fitted     + price_fitted:cluster + 
                 price_fitted_1m + price_fitted_1m:cluster + 
                 price_fitted_2m + price_fitted_2m:cluster +
                 # price_fitted:expenditure + price_fitted_1m:expenditure + price_fitted_2m:expenditure +
                 
                 gt_1_car_t + gt_2_com_t + gt_3_eco_t + 
                 gt_4_ene_t + gt_5_gro_t + gt_6_hea_t +
                 gt_7_rea_t + gt_8_res_t + gt_9_war_t +
                 
                 lag(gt_1_car_t, 1:2) + lag(gt_2_com_t, 1:2) + lag(gt_3_eco_t, 1:2) + 
                 lag(gt_4_ene_t, 1:2) + lag(gt_5_gro_t, 1:2) + lag(gt_6_hea_t, 1:2) +
                 lag(gt_7_rea_t, 1:2) + lag(gt_8_res_t, 1:2) + lag(gt_9_war_t, 1:2) +
                 
                 price_resid + price_resid:cluster + 
                 price_resid_1m + price_resid_1m:cluster + 
                 price_resid_2m + price_resid_2m:cluster
               # price_resid:expenditure + price_resid_1m:expenditure + price_resid_2m:expenditure
               , 
               model = "within",
               data = tmp1,
               index = c("cluster", "time")) 
    
    print(summary(fit))
  }
}



for (unit in c("_n")) {
  df_value <- df_master_m %>% 
    select(time, cluster, merch, paste0("scl", unit)) %>% 
    rename(value = 4)
  tmp2 <- df_value %>% 
    left_join(df_master_m, by = c("time", "cluster", "merch"))
  for(mercher in as.character(merchants)) {
    tmp1 <- tmp2 %>% 
      filter(merch == mercher) %>% 
      as.data.frame()
    
    print("Below")
    print(paste0("merch type: ", mercher))
    print(paste0("value type: ", unit))
    
    
    fit <- plm(scl_v ~ price_fitted     + price_fitted:cluster + 
                 price_fitted_1m + price_fitted_1m:cluster + 
                 price_fitted_2m + price_fitted_2m:cluster +
                 # price_fitted:expenditure + price_fitted_1m:expenditure + price_fitted_2m:expenditure +
                 
                 gt_1_car_t + gt_2_com_t + gt_3_eco_t + 
                 gt_4_ene_t + gt_5_gro_t + gt_6_hea_t +
                 gt_7_rea_t + gt_8_res_t + gt_9_war_t +
                 
                 lag(gt_1_car_t, 1:2) + lag(gt_2_com_t, 1:2) + lag(gt_3_eco_t, 1:2) + 
                 lag(gt_4_ene_t, 1:2) + lag(gt_5_gro_t, 1:2) + lag(gt_6_hea_t, 1:2) +
                 lag(gt_7_rea_t, 1:2) + lag(gt_8_res_t, 1:2) + lag(gt_9_war_t, 1:2) +
                 
                 price_resid + price_resid:cluster + 
                 price_resid_1m + price_resid_1m:cluster + 
                 price_resid_2m + price_resid_2m:cluster
               # price_resid:expenditure + price_resid_1m:expenditure + price_resid_2m:expenditure
               , 
               model = "within",
               data = tmp1,
               index = c("cluster", "time")) 
    
    print(summary(fit))
  }
}



for (unit in c("_r")) {
  df_value <- df_master_m %>% 
    select(time, cluster, merch, paste0("scl", unit)) %>% 
    rename(value = 4)
  tmp2 <- df_value %>% 
    left_join(df_master_m, by = c("time", "cluster", "merch"))
  for(mercher in as.character(merchants)) {
    tmp1 <- tmp2 %>% 
      filter(merch == mercher) %>% 
      as.data.frame()
    
    print("Below")
    print(paste0("merch type: ", mercher))
    print(paste0("value type: ", unit))
    
    
    fit <- plm(scl_v ~ price_fitted     + price_fitted:cluster + 
                 price_fitted_1m + price_fitted_1m:cluster + 
                 price_fitted_2m + price_fitted_2m:cluster +
                 # price_fitted:expenditure + price_fitted_1m:expenditure + price_fitted_2m:expenditure +
                 
                 gt_1_car_t + gt_2_com_t + gt_3_eco_t + 
                 gt_4_ene_t + gt_5_gro_t + gt_6_hea_t +
                 gt_7_rea_t + gt_8_res_t + gt_9_war_t +
                 
                 lag(gt_1_car_t, 1:2) + lag(gt_2_com_t, 1:2) + lag(gt_3_eco_t, 1:2) + 
                 lag(gt_4_ene_t, 1:2) + lag(gt_5_gro_t, 1:2) + lag(gt_6_hea_t, 1:2) +
                 lag(gt_7_rea_t, 1:2) + lag(gt_8_res_t, 1:2) + lag(gt_9_war_t, 1:2) +
                 
                 price_resid + price_resid:cluster + 
                 price_resid_1m + price_resid_1m:cluster + 
                 price_resid_2m + price_resid_2m:cluster
               # price_resid:expenditure + price_resid_1m:expenditure + price_resid_2m:expenditure
               , 
               model = "within",
               data = tmp1,
               index = c("cluster", "time")) 
    
    print(summary(fit))
  }
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------



plm(scl_v ~ price_fitted     + price_fitted:cluster + 
      price_fitted_1m + price_fitted_1m:cluster + 
      price_fitted_2m + price_fitted_2m:cluster +
      # price_fitted:expenditure + price_fitted_1m:expenditure + price_fitted_2m:expenditure +
      
      gt_1_car_t + gt_2_com_t + gt_3_eco_t + 
      gt_4_ene_t + gt_5_gro_t + gt_6_hea_t +
      gt_7_rea_t + gt_8_res_t + gt_9_war_t +
      
      gt_1_car_m1 + gt_2_com_m1 + gt_3_eco_m1 + 
      gt_4_ene_m1 + gt_5_gro_m1 + gt_6_hea_m1 +
      gt_7_rea_m1 + gt_8_res_m1 + gt_9_war_m1 +
      
      gt_1_car_m2 + gt_2_com_m2 + gt_3_eco_m2 + 
      gt_4_ene_m2 + gt_5_gro_m2 + gt_6_hea_m2 +
      gt_7_rea_m2 + gt_8_res_m2 + gt_9_war_m2 +
      
      
      price_resid + price_resid:cluster + 
      price_resid_1m + price_resid_1m:cluster + 
      price_resid_2m + price_resid_2m:cluster
    # price_resid:expenditure + price_resid_1m:expenditure + price_resid_2m:expenditure
    , 
    model = "within",
    data = tmp,
    index = c("cluster", "time")) %>% 
  summary()


# OLD *********** ---------------------------------------------------------





tmp %>% str()

# plot
p <- df_eq1_fuel_v %>% ggplot() + 
  geom_line(aes(x=time, y=value, color=agglo)); p
df_eq1_fuel_v_gg <- p %>% ggplotly()

tmp2 <- tmp %>%
  group_by(agglo) %>% 
  mutate(price_fitted_1st_month = dplyr::lag(price_fitted,1)
         + dplyr::lag(price_fitted,2)
         + dplyr::lag(price_fitted,3)
         + dplyr::lag(price_fitted,4),
         price_fitted_2nd_month = dplyr::lag(price_fitted_1st_month,4)) %>%
  mutate(price_resid_1st_month = dplyr::lag(price_resid,1)
         + dplyr::lag(price_resid,2)
         + dplyr::lag(price_resid,3)
         + dplyr::lag(price_resid,4),
         price_resid_2nd_month = dplyr::lag(price_resid_1st_month,4)) %>%
  drop_na() %>% 
  ungroup() %>% 
  rename(agglo2 = agglo) %>% 
  mutate(agglo = agglo2, .after=1) %>% 
  select(-agglo2)

df_eq1_fuel_v22 <- tmp2 %>% 
  ts_data.frame()
df_eq1_fuel_v22 %>% str()



plm(value ~ price_fitted     + price_fitted:agglo + 
      price_fitted_1st_month + price_fitted_1st_month:agglo + 
      price_fitted_2nd_month + price_fitted_2nd_month:agglo +
      price_fitted:pp_dev + price_fitted_1st_month:pp_dev + price_fitted_2nd_month:pp_dev +
      price_fitted:expenditure + price_fitted_1st_month:expenditure + price_fitted_2nd_month:expenditure +
      
      price_resid + price_resid:agglo + 
      price_resid_1st_month + price_resid_1st_month:agglo + 
      price_resid_2nd_month + price_resid_2nd_month:agglo +
      price_resid:pp_dev + price_resid_1st_month:pp_dev + price_resid_2nd_month:pp_dev +
      price_resid:expenditure + price_resid_1st_month:expenditure + price_resid_2nd_month:expenditure, 
    model = "pooling",
    data = df_eq1_fuel_v22,
    index = c("agglo", "time")) %>% 
  summary()


plm(value ~ price_fitted + price_fitted:agglo + 
      price_fitted_1st_month + price_fitted_1st_month:agglo +
      price_fitted_2nd_month + price_fitted_2nd_month:agglo +
      price_resid + price_resid:agglo + 
      price_resid_1st_month + price_resid_1st_month:agglo + 
      price_resid_2nd_month + price_resid_2nd_month:agglo, 
    model = "within",
    data = df_eq1_fuel_v22,
    index = c("agglo", "time")) %>% 
  summary()




plm(value ~ price_fitted + 
      price_fitted_1st_month + price_fitted_2nd_month +
      price_resid + 
      price_resid_1st_month + price_resid_2nd_month, 
    model = "within",
    data = df_eq1_fuel_v22,
    index = c("agglo", "time")) %>% 
  summary()





plm(value ~ price_fitted + price_fitted:agglo + 
      price_fitted_1st_month + price_fitted_1st_month:agglo +
      price_resid + price_resid:agglo + 
      price_resid_1st_month + price_resid_1st_month:agglo, 
    model = "within",
    data = df_eq1_fuel_v22,
    index = c("time", "agglo")) %>% 
  summary()







# end data ----------------------------------------------------------------




# 4 lags
plm_fuel_v_4a <- plm(value~lag(price_fitted, 0:4) + lag(price_resid, 0:4), data = df_eq1_fuel_v,
                     model = "within", index = c("agglo"))
plm_fuel_v_4a %>%  summary()

plm_fuel_v_4b <- plm(value ~ price_fitted + price_fitted_1st_month + 
                       price_resid + price_resid_1st_month, data = df_eq1_fuel_v2,
                     model = "within", index = c("agglo"))
plm_fuel_v_4b %>%   summary()

# NEW: Morning ------------------------------------------------------------


df_eq1_fuel_v2 %>% summary()

df_eq1_fuel_v2 %>% distinct(agglo)
df_eq1_fuel_v2$agglo %>% class()
df_eq1_fuel_v2 %>% str()

df_eq1_fuel_v2 %>% ts_tbl


model_rest1 <- plm(value ~ price_fitted + price_fitted_1st_month,
                   data = df_eq1_fuel_v2,
                   model = "within", index = c("agglo", "time"))
model_rest1 %>% summary()


model_rest1b <- plm(value ~ price_fitted + price_fitted_1st_month +
                      price_resid + price_resid_1st_month, 
                    data = df_eq1_fuel_v2,
                    model = "within", index = c("agglo", "time"))
model_rest1b %>% summary()


model_rest1c <- plm(value ~ price_fitted + price_fitted_1st_month + price_fitted_2nd_month +
                      price_resid + price_resid_1st_month + price_resid_2nd_month, 
                    data = df_eq1_fuel_v2,
                    model = "within", index = c("agglo", "time"))
model_rest1c %>% summary()


# model_rest1c_time <- plm(value ~ price_fitted + price_fitted_1st_month + price_fitted_2nd_month +
#                       price_resid + price_resid_1st_month + price_resid_2nd_month +
#                       factor(time), 
#                     data = df_eq1_fuel_v2,
#                     model = "within", index = c("agglo", "time"))
# model_rest1c_time %>% summary()


model_rest1d <- plm(value ~ price_fitted + price_fitted_1st_month +
                      price_resid + price_resid_1st_month +
                      factor(time), 
                    data = df_eq1_fuel_v2,
                    model = "within", index = c("agglo", "time"))
model_rest1d %>% summary()



model_rest1e <- plm(value ~ price_fitted + price_fitted_1st_month + price_fitted_2nd_month +
                      price_resid + price_resid_1st_month + price_resid_2nd_month, 
                    data = df_eq1_fuel_v2,
                    model = "within", index = c("agglo", "time"), effect = "individual")
model_rest1e %>% summary()


model_rest1f <- plm(value ~ price_fitted + price_fitted_1st_month + price_fitted_2nd_month +
                      price_resid + price_resid_1st_month + price_resid_2nd_month, 
                    data = df_eq1_fuel_v2,
                    model = "random", index = c("agglo", "time"))
model_rest1f %>% summary()

# pFtest(UNrestricted, Restricted)

pFtest(model_rest1c, model_rest1b)
# pFtest(model_rest1b, model_rest1c) WRONG ORDER
pFtest(model_rest1e, model_rest1c) 
pFtest(model_rest1c, model_rest1e)


phtest(model_rest1f, model_rest1c)

# time

# pFtest(model_rest1c, model_rest1c_time)
pFtest(model_rest1c_time, model_rest1c) # time is relevant




pFtest(model_rest1, model_rest1b)

pFtest(model_rest1b, model_rest1)

plmtest(model_rest1b, c("time"), type=("bp"))
pFtest(model_rest1d, model_rest1b)


t1 <- model_rest1 %>% summary()
t2 <- model_rest1b %>% summary()

t1 %>% dput()
t2 %>% dput()


model_rest2 <- plm(value ~ price_fitted + price_fitted_1st_month + price_fitted_2nd_month +
                     price_resid + price_resid_1st_month + price_resid_2nd_month, data = df_eq1_fuel_v2,
                   model = "within", index = c("agglo"))
model_rest2 %>%   summary()

model_rest2b <- plm(value ~ price_fitted + price_fitted_1st_month + price_fitted_2nd_month +
                      price_resid + price_resid_1st_month + price_resid_2nd_month, data = df_eq1_fuel_v2,
                    model = "within", index = c("agglo", 'time'))

###



plm_fuel_v_4b <- plm(value ~ price_fitted + price_fitted:agglo + 
                       price_fitted_1st_month + price_fitted_1st_month:agglo + 
                       price_fitted_2nd_month + price_fitted_2nd_month:agglo +
                       price_resid + price_resid:agglo + 
                       price_resid_1st_month + price_resid_1st_month:agglo + 
                       price_resid_2nd_month + price_resid_2nd_month:agglo +
                       agglo, 
                     data = df_eq1_fuel_v2,
                     model = "within", index = c("agglo", "time"))
plm_fuel_v_4b %>% summary()

plm_fuel_v_4b_random <- plm(value ~ price_fitted + 
                              price_fitted_1st_month + 
                              price_fitted_2nd_month + 
                              price_resid +  
                              price_resid_1st_month + 
                              price_resid_2nd_month , 
                            data = df_eq1_fuel_v2,
                            model = "random", index = c("agglo", "time"))
plm_fuel_v_4b_random %>% summary()

phtest(plm_fuel_v_4b_random, plm_fuel_v_4b)

pFtest(plm_fuel_v_4b, model_rest1b)
pFtest(plm_fuel_v_4b, model_rest1c)
# The two above tests motivate the merger of agglomeration types,
# to a total of 3 different groups


# NEW ---------------------------------------------------------------------

## Pooled
m_p <- plm(value ~ price_fitted + price_fitted:agglo + 
             price_fitted_1st_month + price_fitted_1st_month:agglo + 
             price_fitted_2nd_month + price_fitted_2nd_month:agglo +
             price_resid + price_resid:agglo + 
             price_resid_1st_month + price_resid_1st_month:agglo + 
             price_resid_2nd_month + price_resid_2nd_month:agglo, 
           data = df_eq1_fuel_v2,
           model = "pooling", index = c("agglo", "time"))
m_p %>% summary()

m_p2 <- plm(value ~ price_fitted + 
              price_fitted_1st_month + 
              price_fitted_2nd_month + 
              price_resid +  
              price_resid_1st_month + 
              price_resid_2nd_month , 
            data = df_eq1_fuel_v2,
            model = "pooling", index = c("agglo", "time"))
m_p %>% summary()

## Within
m_w <- plm(value ~ price_fitted + price_fitted:agglo + 
             price_fitted_1st_month + price_fitted_1st_month:agglo + 
             price_fitted_2nd_month + price_fitted_2nd_month:agglo +
             price_resid + price_resid:agglo + 
             price_resid_1st_month + price_resid_1st_month:agglo + 
             price_resid_2nd_month + price_resid_2nd_month:agglo, 
           data = df_eq1_fuel_v2,
           model = "within", index = c("agglo", "time"), effect = "individual")
m_w %>% summary()

# Fixed effects test: Ho:'No fixed effects'
pFtest(m_w, m_p)

m_w2 <- plm(value ~ price_fitted + 
              price_fitted_1st_month + 
              price_fitted_2nd_month + 
              price_resid +  
              price_resid_1st_month + 
              price_resid_2nd_month , 
            data = df_eq1_fuel_v2,
            model = "within", index = c("agglo", "time"), effect = "individual")
m_w2 %>% summary()


# Fixed effects test: Ho:'No fixed effects'
pFtest(m_w2, m_p2)

## Random
m_r <- plm(value ~ price_fitted + 
             price_fitted_1st_month + 
             price_fitted_2nd_month + 
             price_resid +  
             price_resid_1st_month + 
             price_resid_2nd_month , 
           data = df_eq1_fuel_v2,
           model = "random", index = c("agglo", "time"), effect = "individual")
m_r %>% summary()

# A random effects test for the wage equation
plmtest(m_p, effect="individual")
plmtest(m_p2, effect="individual")


# Hausman endogeneity test for the random effects wage model
phtest(m_w, m_r)
phtest(m_w2, m_r)



# FINISHED HERE -----------------------------------------------------------


pFtest(model_rest1b, plm_fuel_v_4b)


pFtest(plm_fuel_v_4b, model_rest1c)
pFtest(model_rest1c, plm_fuel_v_4b)

model_rest1d

# ?plmtest
pFtest(model_rest1, model_rest1b)
pFtest(model_rest2, model_rest2b)
pFtest(model_rest1, model_rest2)
pFtest(model_rest1b, model_rest2b)

pFtest(model_rest2b, plm_fuel_v_4b)

model_rest1 %>% summary()
pFtest(value ~ price_fitted + price_fitted_1st_month +
         price_resid + price_resid_1st_month, data = df_eq1_fuel_v2,
       model = "within", index = c("agglo", "time")) 


# Reject H0 of homoskedasticity is rejected and heteroskedasticity assumed.
plmtest(plm_fuel_v_4b, type = "bp")
lmtest::bgtest(plm_fuel_v_4b)



waldtest(model_rest, plm_fuel_v_4b, vcov = vcovHC(plm_fuel_v_4b, type = "HC0"))

waldtest(model_rest, plm_fuel_v_4b, vcov = vcovHC(plm_fuel_v_4b, type = "HC3"))

waldtest(model_rest, plm_fuel_v_4b, vcov = vcovSCC(plm_fuel_v_4b, cluster = "group", type = "HC3"))



# ?vcovHC.plm
c1 <- coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "group", type = "HC3")); c1
c2 <- coeftest(plm_fuel_v_4b, vcovHC(plm_fuel_v_4b, cluster = "group", type = "HC3")); c2 # same as vcovHC
c3 <- coeftest(plm_fuel_v_4b, vcovHC(plm_fuel_v_4b, method = "arellano", cluster = "group", type = "HC3")); c3
c4 <- coeftest(plm_fuel_v_4b, vcovHC(plm_fuel_v_4b, type = "HC3")); c4
c5 <- coeftest(plm_fuel_v_4b, vcovHC(plm_fuel_v_4b, method = "arellano", type = "HC3")); c5

c6 <- coeftest(plm_fuel_v_4b, vcovHC(plm_fuel_v_4b, cluster = "time", type = "HC3")); c6 # different


# c1 %>% dput()
identical(c1, c2)
identical(c1, c3)

identical(c2, c3)

identical(c2, c4)
identical(c3, c5)
identical(c4, c5)

c1 == c2

coeftest(plm_fuel_v_4b, vcovHC(plm_fuel_v_4b, cluster = "group", type = "sss"))

coeftest(plm_fuel_v_4b, vcovHC(plm_fuel_v_4b, method = "white1", cluster = "group", type = "HC3"))
coeftest(plm_fuel_v_4b, vcovHC(plm_fuel_v_4b, method = "white2", cluster = "group", type = "HC3"))







# coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "time", type = "HC3"))
# 
# coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "group", type = "sss"))
# coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "time", type = "sss"))
# 
# coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "group", type = "HC0"))
# coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "time", type = "HC0"))
# 
# coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "group", type = "HC1"))
# coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "time", type = "HC1"))
# 
# coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "group", type = "HC2"))
# coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "time", type = "HC2"))
# 
# coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "group", type = "HC4"))
# coeftest(plm_fuel_v_4b, vcovSCC(plm_fuel_v_4b, cluster = "time", type = "HC4"))



?sandwich::vcovHC


# figuring out panel data

plm_fuel_v_4b <- plm(value ~ price_fitted + price_fitted_1st_month + 
                       price_fitted:agglo + price_fitted_1st_month:agglo +
                       price_resid + price_resid_1st_month +
                       price_resid:agglo + price_resid_1st_month:agglo +
                       agglo, 
                     data = df_eq1_fuel_v2)
plm_fuel_v_4b %>%   summary()



plm_fuel_v_4b <- lm(value ~ price_fitted + price_fitted_1st_month + 
                      price_fitted:agglo + price_fitted_1st_month:agglo +
                      price_resid + price_resid_1st_month +
                      price_resid:agglo + price_resid_1st_month:agglo +
                      agglo, 
                    data = df_eq1_fuel_v2)
plm_fuel_v_4b %>%   summary()







# END: MORNING ------------------------------------------------------------



# 8 lags
plm_fuel_v_8a <- plm(value~lag(price_fitted, 0:8) + lag(price_resid, 0:8), data = df_eq1_fuel_v,
                     model = "within", index = c("agglo"))
plm_fuel_v_8a %>% summary()

plm_fuel_v_8b <- plm(value ~ price_fitted + price_fitted_1st_month + price_fitted_2nd_month + 
                       price_resid + price_resid_1st_month + price_resid_2nd_month, data = df_eq1_fuel_v2,
                     model = "within", index = c("agglo"))
plm_fuel_v_8b %>% summary()


## total number
tmp <- fit_df_sp95 %>%
  filter(id != "sp95") %>%
  mutate(id = if_else(id=="fitted values", "price_fitted", "price_resid")) %>%
  ts_wide() %>% 
  left_join(agglo_retail_fuel_n, by = "time") %>% 
  rename(agglo = id)

df_eq1_fuel_n <- tmp %>% 
  ts_data.frame()

# plot
p <- df_eq1_fuel_n %>% ggplot() + 
  geom_line(aes(x=time, y=value, color=agglo))
df_eq1_fuel_n_gg <- p %>% ggplotly()

df_eq1_fuel_n2 <-   tmp %>% 
  group_by(agglo) %>% 
  mutate(price_fitted_1st_month = dplyr::lag(price_fitted,1)
         + dplyr::lag(price_fitted,2)
         + dplyr::lag(price_fitted,3)
         + dplyr::lag(price_fitted,4),
         price_fitted_2nd_month = dplyr::lag(price_fitted_1st_month,4)) %>%
  mutate(price_resid_1st_month = dplyr::lag(price_resid,1)
         + dplyr::lag(price_resid,2)
         + dplyr::lag(price_resid,3)
         + dplyr::lag(price_resid,4),
         price_resid_2nd_month = dplyr::lag(price_resid_1st_month,4)) %>%
  drop_na() %>% 
  ungroup()%>% 
  ts_data.frame()


# 4 lags
plm_fuel_n_4a <- plm(value~lag(price_fitted, 0:4) + lag(price_resid, 0:4), data = df_eq1_fuel_n,
                     model = "within", index = c("agglo"))
plm_fuel_n_4a %>%  summary()

plm_fuel_n_4b <- plm(value ~ price_fitted + price_fitted_1st_month + 
                       price_resid + price_resid_1st_month, data = df_eq1_fuel_n2,
                     model = "within", index = c("agglo"))
plm_fuel_n_4b %>%   summary()

# 8 lags
plm_fuel_n_8a <- plm(value~lag(price_fitted, 0:8) + lag(price_resid, 0:8), data = df_eq1_fuel_n,
                     model = "within", index = c("agglo"))
plm_fuel_n_8a %>% summary()

plm_fuel_n_8b <- plm(value ~ price_fitted + price_fitted_1st_month + price_fitted_2nd_month + 
                       price_resid + price_resid_1st_month + price_resid_2nd_month, data = df_eq1_fuel_n2,
                     model = "within", index = c("agglo"))
plm_fuel_n_8b %>% summary()


## ratio: value/number
tmp <- fit_df_sp95 %>%
  filter(id != "sp95") %>%
  mutate(id = if_else(id=="fitted values", "price_fitted", "price_resid")) %>%
  ts_wide() %>% 
  left_join(agglo_retail_fuel_r_v_n, by = "time") %>% 
  rename(agglo = id)

df_eq1_fuel_r_v_n <- tmp %>% 
  ts_data.frame()

# plot
p <- df_eq1_fuel_r_v_n %>% ggplot() + 
  geom_line(aes(x=time, y=value, color=agglo))
df_eq1_fuel_r_v_n_gg <- p %>% ggplotly()


df_eq1_fuel_r_v_n2 <-   tmp %>% 
  group_by(agglo) %>% 
  mutate(price_fitted_1st_month = dplyr::lag(price_fitted,1)
         + dplyr::lag(price_fitted,2)
         + dplyr::lag(price_fitted,3)
         + dplyr::lag(price_fitted,4),
         price_fitted_2nd_month = dplyr::lag(price_fitted_1st_month,4)) %>%
  mutate(price_resid_1st_month = dplyr::lag(price_resid,1)
         + dplyr::lag(price_resid,2)
         + dplyr::lag(price_resid,3)
         + dplyr::lag(price_resid,4),
         price_resid_2nd_month = dplyr::lag(price_resid_1st_month,4)) %>%
  drop_na() %>% 
  ungroup()%>% 
  ts_data.frame()


# 4 lags
plm_fuel_r_v_n_4a <- plm(value~lag(price_fitted, 0:4) + lag(price_resid, 0:4), data = df_eq1_fuel_r_v_n,
                         model = "within", index = c("agglo"))
plm_fuel_r_v_n_4a %>%  summary()

plm_fuel_r_v_n_4b <- plm(value ~ price_fitted + price_fitted_1st_month + 
                           price_resid + price_resid_1st_month, data = df_eq1_fuel_r_v_n2,
                         model = "within", index = c("agglo"))
plm_fuel_r_v_n_4b %>%   summary()

# 8 lags
plm_fuel_r_v_n_8a <- plm(value~lag(price_fitted, 0:8) + lag(price_resid, 0:8), data = df_eq1_fuel_r_v_n,
                         model = "within", index = c("agglo"))
plm_fuel_r_v_n_8a %>% summary()

plm_fuel_r_v_n_8b <- plm(value ~ price_fitted + price_fitted_1st_month + price_fitted_2nd_month + 
                           price_resid + price_resid_1st_month + price_resid_2nd_month, data = df_eq1_fuel_r_v_n2,
                         model = "within", index = c("agglo"))
plm_fuel_r_v_n_8b %>% summary()





###
# knitr::kable(plm_l4a, plm_l8a, plm_l4b, plm_l8b)

# tab1 <- stargazer::stargazer(plm_l4a, plm_l8a, plm_l4b, plm_l8b, type = "html")
