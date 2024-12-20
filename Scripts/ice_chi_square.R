### create historical_ice_df and combined_df using internal code in ice function

combined_df <- combined_df |> filter(Date < as.Date('2023-01-01'))

combined_df <- dplyr::bind_rows(historic_wq_ice_df, current_ice_df) |> select(Date = datetime,ice_presence)

##create daily df of historical ice 

ice_df_build <- data.frame(datetime = seq.Date(min(as.Date(historic_ice_df$datetime)), 
                                               max(as.Date(historic_ice_df$datetime)), 
                                               by = 'day'))
daily_historical_ice_df <- ice_df_build |> 
  dplyr::full_join(historic_ice_df, by = c('datetime')) |> 
  mutate(ice_presence = zoo::na.locf(ice_presence))


## read maint log 
log_read <- gsheet::gsheet2tbl(maint_log) |> 
  filter(Reservoir == ice_site, 
         DateTime < as.Date('2024-01-01'),
         DateTime > as.Date('2018-07-04')) |> 
  select(Reservoir, Date = DateTime, Maint_Ice_Presence = Ice_Presence, Flag_Ice_Presence)


## join visual obs with derived method to test together
ice_test_df <- log_read |> 
  left_join(combined_df, by = 'Date') |> 
  select(Date,Maint_Ice_Presence,ice_presence)

#ice_table <- table(ice_test_df$ice_presence, ice_test_df$Maint_Ice_Presence)
ice_table <- table(ice_test_df$Maint_Ice_Presence, ice_test_df$ice_presence)

## RESULTS
# chisq.test(ice_table)
# 
# data:  ice_table
# X-squared = 2.496e-32, df = 1, p-value = 1
