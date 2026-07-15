#### HPB water level corrections and discharge calculations 
## Dexter Howard Jan 2026


#### packages
library(tidyverse)
library(ggpmisc) #stat poly line



#### Load data sets ####

#get corrected stages
stage <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCR_Inflow/2024/HPB_stage_2024_2025.csv")

daily_stage <- stage |> 
  mutate(Date = as.Date(DateTime_EST)) |> 
  group_by(Date) |> 
  summarise(stage_cm = mean(waterlevel_cm, na.rm = T))


#get manual water level
hpb_manual_stage <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1OOh1QfOad3ez_Hk4Sl8ziPYxmWWcoSv1GNeuJkrFec8/edit?gid=0#gid=0") 

hpb_manual_stage$Date = lubridate::parse_date_time(hpb_manual_stage$DateTime, orders = c('ymd HMS','ymd HM','ymd', 'mdy','mdy HM', 'mdy HMS', 'mdy HM'), tz = "America/New_York")

manstage <- hpb_manual_stage |> 
  mutate(Date = as.Date(Date)) |> 
  select(Date, manual_depth_cm)


#flowmate Q
flowmate <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/454/9/0e7fe16623a1ad2a67774c23ce8a29d8")

hpb_flowmate <- flowmate |> 
  filter(Reservoir == "CCR", 
         Site == 101,
         year(DateTime) > 2023) |> 
  mutate(Date = as.Date(DateTime)) |> 
  mutate(Flow_L_s = Flow_cms * 1000) |> 
  select(Date, Flow_L_s)



#### PT stage ~ manual measurements ####

##plot daily relationship for all data
left_join(daily_stage, manstage, by = "Date") |> 
  ggplot(aes(x = stage_cm, y = manual_depth_cm))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = stage_cm, y = manual_depth_cm, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  labs(title = "All data")

##plot daily relationship for 2024
left_join(daily_stage, manstage, by = "Date") |> 
  filter(Date < ymd("2025-01-01")) |> 
  ggplot(aes(x = stage_cm, y = manual_depth_cm))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = stage_cm, y = manual_depth_cm, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  labs(title = "2024 data")

##plot daily relationship for 2025
left_join(daily_stage, manstage, by = "Date") |> 
  filter(Date > ymd("2025-01-01")) |> 
  ggplot(aes(x = stage_cm, y = manual_depth_cm))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = stage_cm, y = manual_depth_cm, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  labs(title = "2025 data")


#### Stage to Q ####

##join data
stage_Q_df <- left_join(daily_stage, hpb_flowmate, by = "Date")

## quick data viz
stage_Q_df |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = Date, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)+
  theme_bw()

stage_Q_df |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = Date, y = value, color = name, shape = name))+
  geom_point()+
  theme_bw()


#### linear regression
stage_Q_df |> 
  # filter(L_s > 0.05) |> #0 point
  ggplot(aes(x = stage_cm, y = Flow_L_s, color = month(Date))) +
  geom_point() +
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = stage_cm, y = Flow_L_s, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  labs(x = "HPB water level (cm)", y = "Flowmate (L/s)")+
  theme_bw()+
  scale_color_viridis_c()



#### Fitting a non-linear curve, code resources below
## https://rpubs.com/kwf/hydrographs_covino
## other resource to check out: https://thewetlandblog.wordpress.com/2013/06/17/fitting-rating-curves-with-r/


#set up data frame to run nls function: need to remove 0 Q so flow will fit
#other filters are to test by year and different flow ranges, biggest issues seems to be two high flow days of ~70 and 30 L/s that mess up fit
stage_df <- stage_Q_df |> 
   #filter(year(Date) == 2025) |> 
  # filter(Date < ymd("2025-10-15")) |> 
  select(Date, stage_cm, Flow_L_s) |> 
  filter(!is.na(Flow_L_s),
         Flow_L_s > 0,
         Flow_L_s < 30
         ) #0 is model will fit, 30 is testing to get a better fit 30 is day new HOBO was deployed


# Fit nonlinear model: Q = a * stage^b
rating_fit <- nls(Flow_L_s ~ a * stage_cm^b, data = stage_df, start = list(a = 1, b = 1))
summary(rating_fit)

#add predicted values to stage data
stage_df <- stage_df |> 
  mutate(Q_pred = predict(rating_fit))

#plot
stage_df |> 
  ggplot(aes(x = stage_cm)) +
  geom_point(aes(y = Flow_L_s)) +
  geom_line(aes(y = Q_pred), color = "red", size = 1) +
  theme_bw() +
  labs(title = "Rating Curve Fit",
       x = "Stage (cm)",
       y = "Discharge (L_s)",
       color = "Legend")


#### Use this rating to curve to calc Q for high frequency stage ####
## This is is for the sake of getting something staged as of 21jan26; lets revisit before pub

#get fit data for equation
summary(rating_fit) #the summary of the model
coef(rating_fit) #pull out the coefficients
a <- coef(rating_fit)[["a"]] #store value of a
b <- coef(rating_fit)[["b"]] #store value of b


#### Calc Q from stage
stage_Q_calc <- stage |> 
  mutate(Q_L_s = a * waterlevel_cm^b,
         Flow_cms = Q_L_s / 1000)


#### Clean up df for staging 
Q_for_EDI <- stage_Q_calc |> 
  mutate(Reservoir = "CCR", Site = 101,
         Flag_Flow_cms = Flag_HOBO_Abs_Pres_kPa,
         Flag_Stage_cm = Flag_HOBO_Abs_Pres_kPa) |> 
  select(Reservoir, Site, DateTime_EST, corrected_Pres_kPa, Temp_C, waterlevel_cm, Flow_cms,
         Flag_HOBO_Abs_Pres_kPa, Flag_Temp_C, Flag_Stage_cm, Flag_Flow_cms) |> 
  rename(Flag_Pressure_kPa = Flag_HOBO_Abs_Pres_kPa,
         DateTime = DateTime_EST,
         Pressure_kPa = corrected_Pres_kPa,
         Stage_cm = waterlevel_cm)


#write.csv(Q_for_EDI, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCR_Inflow/2024/ccr_hpb-inflow_2024_2025.csv", row.names = F)  













