#### HPB water level corrections and discharge calculations 
## Dexter Howard Jan 2026


#### packages
library(tidyverse)
library(ggpmisc) #stat poly line



#### Load data sets ####

#get corrected stage generated in Stage_QAQC.Rmd 
stage <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCR_Inflow/2024/HPB_stage_2024_2025.csv")

# stage <- stage |> 
#   mutate(waterlevel_cm = ifelse(DateTime_EST > ymd_hms("2025-01-01 00:00:00"), waterlevel_cm - 10.3, waterlevel_cm  )
#   )

##calc hourly stage to bind with manual discharge for missing gaps 
stage_hourly <- stage |> 
  mutate(DateTime = floor_date(DateTime_EST, "hour")) %>%
  group_by(DateTime) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))



#flowmate Q
flowmate <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/454/9/0e7fe16623a1ad2a67774c23ce8a29d8")

hpb_flowmate <- flowmate |> 
  filter(Reservoir == "CCR", 
         Site == 101,
         year(DateTime) > 2023) |> 
  mutate(DateTime = floor_date(DateTime, "hour")) %>%
  select(DateTime, Flow_cms)


#### Stage to Q ####

##join data
stage_Q_df <- left_join(hpb_flowmate, stage_hourly, by = "DateTime") |> 
  select(-Flag_Pres, -Flag_Temp_C, -Temp_C, -corrected_Pres_kPa)

## quick data viz
stage_Q_df |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = DateTime, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)+
  theme_bw()


#### linear regression
stage_Q_df |> 
  # filter(!Flow_cms == 0) |> #0 point
  ggplot(aes(x = waterlevel_cm, y = Flow_cms, color = month(DateTime))) +
  geom_point() +
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = waterlevel_cm, y = Flow_cms, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  labs(x = "HPB water level (cm)", y = "Flowmate (cms)")+
  theme_bw()+
  scale_color_viridis_c()

stage_Q_df |> 
  mutate(year = year(DateTime)) |> 
  ggplot(aes(x = waterlevel_cm, y = Flow_cms, color = month(DateTime))) +
  geom_point() +
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = waterlevel_cm, y = Flow_cms, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  labs(x = "HPB water level (cm)", y = "Flowmate (cms)")+
  theme_bw()+
  facet_wrap(~year)+
  scale_color_viridis_c()



#### Fitting a non-linear curve, code resources below
## https://rpubs.com/kwf/hydrographs_covino
## other resource to check out: https://thewetlandblog.wordpress.com/2013/06/17/fitting-rating-curves-with-r/


#set up data frame to run nls function: need to remove 0 Q so flow will fit
#other filters are to test by year and different flow ranges, biggest issues seems to be two high flow days that mess up fit
stage_df <- stage_Q_df |> 
   #filter(year(Date) == 2025) |> 
  select(DateTime, waterlevel_cm, Flow_cms) |> 
  filter(!is.na(Flow_cms),
         !is.na(waterlevel_cm),
         waterlevel_cm > 0,
         Flow_cms > 0,
         ) #0 is so the model will fit, 


# Fit nonlinear model: Q = a * stage^b
rating_fit <- nls(Flow_cms ~ a * waterlevel_cm^b, data = stage_df, start = list(a = 0.1, b = 0.1))
summary(rating_fit)
coef(rating_fit) #pull out the coefficients
a <- coef(rating_fit)[["a"]] #store value of a
b <- coef(rating_fit)[["b"]] #store value of b

#can set this as 20 to get ~range of stages from flowmate dates; or 60 will show what this range looks like up until max stage in HF stage data
fitline <- data.frame(
  stage = seq(1, 20, by = 0.5))

fitline$q <- a * fitline$stage^b

plot(fitline$q, fitline$stage)


#plot
stage_df |> 
  ggplot(aes(x = Flow_cms)) +
  geom_point(aes(y = waterlevel_cm), size = 2) +
  geom_line(data = fitline, aes(x = q, y = stage), color = "blue", size = 1)+
  theme_bw() +
  # xlim(0,1000)+
  labs(title = "Rating Curve Fit all years",
       x = "Stage (cm)",
       y = "Discharge (cms)",
       color = "Legend")



#### Calc Q from stage
stage_Q_calc <- stage |> 
  mutate(Flow_cms = a * waterlevel_cm^b)

##plot
stage_Q_calc |> 
  # filter(as.Date(DateTime_EST) > ymd("2025-01-01")) |> 
  select(DateTime_EST, waterlevel_cm, Temp_C, Flow_cms) |> 
  # filter(Flow_cms < 2) |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = DateTime_EST, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)

###notes from this plot. the peak in may 2025 seems crazy high and so does Helene peak in sept 2024
##cross refed some of these values to nearby USGS gauge: https://waterdata.usgs.gov/monitoring-location/USGS-02055100/ also can look at site 02018500
#Helene peak seems maybe reasonable, May peak seems to have happened but this is too high, thinking leave for now and flag values that are above upper curve range 


#Daily means
stage_Q_calc |> 
  select(DateTime_EST, waterlevel_cm, Temp_C, Flow_cms) |> 
  mutate(Date = as.Date(DateTime_EST)) |> 
  group_by(Date) |> 
  summarise(level = mean(waterlevel_cm, na.rm = T),
            temp = mean(Temp_C, na.rm = T),
            flow_cms = mean(Flow_cms, na.rm = T)) |> 
  # filter(flow_cms < 0.5) |>
  pivot_longer(-1) |> 
  ggplot(aes(x = Date, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)

##plot v flowmate
flowmate_join <- hpb_flowmate |>  mutate(Date = as.Date(DateTime)) |> rename(Flow_cms_flowmate = Flow_cms)

stage_Q_calc_join <- stage_Q_calc |> 
  mutate(Date = as.Date(DateTime_EST)) 


left_join(flowmate_join, stage_Q_calc_join, by = c( "DateTime" = "DateTime_EST" ))  |> 
  # filter(Flow_cms < 20) |> 
    ggplot(aes(x = Flow_cms_flowmate, y = Flow_cms))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = Flow_cms_flowmate, y = Flow_cms, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  labs(x = "Flowmate (L/s)", y = "Curve (L/s)")+
  geom_abline(intercept = 0, slope = 1)

left_join(flowmate_join, stage_Q_calc_join, by = c( "DateTime" = "DateTime_EST" )) |> 
  mutate(resid = Flow_cms - Flow_cms_flowmate) |> 
  ggplot(aes(x = DateTime, y = resid))+
  geom_point()+
  geom_hline(yintercept = 0)


#### Write csv for publishing Q 

#### Clean up df for staging 
Q_for_EDI <- stage_Q_calc |> 
  ##make flag for Flow
  mutate(Flag_Flow_cms = Flag_Pres) |> 
  mutate(Flag_Flow_cms = if_else(
    Flow_cms > 0.1 & !is.na(Flow_cms),
    paste0("6", Flag_Flow_cms),
    as.character(Flag_Flow_cms)
  )) |> 
  ##set up DF 
  mutate(DateTime_EST = as.character(DateTime_EST)) |> 
  mutate(Reservoir = "CCR", Site = 101) |> 
  select(Reservoir, Site, DateTime_EST, corrected_Pres_kPa, waterlevel_cm, Temp_C, Flow_cms,
         Flag_Pres, Flag_Temp_C, Flag_Flow_cms) |> 
  rename(DateTime = DateTime_EST,
         Pressure_kPa = corrected_Pres_kPa,
         Stage_cm = waterlevel_cm)


#write.csv(Q_for_EDI, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCR_Inflow/2024/ccr_hpb-inflow_2024_2025.csv", row.names = F)  













