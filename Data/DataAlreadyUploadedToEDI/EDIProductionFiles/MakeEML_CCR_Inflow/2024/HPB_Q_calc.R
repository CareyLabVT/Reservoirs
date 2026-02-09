#### HPB water level corrections and discharge calculations 
## Dexter Howard Jan 2026

## Edit: Feb. 4, 2026 - ABP- took out the May 2025 flowmate obs because seems too high. The rating curve looks much better. 


#### packages
pacman::p_load(tidyverse, ggpmisc)
# use ggpmisc for the stat poly line


#### Load data sets ####

#get corrected stage generated in Stage_QAQC.Rmd 
stage <- read_csv("./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCR_Inflow/2024/HPB_stage_2024_2025.csv")

# Put the time into EST
stage$DateTime <- force_tz(as.POSIXct(stage$DateTime), tz = "EST")


##calc hourly stage to bind with manual discharge for missing gaps 
# stage_hourly <- stage |> 
#   mutate(DateTime = floor_date(DateTime, "hour")) %>%
#   group_by(DateTime) %>%
#   summarise(across(where(is.numeric), mean, na.rm = TRUE))



#flowmate Q
flowmate <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/454/9/0e7fe16623a1ad2a67774c23ce8a29d8")

# set timezone to American/New_york
flowmate$DateTime <- force_tz(as.POSIXct(flowmate$DateTime), tz = "America/New_York")

hpb_flowmate <- flowmate |> 
  filter(Reservoir == "CCR", 
         Site == 101,
         year(DateTime) > 2023) |> 
  #mutate(DateTime = floor_date(DateTime, "hour")) %>%
  select(DateTime, Flow_cms)

# Put the dates in EST 

# convert to EST
hpb_flowmate$DateTime <- with_tz(as.POSIXct(hpb_flowmate$DateTime), tz = "EST")


#### Stage to Q ####

##join data by closest observation 
wl <- stage|>
  select(DateTime, waterlevel_cm)|>
  drop_na() # drop any NAs in the waterlevel file


by <- join_by(closest(DateTime >= DateTime))

stage_Q_df <- left_join(hpb_flowmate, wl, by)|>
  dplyr::rename(DateTime = DateTime.x)|>
  select(-DateTime.y)

# stage_Q_df <- left_join(hpb_flowmate, stage_hourly, by = "DateTime") |> 
#   select(-Flag_Pres, -Flag_Temp_C, -Temp_C, -corrected_Pres_kPa)

## quick data viz
stage_Q_df |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = DateTime, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)+
  theme_bw()


#### linear regression

# linear regression of water level vs. maual flow measurements
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

# plot the relationship of water level and manual discharge measurements by year
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


### What does it look like if we take out the very high point in May 2025

stage_Q_df |> 
filter(waterlevel_cm<18)|>
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
  filter(waterlevel_cm<18)|>
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

# Now R2 is 0.85 for 2024 and 0.88 for 2025


#### Fitting a non-linear curve, code resources below
## https://rpubs.com/kwf/hydrographs_covino
## other resource to check out: https://thewetlandblog.wordpress.com/2013/06/17/fitting-rating-curves-with-r/


#set up data frame to run nls function: need to remove 0 Q so flow will fit
#other filters are to test by year and different flow ranges, biggest issues seems to be two high flow days that mess up fit
stage_df <- stage_Q_df |> 
  filter(waterlevel_cm < 18) |> # remove the high flow value from May 2025 for now
  select(DateTime, waterlevel_cm, Flow_cms) |> 
  filter(!is.na(Flow_cms),
         !is.na(waterlevel_cm),
         waterlevel_cm > 0,
         Flow_cms > 0,
         ) #0 is so the model will fit, 


# Fit nonlinear model: Q = a * stage^b

# Got an error "singular gradient" when a = 0.1 and b = 0.1 so changed them to a = 0.01 and b = 0.01. That worked!

rating_fit <- nls(Flow_cms ~ a * waterlevel_cm^b, data = stage_df, start = list(a = 0.01, b = 0.01))

# Look at the summary file
summary(rating_fit)

coef(rating_fit) #pull out the coefficients
a <- coef(rating_fit)[["a"]] #store value of a
b <- coef(rating_fit)[["b"]] #store value of b

#can set this as 20 to get ~range of stages from flowmate dates; or 60 will show what this range looks like up until max stage in HF stage data
fitline <- data.frame(
  stage = seq(1, 60, by = 0.5))

fitline$q <- a * fitline$stage^b

plot(fitline$q, fitline$stage)

# Let's see what it look like to show the range of the stage from flowmate dates

fitline <- data.frame(
  stage = seq(1, 20, by = 0.5))

fitline$q <- a * fitline$stage^b

plot(fitline$q, fitline$stage)



#plot the Flow vs. the stage with water level over the rating curve
stage_df |> 
  ggplot(aes(x = Flow_cms)) +
  geom_point(aes(y = waterlevel_cm), size = 2) +
  geom_line(data = fitline, aes(x = q, y = stage), color = "blue", size = 1)+
  theme_bw() +
  # xlim(0,1000)+
  labs(title = "Rating Curve Fit all years",
       x = "Discharge (cms)",
       y = "stage (cm)",
       color = "Legend")



#### Calc Q from stage
stage_Q_calc <- stage |> 
  mutate(Flow_cms = a * waterlevel_cm^b)|>
  select(DateTime, waterlevel_cm, Temp_C, Flow_cms)

##plot the time series of flows, Temp and water level
stage_Q_calc |> 
  # filter(as.Date(DateTime_EST) > ymd("2025-01-01")) |> 
  # filter(Flow_cms < 0.2) |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = DateTime, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)+
  theme_bw()

###notes from this plot. the peak in may 2025 seems crazy high and so does Helene peak in sept 2024
##cross refed some of these values to nearby USGS gauge: https://waterdata.usgs.gov/monitoring-location/USGS-02055100/ also can look at site 02018500
#Helene peak seems maybe reasonable, May peak seems to have happened but this is too high, thinking leave for now and flag values that are above upper curve range 


#Daily means
stage_Q_calc |> 
  select(DateTime, waterlevel_cm, Temp_C, Flow_cms) |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  select(-DateTime) |>
  pivot_longer(!Date, names_to = "var", values_to = "value")|>
  dplyr::group_by(Date, var)|>
  dplyr::summarise(value = mean(value, na.rm = T))|>
  ggplot(aes(x = Date, y = value))+
  geom_point()+
  facet_wrap(~var, scales = "free_y", ncol = 1)

##plot v flowmate
flowmate_join <- hpb_flowmate |>  mutate(Date = as.Date(DateTime)) |> rename(Flow_cms_flowmate = Flow_cms)  
  #filter(Flow_cms_flowmate < 0.07) #remove high may flow

stage_Q_calc_join <- stage_Q_calc |> 
  mutate(Date = as.Date(DateTime)) 

# plot the manual discharge measurements vs. the calculated discharge measurements 
left_join(flowmate_join, stage_Q_calc_join, by = "DateTime")  |> 
  # filter(Flow_cms < 20) |> 
    ggplot(aes(x = Flow_cms_flowmate, y = Flow_cms))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = Flow_cms_flowmate, y = Flow_cms, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  labs(x = "Flowmate (L/s)", y = "Curve (L/s)")+
  geom_abline(intercept = 0, slope = 1)


# plot the difference from calculated flow to observed flow and see how far from 0 we are. It looks like we do well except for 

# make a data frame to join the closest flowmate with calculated flow and then plot the difference. Looks pretty good. +/- 0.005

left_join(flowmate_join, stage_Q_calc_join, by)|> 
  mutate(resid = Flow_cms - Flow_cms_flowmate) |> 
  ggplot(aes(x = DateTime.x, y = resid))+
  geom_point()+
  geom_hline(yintercept = 0)


#### Write csv for publishing Q 

#merge the flows with the stage data frame created from Stage_QAQC.Rmd
max_flowmate_Q <- max(stage_df$Flow_cms)


Q_for_EDI <- stage_Q_calc |>
  select(DateTime, Flow_cms) %>%
  left_join(stage, . , by = "DateTime")|>
  relocate(Flow_cms, .after = waterlevel_cm) |>
  relocate(Flag_waterlevel_cm, .after = Flag_corrected_Pres_kPa)|>
  mutate(Flag_Flow_cms = Flag_waterlevel_cm)|>
  relocate(Flag_Flow_cms, .after = Flag_waterlevel_cm)|>
  rename(Stage_cm = waterlevel_cm,
         Flag_Stage_cm = Flag_waterlevel_cm) |> 
  ## add flag for times Flow exceeds upper Q in rating curve
  mutate(Flag_Flow_cms = ifelse(Flow_cms > max_flowmate_Q, 
                                6, 
                                Flag_Flow_cms)) |> 
  #add site identifiers and order data
  mutate(Reservoir = "CCR", 
         Site = 101)|>
  select(Reservoir, Site, everything())

# convert datetimes to characters so that they are properly formatted in the output file
Q_for_EDI$DateTime <- as.character(format(Q_for_EDI$DateTime))

# Write the data frame

write.csv(Q_for_EDI, "./Data/DataAlreadyUploadedToEDI/EDIProductionFiles/MakeEML_CCR_Inflow/2024/ccr_hpb-inflow_2024_2025.csv", row.names = F)  
           