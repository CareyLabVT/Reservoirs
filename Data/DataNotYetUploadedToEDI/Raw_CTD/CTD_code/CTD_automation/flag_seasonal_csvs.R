#' 
#' @author Abigail Lewis
#' @title flag_seasonal_csvs
#' @description This function loads the saved CTD csv from this year and adds data flags
#' 
#' @param ctd_season_csvs directory of CTD seasonal csvs
#' @param input_file_name file name of un-flagged dataset
#' @param output_file_name file name of flagged dataset
#' @param CTD_FOLDER high level CTD folder where L1 output will be stored
#'
#' @return no output
#'

flag_seasonal_csvs <- function(ctd_season_csvs = "../../CTD_season_csvs",
                               input_file_name = "CTD_Meta_2023.csv",
                               output_file_name = "CTD_2023.csv",
                               CTD_FOLDER = "../../") {
  
  ctd1 <- read.csv(paste0(ctd_season_csvs, "/", input_file_name)) #Load saved data
  ctd = ctd1 %>%
    mutate(Date = as.POSIXct(Date, format = "%Y-%m-%dT%H:%M:%SZ"),
           Reservoir = as.factor(Reservoir))
  
  #Flag codes
  #0=Not suspect, 
  #1=Sample not taken, 
  #2=Instrument malfunction, 
  #3=Sample below detection,
  #4=Negative value set to 0 or NA
  #5=No sensor on CTD,
  #6=Measurement above water (removed for most vars)
  #7=Datetime missing time (date is meaningful but not time)
  #8=Measurement outside of expected range but retained in dataset
  
  
  ctd_flagged = ctd %>% #Add flags
    select(-Flag)%>%
    mutate(Flag_Temp = 0,
           Flag_DO= 0,
           Flag_DO_pSat = 0,
           Flag_Cond = 0,
           Flag_SpecCond = 0,
           Flag_Chla = 0,
           Flag_Turb = 0,
           Flag_pH = 0,
           Flag_ORP = 0,
           Flag_PAR = 0,
           Flag_DescRate = 0,
           Flag_DateTime = 0) %>%
    mutate(
      #TEMP
      Flag_Temp = ifelse(is.na(Temp_C),2,Flag_Temp), #Flag NA temperatures
      
      #DO
      Flag_DO = ifelse(DO_mgL < 0,4,Flag_DO),
      DO_mgL = ifelse(DO_mgL < 0, 0, DO_mgL), #Flag DO<0
      Flag_DO = ifelse(is.na(DO_mgL),2,Flag_DO), #Flag NA
      
      #DO pSat
      Flag_DO_pSat = ifelse(DO_pSat < 0,4,Flag_DO_pSat),
      Flag_DO_pSat = ifelse(is.na(DO_pSat),2,Flag_DO_pSat), #Flag NA
      DO_pSat = ifelse(DO_pSat < 0, 0, DO_pSat), #Flag pSat<0
      
      #COND
      Flag_Cond = ifelse(is.na(Cond_uScm),2,Flag_Cond), #Flag NA
      Flag_Cond = ifelse(Cond_uScm < 0,4,Flag_Cond),
      Cond_uScm = ifelse(Cond_uScm < 0, NA, Cond_uScm), #Flag Cond < 0. 
      
      #SPECCOND
      Flag_SpecCond = ifelse(is.na(Spec_Cond_uScm),2,Flag_SpecCond), #Flag NA
      Flag_SpecCond = ifelse(Spec_Cond_uScm < 0,4,Flag_SpecCond),
      Spec_Cond_uScm = ifelse(Spec_Cond_uScm < 0, NA, Spec_Cond_uScm), #Flag Cond < 0.
      
      #CHLA
      Flag_Chla = ifelse(is.na(Chla_ugL),2,Flag_Chla), #Flag NA
      Flag_Chla = ifelse(Chla_ugL < 0,4,Flag_Chla),
      Chla_ugL = ifelse(Chla_ugL < 0, 0, Chla_ugL), #Flag Chla <0
      
      #TURB
      Flag_Turb = ifelse(is.na(Turb_NTU),2,Flag_Turb), #Flag NA
      Flag_Turb = ifelse(Turb_NTU < 0,4,Flag_Turb),
      Turb_NTU = ifelse(Turb_NTU < 0, 0, Turb_NTU), #Flag turbidity <0
      
      #pH
      Flag_pH = ifelse(is.na(pH),2,Flag_pH), #Flag NA
      Flag_pH = ifelse(pH < 0,4,Flag_pH),
      pH = ifelse(pH < 0, 0, pH), #Flag pH < 0 
      
      #ORP
      Flag_ORP = ifelse(is.na(ORP_mV),2,Flag_ORP), #Flag NA
      
      #PAR
      Flag_PAR = ifelse(is.na(PAR_umolm2s),2,Flag_PAR), #Flag NA
      Flag_PAR = ifelse(!is.na(PAR_umolm2s)&PAR_umolm2s < 0,4,Flag_PAR), #Flag negative
      PAR_umolm2s = ifelse(!is.na(PAR_umolm2s)&PAR_umolm2s < 0, NA, PAR_umolm2s), 
      
      #DESC RATE
      Flag_DescRate = ifelse(is.na(Desc_rate),2,Flag_DescRate)) #Flag NA
  
  
  #Not all variables are meaningful out of the water
  Above_surface_flag = 6
  ctd_flagged[ctd_flagged$Depth_m<0,c("Chla_ugL","Turb_NTU","Cond_uScm","Spec_Cond_uScm","DO_mgL","DO_pSat","pH","ORP_mV")]<-NA
  ctd_flagged[ctd_flagged$Depth_m<0,c("Flag_Chla","Flag_Turb","Flag_Cond","Flag_SpecCond","Flag_DO","Flag_DO_pSat","Flag_pH","Flag_ORP")]<-Above_surface_flag
  
  final = ctd_flagged%>%
    mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"))%>%
    select(Reservoir, Site, Date, Depth_m, Temp_C, DO_mgL, DO_pSat, Cond_uScm, Spec_Cond_uScm, Chla_ugL, Turb_NTU, pH, ORP_mV,PAR_umolm2s, Desc_rate, Flag_Temp, Flag_DO, Flag_Cond, Flag_SpecCond, Flag_Chla, Flag_Turb, Flag_pH, Flag_ORP, Flag_PAR, Flag_DescRate, Flag_DateTime)
  
  #Fix for CTD when conductivity and specific conductivity columns were switched
  #spec_Cond_uScm=Cond_uScm/(1+(0.02*(Temp_C-25)))) so if temp is less than 25 conductivity is
  # less than specific conductivity and if temp is greater than 25 then conductivity is greater than 
  # specific conductivity. Based on this I created the a CTD_check column if the columns were good or bad. 
  # If they were bad then the conductivity and the spec. conductivity column need to be flipped. 
  
  #ABP 10 DEC 21
  
  CTD_fix=final%>%
    add_column(CTD_check = NA)%>%#create the CTD_check column
    #sets up criteria for the CTD_check column either "good","bad" or "NA"(if no data)
    mutate(
      CTD_check=ifelse(Temp_C<25& Cond_uScm<Spec_Cond_uScm & !is.na(Spec_Cond_uScm), "good",CTD_check),
      CTD_check=ifelse(Temp_C<25& Cond_uScm>Spec_Cond_uScm & !is.na(Spec_Cond_uScm), "bad",CTD_check),
      CTD_check=ifelse(Temp_C>25& Cond_uScm>Spec_Cond_uScm & !is.na(Spec_Cond_uScm), "good",CTD_check),
      CTD_check=ifelse(Temp_C>25& Cond_uScm<Spec_Cond_uScm & !is.na(Spec_Cond_uScm), "bad",CTD_check),
      CTD_check=ifelse(is.na(Spec_Cond_uScm), "good",CTD_check),
      CTD_check=ifelse(Cond_uScm==0, "bad", CTD_check))%>%
    #the next part switches the column if labeled "bad" in CTD_check 
    transform(., Spec_Cond_uScm = ifelse(CTD_check == 'bad' & !is.na(Spec_Cond_uScm), Cond_uScm, Spec_Cond_uScm), 
              Cond_uScm = ifelse(CTD_check == 'bad' & !is.na(Spec_Cond_uScm), Spec_Cond_uScm, Cond_uScm))%>%
    select(-CTD_check)%>%
    mutate(Site=ifelse(Reservoir=="BVR"&Site==1,40,Site),
           Site=ifelse(Site==49,50,Site))%>%
    
    mutate(
      #DateTime needs to be flagged
      Flag_DateTime = ifelse(hour(Date)==12&minute(Date)==0,7,0)
    )
  
  CTD_fix_renamed = CTD_fix%>% #Renaming flag columns in 2022
    rename(DateTime = Date,
           DOsat_percent = DO_pSat,
           DescRate_ms = Desc_rate,
           SpCond_uScm = Spec_Cond_uScm,
           Turbidity_NTU = Turb_NTU,
           Flag_Temp_C = Flag_Temp,
           Flag_DO_mgL = Flag_DO,
           Flag_Cond_uScm = Flag_Cond,
           Flag_SpCond_uScm = Flag_SpecCond,
           Flag_Chla_ugL = Flag_Chla,
           Flag_Turbidity_NTU = Flag_Turb,
           Flag_ORP_mV = Flag_ORP,
           Flag_PAR_umolm2s = Flag_PAR,
           Flag_DescRate_ms = Flag_DescRate)%>%
    mutate(Flag_DOsat_percent = Flag_DO_mgL)%>% #Adding this column, and it is currently the same as DO_mgL flags
    select(Reservoir, Site, DateTime, Depth_m, Temp_C, DO_mgL, DOsat_percent, Cond_uScm, SpCond_uScm, Chla_ugL, Turbidity_NTU, pH, ORP_mV, PAR_umolm2s, DescRate_ms, Flag_DateTime, Flag_Temp_C, Flag_DO_mgL, Flag_DOsat_percent, Flag_Cond_uScm, Flag_SpCond_uScm, Flag_Chla_ugL, Flag_Turbidity_NTU, Flag_pH, Flag_ORP_mV, Flag_PAR_umolm2s, Flag_DescRate_ms)
  
  CTD_fix_renamed%>%
    filter(Flag_DateTime==7)
  
  write.csv(CTD_fix_renamed,paste0(CTD_FOLDER, output_file_name), row.names = FALSE)
  message(paste0("Successfully updated ", output_file_name))
}