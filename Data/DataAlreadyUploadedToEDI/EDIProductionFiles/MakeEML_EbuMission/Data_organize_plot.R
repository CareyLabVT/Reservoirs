getwd()

library(tidyverse)

ebu_15_16 <- read_csv("fifteen_sixteen_EBU.csv")
ebu_17_18_19 <- read_csv("EDI_DATA_EBU_DIFF_DEPTH.csv")

ebu_15_16 <- ebu_15_16 %>% group_by(DateTime,Site)%>%summarise_all(funs(mean), na.rm = F) %>%
  mutate(Reservoir = "FCR")%>%
  mutate(Transect = "NT")%>%
  select(Reservoir, Site, Transect, DateTime, Depth_m, Ebu_rate, Diff_rate, Flag_depth, Flag_ebu, Flag_diff)

ebu_all <- rbind(ebu_15_16, ebu_17_18_19) %>%
  arrange(Reservoir, Site, Transect, DateTime)

plot <- ggplot(ebu_all, aes(DateTime, Ebu_rate, color = Site))+
  geom_point()

write_csv(ebu_all, "EDI_DATA_EBU_DIFF_DEPTH.csv")
