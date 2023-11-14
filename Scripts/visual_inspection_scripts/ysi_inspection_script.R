#### YSI diagnostic plots ####

ysi <- read_csv() ## open the L1 file

ysi_long <- ysi %>% 
  gather(metric, value, Temp_C:pH) %>% 
  mutate(month = strftime(DateTime, "%b")) %>%
  mutate(DateTime = as.Date(DateTime))

# FCR only; all sampling sites 
ggplot(subset(ysi_long, Reservoir=='FCR'), aes(x = DateTime, y = value, col=Depth_m)) +
  geom_point(cex=2) +
  facet_grid(metric ~ Site, scales='free') +
  scale_y_continuous("Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_gradient("Depth (m)", high = "black", low = "deepskyblue")