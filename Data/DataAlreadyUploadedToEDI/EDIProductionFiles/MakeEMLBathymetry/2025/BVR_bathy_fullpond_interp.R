#### BVR bathymetry interpolation to full pond
## DWH sept 2026

library(tidyverse)


#### read in hypsometry data
hypso <- read.csv("C:/Users/dwh18/OneDrive/Documents/Bathy_EDI_2026/Bathy_EDI_updates/hypso_sept2026/Bathy_comb_V1.csv")


#### Set up BVR data
## We know at full pond BVR is 13 m deep (Lofton et al. 2019; Hamre et al. 2018)
## and has a surface area of 0.271 km^2; from USGS shapefile provided in repo (0-2 m depth layer)

### filter data to BVR and set depths to max of 13 m
bvr <- hypso |> filter(Reservoir == "BVR") |> 
  mutate(Depth_m = Depth_m+2.5) |> 
  mutate(Depth_elevation_m = 586.74 - Depth_m)

### add surface for 0 meter depth 
sa_0m <- 0.271 * 1e6 

# add the 0m row
bvr <- bvr |>
  bind_rows(
    tibble(Reservoir = "BVR", Depth_m = 0, SA_m2 = sa_0m, 
           Volume_layer_L = NA, Volume_below_L = NA,
           Depth_elevation_m = 586.74),
    tibble(Reservoir = "BVR", 
           Depth_m = c(0.5, 1.0, 1.5, 2.0),
           SA_m2 = NA, Volume_layer_L = NA, Volume_below_L = NA,
           Depth_elevation_m = 586.74 - c(0.5, 1.0, 1.5, 2.0))
  ) |>
  arrange(Depth_m)



#### Plot and fit curve for surface area

### check simple curve first
bvr |> 
  ggplot(aes(y = Depth_elevation_m, x = SA_m2)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "blue", se = TRUE) +
  labs(y = "Depth (elevation meters)", x = "Surface Area (m²)", title = "BVR Bathymetry: Depth vs SA") +
  theme_bw()


# fit polynomial
sa_poly <- lm(SA_m2 ~ poly(Depth_elevation_m, 2, raw = TRUE), 
              data = bvr)

summary(sa_poly)


# back-estimate SA from elevation
bvr <- bvr |>
  mutate(SA_predicted_m2 = predict(sa_poly, newdata = bvr)) |> 
  mutate(SA_filled_m2 = ifelse(is.na(SA_m2), SA_predicted_m2, SA_m2))

#check predict v known for SA
bvr |>
  filter(!is.na(SA_m2), !is.na(SA_predicted_m2)) |>
  ggplot(aes(x = SA_m2, y = SA_predicted_m2)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  annotate("text", x = Inf, y = -Inf,
           label = paste0("R² = ", round(summary(lm(SA_predicted_m2 ~ SA_m2, 
                                                    data = filter(bvr, !is.na(SA_m2), !is.na(SA_predicted_m2))))$r.squared, 3),
                          "\np = ", round(summary(lm(SA_predicted_m2 ~ SA_m2, 
                                                     data = filter(bvr, !is.na(SA_m2), !is.na(SA_predicted_m2))))$coefficients[2,4], 4)),
           hjust = 3.1, vjust = -0.5, size = 5) +
  labs(x = "Observed SA (m²)", y = "Predicted SA (m²)", 
       title = "Known vs Predicted Surface Area") +
  theme_bw()


#### Try to back estimate volume
bvr |> 
  ggplot(aes(x = SA_filled_m2, y = Volume_below_L))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "blue", se = TRUE) +
  theme_bw()


#### check how area to volume looks across reservoir 
hypso |> 
  filter(!is.na(SA_m2)) |> 
  ggplot(aes(x = SA_m2, y = Volume_below_L))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              color = "blue", se = TRUE) +
  theme_bw()+
  facet_wrap(~Reservoir, scales = "free")


#### fit BVR curve for SA to volume 
# fit polynomial for Volume_below ~ Depth_elevation_m
vol_poly <- lm(Volume_below_L ~ poly(SA_filled_m2, 2, raw = TRUE),
               data = bvr)

summary(vol_poly)

# predict volume for all rows including interpolated depths
bvr <- bvr |>
  mutate(Volume_below_predicted_L = predict(vol_poly, newdata = bvr))

#check predict v known for SA
bvr |>
  filter(!is.na(Volume_below_L), !is.na(Volume_below_predicted_L)) |>
  ggplot(aes(x = Volume_below_L, y = Volume_below_predicted_L)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  annotate("text", x = Inf, y = -Inf,
           label = paste0("R² = ", round(summary(lm(SA_predicted_m2 ~ SA_m2, 
                                                    data = filter(bvr, !is.na(SA_m2), !is.na(SA_predicted_m2))))$r.squared, 3),
                          "\np = ", round(summary(lm(SA_predicted_m2 ~ SA_m2, 
                                                     data = filter(bvr, !is.na(SA_m2), !is.na(SA_predicted_m2))))$coefficients[2,4], 4)),
           hjust = 3.1, vjust = -0.5, size = 5) +
  labs(x = "Observed volume (L)", y = "Predicted volume (L)", 
       title = "Known vs Predicted Volume") +
  theme_bw()


#plot volume curves
bvr |> 
  select(SA_filled_m2, Volume_below_L, Volume_below_predicted_L) |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = SA_filled_m2, y = value, color = name))+ geom_point()


#### Clean up BVR data frame ----
head(bvr)

bvr_final <- bvr |> 
  mutate(Volume_below_filled_L = ifelse(is.na(Volume_below_L), Volume_below_predicted_L, Volume_below_L)) |> 
  #add flags for interp value
  mutate(Flag_SA_m2 = ifelse(is.na(SA_m2), 1, 0),
         Flag_Volume_L = ifelse(is.na(Volume_below_L), 1, 0)) |> 
  #select columns I'm keeping 
  select(Reservoir, Depth_m, SA_filled_m2, Volume_layer_L, Volume_below_filled_L, Flag_SA_m2, Flag_Volume_L) |> 
  #recalcuate volume by layer for interp layers
  mutate(Volume_layer_L = Volume_below_filled_L - lead(Volume_below_filled_L),
         Volume_layer_L = ifelse(is.na(Volume_layer_L), 0, Volume_layer_L) #set to 0 last layer that has no area
         ) |> 
  #rename columns back to match
  rename(SA_m2 = SA_filled_m2,
         Volume_below_L = Volume_below_filled_L)


#### Bind BVR back to other reservoirs ----
hypso_forbind <- hypso |> 
  filter(Reservoir != "BVR",
         !is.na(Depth_m)) |> 
  mutate(Flag_SA_m2 = 0,
         Flag_Volume_L = 0)


hypso_final <- rbind(bvr_final, hypso_forbind)

# write.csv(hypso_final, "C:/Users/dwh18/OneDrive/Documents/Bathy_EDI_2026/Bathy_EDI_updates/hypso_sept2026/Bathymetry_combined_Final.csv", row.names = F)  
