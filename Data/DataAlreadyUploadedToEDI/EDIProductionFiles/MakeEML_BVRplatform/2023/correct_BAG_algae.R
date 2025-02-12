wed <- bvrdata|>
  select(DateTime, EXOBGAPC_RFU_1.5, EXOBGAPC_ugL_1.5)|>
  filter(DateTime>as.Date("2022-12-01") & DateTime<as.Date("2023-04-15"))


# 0.38 before after 2.5 subtract -2.12 for RFU

# 0.38 before after 1.45 subtract 1.07 for ugL