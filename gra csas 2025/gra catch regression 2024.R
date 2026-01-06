# These are the landings according to MARFIS
catch |> 
  filter(YEAR <= 2024) |> 
  filter(RIVERNAME_CLEANED == "GASPEREAU") |> 
  arrange(YEAR) |> 
  group_by(YEAR) |> 
  summarise(total = sum(FV_WEIGHT)) |> 
  mutate(total_mt = round(total / 1000, 2)) |> 
  mutate(reporting = c(0.44, 0.75, 0.75, 0.56, 0.75, 0.75, 0.71, 0.80, 0.87, 0.80, 0.87, 0.87, 1, 1, 1, 1, 0.87)) |>
  mutate(adjusted = round(total_mt / reporting, 2)) |> 
  mutate(buyers = c(NA, NA, NA, NA, 384803, 387333, 439000, 705500, 769133, 605900, 903655, 784152, 1202604, 1231005, 1562900, 1431500, 1265348)) |>
  select(year = YEAR, adjusted, buyers) -> model_df

# We want the -1 so there is no intercept
mod <- lm(buyers ~ adjusted - 1, data = model_df)

model_df |> 
  mutate(predicted = predict(mod, newdata = model_df)) -> model_df

# This tells me that each mt of fish in MARFIS (adjusted for those who are
# not reporting) actually represents 4600 fish. Assuming that one mt is about
# 4400 fish, this seems pretty close
summary(mod)

model_df |> select(year, predicted)