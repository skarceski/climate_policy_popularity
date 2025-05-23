

library(tidyverse) 

policy_cb <- read_csv("data/stage_1_cb.csv") 
data <- read_csv("data/stage_1_data.csv")
data_long <- read_csv("data/stage_1_policies_long.csv")

sum_data <- 
  data_long |> 
  group_by(item) |> 
  summarize(across(c(score:sup), ~ mean(., na.rm = T))) |> 
  arrange(desc(np)) |> 
  pivot_longer(score:sup, names_to = "measure") |> 
  left_join(data_long |> 
              filter(ideo3 != "Moderate") |> 
              group_by(item, ideo3) |> 
              summarize(across(c(score, np, sup), ~ mean(., na.rm = T))) |> 
              ungroup() |> 
              pivot_longer(score:sup, names_to = "measure") |> 
              pivot_wider(names_from = ideo3, values_from = value)) |> 
  left_join(policy_cb) |> 
  mutate(diff = Liberal - Conservative)  

# figure 1 prep 

f1_long <- 
  data |> 
  select(id, ideo3, ends_with("_cat"), contains("loop")) |> 
  select(!loop) |> 
  pivot_longer(contains("loop"), names_to = "item") |> 
  filter(!is.na(value), item != "9_loop1") |> 
  mutate(score = case_match(value,
                            "Don't know enough to say" ~ 0,
                            # "Somewhat favor" ~ 2/3,
                            # "Somewhat oppose" ~ -2/3,
                            "Somewhat favor" ~ 1,
                            "Somewhat oppose" ~ -1,
                            "Strongly favor" ~ 2, 
                            "Strongly oppose" ~ -2),
         np = case_match(value,
                         "Don't know enough to say" ~ 0,
                         "Somewhat favor" ~ 1,
                         "Somewhat oppose" ~ -1,
                         "Strongly favor" ~ 1, 
                         "Strongly oppose" ~ -1),
         sup = case_match(value,
                          # "Don't know enough to say" ~ 0,
                          "Somewhat favor" ~ 1,
                          "Somewhat oppose" ~ 0,
                          "Strongly favor" ~ 1, 
                          "Strongly oppose" ~ 0), 
         loop = str_sub(item, -1L)) |> 
  left_join(policy_cb |> select(item, policy))

f1_data <- 
  f1_long |> 
  mutate(cat = "Everyone") |> 
  select(item, cat, score) |> 
  bind_rows(f1_long |> select(item, score, cat = pid_cat)) |>
  bind_rows(f1_long |> select(item, score, cat = age_cat)) |>
  bind_rows(f1_long |> select(item, score, cat = conf_cat)) |> 
  bind_rows(f1_long |> select(item, score, cat = ideo_cat)) |>
  bind_rows(f1_long |> select(item, score, cat = worried_cat)) |>
  bind_rows(f1_long |> select(item, score, cat = need_cat)) |> 
  bind_rows(f1_long |> select(item, score, cat = bach_cat)) |> 
  bind_rows(f1_long |> select(item, score, cat = gender_cat)) 

f1_sum <-
  f1_data |> 
  drop_na() |> 
  # mutate(fct_rank = if_else(cat == "Everyone", score, NA_real_)) |> 
  group_by(cat, item) |> 
  summarize(score = mean(score, na.rm = T)) |> 
  ungroup()

# figure 1 

f1_sum |> 
  left_join(f1_data |> 
              filter(cat == "Everyone") |> 
              group_by(item) |> 
              summarize(Everyone = mean(score, na.rm = T))) |> 
  mutate(item = fct_reorder(item, Everyone),
         shape = case_when(cat %in% c("Less than bach",
                                      "Liberal",
                                      "Republican",
                                      "Confidence",
                                      "Male",
                                      "Need policy",
                                      "Older") ~ "a", 
                           cat == "Everyone" ~ "b", 
                           T ~ "c"),
         pair = case_when(cat %in% c("Bach or more", "Less than bach") ~ "Edu",
                          cat %in% c("Republican", "Democrat") ~ "Party",
                          cat %in% c("Conservative", "Liberal") ~ "Ideo",
                          cat %in% c("Confidence", "Doubt") ~ "Confidence",
                          cat %in% c("Male", "Female") ~ "Gender",
                          cat %in% c("Need policy", "No need") ~ "Policy",
                          cat %in% c("Older", "Younger") ~ "Age",
                          cat == "Everyone" ~ "All")) |>
  select(!Everyone) |> 
  # filter(cat = %in% c("Worried", "Not worried",
  #                     "Need"))
  # filter(!cat %in% c("Worried", "Not worried")) |>
  filter(!cat %in% c("Age", "Ideo")) |>
  ggplot(aes(x = score, y = item, color = pair, shape = shape)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19, 5)) +
  # scale_color_manual(values = c()) +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.1) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.position = "none")

# table 1 

f1_sum |> 
  filter(cat != "Everyone") |> 
  group_by(cat) |> 
  slice_max(score, n = 25) |> 
  ungroup() |> 
  count(item, sort = T) |> 
  left_join(policy_cb |> select(item, policy, type)) |> 
  filter(n == 16) |> 
  left_join(sum_data |> 
              select(item, name = measure, value) |> 
              pivot_wider()) |> 
  mutate(type2 = str_to_title(type),
         Rank = rank(desc(score))) |> 
  select(Rank, Policy = policy, Type = type, Avg = score, Sup = sup) |> 
  mutate(Sup = round(100*Sup, 1),
         Avg = round(Avg, 2)) |> 
  arrange(desc(Avg)) |> 
  rename(`Sup%` = Sup) |> 
  gt::gt()

# figure 2 

sum_data |> 
  filter(measure == "score") |> 
  mutate(type = str_to_title(type),
         type = fct_reorder(type, value)) |> 
  ggplot(aes(x = value, y = type)) + 
  geom_boxplot() +
  # scale_color_brewer(palette = "Dark1") +
  theme_bw() +
  labs(x = "Average support", y = NULL)  



