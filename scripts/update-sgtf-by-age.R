

by_region_and_age <- english_pillars %>%
  filter(!is.na(age_group)) %>%
  select(-lower_age_limit, -positive, -total) %>%
  group_by(pillar, week_infection, nhser_name, age_group) %>%
  summarise_if(is.numeric, sum) %>%
  ungroup() %>%
  pivot_longer(starts_with("sgene"), names_to = "sgene_result",
               values_to = "n") %>%
  mutate(sgene_result = sub("^sgene_", "", sgene_result))
