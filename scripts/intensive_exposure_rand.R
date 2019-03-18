#------------------------------------------------------------------------------*
# Intensive exposure substudy randomization
#------------------------------------------------------------------------------*


# Load used packages
library(package = "tidyverse")




#------------------------------------------------------------------------------*
# Prepare data ----
#------------------------------------------------------------------------------*


# Get Emory RedCap export data
source(file = "scripts/0_get_emory_data.R", encoding = "UTF-8")
source(file = "scripts/0_get_intensive_data.R", encoding = "UTF-8")



# get minimal dataset needed for intensive exposure participant selection
ie_randomization <- gt_emory_data %>%
  select(id, s6_date, s6_arm, bl_date = m11_date) %>%
  filter(!is.na(s6_date)) %>%
  # add gestational age
  left_join(
    gt_emory_data %>%
      filter(!is.na(s4_date)) %>%
      select(id = s4_main_id, edd = m17_ga, s4_date)
  ) %>%
  # add p1 visit
  left_join(
    gt_emory_data %>%
      filter(visit == "p1", !is.na(m11_date)) %>%
      select(id, p1_date = m11_date)
  ) %>%
  # add study exit data
  left_join(
    gt_emory_data %>%
      filter(
        !is.na(e3_date) | !is.na(e3_date_o) | !is.na(e3_date_c)
      ) %>%
      select(id, matches("e3_date(_[co])?$"))
  ) %>%
  # arrange by randomization date and then id to keep consistent order
  arrange(
    s6_date, id
  ) %>%
  # label groups
  mutate(
    group = case_when(
      grepl("^33", id) ~ "pw",
      grepl("^35", id) ~ "pw+oaw"
    ),
    arm = recode(
      s6_arm,
      "0" = "control",
      "1" = "intervention"
    )
  ) %>%
  select(
    id, group, arm, s4_date, edd, bl_date, s6_date, s6_arm, p1_date,
    matches("e3_date")
  ) %>%
  group_by(group, arm) %>%
  mutate(
    correlative = 1:n()
  ) %>%
  ungroup() %>%
  print()


# get date for most recent Monday
if(
  iconv(
    lubridate::wday(Sys.Date(), label = TRUE),
    from = "UTF-8",
    to = "ASCII//TRANSLIT"
  ) %in% c(
    "sab", "dom", "lun", "sat", "sun", "mon"
  )
){
  this_monday <- Sys.Date() %>%
    lubridate::floor_date(unit = "week", week_start = 1)
} else {
  this_monday <- Sys.Date() %>%
    lubridate::ceiling_date(unit = "week", week_start = 1)
}


# get date for Monday two weeks ago
monday_month_ago <- this_monday %>%
  magrittr::subtract(
    lubridate::weeks(4)
  ) %>%
  print()


# Dates every two weeks for randomization
all_week_pairs <- ie_randomization %>%
  pull(s6_date) %>%
  range() %>%
  lubridate::floor_date(unit = "week", week_start = 1) %>%
  set_names(c("from", "to")) %>%
  as.list() %>%
  c(by = "1 week") %>%
  do.call(seq.Date, .) %>%
  tibble(
    week = .,
    rand_period = week - lubridate::weeks(c(0, 1))
  ) %>%
  print()




#------------------------------------------------------------------------------*
# Randomize households ----
#------------------------------------------------------------------------------*
# The selected strategy is to randomize every two weeks, by stratum, trying
# to bring the cummulative total of selected household close to the target
# proportion.
# The sampling frame will include all households randomized during the
# preceding four weeks,
# considering only those with an opportunity of participating timely
# in the first intensive visit.
#------------------------------------------------------------------------------*


target_p <- 0.15


# Use existing randomization record to evaluate each sampling
rand_record <- list.files(
  "output/intensive-exposure", pattern = "randomized", full.names = TRUE
) %>%
  tibble(
    file = .,
    rand_date = file %>% sub(".+/([-0-9]+)_[^0-9/]+", "\\1", .) %>% as.Date()
  ) %>%
  slice(which.max(rand_date)) %>%
  pull(file) %>%
  read_csv(
    col_types = cols(
      randomization_date = col_date(),
      group = col_character(),
      arm = col_character(),
      id = col_character(),
      enrollment_date = col_date(),
      baseline_date = col_date(),
      expected_p1_date = col_date(),
      actual_p1_date = col_date(),
      frame_group = col_character(),
      in_frame = col_logical(),
      selected = col_logical(),
      invited = col_logical(),
      interested = col_logical(),
      enrolled = col_logical()
    )
  )


# define set to check ongoing randomization (all strata)
blocked_check <- rand_record %>%
  group_by(group, arm) %>%
  summarize(
    total_randomized = sum(frame_group != "not-in-frame"),
    total_enrolled = sum(enrolled, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  print()


# define the sampling frame
sampling_frame <- ie_randomization %>%
  # leave out randomizations that are too recent
  filter(s6_date < this_monday) %>%
  # Remove those that have already been considered
  anti_join(
    rand_record %>%
      filter(
        frame_group == "not-in-frame" |
          (selected & (!interested | enrolled))
      )
  ) %>%
  mutate(
    conception_date = edd - lubridate::days(280),
    bl_p1_midpoint = conception_date + lubridate::weeks(22),
    # define frame group
    frame_group = case_when(
      s6_date < monday_month_ago ~ "not-in-frame",
      this_monday > bl_p1_midpoint ~ "not-eligible",
      this_monday <= bl_p1_midpoint ~ "in-sampling-frame"
    ),
    # define if a household is in sampling frame
    in_frame = frame_group == "in-sampling-frame"
  )


# ensure reproducible sampling
set.seed(0)


# randomly arrange available households
shuffled <- sampling_frame %>%
  filter(frame_group == "in-sampling-frame") %>%
  group_by(group, arm) %>%
  sample_n(n())


# calculate how many to sample per strata given previous randomizations
group_n <- sampling_frame %>%
  filter(frame_group == "in-sampling-frame") %>%
  count(group, arm) %>%
  full_join(blocked_check, by = c("group", "arm")) %>%
  mutate_if(
    is.integer,
    list(~if_else(condition = is.na(.), 0L, .))
  ) %>%
  mutate(
    all_randomized = n + total_randomized,
    target = round(all_randomized * target_p),
    needed = target - total_enrolled,
    take = pmin(n, needed),
    total_selected = total_enrolled + take
  )


# keep selected households
randomized <- shuffled %>%
  left_join(select(group_n, group, arm, take), by = c("group", "arm")) %>%
  mutate(
    n = seq(1, n()),
    selected = n <= first(take)
  ) %>%
  select(id, group, arm, selected) %>%
  ungroup()


# organize ranzomization records
biweekly_randomization <- sampling_frame %>%
  left_join(randomized) %>%
  # add context variables
  left_join(
    gt_emory_data %>%
      select(id, hapin_rand_date = s6_date) %>%
      filter(!is.na(hapin_rand_date))
  ) %>%
  left_join(
    gt_emory_data %>%
      select(id = s4_main_id, edd = m17_ga) %>%
      filter(!is.na(edd))
  ) %>%
  mutate(
    ga_today = round((Sys.Date() - (as.Date(edd) - 280)) / 7, 1),
    ga_at_rand = round((hapin_rand_date - (as.Date(edd) - 280)) / 7, 1),
    randomization_date = Sys.Date(),
    expected_p1_date = edd - 280 + 25*7,
    first_intensive_visit = if_else(
      condition = selected,
      true = as.character(bl_p1_midpoint),
      false = NA_character_
    )
  ) %>%
  select(
    randomization_date, group, arm, id,
    enrollment_date = s4_date, baseline_date = bl_date, expected_p1_date,
    actual_p1_date = p1_date,
    ga_today, hapin_rand_date, ga_at_rand,
    frame_group, in_frame, selected,
    
    first_intensive_visit
  ) %>%
  print()


# save record
biweekly_randomization %>%
  write_csv(
    path = paste0(
      "output/intensive-exposure/",
      Sys.Date(),
      "_intensive_exposure_randomized.csv"
    ),
    na = ""
  )


# summarize selection
biweekly_randomization %>%
  filter(selected) %>%
  group_by(group, arm) %>%
  summarize(
    n = n(),
    ids = id %>% paste(collapse = ", ")
  ) %>%
  ungroup()


group_n %>%
  select(group, arm, total_randomized = all_randomized, total_selected) %>%
  mutate(
    p_intensive = total_selected / total_randomized
  )


# End of script
