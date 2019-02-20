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
  select(id, group, arm, s4_date, edd, bl_date, s6_date, s6_arm, p1_date) %>%
  group_by(group, arm) %>%
  mutate(
    correlative = 1:n()
  ) %>%
  ungroup() %>%
  print()


# get date for Monday two weeks ago
monday_fortnight_ago <- Sys.Date() %>%
  lubridate::floor_date(unit = "week", week_start = 1) %>%
  magrittr::subtract(
    lubridate::weeks(2)
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
# Check scenarios ----
#------------------------------------------------------------------------------*

# only run tests interactively
if(interactive()){
  # reference list
  
  set.seed(0) # This ensures pseudorandom sampling is reproducible
  
  # create reference randomization list
  ref_ie_rand <- crossing(
    # list all possible options
    group = c("pw", "pw+oaw"),
    arm = c("control", "intervention"),
    correlative = seq(from = 1, to = 300)
  ) %>%
    # keep only totals expexted for each group
    filter(group == "pw" | correlative <= 100) %>%
    # for each subgroup
    group_by(group, arm) %>%
    # randomize intensive sampling exact proportions
    mutate(
      ie_group = c(
        # 20% intensive
        rep("ie_intensive", times = round(n() * 0.2)),
        # 80% normal
        rep("ie_normal", times = round(n() * 0.8))
      ) %>%
        # randomize order of options
        sample(x = ., size = n(), replace = FALSE)
    ) %>%
    ungroup() %>%
    print()
  
  
  rand_list <- ie_randomization %>%
    ungroup() %>%
    left_join(ref_ie_rand) %>%
    print()
  
  
  
  
  # simple randomization every two weeks
  
  set.seed(0)
  
  # randomize from the sampling frame every two weeks
  biweekly_simple <- ie_randomization %>%
    ungroup() %>%
    arrange(s6_date) %>%
    mutate(
      week = lubridate::floor_date(s6_date, unit = "week", week_start = 1)
    ) %>%
    left_join(
      all_week_pairs
    ) %>%
    nest(-rand_period) %>%
    mutate(
      data = map(
        data,
        ~ .x %>%
          group_by(group, arm) %>%
          mutate(
            intensive = sample(
              x = c("intensive", "normal"),
              size = n(),
              replace = TRUE,
              prob = c(0.2, 0.8)
            )
          )
      )
    ) %>%
    unnest() %>%
    count(group, arm, rand_period, intensive) %>%
    spread(intensive, n, fill = 0L) %>%
    group_by(group, arm) %>%
    mutate(
      total = intensive + normal,
      cummulative = cumsum(total),
      week_p_int = intensive / total * 100,
      cumm_p_int = cumsum(intensive) / cummulative * 100
    ) %>%
    print(n = Inf)
  
  
  
  # blocked over two week periods ----
  
  set.seed(0)
  
  
  # define set to check ongoing randomization (all strata)
  blocked_check <- crossing(
    group = c("pw", "pw+oaw"),
    arm = c("control", "intervention"),
    total_rand = 0L,
    total_int = 0L
  )
  
  
  # split the data in randomization periods
  rand_periods <- ie_randomization %>%
    ungroup() %>%
    arrange(s6_date) %>%
    mutate(
      week = lubridate::floor_date(s6_date, unit = "week", week_start = 1)
    ) %>%
    left_join(
      all_week_pairs
    ) %>%
    arrange(group, arm, s6_date, id) %>%
    split(.$rand_period)
  
  
  # setup object to add during randomizations
  biweekly_blocked <- NULL
  
  
  # randomize every two weeks
  rand_periods %>%
    for(houses in .){
      cat(
        "\n\nRandomization period starting on",
        first(houses$rand_period) %>% as.character(),
        "\n\n"
      )
      
      # randomly arrange available households
      shuffled <- houses %>%
        group_by(group, arm) %>%
        sample_n(n())
      
      # calculate how many to sample per strata given previous randomizations
      group_n <- houses %>%
        count(group, arm) %>%
        full_join(blocked_check, by = c("group", "arm")) %>%
        mutate_if(
          is.integer,
          list(~if_else(condition = is.na(.), 0L, .))
        ) %>%
        mutate(
          all = n + total_rand,
          target = round(all * 0.2),
          needed = target - total_int,
          take = pmin(n, needed),
          new_int = total_int + take
        )
      
      # keep selected households
      selected <- shuffled %>%
        left_join(select(group_n, group, arm, take), by = c("group", "arm")) %>%
        slice(seq(1, by = 1, length.out = first(take))) %>%
        select(-take) %>%
        ungroup() %>%
        print()
      
      # update randomization records
      blocked_check <<- group_n %>%     
        select(group, arm, total_rand = all, total_int = new_int) %>%
        mutate(
          cummul_p_intensive = total_int / total_rand
        )
      
      # accumulate all randomizations
      biweekly_blocked <<- c(list(selected), biweekly_blocked)
    }
  
  # organize ranzomization records
  biweekly_blocked <- biweekly_blocked %>%
    bind_rows() %>%
    print()
  
  
  
  # compare different strategies
  list(
    
    "Randomization list" = rand_list %>%
      count(group, arm, ie_group) %>%
      separate(ie_group, into = c(NA, "ie_group")) %>%
      spread(ie_group, n) %>%
      mutate(
        total_rand = intensive + normal,
        cummul_p_intensive = intensive / total_rand
      ) %>%
      select(group, arm, total_rand, total_int = intensive, cummul_p_intensive),
    
    "Simple biweekly" = biweekly_simple %>%
      group_by(group, arm) %>%
      summarize(
        total_rand = sum(normal) + sum(intensive),
        total_int = sum(intensive),
        cummul_p_intensive = total_int / total_rand
      ) %>%
      ungroup(),
    
    "Blocked biweekly" = blocked_check
    
  )
}




#------------------------------------------------------------------------------*
# Randomize households ----
#------------------------------------------------------------------------------*
# The selected strategy is to randomize every two weeks, by stratum, trying
# to bring the cummulative total of selected household close to the target
# proportion, using as the sampling frames all households randomized the
# preceding two weeks.
#------------------------------------------------------------------------------*


# Code to start a randomization record the first time this is done
# ie_randomization %>%
#   filter(s6_date < as.Date("2019-02-04")) %>%
#   mutate(
#     intensive = NA,
#     expected_p1_date = edd - 280 + 25*7
#   ) %>%
#   ungroup() %>%
#   select(
#     group, arm, id, intensive,
#     baseline_date = bl_date, expected_p1_date,
#     actual_p1_date = p1_date
#   ) %>%
#   write_csv(path = "output/intensive_exposure_randomized.csv", na = "")
 

target_p <- 0.2


# Use existing randomization record to evaluate each sampling
rand_record <- read_csv(
  "output/intensive_exposure_randomized.csv",
  col_types = cols(
    col_character(), col_character(), col_character(),
    col_logical(), col_date(), col_date(), col_date()
  )
)


# define set to check ongoing randomization (all strata)
blocked_check <- rand_record %>%
  group_by(group, arm) %>%
  summarize(
    total_rand = sum(!is.na(intensive)),
    total_int = sum(intensive, na.rm = TRUE)
  ) %>%
  ungroup()


# split the data in randomization periods
rand_periods <- ie_randomization %>%
  ungroup() %>%
  anti_join(rand_record) %>%
  arrange(s6_date) %>%
  mutate(
    week = lubridate::floor_date(s6_date, unit = "week", week_start = 1)
  ) %>%
  left_join(
    all_week_pairs
  ) %>%
  arrange(group, arm, s6_date, id) %>%
  split(.$rand_period)


# setup object to add during randomizations
biweekly_blocked <- NULL


set.seed(0) # ensure reproducible sampling


# randomize every two weeks
rand_periods %>%
  for(houses in .){
    cat(
      "\n\nRandomization period starting on",
      first(houses$rand_period) %>% as.character(),
      "\n\n"
    )
    
    
    # randomly arrange available households
    shuffled <- houses %>%
      group_by(group, arm) %>%
      sample_n(n())
    
    
    # calculate how many to sample per strata given previous randomizations
    group_n <- houses %>%
      count(group, arm) %>%
      full_join(blocked_check, by = c("group", "arm")) %>%
      mutate_if(
        is.integer,
        list(~if_else(condition = is.na(.), 0L, .))
      ) %>%
      mutate(
        all = n + total_rand,
        target = round(all * target_p),
        needed = target - total_int,
        take = pmin(n, needed),
        new_int = total_int + take
      )
    
    
    # keep selected households
    selected <- shuffled %>%
      left_join(select(group_n, group, arm, take), by = c("group", "arm")) %>%
      mutate(
        n = seq(1, n()),
        intensive = n <= first(take)
      ) %>%
      select(-take, -n) %>%
      ungroup()
    
    
    # update randomization records
    blocked_check <<- group_n %>%     
      select(group, arm, total_rand = all, total_int = new_int) %>%
      mutate(
        cummul_p_intensive = total_int / total_rand
      )
    
    
    # accumulate all randomizations
    biweekly_blocked <<- c(list(selected), biweekly_blocked)
  }


# organize ranzomization records
if(!is.null(biweekly_blocked)){
  biweekly_blocked <- biweekly_blocked %>%
    bind_rows() %>%
    mutate(
      expected_p1_date = edd - 280 + 25*7
    ) %>%
    select(
      group, arm, id, intensive, baseline_date = bl_date, expected_p1_date,
      actual_p1_date = p1_date
    ) %>%
    print()
  
  # save record
  rand_record %>%
    bind_rows(biweekly_blocked) %>%
    write_csv(path = "output/intensive_exposure_randomized.csv", na = "")
}


# End of script
