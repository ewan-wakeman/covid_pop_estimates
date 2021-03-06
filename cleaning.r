# Population Italia
# Written by Ewan Wakeman

# A basic cleanup of Italian Population Estimates for 2019 from https://www.istat.it/en/population-and-households?data-and-indicators

# for cleaning
require(tidyverse)

# there are some weird codes we need to deal with individually (IT108-111) these are
# ITTER107 Territory            
# <chr>    <chr>                
# 1 IT108    Monza e della Brianza
# 2 IT109    Fermo                
# 3 IT110    Barletta-Andria-Trani
# 4 IT111    Sud Sardegna         

pop <- 
  # read csv
  read_csv('data/raw_pop_estimates.csv') %>%
  # drop marital status to just totals (as not required) and remove totals for age/gender to avoid doulbe counting
  filter(`Marital status` == 'total', Age != 'total', Gender != 'total') %>%
  transmute(
    # first three characters indicate region
    region_id = case_when(ITTER107 == 'IT108' ~ 'ITC',
                          ITTER107 == 'IT109' ~ 'ITE',
                          ITTER107 == 'IT110' ~ 'ITF',
                          ITTER107 == 'IT111' ~ 'ITG',
                          T ~ str_sub(ITTER107, end = 3)),
    # first 4 indicate province
    province_id = case_when(ITTER107 == 'IT108' ~ 'ITC4',
                            ITTER107 == 'IT109' ~ 'ITE3',
                            ITTER107 == 'IT110' ~ 'ITF4',
                            ITTER107 == 'IT111' ~ 'ITG2',
                            T ~ str_sub(ITTER107, end = 4)),
    # full id is equal to territory
    terr_id = ITTER107,
    territory = Territory,
    gender_code = SEXISTAT1,
    gender = Gender,
    age = str_remove_all(Age, '\\syears') %>% as.numeric(),
    time = TIME,
    pop = Value
  )

# create a lookup to provice full hiearchy in tabular format
lookup <- pop %>% select(terr_id, territory) %>% unique()

pop <-
  pop %>%
  filter(terr_id != province_id) %>%
  # add in region
  left_join(
    y = transmute(lookup, region_id = terr_id, region = territory),
    by = 'region_id'
  ) %>%
  # add in province
  left_join(
    y = transmute(lookup, province_id = terr_id, province = territory),
    by = 'province_id'
  ) %>%
  transmute(
    region_id = region_id,
    region = region,
    province_id = province_id,
    province = province,
    territory_id = terr_id,
    territory = territory,
    sex_code = gender_code,
    sex = gender,
    age = age,
    pop = pop
  )

# write it
write_csv(pop, 'it_pop_data_agesex.csv', na = '')
