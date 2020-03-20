# England Population
# Written by Ewan Wakeman

# a basic clean up of england population estimates by age and 

require('tidyverse')
require('readxl')


lsoa_pop <- 'https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip'
lsoa_to_ics <- 'https://opendata.arcgis.com/datasets/520e9cd294c84dfaaf97cc91494237ac_0.csv'

if(!dir.exists('data')){dir.create('data')}
if(!file.exists('data/estimates.zip')){
  download.file(lsoa_pop, destfile = 'data/estimates.zip', mode = 'wb', timeout = 10000)
}
if(!file.exists('data/lookup_19.csv')){
  download.file(lsoa_to_ics, destfile = 'data/lookup_19.csv')
}

unzip('data/estimates.zip', exdir = 'data')

d <- 
  list.files('data', pattern = 'xlsx', full.names = T) %>%
  map_dfr(function(x){
    s <- excel_sheets(x)
    sf <- s[str_detect(s, 'ales')]
    sd <- map(sf, function(s){
      df <- read_excel(x, sheet = s, skip = 4)
      df <- gather(df, key = 'age', value = 'pop', -c(`Area Codes`, `LA (2019 boundaries)`, LSOA, `All Ages`))
      df <- transmute(df, 
                      lsoa_cd = `Area Codes`, 
                      lsoa = LSOA, 
                      total_pop = `All Ages`, 
                      age = if_else(is.na(as.numeric(age)), 90, as.numeric(age)),
                      age_chr = age,
                      sex = str_extract(s, '(?<=\\s).*ale') %>% str_to_lower(), 
                      pop = pop)
      return(df)
      }
    ) %>%
      reduce(bind_rows)
    return(sd)
  })

lsoa_lookup <- read_csv('data/lookup_19.csv') %>% rename_all(~str_to_lower(.) %>% str_replace_all('[0-9]{2}', '_'))

d <-
  d %>%
  filter(!is.na(lsoa)) %>%
  left_join(x = lsoa_lookup, y = ., by = 'lsoa_cd') %>%
  select(-fid, - total_pop)

by_lad <-
  d %>%
  group_by(lad_cd, lad_nm, age, sex) %>%
  summarise(pop = sum(pop, na.rm =T))

by_ccg <-
  d %>%
  group_by(stp_cd, stp_nm, ccg_cd, ccg_cdh, ccg_nm, age, sex) %>%
  summarise(pop = sum(pop, na.rm =T))

write_csv(by_lad, 'uk_pop_data_lad_agesex.csv', na = '')
write_csv(by_ccg, 'uk_pop_data_ccg_agesex.csv', na = '')
