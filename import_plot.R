library(tidyverse)
library(lubridate)
# library(stringdist)
library(rvest)
library(fs)
options(tibble.print_max = 300)
source('covid_functions.R')
# case and death data is from JHU github https://github.com/CSSEGISandData/COVID-19,
# population and lockdown dates from wikipedia

# get country populations from Wikipedia
pop = read_html("https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population") %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table(fill = T)
pop <- select(pop, country = `Country (or dependent territory)`, 
              pop = Population) %>%
  mutate(country = str_remove(country, '\\[.\\]$'),
         pop = as.numeric(str_remove_all(pop, ','))) %>%
  # adding 'cruise ship' combined 'population' of ~ 3 x 5000
  bind_rows(tibble(country = 'cruise', pop = 25000))
# get the lockdown dates from Wikipedia table, they keep changing the xpath, so need to verify on each update...
wiki_qua <- read_html("https://en.wikipedia.org/wiki/National_responses_to_the_2019%E2%80%9320_coronavirus_pandemic")
lockdowns <- wiki_qua %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>%
  html_table(fill = TRUE) %>% .[[1]] 
names(lockdowns) <- lockdowns[1,]
(lockdowns <- lockdowns[-c(1,2, nrow(lockdowns),nrow(lockdowns)-1),] %>%
  rename_at(vars(matches('date')), ~ str_extract(., '^[^ ]+')) %>%
  mutate_at(vars(Start, End), ~ str_sub(., 1, 10) %>% ymd(.)) %>%
  rename_all(str_to_lower) %>% arrange(desc(start)))
lockdown_country <- lockdowns %>%
  arrange(end, start) %>% group_by(country) %>% summarise(start = first(start), end = first(end))
# read the data from CSV files after syncing to JHU github, setup for that not covered here...
dat <- map(fs::dir_ls(path = "../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports",
       regexp = 'csv$'), read_csv, locale = locale(tz = 'UTC'))
# Changing colnames in data up to 23/3 to match later (proper) naming
dat <- dat %>% map(~ rename_at(., vars(matches('/| ')), ~ str_replace(., '/| ', '_')))
# the date format in early did not parse, marking them
fix <- map_lgl(dat, ~ class(.$Last_Update)[1] == 'character')
# fixing 'Last update'
dat[fix] <- dat[fix] %>%
  map(~ mutate(., Last_Update = mdy_hm(Last_Update, tz = 'UTC')))
# combining daily reports and fixing naming inconsistencies
dat_b <- dat %>%
  map(select, Province_State, Country_Region, date_upd = Last_Update, cases = Confirmed, dead = Deaths) %>%
  bind_rows(.id = 'filename') %>%
  # make a date from the csv file name, matching the 5pm update time for early timestamps
  mutate(date = mdy(str_match(filename, '/(.{10})\\.csv$')[,2], tz = 'UTC') + 3600*17 ) %>%
  select(-filename) %>%
  # country names vary from day to day, these fixes most of variation
  mutate(country = case_when(
    str_detect(Country_Region, 'hina|Maca| ong') | str_detect(Province_State, 'Kong') ~ 'China',
    str_detect(Country_Region, 'Others|ruise|rincess')  |
      str_detect(Province_State, 'Others|ruise|rincess') ~ 'cruise',
    str_detect(Country_Region, 'Korea') ~ 'South Korea',
    str_detect(Country_Region, 'US') | str_detect(Province_State, 'US') ~ 'United States',
    str_detect(Country_Region, 'Iran') ~ 'Iran',
    str_detect(Country_Region, 'UK') ~ 'United Kingdom',
    str_detect(Country_Region, 'Viet') ~ 'Vietnam',
    str_detect(Country_Region, 'Russia') ~ 'Russia',
    str_detect(Country_Region, 'Gambia') ~ 'Gambia',
    str_detect(Country_Region, 'Palest') ~ 'Palestine',
    str_detect(Country_Region, 'Maurit') ~ 'Mauritania',
    str_detect(Country_Region, 'Ireland') ~ 'Ireland',
    str_detect(Country_Region, 'Moldova') ~ 'Moldova',
    str_detect(Country_Region, 'Bahamas') ~ 'Bahamas',
    str_detect(Country_Region, 'Czech') ~ 'Czech Republic',
    str_detect(Country_Region, 'Faroe') ~ 'Denmark',
    str_detect(Country_Region, 'Tai') ~ 'Taiwan',
    str_detect(Country_Region, 'Guinea') ~ 'Guinea',
    str_detect(Country_Region, 'S.+Martin') ~ 'St. Martin',
    str_detect(Country_Region, "Cote d'Ivoire") ~ 'Ivory Coast',
    str_detect(Country_Region, "Cabo Verde") ~ 'Cape Verde',
    str_detect(Country_Region, "Congo") ~ 'Congo',
    str_detect(Country_Region, "Dominica") ~ 'Dominican Republic',
    str_detect(Country_Region, "Holy See") ~ 'Vatican City',
    TRUE ~ Country_Region),
    Province_State = case_when(
      str_detect(Province_State, 'ruise|rincess') ~ 'cruise',
      str_detect(Province_State, 'Chicago') ~ 'Chicago',
      str_detect(Province_State, 'Orange') ~ 'Orange',
      str_detect(Province_State, ', ON') ~ 'Ontario',
      str_detect(Province_State, 'Fench Guiana') ~ 'French Guiana',
      str_detect(Province_State, '^UK') ~ NA_character_,
      Province_State == country ~ NA_character_,
      TRUE ~ Province_State)
    ) %>%
  unite(location, Province_State, country, remove = F) %>%
  select(-Province_State, -Country_Region)
# list countries and locations with similar names
# dat_b$country %>% unique() %>% sort
# dat_b$location %>% unique() %>% sort %>% match_names(min_dist = 3, method = 'osa')
# making long, filtering
dat_d <- dat_b %>%
  mutate(date = as_date(date)) %>%
  # adding up location totals on a date
  group_by(location, country, date) %>%
  summarise_at(vars(cases, dead), sum, na.rm = T) %>%
  # combine cases and deaths into long format, drop NAs
  pivot_longer(c(cases, dead), values_drop_na = TRUE) %>%
  # add country info
  left_join(lockdown_country, by = 'country') %>%
  left_join(pop, by = 'country') %>%
  # filter when 0 was reported as well
  filter(value > 0, !is.na(pop)) %>%
  arrange(country, location, date)
# worst locations
cd_plot(dat_d, location, min_dead = 100)
# countries with > 500 dead, differenced
cd_plot(dat_d, country, min_dead = 100, differenced = T)
# countries, > 10 dead per million of population, most informative for dynamics
dat_d %>% 
  filter(pop > 300000) %>%
  mutate(value = value / pop * 1000000) %>%
  cd_plot(country, min_dead = 10, comment = ', per 1 million of population', differenced = T)
# countries I care about, with locations breakout and smaller threshold
interest <- c('Australia', 'Russia', 'Canada', 'New Zealand', 'Israel', 'United Kingdom', 'Iceland', 'United States')
# daily increases per 1M of pop in countries of interest
dat_d %>%
  filter(country %in% interest) %>%
  mutate(value = value / pop * 1000000) %>%
  cd_plot(country, min_dead = 0, comment = ', per 1 million of population')
# the Iceland story, csvs have to be downloaded manually from covid19.is
# download manually from https://covid.is/data and https://ourworldindata.org/coronavirus#testing-for-covid-19
ice <- map(dir_ls(regexp = '^ice.+\\.csv'), read_csv, col_types = 'ccc') %>%
  bind_rows(.id = 'type') %>%
  mutate(date = str_replace(X1, '\\.', '-') %>% paste0(., '-2020') %>% dmy) %>%
  rename_at(3:4, ~c('nuhi', 'decode')) %>%
  separate(type, into = c(NA, 'type', NA)) %>%
  pivot_longer(c(nuhi, decode), values_drop_na = T) %>%
  mutate(value = as.numeric(value)) %>%
  filter(value > 0) %>%
  group_by(type,) %>%
  arrange(type, name, date) %>%
  mutate(value_cum = cumsum(value))
group_by(ice, type, name) %>%
  summarise(counts = last(value_cum),
            prop_100K = counts*100000/pop$pop[pop$country=='Iceland'])
ggplot(ice, aes(date, value, colour=name))+geom_point()+facet_wrap(~type)+geom_line()+
  scale_y_log10()
ice_wide <- ice %>% select(-value_cum) %>%
  pivot_wider(names_from = type, values_from = c(value)) %>%
  mutate(ratio = cases/tests)
ggplot(ice_wide, aes(date, ratio, colour = name)) + geom_line()+geom_point()+scale_y_log10()
ggplot(ice_wide, aes(tests, cases, colour = name)) + geom_line()+
  geom_point() + geom_smooth(method = 'lm')+scale_x_log10()+scale_y_log10()
# lmi <- ice_wide %>% drop_na %>% lme(log(cases) ~ log(tests), data = .,
#                                    random = ~ 1 |name)
# exp(ranef(lmi))
# exp(coef(lmi))
save(dat_d, file = 'imported.Rdata')
