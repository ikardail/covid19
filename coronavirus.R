library(tidyverse)
library(lubridate)
# library(stringdist)
library(broom)
library(rvest)
library(rlang)
library(nlme)
library(fs)
options(tibble.print_max = 300)

# case and death data is from JHU github https://github.com/CSSEGISandData/COVID-19,
# population and lockdown dates from wikipedia

# function to match similar names within a list of names
# match_names <- function(x, min_dist = 2, ...) {
#   m <- stringdistmatrix(x, x, ...)
#   diag(m) <- NA
#   m[m > min_dist] <- NA
#   a = apply(m, 2, which.min)
#   b = map_int(a, length) > 0
#   map2_chr(x[b], a[b], ~ paste(x[.y], .x, sep = '___'))
# }
# function to plot on dual axes on log10 scale, after ordering by number of cases and filtering for a min
cd_plot <- function(x, wrap, scale_dead = 100L, differenced = TRUE,
                    min_cases = 200L, comment = NULL, ...) { #browser()
  wrap = enquo(wrap)
  if(differenced) {
    x <- rename_at(x, vars(matches('^diff_value_')), ~ str_remove(., '^diff_value_'))
  } else {
    x <- rename_at(x, vars(matches('^value_')), ~ str_remove(., '^value_'))
  }
  y <- x %>%
    group_by(!!wrap) %>%
    summarise(cases_tot = if(differenced) {sum(cases, na.rm = T)} else {max(cases, na.rm = T)}) %>%
    filter(min_cases < cases_tot) %>%
    arrange(desc(cases_tot))
  left_join(y, x, by = quo_name(wrap)) %>%
    ungroup %>%
    mutate(!!wrap := factor(!!wrap, levels = y[[1]])) %>%
    group_by(!!wrap, date, start, end) %>%
    summarise_at(vars(cases, dead), sum, na.rm = T) %>%
    ggplot(aes(date, cases, colour = 'Cases')) + 
    geom_point(aes(y = dead * scale_dead, colour = 'Deaths')) +
    geom_point() + facet_wrap(vars(!!wrap)) + geom_line() +
    geom_vline(aes(xintercept = start), linetype = 2) +
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
            sec.axis = sec_axis( ~ . / scale_dead, name = 'Deaths',
                        labels = scales::trans_format("log10", scales::math_format(10^.x)))) +
    scale_colour_manual(values = c("red", "black")) +
    theme_bw() + labs(y='Cases', title = paste0('Confirmed cases and deaths',
          if(differenced) {", newly reported"}, comment),
          subtitle = paste0('Log10 scaled values, min total cases > ', min_cases,
                            ', vertical line marks lockdown date'),
          caption = paste0('Cases on the left axis are at ', scale_dead,
                           'x scale compared to deaths on the right axis)'))
} 
# get country populations from Wikipedia
b = read_html("https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population") %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table(fill = T)
pop <- select(b, country = `Country (or dependent territory)`, 
              pop = Population, date = Date) %>%
  mutate(country = str_remove(country, '\\[.\\]$'),
         pop = as.numeric(str_remove_all(pop, ',')),
         date = dmy(date)) %>%
  # adding 'cruise ship' combined 'population' of ~ 3 x 5000
  bind_rows(tibble(country = 'cruise', pop = 15000))
# get the lockdown dates from Wikipedia table, they keep changing the xpath, so need to verify on each update...
wiki_qua <- read_html("https://en.wikipedia.org/wiki/National_responses_to_the_2019%E2%80%9320_coronavirus_pandemic")
lockdowns <- wiki_qua %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[5]') %>%
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
# Changing colnames in previous to match and make proper
dat_a <- dat %>% map(~ rename_at(., vars(matches('/| ')), ~ str_replace(., '/| ', '_')))
# the date format in early did not parse, marking them
fix <- map_lgl(dat_a, ~ class(.$Last_Update)[1] == 'character')
# fixing 'Last update'
dat_a[fix] <- dat_a[fix] %>%
  map(~ mutate(., Last_Update = mdy_hm(Last_Update, tz = 'UTC')))
# combining daily reports and fixing naming inconsistencies
dat_b <- dat_a %>%
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
# dat_b$location %>% unique() %>% sort %>% match_names(min_dist = 3, method = 'osa')
dat_c <- dat_b %>%
  mutate(date = as_date(date)) %>%
  # adding up location totals on a date
  group_by(location, country, date) %>%
  summarise_at(vars(cases, dead), sum, na.rm = T) %>%
  # combine cases and deaths into long format, drop NAs
  pivot_longer(c(cases, dead), values_drop_na = TRUE) %>%
  # filter when 0 was reported as well
  filter(value > 0) %>%
  # differencing per location using date
  group_by(location, country, name) %>%
  arrange(date) %>%
  mutate(diff_value = case_when(
    is.na(lag(value)) ~ value,
    TRUE ~ value - lag(value)) ) %>%
  # remove redundant entries
  filter(diff_value > 0)
dat_d <- dat_c %>%
  # add population size of countries and lockdown dates, if any
  left_join(lockdown_country, by = 'country') %>%
  left_join(select(pop, country, pop), by = 'country') %>%
  # spread cumulative and differenced counts
  pivot_wider(values_from = c(value, diff_value)) %>%
  filter(!is.na(value_cases)) %>%
  arrange(country, location, date)
# check that data labels are consistent for China
# dat_d %>%
#   filter(country == 'China') %>%
#   ggplot(aes(date, value_cases)) +
#   geom_point() + facet_wrap(~ location) +
#   scale_y_log10()
# plot locations with > 5000 cases total
cd_plot(dat_d, location, min_cases = 5000)
cd_plot(dat_d, location, min_cases = 5000, differenced = F)
# countries with > 10000 cases, differenced
cd_plot(dat_d, country, min_cases = 10000, differenced = T)
# countries, > 50 cases per million of population
dat_d %>% 
  filter(pop > 300000) %>%
  mutate_at(vars(matches('cases|dead')), ~ . / pop * 1000000) %>%
  cd_plot(country, min_cases = 100, comment = ', per 1 million of population', differenced = T)
# countries I care about, with locations breakout and smaller threshold
interest <- c('Australia', 'Russia', 'Canada', 'New Zealand', 'Israel', 'United Kingdom', 'Iceland', 'United States')
# total counts per location in country of interest
dat_d %>%
  filter(country %in% interest) %>%
  cd_plot(location, min_cases = 20, differenced = F)
# daily increases per 1M of pop in countries of interest
dat_d %>%
  filter(country %in% interest) %>%
  mutate_at(vars(matches('cases|dead')), ~ . / pop * 1000000) %>%
  cd_plot(country, min_cases = 0, comment = ', per 1 million of population')
# make dataset for modeling by country, using deaths
dat_e <- dat_d %>%
  group_by(country, pop, date, start) %>%
  # country-wide sums on a day
  summarise_at(vars(value_cases, value_dead), sum, na.rm = T) %>%
  # values per 1M of population
  mutate_at(vars(value_cases, value_dead), ~ . / pop * 1000000  ) %>%
  rename_all(~str_remove(., 'value_')) %>%
  group_by(country) %>%
  # some filtering
  filter(!country %in% c('cruise'),
         # from 0.01 dead per 1M pop and up
         dead > 0.01,
         !is.na(pop) ) %>%
  # have at least 5 datapoints
  filter(n() > 5) %>%
  arrange(date) %>%
  #age as days from first time it was >1 per 1M
  mutate(age = as.numeric(date - first(date)))
offsets <- dat_e %>%
  # largest daily increase is ~ flex point, or end of exponential
  mutate(mid = which.max(dead - lag(dead)),
         mid = ifelse(mid < 4, n(), mid)) %>%
  # take data up to that one to estimate offset better
  filter(row_number() < mid) %>%
  nest() %>%
  # lm of logged value over age in each country
  mutate(lm = map(data, ~ lm(log(dead) ~ age, data = .) ),
         off = map_dbl(lm, ~ coef(.)[1]/coef(.)[2]),
         mu = map_dbl(lm, ~ coef(.)[2]),
         f = map(data, summarise_at, vars(date, pop, start), first)) %>%
  unnest(c(f)) %>%
  # date when reach 1 dead per 1M pop
  mutate(date_1 = date - off) %>% select(-lm, -date, -data)
# plot exponential fits for offset values to make sure the fit worked
# offsets %>% unnest(c(data)) %>% mutate(age = age + off) %>%
  # ggplot(aes(age, log(dead))) +geom_point()+facet_wrap(~country)+geom_smooth(method='lm')+
  # geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+geom_abline(aes(slope = mu, intercept = 0))
ungroup(offsets) %>% summarise_at(vars(mu, off), mean)
# adjusting ages in original data for global fit
dat_f <- offsets %>%
  # select(country, off) %>%
  left_join(dat_e, by = c('country', 'pop', 'start') ) %>%
  mutate(age = age + off)
# fitting Gompertz model to cumulative dead by country or location
fits <- nlme(log(dead) ~ A/B * (1 - exp(- B * age)),
       data = dat_f,
       fixed = B + A  ~ 1,
       random = A + B ~ 1 | country,
       start = c(A = 0.24, B = 0.02),
       control = list(msVerbose = T, maxIter = 10000, msMaxIter = 1000))
summary(fits)
ranef(fits)
# extracting coefficients and errors
fix_cofs <- coef(summary(fits)) %>% data.frame %>%
  rownames_to_column('name') %>%
  select(name, val = Value, err = Std.Error)
cofs <- VarCorr(fits) %>% unclass %>% 
  data.frame(stringsAsFactors = F) %>%
  rownames_to_column('name') %>%
  slice(1:2) %>%
  select(name, err = StdDev) %>%
  mutate(err = as.numeric(err)) %>%
  bind_rows(fix_cofs) %>%
  group_by(name) %>%
  summarise_all(sum, na.rm = T) %>%
  pivot_wider(values_from = c(val, err))
# general curve for all that fitted
curve(exp(cofs$val_A/cofs$val_B * (1 - exp(-cofs$val_B * x))), 0, 300, 
      # ylim = c(0, 1.05 * exp(cofs$val_A + cofs$err_A)),
      main = 'Average fitted dead per 1 million of population',
      ylab = 'total dead per 1M pop',
      xlab = 'days since 1 death per 1M pop')
abline(v = log(cofs$val_A/cofs$val_B) / cofs$val_B, lty = 2)
abline(h = c(exp(cofs$val_A / cofs$val_B)), lty = 2)
# plot fits vs actual for all, with predicted asymptots and inflexons
coef(fits) %>% data.frame(stringsAsFactors = F) %>%
  rownames_to_column('country') %>%
  mutate(as = exp(A/B),
         tm = log(A/B)/B) %>%
  left_join(select(offsets, -off), by = 'country') %>%
  mutate(date_tm = date_1 + tm,
         as_tot = as * pop / 1000000) %>%
  filter(as_tot < pop, !is.na(tm)) %>%
  select(country, pop, start, date_1, date_tm, as, as_tot) %>%
  arrange(desc(as))
augment(fits, data = dat_f) %>%
  mutate(fit = exp(.fitted)) %>%
  select(names(dat_f), fit) %>%
  ggplot(aes(age, dead)) +
  geom_line() +
  geom_line(aes(y = fit), linetype = 2) +
  facet_wrap(~ country) +
  scale_y_log10()

# the Iceland story, csvs have to be downloaded manually from covid19.is
ice <- map(dir_ls(regexp = '\\.csv'), read_csv, col_types = 'ccc') %>%
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
ggplot(ice, aes(date, value, colour=name))+geom_point()+facet_wrap(~type)+geom_line()+
  scale_y_log10()
ice_wide <- ice %>% select(-value_cum) %>%
  pivot_wider(names_from = type, values_from = c(value)) %>%
  mutate(ratio = cases/tests)
ggplot(ice_wide, aes(date, ratio, colour = name)) + geom_line()+geom_point()+scale_y_log10()
ggplot(ice_wide, aes(tests, cases, colour = name)) + geom_line()+
  geom_point() + geom_smooth(method = 'lm')+scale_x_log10()+scale_y_log10()
lmi <- ice_wide %>% drop_na %>% lme(log(cases) ~ log(tests), data = .,
                                   random = ~ 1 |name)
exp(ranef(lmi))
exp(coef(lmi))