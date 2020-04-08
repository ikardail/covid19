library(tidyverse)
library(lme4)
library(rstanarm)
options(mc.cores = parallel::detectCores())
options(tibble.print_max = 300)
load('imported.Rdata')
# country-wide sums on a day of just one measure
dat_e <- dat_d %>%
  filter(name == 'dead', country != 'cruise') %>%
  # values per 1M of population
  mutate(value = value * 1000000 / pop) %>%
  group_by(country, pop, date, start) %>%
  summarise(value = sum(value, na.rm = T) ) %>%
  group_by(country) %>%
  arrange(date) %>%
  # log of ratio of daily change, to linearise on, after offsets
  mutate(lv = log(value / lag(value))) %>%
  # removes errors and redundant observations, requiring monotonic increase
  filter(lv > 0, !is.na(pop), !is.na(lv)) %>%
  # has to have a min number of observations to work
  filter(n() > 3) %>%
  mutate(age = case_when(
    is.na(lag(date)) ~ 0,
    TRUE ~ as.numeric(date - first(date))
  ))
# offsets per country individually, using early observations
# offsets <- dat_e %>% nest() %>% 
#   mutate(lms = map(data, ~ try(lm(log(value) ~ age, data = filter(., value < 10, value > 1))))) %>%
#   filter(map_lgl(lms, ~ class(.) == 'lm')) %>%
#   mutate(off = map_dbl(lms, ~ coef(.)[1]/coef(.)[2]))
# offsets as random effect on early obs
offsets <- lmer(log(value) ~ age|country,
                data = filter(dat_e, between(value, 1, 10)|(date < start + 3 & !is.na(start))))
# adjusting the age by offsets calculated from early exp fit
dat_f <- coef(offsets)[[1]] %>%
  rownames_to_column('country') %>%
  transmute(country = country, off = `(Intercept)`/age) %>%
  filter(abs(off) < 100) %>%
  left_join(dat_e) %>%
  # adjust age with offsets
  mutate(age = age + off) %>%
  select(-off)
# fit lv model to each country to further refine the offsets, from the md time
# offsets_2 <- dat_f %>% group_by(country) %>% nest %>%
#   mutate(lms = map(data, ~ try(lm(log(lv) ~ age, data = .))))%>%
#     filter(map_lgl(lms, ~ class(.) == 'lm')) %>%
#     mutate(md = map_dbl(lms, ~ -(coef(.)[1] - log(-coef(.)[2]))/coef(.)[2])) %>%
#   filter(md < 50) %>%
#   select(country, md) %>%
#   left_join(dat_f) %>%
#   filter(age < md, between(value, 1, 10)) %>%
#   lmer(log(value) ~ age|country, data = .)
# fit ML ranef model (quick)
ff <- lmer(log(lv) ~ age + (age | country), dat_f)
summary(ff)
# update offsets
offsets_2_data <- coef(ff)[[1]] %>%
  rownames_to_column(var = 'country') %>%
  filter(age <= 0) %>%
  transmute(country = country, md = - (`(Intercept)` - log(-age)) / age ) %>%
  left_join(dat_f) %>%
  filter(age < md)
offsets_2 <- lmer(log(value) ~ age|country, data = offsets_2_data)
# adjust age with offsets again, mostly much smaller
dat_g <- coef(offsets_2)[[1]] %>%
  rownames_to_column('country') %>%
  transmute(country = country, off = `(Intercept)`/age, mu = age) %>%
  left_join(dat_f) %>%
  mutate(age = age + off) %>%
  select(-off)
# plot to make sure offsets lined up, and modelling makes sense
dat_g %>% 
  filter(str_detect(country, '^A|^B')) %>%
  ggplot(aes(age, log(value))) +geom_point()+facet_wrap(~country) +
  geom_hline(yintercept = 0) + geom_abline(aes(slope = mu, intercept = 0), linetype = 2)
ggplot(dat_f,aes(age, log(lv)))+geom_point()+facet_wrap(~country)
# MCMC in stan to get conf intervals for estimates
sn = stan_lmer(log(lv) ~ age + (age | country), dat_g, cores = 4, iter = 5000) # ~ 1 mins/1000 iterations
summary(sn)
# filter so that 95% end of B is still negative, i.e. decline significantly present
decl_country <- posterior_interval(sn, regex_pars = 'b\\[age') %>%
  data.frame(stringsAsFactors = F) %>%
  rownames_to_column(var = 'nm') %>%
  mutate(nm = str_remove_all(nm, '^b\\[|\\]| country') %>%
           str_replace_all('_', ' ')) %>%
  separate(nm, c('name','country'), sep = ':') %>%
  filter(X95. + fixef(sn)[2] < 0) %>%
  select(country)
# calculate asympt level and flex date with errors from coefs, propagating coeff correlation into errors
VarCorr(sn)
cofs_country <- data.frame(se = se(sn), stringsAsFactors = F) %>%
  rownames_to_column(var = 'nm') %>%
  mutate(nm = str_remove_all(nm, '^b\\[|\\]| country') %>%
           str_replace('\\(Intercept\\)', 'int') %>%
           str_replace_all('_', ' ')) %>%
  separate(nm, c('name','country'), sep = ':') %>%
  pivot_wider(values_from = se, names_prefix = 'se_') %>%
  right_join(rownames_to_column(coef(sn)[[1]], var = 'country')) %>%
  mutate(B = - age, 
         se_B = se_age,
         lA = `(Intercept)` - log(B),
         se_lA = sqrt(se_int^2 + se_B^2 / B^2 + 2 * attr(VarCorr(sn)$country,'correlation')[1,2] *
           se_int * se_B / B),
         A = exp(lA),
         as = exp(A),
         as_high = exp(exp(lA + se_lA)),
         md = lA / B,
         se_md = sqrt(md^2*(se_lA^2/lA^2+se_B^2/B^2-
                   se_lA/lA*se_B/B*2*attr(VarCorr(sn)$country,'correlation')[1,2])) %>%
           round
         ) %>%
  filter(se_lA < lA, as < 1000000, !is.na(A)) #%>%
  # select(country, A, B, as, as_high, md, se_md) %>%
  # inner_join(decl_country)
(cofs = summarise_at(cofs_country, vars(A, B, 
                                        as_high,
                                        as), median, na.rm =T))
# general curve for all that fitted
curve(exp(cofs$A * (1 - exp(-cofs$B * x))), 0, 100, 
      # ylim = c(0, cofs$as_high),
      main = 'Average fitted dead per 1 million of population',
      ylab = 'total dead per 1M pop',
      xlab = 'days since 1 death per 1M pop')
abline(v = log(cofs$A) / cofs$B, lty = 2)
abline(h = c(exp(cofs$A)
             # , cofs$as_high
             ), lty = 2)
# plot fits vs actual for all, with predicted asymptots and inflexons
preds <- cofs_country %>%
  select(country, A, B, as, as_high, md, se_md) %>%
  left_join(dat_g) %>%
  mutate(pred = exp(A * (1 - exp(-B * age))),
         date_1 = date - age,
         md = date_1 + md) %>%
  mutate(country = fct_reorder(country, value, max, .desc = T))

preds %>%
  # filter(country %in% c('Italy', 'Spain', 'France')) %>%
  # filter(country %in% c('South Korea', 'Japan', 'China')) %>%
  ggplot(aes(date, value)) +
  geom_vline(aes(xintercept = md), linetype = 1, colour = 'green') +
  geom_point(alpha = 0.7, size = 1.5) +
  geom_line(aes(y = pred), colour = 'red', size = 1) +
  geom_hline(aes(yintercept = as), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_vline(aes(xintercept = date_1), linetype = 2) +
  facet_wrap(~ country) +
  scale_y_log10() +
  theme_bw()
preds %>% group_by(country, pop, start, date_1) %>%
  summarise_at(vars(value, as, as_high, md, se_md), last) %>%
  arrange(desc(md))
