library(tidyverse)
library(lme4)
library(rstanarm)
options(mc.cores = parallel::detectCores())
options(tibble.print_max = 300)
# source('covid_functions.R')# make dataset for modeling by country, using deaths
load('imported.Rdata')
# country-wide sums on a day, values per 1M of population
dat_e <- dat_d %>%
  filter(name != 'dead') %>%
  mutate(value = value * 1000000 / pop) %>%
  group_by(country, pop, date, start) %>%
  summarise(value = sum(value, na.rm = T) ) %>%
  group_by(country) %>%
  filter(n() > 3) %>%
  arrange(date) %>%
  mutate(age = as.numeric(date - first(date)),
         lv = log(value / lag(value))) %>%
  filter(lv > 0)
# calculate individual country offsets as random effect on early obs when < 1 dead/1M
offsets <- lmer(log(value) ~ age|country, data = filter(dat_e, value < 10))
dat_f <- coef(offsets)[[1]] %>%
  rownames_to_column('country') %>%
  transmute(country = country, off = `(Intercept)`/age) %>%
  filter(abs(off) < 100) %>%
  left_join(dat_e) %>%
  # adjust age with offsets
  mutate(age = age + off) %>%
  select(-off)
# fit ML ranef model (quick)
ff <- lmer(log(lv) ~ age | country, dat_f)
# further refine offset, by determining the tm flex point, and using data before
offsets_2 <- coef(ff)[[1]] %>%
  rownames_to_column(var = 'country') %>%
  filter(age <= 0) %>%
  transmute(country = country, md = - (`(Intercept)` - log(-age)) / age ) %>%
  left_join(dat_f) %>%
  filter(age < md) %>%
  lmer(log(value) ~ age|country, data = .)
# adjust age with offsets again, mostly much smaller
dat_g <- coef(offsets_2)[[1]] %>%
  rownames_to_column('country') %>%
  transmute(country = country, off = `(Intercept)`/age, mu = age) %>%
  left_join(dat_f) %>%
  mutate(age = age + off) %>%
  select(-off)
# plot to make sure offsets lined up, and modelling makes sense
dat_g %>% ggplot(aes(age, log(value))) +geom_point()+facet_wrap(~country) +
geom_hline(yintercept = 0) + geom_abline(aes(slope = mu, intercept = 0), linetype = 2)
ggplot(dat_f,aes(age, log(lv)))+geom_point()+facet_wrap(~country)
# MCMC in stan to get conf intervals for estimates
sn = stan_lmer(log(lv) ~ age | country, dat_g, cores = 4, iter = 5000) # ~ 40 mins
sn; summary(sn); VarCorr(sn)
# filter so that 95% end of B is still negative, i.e. decline significantly present
decl_country <- posterior_interval(sn, regex_pars = 'b\\[age') %>%
  data.frame(stringsAsFactors = F) %>%
  rownames_to_column(var = 'nm') %>%
  mutate(nm = str_remove_all(nm, '^b\\[|\\]| country') %>%
           str_replace_all('_', ' ')) %>%
  separate(nm, c('name','country'), sep = ':') %>%
  filter(X95. < 0) %>%
  select(country)
# calculate asympt level and flex date with errors from coefs, propagating coeff correlation into errors
cofs_country <- data.frame(se = se(sn)[-1], stringsAsFactors = F) %>%
  rownames_to_column(var = 'nm') %>%
  mutate(nm = str_remove_all(nm, '^b\\[|\\]| country') %>%
           str_replace('\\(Intercept\\)', 'int') %>%
           str_replace_all('_', ' ')) %>%
  separate(nm, c('name','country'), sep = ':') %>%
  pivot_wider(values_from = se, names_prefix = 'se_') %>%
  bind_cols(coef(sn)[[1]]) %>%
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
                              se_lA/lA*se_B/B*2*attr(VarCorr(sn)$country,'correlation')[1,2]))
         ) %>%
  filter(se_lA < 0.8 * lA, as_high < 1000000, !is.na(A)) %>%
  # select(country, A, B, as, as_high, md, se_md) %>%
  inner_join(decl_country)
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
  left_join(dat_g) %>%
  mutate(pred = exp(A * (1 - exp(-B * age))),
         date_1 = date - age,
         md = date_1 + md) %>%
  mutate(country = fct_reorder(country, value, max, .desc = T))

preds %>%
  # filter(country %in% c('Italy', 'Spain', 'France')) %>%
  # filter(country %in% c('South Korea', 'Japan', 'China')) %>%
  ggplot(aes(date, value)) +
  geom_vline(aes(xintercept = md), linetype = 2, colour = 'green') +
  geom_point(alpha = 0.7, size = 1.5) +
  geom_line(aes(y = pred), colour = 'red', size = 1) +
  geom_hline(aes(yintercept = as), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_vline(aes(xintercept = date_1), linetype = 2) +
  facet_wrap(~ country) +
  scale_y_log10() +
  theme_bw()
preds %>% group_by(country, pop, start, date_1) %>%
  summarise_at(vars(as, as_high, md, se_md, value), last) %>%
  arrange(md)
