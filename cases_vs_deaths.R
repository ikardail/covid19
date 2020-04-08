library(tidyverse)
library(lme4)
library(broom)
# library(rstanarm)
# options(mc.cores = parallel::detectCores())
options(tibble.print_max = 300)
load('imported.Rdata')
# country-wide sums on a day of just one measure
dat_e <- dat_d %>%
  filter(!is.na(pop), 
         # name == 'dead', 
         !is.na(start)) %>%
  group_by(country, pop, date, start, name) %>%
  summarise(value = sum(value, na.rm = T) ) %>%
  group_by(country, name) %>%
  arrange(date) %>%
  mutate(to_lock = as.numeric(start - date)) %>%
  filter(n() > 3)
lms <- lmer(log(value) ~ to_lock + (to_lock | country / name), 
            data = filter(dat_e, between(as.numeric(to_lock), -23, 7)))
summary(lms)
augment(lms) %>%
  ggplot(aes(log.value., .fitted, colour = name))+facet_wrap(~country)+geom_point()
augment(lms, filter(dat_e, between(as.numeric(to_lock), -23, 7))) %>%
  select(names(dat_e), d_fit = .fitted) %>%
  mutate(d_fit = exp(d_fit)) %>%
  ggplot(aes(date, value, shape = name))+geom_point()+
  geom_line(aes(y=d_fit), colour = 'red')+
  scale_y_log10()+facet_wrap(~country)
cofs <- coef(lms)[[1]] %>%
  rownames_to_column() %>%
  separate(rowname, c('name', 'country'), sep = ':') %>%
  left_join(coef(lms)[[2]] %>%
              rownames_to_column(var = 'country'), by = 'country') %>%
  rename_at(vars(matches('\\.')), ~ str_remove_all(., '\\(|\\)')) %>%
  gather('key', 'value', matches('\\.')) %>%
  separate(key, c('par', 'p'), sep = '\\.') %>%
  group_by(name, country, par) %>%
  summarise(value = sum(value)) %>%
  spread(par, value) %>%
  rename(I = Intercept, S = to_lock) %>%
  pivot_wider(values_from = c(I, S)) %>% 
  mutate(dc = I_cases/S_cases,
         dd = I_dead/S_dead,
         d = dd - dc)
cofs
plot(dd ~ dc, cofs)
plot(S_dead ~ S_cases, cofs)
summary(lm(dd ~ dc, cofs))$coefficients
summary(lm(S_dead ~ S_cases, cofs))$coefficients
summary(cofs$d)
