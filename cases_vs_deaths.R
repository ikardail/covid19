library(tidyverse)
library(lme4)
library(broom)
<<<<<<< HEAD
=======
# library(rstanarm)
# options(mc.cores = parallel::detectCores())
>>>>>>> 16732ed3972c05d229e62c57fb16c2fbaee9d8af
options(tibble.print_max = 300)
load('imported.Rdata')
# country-wide sums on a day of just one measure
dat_e <- dat_d %>%
  filter(!is.na(pop), 
<<<<<<< HEAD
=======
         # name == 'dead', 
>>>>>>> 16732ed3972c05d229e62c57fb16c2fbaee9d8af
         !is.na(start)) %>%
  group_by(country, pop, date, start, name) %>%
  summarise(value = sum(value, na.rm = T) ) %>%
  group_by(country, name) %>%
  arrange(date) %>%
  mutate(to_lock = as.numeric(start - date)) %>%
  filter(n() > 3)
<<<<<<< HEAD
dat_f <- dat_e %>%
  filter((between(as.numeric(to_lock), -1, 10) & name == 'cases') |
           (between(as.numeric(to_lock), -10, 10) & name == 'dead') )
lms <- lmer(log(value) ~ to_lock + name + (to_lock | country / name), 
            data = dat_f)
summary(lms)
new_data <- group_by(dat_f, country, name) %>%
  summarise() %>%
  left_join(dat_e) %>%
  ungroup() %>%
  filter(to_lock > -30)
augment(lms) %>%
  ggplot(aes(log.value., .fitted, colour = name))+facet_wrap(~country)+geom_point()
mutate(new_data, d_fit = exp(predict(lms, newdata = new_data))) %>%
  ggplot(aes(date, value, colour = name))+geom_point(size = 0.5)+
  geom_line(aes(y=d_fit, colour = name))+
  scale_y_log10()+facet_wrap(~country)+theme_bw()
cofs <- coef(lms)[[1]] %>%
  rownames_to_column() %>%
  separate(rowname, c('name', 'country'), sep = ':') %>%
  mutate(`(Intercept)` = case_when(
    name == 'dead' ~ `(Intercept)` + namedead,
    TRUE ~ `(Intercept)`)) %>%
  rename(I = `(Intercept)`, S = to_lock) %>%
  select(name, country, I, S) %>%
=======
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
>>>>>>> 16732ed3972c05d229e62c57fb16c2fbaee9d8af
  pivot_wider(values_from = c(I, S)) %>% 
  mutate(dc = I_cases/S_cases,
         dd = I_dead/S_dead,
         d = dd - dc)
cofs
plot(dd ~ dc, cofs)
plot(S_dead ~ S_cases, cofs)
<<<<<<< HEAD
=======
summary(lm(dd ~ dc, cofs))$coefficients
summary(lm(S_dead ~ S_cases, cofs))$coefficients
>>>>>>> 16732ed3972c05d229e62c57fb16c2fbaee9d8af
summary(cofs$d)
