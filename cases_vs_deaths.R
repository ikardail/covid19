library(tidyverse)
library(lme4)
library(broom)
options(tibble.print_max = 300)
load('imported.Rdata')
# country-wide sums on a day of just one measure
dat_e <- dat_d %>%
  filter(!is.na(pop), 
         !is.na(start)) %>%
  group_by(country, pop, date, start, name) %>%
  summarise(value = sum(value, na.rm = T) ) %>%
  group_by(country, name) %>%
  arrange(date) %>%
  mutate(to_lock = as.numeric(start - date)) %>%
  filter(n() > 3)
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
  pivot_wider(values_from = c(I, S)) %>% 
  mutate(dc = I_cases/S_cases,
         dd = I_dead/S_dead,
         d = dd - dc)
cofs
plot(dd ~ dc, cofs)
plot(S_dead ~ S_cases, cofs)
summary(cofs$d)
