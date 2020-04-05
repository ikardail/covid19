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
cd_plot <- function(z, wrap, scale_dead = 10, differenced = TRUE,
                    min_dead = 20, comment = NULL, ...) { #browser()
  wrap = enquo(wrap)
  y <- z %>%
    filter(name == 'dead') %>%
    group_by(!!wrap) %>%
    summarise(dead_tot = max(value, na.rm = T)) %>%
    filter(min_dead < dead_tot) %>%
    arrange(desc(dead_tot))
  g <- y %>%
    left_join(z, by = quo_name(wrap)) %>%
    ungroup %>%
    mutate(!!wrap := factor(!!wrap, levels = y[[1]])) %>%
    arrange(date) %>%
    group_by(!!wrap, name, date, start) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    group_by(!!wrap, name, start) %>%
    mutate(value = case_when(
      differenced & !is.na(lag(value)) ~ value - lag(value),
      TRUE ~ value
    )) %>%
    mutate(value = case_when(
      name == 'dead' ~ value * scale_dead,
      TRUE ~ value
    )) %>%
    ggplot(aes(date, value, colour = name)) + 
    facet_wrap(vars(!!wrap)) +
    geom_vline(aes(xintercept = start), linetype = 2) +
    scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)),
                  sec.axis = sec_axis( ~ . / scale_dead, name = 'Deaths',
                                       labels = scales::trans_format("log10", scales::math_format(10^.x)))) +
    scale_colour_manual(values = c(cases = "red", dead = "black"), labels = c('Cases', 'Deaths')) +
    theme_bw() + labs(y='Cases', title = paste0('Confirmed cases and deaths',
                                                if(differenced) {", newly reported"}, comment),
                      subtitle = paste0('Log10 scaled values, total dead > ', min_dead,
                                        ', vertical line marks lockdown date, panels ordered descending by total deaths'),
                      colour = 'Points',
                      caption = paste0('Cases on the left axis are at ', scale_dead,
                                       'x scale compared to deaths on the right axis)'))
  if(differenced) {g + geom_point()} else {g + geom_line()}
    
} 
