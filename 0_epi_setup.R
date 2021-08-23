library(epidemia)

cases <- read_csv("input/analysis.csv") %>%
    mutate(week = lubridate::week(date),
           wday = factor(wday(date, label = TRUE, week_start = 1), ordered = FALSE),
           masks = factor(masks),
           gatherlim = factor(gatherlim),
           ntests_sqrt = sqrt(ntests),
           newcases = ifelse(date < (min(date) + 15), NA, newcases))

# Andreas Kohers delay distributions (not used. ours is narrower)
delaylist <- read_csv("input/delaydist.csv")

serial_interval <- EuropeCovid2$si
plot(serial_interval)
inf2test <- dlnorm(1:20, log(5), log(1.4))
plot(inf2test)

rt <- epirt(formula = R(country, date) ~ 1 + match + other_match +
                #masks + gatherlim + # restriction
                #perc_sequences +
                rw(prior_scale = 0.05),
            prior_intercept = normal(log(2), 0.2), link = 'log')

obs <-  epiobs(formula = newcases ~ 0 + wday, #+ ntests_sqrt,# + offset(rep(1,93)),
               link = "identity",
               i2o = inf2test,
               prior = normal(1.1, 0.2)) # 75% of infected test positive

inf <- epiinf(gen = serial_interval,
              prior_seeds = normal(1000, 1500),
              seed_days = 15)
