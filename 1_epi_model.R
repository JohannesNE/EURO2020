source("0_epi_setup.R")

cases_mult <- filter(cases, country %in% c("denmark", "england", "italy", "spain")) %>%
    mutate(match_other = ifelse(date %in% date[match == 1], 1, 0),
           match_other = match_other - match)

matches <- select(cases_mult, country, date, match) %>%
    pivot_wider(names_from = country, values_from = match,
                names_prefix = "match_", values_fill = 0)

eng_matches <- matches %>%
    mutate(match_other = as.numeric(match_denmark | match_italy | match_spain)) %>%
    select(date, match_england, match_other)

ctrl_cases_mult <- filter(cases, country %in% c("norway", "ireland", "greece", "estonia")) %>%
    left_join(eng_matches, by = "date")


cases_mult <- left_join(cases_mult, matches, by = "date")

# Set new regression model for R(t) since the variable names for
# Participants and controls are slightly different
rt_matches_participants <- epirt(formula = R(country, date) ~ 1 +
                match +
                match_other +
                #masks + gatherlim + # restriction
                #perc_sequences +
                rw(prior_scale = 0.05),
            prior_intercept = normal(log(2), 0.2), link = 'log')

rt_matches_ctrl <- epirt(formula = R(country, date) ~ 1 +
                        match_england +
                        match_other +
                        #masks + gatherlim + # restriction
                        #perc_sequences +
                        rw(prior_scale = 0.05),
                    prior_intercept = normal(log(2), 0.2), link = 'log')

args <- list(rt = rt_matches_participants, obs = obs, inf = inf, data = cases_mult,
             iter = 1e3,
             control = list(max_treedepth = 10),
             chains = 4,
             algorithm = "sampling",
             seed = 12345)

fm_dk <- do.call(epim, c(args, group_subset = "denmark"))
fm_en <- do.call(epim, c(args, group_subset = "england"))
fm_it <- do.call(epim, c(args, group_subset = "italy"))
fm_sp <- do.call(epim, c(args, group_subset = "spain"))

model_list <- list("fm_dk" = fm_dk, "fm_en" = fm_en, "fm_it" = fm_it, "fm_sp" = fm_sp)
saveRDS(model_list, "case_models.rds")



plot_obs(fm_dk, type = "newcases", levels = c(50, 95))
plot_obs(fm_en, type = "newcases", levels = c(50, 95))
plot_obs(fm_it, type = "newcases", levels = c(50, 95))
plot_obs(fm_sp, type = "newcases", levels = c(50, 95))


plot_rt(fm_dk, type = "newcases", levels = c(50, 95))
plot_rt(fm_en, type = "newcases", levels = c(50, 95))
plot_rt(fm_it, type = "newcases", levels = c(50, 95))
plot_rt(fm_sp, type = "newcases", levels = c(50, 95))

as.matrix(fm_dk, regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals()
as.matrix(fm_en, regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals()
as.matrix(fm_it, regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals()
as.matrix(fm_sp, regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals()



args_ctrl <- list(rt = rt_matches_ctrl, obs = obs, inf = inf, data = ctrl_cases_mult,
                  iter = 1e3,
                  chains = 4,
                  control = list(max_treedepth = 10),
                  algorithm = "sampling",
                  seed = 12345)

fm_no <- do.call(epim, c(args_ctrl, group_subset = "norway"))
fm_ir <- do.call(epim, c(args_ctrl, group_subset = "ireland"))
fm_gr <- do.call(epim, c(args_ctrl, group_subset = "greece"))
fm_est <- do.call(epim, c(args_ctrl, group_subset = "estonia"))

model_list_ctrl <- list("fm_no" = fm_no, "fm_ir" = fm_ir, "fm_gr" = fm_gr, "fm_est" = fm_est)
saveRDS(model_list_ctrl, "control_models.rds")

plot_obs(fm_no, type = "newcases", levels = c(50, 95))
plot_obs(fm_ir, type = "newcases", levels = c(50, 95))
plot_obs(fm_gr, type = "newcases", levels = c(50, 95))
plot_obs(fm_est, type = "newcases", levels = c(50, 95))


plot_rt(fm_no, type = "newcases", levels = c(50, 95))
plot_rt(fm_ir, type = "newcases", levels = c(50, 95))
plot_rt(fm_gr, type = "newcases", levels = c(50, 95))
plot_rt(fm_est,type = "newcases", levels = c(50, 95))

as.matrix(fm_no,  regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals()
as.matrix(fm_ir,  regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals()
as.matrix(fm_gr,  regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals()
as.matrix(fm_est, regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals()
