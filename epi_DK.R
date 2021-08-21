source("epi_setup.R")

dk_cases <- filter(cases, country == "denmark")

dk_cases$newcases[1:15] <- NA

args <- list(rt = rt, obs = obs, inf = inf, data = dk_cases, iter = 1e3,
             seed = 12345)

fm_dk <- do.call(epim, args)


p <- plot_obs(fm_dk, type = "newcases", levels = c(50, 95))
p

plot_rt(fm_dk,step = T,
        levels = seq(10, 90, by = 10))

p_infect <- plot_infections(fm_dk, levels = c(50, 95))


beta <- as.matrix(fm_dk, par_models = "R", par_types = "fixed")
b_match <- as.matrix(fm_dk, regex_pars = "^R\\|match")
b_delta <- as.matrix(fm_dk, regex_pars = "^R\\|perc_sequences")
b_masks <- as.matrix(fm_dk, regex_pars = "^R\\|.*masks")
b_gatherlim <- as.matrix(fm_dk, regex_pars = "^R\\|.*gatherlim")
mat <- cbind(b_match, b_masks, b_gatherlim, b_delta)
#colnames(mat) <- c("Match", "Masks", "Gatherlim")
bayesplot::mcmc_intervals(mat) + ggplot2::ggtitle("Effect sizes")


couterfact_data <- dk_cases %>%
    mutate(match = 0)

pred <- epidemia::posterior_predict(fm_dk, newdata = couterfact_data)
pred_infections <- posterior_infections(fm_dk, newdata = couterfact_data)

pred_df <- data.frame(date = pred$time, median = apply(pred$draws, 2, median),
                      infections = apply(pred_infections$draws, 2, median)[16:123],
                      )

p + geom_line(aes(date, median), data = pred_df)
p_infect +  geom_line(aes(date, infections), data = pred_df)

p_counterfact <- plot_obs(fm_dk, type = "newcases", newdata = couterfact_data, levels = c(50, 95))
plot_obs(fm_dk, type = "newcases", newdata = couterfact_data, cumulative = T, bar = F)
plot_obs(fm_dk, type = "newcases", cumulative = T, bar = F)
p_counterfact

