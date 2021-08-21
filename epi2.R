source("epi_setup.R")

eng_cases <- filter(cases, country == "england")

eng_cases$newcases[1:15] <- NA

args <- list(rt = rt, obs = obs, inf = inf, data = eng_cases, iter = 1e3,
             seed = 12345)

fm_eng <- do.call(epim, args)


p <- plot_obs(fm_eng, type = "newcases", levels = c(50, 95))
p

plot_rt(fm_eng,step = T,
        levels = seq(10, 90, by = 10))

plot_infections(fm_eng)

beta <- as.matrix(fm_eng, par_models = "R", par_types = "fixed")
b_match <- as.matrix(fm_eng, regex_pars = "^R\\|match")
b_masks <- as.matrix(fm_eng, regex_pars = "^R\\|.*masks")
b_gatherlim <- as.matrix(fm_eng, regex_pars = "^R\\|.*gatherlim")
mat <- cbind(b_match, b_masks, b_gatherlim)
colnames(mat) <- c("Match", "Masks", "Gatherlim")
bayesplot::mcmc_intervals(mat) + ggplot2::ggtitle("Effect sizes")
