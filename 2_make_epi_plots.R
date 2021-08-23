library(patchwork)

case_models <- readRDS("case_models.rds")
control_models <- readRDS("control_models.rds")

create_case_and_rt_plots <- function(model) {
    list(plot_obs(fm_no, type = "newcases", levels = c(50, 95)),
         plot_rt)
}


eng_pos_plot <- plot_obs(case_models$fm_en, type = "newcases") + ggtitle("Daily Infections Postive Tests - England")
ggsave("plots/eng_daily_pos.png", eng_pos_plot)

eng_inf_plot <- plot_infections(case_models$fm_en) + ggtitle("Fitted Daily Infections - England")
ggsave("plots/eng_daily_inf.png", eng_inf_plot)



dk_pos_plot <- plot_obs(case_models$fm_dk, type = "newcases") + ggtitle("Denmark")
en_pos_plot <- plot_obs(case_models$fm_en, type = "newcases") + ggtitle("England")
sp_pos_plot <- plot_obs(case_models$fm_sp, type = "newcases") + ggtitle("Spain")
it_pos_plot <- plot_obs(case_models$fm_it, type = "newcases") + ggtitle("Italy")

comb_pos_cases <- dk_pos_plot + en_pos_plot + sp_pos_plot + it_pos_plot & plot_annotation(title = "Daily Infections Postive Tests")
ggsave("plots/combined_daily_pos_cases.png", comb_pos_cases, dpi = 200, width = 20, height = 10)


no_pos_plot <- plot_obs(control_models$fm_no, type = "newcases") + ggtitle("Norway")
ir_pos_plot <- plot_obs(control_models$fm_ir, type = "newcases") + ggtitle("Ireland")
est_pos_plot <- plot_obs(control_models$fm_est, type = "newcases") + ggtitle("Estonia")
gr_pos_plot <- plot_obs(control_models$fm_gr, type = "newcases") + ggtitle("Greece")

comb_pos_controls <- no_pos_plot + ir_pos_plot + est_pos_plot + gr_pos_plot & plot_annotation(title = "Daily Infections Postive Tests")
ggsave("plots/combined_daily_pos_controls.png", dpi = 200, width = 20, height = 10)


# Plot Rt

eng_rt_plot <- plot_rt(case_models$fm_en) + ggtitle("England")
ggsave("plots/eng_rt.png")

ir_rt_plot <- plot_rt(control_models$fm_ir) + ggtitle("Ireland")
ggsave("plots/ir_rt.png")


# Plot effects

eff_dk <- as.matrix(case_models$fm_dk,  regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals() + ggtitle("Denmark")
eff_en <- as.matrix(case_models$fm_en,  regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals() + ggtitle("England")
eff_sp <- as.matrix(case_models$fm_sp,  regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals() + ggtitle("Spain")
eff_it <- as.matrix(case_models$fm_it,  regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals() + ggtitle("Italy")

eff_case_comb <- eff_dk + eff_en + eff_sp + eff_it & plot_layout(ncol = 1)
ggsave("plots/eff_case.png", width = 6, height = 6)


eff_no <- as.matrix(control_models$fm_no,  regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals() + ggtitle("Norway")
eff_ir <- as.matrix(control_models$fm_ir,  regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals() + ggtitle("Ireland")
eff_gr <- as.matrix(control_models$fm_gr,  regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals() + ggtitle("Greece")
eff_est <- as.matrix(control_models$fm_est,  regex_pars = "^R\\|match.*") %>% bayesplot::mcmc_intervals() + ggtitle("Estonia")

eff_control_comb <- eff_no + eff_ir + eff_gr + eff_est & plot_layout(ncol = 1)
ggsave("plots/eff_control.png", width = 6, height = 6)

