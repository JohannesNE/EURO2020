library(prophet)

cpoint <- function(ctry) {
    dk <- cases[country == ctry &
                between(date, as.Date("2021-05-11"),
                              as.Date("2021-08-11"))]
    dk[, y := log(newcases)]
    dk[, ds := date]

    ts <- prophet(df = dk, growth = "linear")

    plot(ts, predict(ts, dk)) +
        geom_vline(xintercept = as.POSIXlt(as.Date(c("2021-06-11", "2021-07-11")))) +
        add_changepoints_to_plot(ts) +
        ggtitle(ctry) +
        theme_minimal() +
        xlab("") +
        ylab("") +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())
}

plts <- pblapply(unique(final[euro > 0]$country), cpoint)

fig3 <- plts[[1]] + plts[[2]] + plts[[3]] + plts[[4]] + plts[[5]] + plts[[6]] +
plts[[7]] + plts[[8]] + plts[[9]] + plts[[10]] + plts[[11]] + plts[[12]] +
plts[[13]] + plts[[14]] + plts[[15]] + plts[[16]] + plts[[17]] + plts[[18]] +
plts[[19]] + plts[[20]] + plts[[21]] + plts[[22]] + plts[[23]] + plts[[24]]

ggsave(plot = fig3, "figures/fig3.pdf", width = 17, height = 8, units = "in")
