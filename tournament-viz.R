library(ggplot2)
library(patchwork)

final <- fread("input/analysis.csv")
# 2021
final[, euro := max(match), by = country]
final[euro == 1,
      euro := fcase(country %in% c("england", "italy"), 5,
                    country %in% c("denmark", "spain"), 4,
                    country %in% c("switzerland", "belgium", "czechia", "ukraine"), 3,
                    country %in% c("sweden", "germany", "croatia", "france",
                                   "wales", "austria", "netherlands", "portugal"), 2,
                    euro == 1, 1)]
plt <- final[(euro > 0 |
             (euro == 0 & country %in% c("estonia", "iceland", "greece", "ireland",
                                         "northern ireland", "norway", "slovenia",
                                         "luxembourg"))) &
             between(date, as.Date("2021-06-11") - 14, as.Date("2021-07-11") + 14)]
plt_countries <- unique(plt[, .(country, euro)])

titles <- c("Not qualified", "Group stages", "Round of 16",
            "Quarterfinals", "Semifinals", "Finals")

plts <- lapply(0:5, function(lvl) {
    ggplot(plt[euro == lvl], aes(x = date, y = newcases)) +
                geom_vline(xintercept = c(as.Date("2021-06-11"),
                            as.Date("2021-07-11")),
                           color = "grey60") +
                geom_point(size = 1) +
                geom_vline(data = plt[euro == lvl &
                                      match == 1],
                           aes(xintercept = date),
                           lty = 2) +
                facet_grid(rows = vars(country),
                           scales = "free") +
        scale_y_log10(breaks = c(10, 100, 1000, 10000, 100000)) +
        ylab("") +
        xlab("") +
        ggtitle(titles[lvl + 1]) +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank())
})

tourney <-
    plts[[1]] + plts[[2]] + plts[[3]] + plts[[4]] + plts[[5]] + plts[[6]] +
        plot_layout(design = "123456
                              123456
                              1234##
                              1234##
                              123###
                              123###
                              123###
                              123###") +
        plot_annotation()

ggsave(plot = tourney, "figures/figure1.pdf", width = 15, height = 8, units = "in")

# 2020
cases <- fread("input/cases.csv")
cases <- cases[plt_countries, on = "country", nomatch = 0L]
plt <- cases[between(date, as.Date("2020-06-11") - 14, as.Date("2020-07-11") + 14)]
plt <- plt[newcases == 0, newcases := NA]

titles <- c("Not qualified", "Group stages", "Round of 16",
            "Quarterfinals", "Semifinals", "Finals")

plts <- lapply(0:5, function(lvl) {
  ggplot(plt[euro == lvl], aes(x = date, y = newcases)) +
    geom_vline(xintercept = c(as.Date("2020-06-11"),
                              as.Date("2020-07-11")),
               color = "grey60") +
    geom_point(size = 1) +
    facet_grid(rows = vars(country),
               scales = "free") +
    scale_y_log10(breaks = c(10, 100, 1000, 10000, 100000)) +
    ylab("") +
    xlab("") +
    ggtitle(titles[lvl + 1]) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
})

tourney2 <-
  plts[[1]] + plts[[2]] + plts[[3]] + plts[[4]] + plts[[5]] + plts[[6]] +
  plot_layout(design = "123456
                              123456
                              1234##
                              1234##
                              123###
                              123###
                              123###
                              123###") +
  plot_annotation(title = "Year 2020 (Control)")
tourney2
ggsave(plot = tourney2, "figures/figure1-control.pdf", width = 15, height = 8, units = "in")
