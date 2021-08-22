library(data.table)
library(patchwork)
library(ggplot2)

stages <- fread("input/analysis.csv")
lag <- 3
wsize <- 7

# Identify stage of the tournament
stages[, nmatches := cumsum(match), by = country]
stages[, maxmatches := max(nmatches), by = country]
noqual <- stages[country %in% c("greece", "ireland",
                                "northern ireland", "norway")]
finals <- stages[maxmatches > 4]

#
stages <- c("Round of 16", "Quarterfinals", "Semifinals", "Finals")
stage_its <- function(x) {
    quarters <- finals[maxmatches >= x]
    quarters[, matchday := .SD[nmatches >= x, min(date)], by = country]
    quarters[, index := matchday + lag]
    quarters <- quarters[between(date, index - wsize, index + wsize)]

    # Create ITS variables
    quarters[, t := date - min(date), by = country]
    quarters[, intv := 1 * (date >= index)]
    quarters[, tpost := pmax(0, date - index)]

    # Fit the log-linear model
    prefit  <- lm(log(newcases) ~ t*country,
                  data = quarters[!is.na(newcases) & intv == 0])
    postfit <- lm(log(newcases) ~ t*country,
                  data = quarters[!is.na(newcases) & intv == 1])
    prepred  <- predict(prefit,  newdata = quarters, interval = "confidence")
    postpred <- predict(postfit, newdata = quarters[intv == 1], interval = "confidence")

    quarters[, `:=`(prepred = prepred[,1], prelb = prepred[,2], preub = prepred[,3])]
    quarters[intv == 1, `:=`(postpred = postpred[,1], postlb = postpred[,2], postub = postpred[,3])]

    ggplot(quarters, aes(x = t, y = log(newcases))) +
        # 95% CIs
        geom_ribbon(aes(ymin = prelb, ymax = preub),   fill = "grey20", alpha = 0.35) +
        geom_ribbon(aes(ymin = postlb, ymax = postub), fill = "grey20", alpha = 0.35) +
        # Observed data
        geom_point() +
        #geom_vline(xintercept = wsize, lty = 2) +
        geom_vline(xintercept = wsize - lag, lty = 2) +
        # Pre- and post-regression line
        geom_line(data = quarters[tpost == 0], aes(y = prepred), size = 1) +
        geom_line(data = quarters[intv == 1],  aes(y = prepred), lty = 2, size = 1) +
        geom_line(aes(y = postpred), size = 1) +
        # Styling
        ylab(stages[x - 3]) +
        coord_cartesian(ylim = c(4.5, 11)) +
        facet_wrap(~ country, ncol = 8) +
        theme_minimal() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.title.y = element_text(face = "bold"),
              axis.title.x = element_blank())
}

its_plots <- lapply(4:7, stage_its)

its <-
    its_plots[[1]] +   its_plots[[2]] +   its_plots[[3]] + its_plots[[4]] +
    plot_layout(design = "11111111
                          22222222
                          ##3333##
                          ###44###")
its
ggsave(plot = its, "figures/figure2.pdf", width = 11, height = 8.5, units = "in")

# Control countries
matchindex <- finals[country == "england", .(date, nmatches)]
noqual[, nmatches := NULL]
noqual <- noqual[matchindex, on = "date"]
noqual[, maxmatches := 7]
finals <- noqual
its_plots <- lapply(4:7, stage_its)

its <-
    its_plots[[1]] +   its_plots[[2]] +   its_plots[[3]] + its_plots[[4]] +
    plot_layout(design = "1111
                          2222
                          3333
                          4444") +
    plot_annotation(title = "Control countries",
                    subtitle = "Matchday was picked from the English team")
its
ggsave(plot = its, "figures/figure2-control.pdf", width = 11, height = 8.5, units = "in")
