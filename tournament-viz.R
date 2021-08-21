library(ggplot2)
library(patchwork)

final[, euro := max(match), by = country]
final[euro == 1,
      euro := fcase(country %in% c("england", "italy"), 5,
                    country %in% c("denmark", "spain"), 4,
                    country %in% c("switzerland", "belgium", "czechia", "ukraine"), 3,
                    country %in% c("sweden", "germany", "croatia", "france",
                                   "wales", "austria", "netherlands", "portugal"), 2,
                    euro == 1, 1)]
plt <- final[euro > 0 &
             between(date, as.Date("2021-06-11") - 14, as.Date("2021-07-11") + 14)]

plts <- lapply(1:5, function(lvl) {
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
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank())
})

plts[[1]] + plts[[2]] + plts[[3]] + plts[[4]] + plts[[5]] +
    plot_layout(design = "12###
                          12###
                          123##
                          12345
                          12345
                          123##
                          12###
                          12###") +
    plot_annotation(tag_levels = "A")
