# code courtesy Hadley Wickham and a stackoverflow post
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
# http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
# also this example: http://rpubs.com/sjackman/grid_arrange_shared_legend

# input to the function is a list of plots, just like grid.arrange

grid_arrange_shared_legend <- function(...) {
      plots <- list(...)
      g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
      legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
      lheight <- sum(legend$height)
      grid.arrange(
            do.call(arrangeGrob, lapply(plots, function(x)
                  x + theme(legend.position="none"))),
            legend,
            ncol = 1,
            heights = unit.c(unit(1, "npc") - lheight, lheight))
}