## correlation-graphs-fancy.R
# a set of fancy correlation graphs that Christine got from the internet
#
# Christine Sierra O'Connell, UMN EEB/IonE


## when learning how to do this, CSO took a bunch of code from these places:
# http://musicroamer.com/blog/2011/01/16/r-tips-and-tricks-modified-pairs-plot/
# http://www.personality-project.org/r/r.graphics.html
# http://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html


########################################################################
# panel.cor - correlations on upper "pairs" panels
########################################################################


## the following code and figure is adapted from the help file for pairs 

##   put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
#first create a function (panel.cor)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y))
      txt <- format(c(r, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex * abs(r))
}



########################################################################
# pairs.panels - histogram and linear fit instead of smooth
########################################################################


# http://musicroamer.com/blog/2011/01/16/r-tips-and-tricks-modified-pairs-plot/
## Hello R, I want straight linear regression lines on my scatterplot called panel.lm

panel.lm <- function (x, y, pch = par("pch"), col.lm = "red", ...)
{ ymin <- min(y)
  ymax <- max(y)
  xmin <- min(x)
  xmax <- max(x)
  ylim <- c(min(ymin,xmin),max(ymax,xmax))
  xlim <- ylim
  points(x, y, pch = pch,ylim = ylim, xlim= xlim,...)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
        abline(lm(y[ok]~ x[ok]),
               col = col.lm, ...)
}

## Next, R please make me a series of pairs plots comparing all the varables, and list correlation vaues:

panel.cor.scale <- function(x, y, digits=2, prefix="", cex.cor)
{
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y,use="pairwise"))
      txt <- format(c(r, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex * abs(r))
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y,use="pairwise"))
      txt <- format(c(r, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex )
}

panel.hist <- function(x, ...)
{
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(usr[1:2], 0, 1.5) )
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks; nB <- length(breaks)
      y <- h$counts; y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

pairs.panels <- function (x,y,smooth=TRUE,scale=FALSE,main="Title")
{if (smooth ){
      if (scale) {
            pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale,lower.panel=panel.lm,main=main)
      }
      else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.lm,main=main)
      } #else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.lm)
}
else #smooth is not true
{ if (scale) {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale,main=main)
} else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,main=main) }
} #end of else (smooth)
} #end of function

pairs.panels.loess <- function (x,y,smooth=TRUE,scale=FALSE,main="Title")
{if (smooth ){
      if (scale) {
            pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale,lower.panel=panel.smooth,main=main)
      }
      else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth,main=main)
      } #else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.lm)
}
else #smooth is not true
{ if (scale) {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale,main=main)
} else {pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,main=main) }
} #end of else (smooth)
} #end of function


########################################################################
# bring in PerformanceAnalytics
########################################################################

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)


########################################################################
# bring in corrplot
########################################################################

#install.packages("corrplot")
library(corrplot)

cor.mtest <- function(mat, conf.level = 0.95) {
      mat <- as.matrix(mat)
      n <- ncol(mat)
      p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
      diag(p.mat) <- 0
      diag(lowCI.mat) <- diag(uppCI.mat) <- 1
      for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
                  tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
                  p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
                  lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
                  uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
            }
      }
      return(list(p.mat, lowCI.mat, uppCI.mat))
}



########################################################################
# print info
########################################################################


print("PerformanceAnalytics loaded, can now use chart.Correlation (ex: chart.Correlation(corrdf, pch = 21) )")
print("corrplot loaded, can now use corrplot and cor.mtest")
print("The following correlation graph functions were also defined:")
print("pairs.panels (to use alone)")
print("pairs.panels.loess (to use alone)")
print("panel.cor (to use with pairs)")
print("panel.cor.scale (feeds pairs.panels / pairs.panels.loess)")
print("panel.hist (feeds pairs.panels / pairs.panels.loess)")
print("panel.lm (feeds pairs.panels / pairs.panels.loess)")




