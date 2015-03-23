# code courtesy user Jason on stackoverflow (https://stackoverflow.com/users/44873/jayden)
# https://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph

# input to the function is m, which is returned from an lm call
# ex:
# p1 = p + annotate(geom="text", x = 25, y = 300, label = lm_eqn(lm(y ~ x, df)), parse = TRUE)
# ex2: 
# m = lm(y ~ x, df); lm_eqn(m) will return string

lm_eqn = function(m) {
      
      l <- list(a = format(coef(m)[1], digits = 2),
                b = format(abs(coef(m)[2]), digits = 2),
                r2 = format(summary(m)$r.squared, digits = 3));
      
      if (coef(m)[2] >= 0)  {
            eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
      } else {
            eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
      }
      
      as.character(as.expression(eq));                 
}
