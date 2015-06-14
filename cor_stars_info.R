# cor_stars_info.R
#
# heavily adapted by CSO from lm_eqn.R
# which in turn was based on:
# code courtesy user Jason on stackoverflow (https://stackoverflow.com/users/44873/jayden)
# https://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph

# input to the function is m, which is returned from an lm call
# ex:
# p1 = p + annotate(geom="text", x = 25, y = 300, label = lm_eqn(lm(y ~ x, df)), parse = TRUE)
# ex2: 
# m = lm(y ~ x, df); lm_eqn(m) will return string

cor_stars_info = function(x,y,subscript) {
      
      cor_info <- cor.test(x, y) # default method is "pearson"
      
      cor_stars <- numeric(length=1)
      # cycle through to set number of stars
      for (i in 1) {
            
            corpval <- cor_info$p.value
            
            if(eval(parse(text=corpval)) < 0.001){
                  cor_stars[i] <- "***"
            } else if(eval(parse(text=corpval)) < 0.01){
                  cor_stars[i] <- "**"
            } else if(eval(parse(text=corpval)) < 0.05){
                  cor_stars[i] <- "*"
            } else {
                  cor_stars[i] <- ""
            }
            
      }
      
      numobs = length(x)
      subsc = subscript
      
      l <- list(cor = round(cor_info$estimate,4), star = cor_stars, numobs = numobs, subsc = subsc)
      
      info <- substitute(ρ[subsc] == cor~star*","~italic(n) == numobs,l)
      as.character(as.expression(info))
      
}
