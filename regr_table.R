# regr_table.R
#
# takes a regression fit and 
#
# code courtesy:
# http://geokitchen.blogspot.com/2012/10/r-writing-regression-summary-to-table.html

###########################################################################
## Writes the regression coefficients in a csv file ##
###########################################################################

# Regression Coefficient as .csv , can be extended to add other objects too
# v 1.1
# 10/22/2012
# Anupam Anand
##email:  123@AdotB where 123 means anupam and  A=umd, B=edu

## reg_model is the regression model, fname is the name of the csv file you want 
regr_table <- function(reg_model, fname){
      
      # coefficients in dataframe
      regr_tab <- data.frame(summary(reg_model)$coefficients)
      
      # grab the coefficients
      colnames(regr_tab) <- colnames(summary(reg_model)$coefficients)
      # get the p-vals 
      regr_tab[ ,4] <- ifelse(regr_tab[ ,4] < .001, "< 0.001", 
                              ifelse(regr_tab[ ,4] < .01, "< 0.01", 
                                     round(regr_tab[ ,4], 3)))
      
      
      # format the table
      summary = format(regr_tab, autoformat = 1)
      
      # write it as a csv file 
      write.csv(summary, paste(fname,"_model_coeff.csv", sep=''))
}
