rm(list=ls())
library(dplyr)

# load input
border_cross = read.csv("/Users/apple/Desktop/GitHub/ZYJ-projects/input/Border_Crossing_Entry_Data.csv",
                        stringsAsFactors = FALSE)
# border_cross = read.csv("/Users/apple/Desktop/GitHub/ZYJ-projects/input/test.csv", stringsAsFactors = FALSE)

attach(border_cross)

# create the ouput table with one row from the input table
output = border_cross[1, 4:7]
output$Value = as.integer(output$Value)

# iterate through the input table and get the sum of VALUEs group by BORDER, DATE, and MEASURE
# the first row is not iterated since it's already captured in output
for (i in 2:nrow(border_cross)) {
  
  # Locate current input row in output
  locate = which (output$Border    == Border[i]   & 
                    output$Date    == Date[i]     &
                    output$Measure == Measure[i]) 
  
  # If current row is not in output, create a row for it;
  # If current row is in the output, find the sum of Value;
  if (length(locate) == 0) {
      output = rbind(output, c(Border[i], Date[i], Measure[i], as.integer(Value[i])))
  } else {
      output$Value[locate] = output$Value[locate] + as.integer(Value[i])
  }

  output$Value = as.integer(output$Value)
}

detach(border_cross)

# order the ouput by DATE, VALUE, MEASURE, BORDER desc
output = output[order(as.POSIXlt(output$Date, format = "%m/%d/%Y %I:%M:%S %p"), 
                      output$Value, 
                      output$Measure, 
                      output$Border, 
                      decreasing = TRUE),]


## calculate monthly averages
output = cbind(output, Average = rep(0, nrow(output)))

# function to calculate difference in months
num_month = function(x) { # given a list of dates as characters formated as "%m/%d/%Y %I:%M:%S %p"
  x_mod = as.POSIXlt(x, format = "%m/%d/%Y %I:%M:%S %p")
  maxdate = max(x_mod)
  mindate = min(x_mod)
  mondf = 12 * (as.integer(format(maxdate, format = '%Y')) - 
                as.integer(format(mindate, format = '%Y')) ) + 
               (as.integer(format(maxdate, format = '%m')) - 
                as.integer(format(mindate, format = '%m')) )
  return(mondf)
}

# function to round the numbers properly
rounding = function (x) {
  if (ceiling(x) - x <= 0.5) {rx = ceiling(x)} else {rx = floor(x)}
  return(rx)
}

# calculate the running monthly averages
for (i in 1:nrow(output)) {
  
  # locate the rows of previous months with the same BORDER and MEASURE
  locate = which (output$Border    == output$Border[i]   &
                  output$Measure   == output$Measure[i])
  locate_curr = locate[locate >= i]
  locate_prev = locate[locate > i]
  
  if (length(locate_prev) > 0) {
    output$Average[i] = rounding(sum(output$Value[locate_prev] / num_month(output$Date[locate_curr])))
  }

}


# output
write.csv(output, file = '/Users/apple/Desktop/GitHub/ZYJ-projects/output/report.csv')








