library(readxl)
library(proto)
library(nls2)
library(ggplot2)

kindata <- data.frame(read_excel("F:\\Practice\\R\\R prac\\curvefit examples\\prac2.xlsx"))

kinetics <- plot(kindata$time,kindata$fluorescence, xlab = "time(hr)", ylab = "fluorescence")

# function needed for visualization purposes
sigmoid = function(params, x) {
  params[1] + ((params[2] - params[1]) / (1 + exp((params[3]-x)/params[4])))
}

x = kindata$time
y = kindata$fluorescence

# fitting code
fitmodel <- nls(y~(a+(b-a)/(1 + exp((c-x)/d))) , start=list(a=100, b=1200, c=6, d=1))

# visualization code
# get the coefficients using the coef function
params=coef(fitmodel)

fitplot <- sigmoid(params,x)
plot(fitplot, type="l", xlab = "time(hr)", ylab = "Fluorescence units")
points(y)

ggpt <- ggplot(kindata, aes(time, fitplot)) 
ggpt + geom_point(y = kindata$fluorescence, color = "green") + geom_line(linetype = "dashed", color = "red") + ggtitle("kinetics data 17-09-2020") + xlab("Time(hrs)") + ylab("Fluorescence units") 
