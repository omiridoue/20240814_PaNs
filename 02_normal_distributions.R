#help(paste)

library(spssstatistics)

install.packages("readxl")
install.packages("psych")
install.packages("dplyr")
install.packages("knitr")
install.packages("ggplot2")

# Question 1 

# standard deviation is the square root of the variance -> sqrt(variance)
# standard deviation = sqrt(6^2)= 6

set.seed(123456, kind="Mersenne-Twister") 

# (1)(a) Generate 36 observations
small_sample <- rnorm(36, mean=100, sd=6)    # provide our own mean and standard deviation

# (1)(b) Generate 36 observations
moderate_sample <- rnorm(100, mean=100, sd=6)    # provide our own mean and standard deviation

# (1)(c) Generate 36 observations
large_sample <- rnorm(10000, mean=100, sd=6)    # provide our own mean and standard deviation

library(psych)


summary_samples <- rbind(Var1 = describe(small_sample,quant=c(.25,.75) ), 
                         Var2 = describe(moderate_sample,quant=c(.25,.75) ), 
                         Var3 = describe(large_sample,quant=c(.25,.75)) )

summary_samples <- summary_samples[,c("mean","se", "sd", "min", "q1", "median", "q3", "max")]

summary_samples

# Question 1 (ii) Summary of Samples 

require(psych)
require(knitr)

summary_samples <- rbind(Var1 = describe(small_sample,quant=c(.25,.75) ), 
                         Var2 = describe(moderate_sample,quant=c(.25,.75) ), 
                         Var3 = describe(large_sample,quant=c(.25,.75)) )

summary_samples <- summary_samples[,c("mean","se", "sd", "min", "Q0.25", "median", "Q0.75", "max")]

summary_samples

## Question 1 (ii) Summary of Student Samples

library(readxl)
require(reshape2)
require(ggplot2)
require(dplyr)

student_samples <- read_excel("C:/Users/2333157O/Documents/EBBRMS/week2_ebbrms_intro_to_stats/MED4048_2023-24_SampleSummaries.xlsx")

## Question 1 (ii) Summary of Samples - Mean

ggplot(data = melt(student_samples[,c("mean36",
                                      "mean100",
                                      "mean10000")]), aes(x=variable, y=value)) +  
      geom_boxplot(aes(fill=variable)) + 
      geom_hline(yintercept = 110, linetype = 2, colour = "darkred")

## Question 1 (ii) Summary of Samples - SD

ggplot(data = melt(student_samples[,c("SD36",
                                      "SD100",
                                      "SD10000")]), aes(x=variable, y=value)) +   
      geom_boxplot(aes(fill=variable)) + 
      geom_hline(yintercept = 6, linetype = 2,  colour = "darkred")

## Question 1 (ii) Summary of Samples - SE

ggplot(data = melt(student_samples[,c("SE36",
                                      "SE100",
                                      "SE10000")]), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable))

## Question 1 (ii) Summary of Samples - Mean

mean_samples <- rbind(mean36 = describe(student_samples$mean36,quant=c(.25,.75) ), 
                         mean100 = describe(student_samples$mean100,quant=c(.25,.75) ), 
                         mean10000 = describe(student_samples$mean10000,quant=c(.25,.75)) )

mean_samples <- mean_samples[,c("mean","se", "sd", "min", "Q0.25", "median", "Q0.75", "max")]

kable(mean_samples)


## Question 1 (ii) Summary of Samples - SD

SD_samples <- rbind(SD36 = describe(student_samples$SD36,quant=c(.25,.75) ), 
                         SD100 = describe(student_samples$SD100,quant=c(.25,.75) ), 
                         SD10000 = describe(student_samples$SD10000,quant=c(.25,.75)) )

SD_samples <- SD_samples[,c("mean","se", "sd", "min", "Q0.25", "median", "Q0.75", "max")]

SD_samples

## Question 1 (ii) Summary of Samples - SE

SE_samples <- rbind(SE36 = describe(student_samples$SE36,quant=c(.25,.75) ), 
                         SE100 = describe(student_samples$SE100,quant=c(.25,.75) ), 
                         SE10000 = describe(student_samples$SE10000,quant=c(.25,.75)) )

SE_samples <- SE_samples[,c("mean","se", "sd", "min", "Q0.25", "median", "Q0.75", "max")]

SE_samples

## Question 1 (iii) Further Plots - Small sample
m = 74; n = 36; mu = 110; sigma = 6; conf.level = .95

x.bar = student_samples$mean36
x.se = student_samples$SE36
t.crit = qt(1-(1-conf.level)/2, n-1)
LCL = x.bar - t.crit*x.se; UCL = x.bar + t.crit*x.se
cover = LCL < mu & UCL > mu
HI = max(UCL); LO = min(LCL) # to set dimensions of the plot
plot(c(0,m+1), c(HI, LO), col="white", ylab="Height (cm)", xlab="Dataset's Row Number",  
     main="95% Confidence Interval for simulated height data \n for samples (n=36)", 
     xaxs="i", ylim = c(100,120))

abline(h=mu, col="green2")
for(i in 1:m) {bar="blue"; if (cover[i]==F) {bar="red"}
lines(c(i,i), c(UCL[i], LCL[i]), col=bar, lwd=2) }

## Question 1 (iii) Further Plots - Moderate sample

m = 74; n = 100; mu = 110; sigma = 6; conf.level = .95
x.bar <- c(); x.se <- c();

x.bar = student_samples$mean100
x.se = student_samples$SE100
t.crit = qt(1-(1-conf.level)/2, n-1)
LCL = x.bar - t.crit*x.se; UCL = x.bar + t.crit*x.se
cover = LCL < mu & UCL > mu
HI = max(UCL); LO = min(LCL) # to set dimensions of the plot
plot(c(0,m+1), c(HI, LO), col="white", ylab="Height (cm)", xlab="Dataset's Row Number",  
     main="95% Confidence Interval for simulated height data \n for samples (n=100)", 
     xaxs="i", ylim = c(100,120))

abline(h=mu, col="green2")
for(i in 1:m) {bar="blue"; if (cover[i]==F) {bar="red"}
lines(c(i,i), c(UCL[i], LCL[i]), col=bar, lwd=2) }

## Question 1 (iii) Further Plots - Large sample 

m = 74; n = 10000; mu = 110; sigma = 6; conf.level = .95
x.bar <- c(); x.se <- c();

x.bar = student_samples$mean10000
x.se = student_samples$SE10000
t.crit = qt(1-(1-conf.level)/2, n-1)
LCL = x.bar - t.crit*x.se; UCL = x.bar + t.crit*x.se
cover = LCL < mu & UCL > mu
HI = max(UCL); LO = min(LCL) # to set dimensions of the plot
plot(c(0,m+1), c(HI, LO), col="white", ylab="Height (cm)", xlab="Dataset's Row Number",  
     main="95% Confidence Interval for simulated height data \n for samples (n=10000)", 
     xaxs="i", ylim = c(100,120))

abline(h=mu, col="green2")
for(i in 1:m) {bar="blue"; if (cover[i]==F) {bar="red"}
lines(c(i,i), c(UCL[i], LCL[i]), col=bar, lwd=2) }

## Question 1 (v) Calculate Confidence Intervals 

##So 95% CI for population mean from my n=36 sample is 

# lower bound of CI
round(summary_samples[1,"mean"],2)-1.96* round(summary_samples[1,"se"]/sqrt(36),3)  

# upper bound of CI
round(summary_samples[1,"mean"],2)+1.96* round(summary_samples[1,"se"]/sqrt(36),3)

## Question 1 (v) Calculate Confidence Intervals 

##So 95% CI for population mean from my n=100 sample is 

# lower bound of CI
round(summary_samples[2,"mean"],2)-1.96* round(summary_samples[2,"se"]/sqrt(100),3)  

# upper bound of CI
round(summary_samples[2,"mean"],2)+1.96* round(summary_samples[2,"se"]/sqrt(100),3)

## Question 1 (v) Calculate Confidence Intervals 

##So 95% CI for population mean from my n=10000 sample is 

# lower bound of CI
round(summary_samples[3,"mean"],2)-1.96* round(summary_samples[3,"se"]/sqrt(10000),3)  

# upper bound of CI
round(summary_samples[3,"mean"],2)+1.96* round(summary_samples[3,"se"]/sqrt(10000),3)

## Question 2

## Question 2

Professor Whimsy has taken the heights of 100 randomly sampled 5-year-old boys from Glasgow as he is interested in whether boys in Glasgow are on average shorter than the UK as a whole, where the average height of 5-year-old boys is known to be 110cm. He has told you that the mean height in the sample is 108.92cm and the standard deviation of the heights is 5.77cm and has asked you to analyse the data

mu = 110
x.sd = 5.77
x.bar = 108.92

## Question 2 (i)
# lower bound of CI
x.bar - 1.96 * x.sd/sqrt(100)

# upper bound of CI
x.bar + 1.96 * x.sd/sqrt(100)

## Question 2 (ii)
# lower bound of CI
x.bar - 1.81 * x.sd/sqrt(100)

# upper bound of CI
x.bar + 1.81 * x.sd/sqrt(100)