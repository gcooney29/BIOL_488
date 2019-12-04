###11/26/2019###
###script for plotting worm data###

#set working directory
getwd()
setwd("/Users/gracecooney/Desktop/Colgate/github/BIOL_488")

#read in data
percen <- read.csv("percentages.csv")

#multiply colums by 100 to get percentage
percen$Control <- percen$Control *100
percen$Vinegar <- percen$Vinegar *100
percen$Baking_Soda <- percen$Baking_Soda *100
percen$Biochar <- percen$Biochar *100
percen$Dia_Earth <- percen$Dia_Earth *100

#create plot with legend
par(mar=c(5.1, 4.2, 4.2, 8.1), xpd=TRUE)
plot(percen$Day, percen$Baking_Soda, type = "l", lty = 1, lwd = 1.5, 
     xlab = "Day of Study", ylab = "Percent Dead",
     cex.axis = .8, cex.lab = .9, font.lab = 2)
lines(percen$Day, percen$Control, type = "l", lty = 1, col = "grey73", lwd = 2.5)
lines(percen$Day, percen$Dia_Earth, type = "l", lty = 3, lwd = 1.5)
lines(percen$Day, percen$Vinegar, type = "l", lty = 4, lwd = 1.5)
lines(percen$Day, percen$Biochar, type = "l", lty = 2, lwd = 1.5)
legend("topright", inset=c(-.37, -.087), 
       legend = c("\n Control \n", "\n Vinegar \n", "\n Baking soda \n", "\n Biochar \n", "\n Diatomaceous \n earth"), 
       title="Treatment Type", col = c("grey73", "black", "black", "black", "black"),
       lty = c(1, 4, 1, 2, 3), lwd = c(2.5, 1.5, 1.5, 1.5, 1.5), cex = 0.69, 
       ncol = 1, text.width = 3.5, box.lty = 0)

###conserved weight analysis###
#read in data
wc <- read.csv("weight_conserved.csv")

#download ggplot2 and dplyr
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

#multiply columns by 100 to get percentage
wc$Control <- wc$Control *100
wc$Biochar <- wc$Biochar *100
wc$D_Earth <- wc$D_Earth *100

#calculate mean and sd
wc_sum <- data.frame(name = c("Control", "D_Earth", "Biochar"),
                     mean = sapply(wc, mean, na.rm = TRUE),
                     sd = sapply(wc, sd, na.rm = TRUE))

#barplot with error bars
ggplot(wc_sum) +
  geom_bar(aes(x=name, y=mean), stat="identity", alpha=0.7, width = .5) +
  geom_errorbar( aes(x=name, ymin=mean-sd, ymax=mean+sd), width=0.2, 
                 colour="black", alpha=0.9, size=.8) +
  xlab("Treatment Type") + ylab("Weight Conserved (percentage)") +
  ylim(0, 115) + theme_bw()

