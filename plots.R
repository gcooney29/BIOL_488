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
     xlab = "Day of study", ylab = "Death (percentage)",
     cex.axis = .8, cex.lab = .9, font.lab = 1)
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
install.packages("car")
library(ggplot2)
library(dplyr)
library(car)

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
  xlab("Treatment") + ylab("Weight conserved (percentage)") +
  scale_x_discrete(breaks=1:3,
                     labels=c("Biochar","Control","Diatomaceous Earth")) +
  ylim(000, 115) + theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
  theme(axis.title.x = element_text(margin = margin(l = 0, r = 0, t = 20, b = 0))) + 
  theme(axis.title.y = element_text(margin = margin(l = 0, r = 20, t = 0, b = 0))) +
  theme(axis.ticks.length = unit(.2, "cm"), 
        axis.text.x = element_text(vjust = -3), 
        axis.text.y = element_text(hjust = -.1), 
        panel.border = element_rect(colour = "black", fill=NA, size=.7))
   
#ANOVA? 
#levene test
leveneTest(days_survived ~ Treatment, data = DS) #no homogeneity

#Kruskall wallis test
DS <- read.csv("days_survived.csv")

kruskal.test(days_survived ~ Treatment, data = DS)

#Dunn Test
dunn.test::dunn.test(DS$days_survived, DS$Treatment) 

#group data (mean, sd)
group_by(DS, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(days_survived, na.rm = TRUE),
    sd = sd(days_survived, na.rm = TRUE)
  )
#boxplot of results
boxplot(DS$days_survived ~ DS$Treatment, ylab = "Days survived",
        xlab = "Treatment", names = c("Baking soda", "Biochar",
                                      "Control", " ",
                                      "Vinegar"), font.lab = 2)
mtext(text= "Diatomaceous\nearth",
      side = 1, line = c(2), at = c(4))
text(x= 1, y= 10, labels= "a")
text(x= 2, y= 15, labels= "b")
text(x= 3, y= 15, labels= "b")
text(x= 4, y= 15, labels= "b")
text(x= 5, y= 1, labels= "c")

#create plot with legend (for poster!)
par(mar=c(5.1, 4.2, 4.2, 8.1), xpd=TRUE)
plot(percen$Day, percen$Vinegar, type = "l", lty = 1, col = "blue", lwd = 1.5, 
     xlab = "Day of Study", ylab = "Death (percentage)",
     cex.axis = .8, cex.lab = .9, font.lab = 2)
lines(percen$Day, percen$Control, type = "l", lty = 1, col = "black", lwd = 1.5)
lines(percen$Day, percen$Dia_Earth, type = "l", lty = 1, col = "goldenrod1", lwd = 1.5)
lines(percen$Day, percen$Baking_Soda, type = "l", lty = 1, col = "red3", lwd = 1.5)
lines(percen$Day, percen$Biochar, type = "l", lty = 1, col = "lightskyblue", lwd = 1.5)
legend("topright", inset=c(-.37, -.087), 
       legend = c("\n Control \n", "\n Vinegar \n", "\n Baking soda \n", "\n Biochar \n", "\n Diatomaceous \n earth"), 
       title="Treatment Type", col = c("black", "blue", "red3", "lightskyblue", "goldenrod1"),
       lty = c(1, 1, 1, 1, 1), lwd = c(1.5, 1.5, 1.5, 1.5, 1.5), cex = 0.69, 
       ncol = 1, text.width = 3.5, box.lty = 0)

#boxplot for poster
boxplot(DS$days_survived ~ DS$Treatment, ylab = expression(bold("Days Survived")),
        xlab = expression(bold("Treatment")), names = c("Baking Soda", "Biochar",
                                      "Control", " ",
                                      "Vinegar"), col = c("red3", "cornflowerblue",
                                                          "white", "goldenrod1", "blue"),
        font.lab = 2)
mtext(text= "Diatomaceous\nEarth",
      side = 1, line = c(2), at = c(4))
text(x= 1, y= 10, labels= "a")
text(x= 2, y= 15, labels= "b")
text(x= 3, y= 15, labels= "b")
text(x= 4, y= 15, labels= "b")
text(x= 5, y= 1, labels= "c")
font("xlab", face = "bold")

