#Final Project
#Kyle Amyx
#PoliSci 3780 12:40PM

washPost <- read.csv("WashPost.csv", header = TRUE, sep = ",")
#Change "" factor to "U"(Unknown race)
levels(washPost$race)[1] <- "U"
levels(washPost$race)
attach(washPost)

library(lubridate)
washPost$date <- ymd(as.character(washPost$date))
detach(washPost)



raceCount <- table(unlist(subset(washPost, select = race)))
barplot(raceCount, names.arg = c("Unknown", "Asian", "Black", "Hispanic", "Native-American", "Other", "White"),xlab = "Race",
        ylab = "Frequency", main = "Frequency by victim's race(National)", col = c("black", "red", "blue","green","orange","purple","yellow"))
#Whites make up 48% of these documented shootings
#76.9% of population(Include Hispanic-White)


#Heat map showing state with highest concentration of shootings

library(rworldmap)
library(maps)
library(mapdata)





#Create count column to hold total number of shootings PER state
washPost$state <- as.character(washPost$state)
library(dplyr)
for(j in 1:length(unique(washPost$state))){
  washPost$total[washPost$state == unique(washPost$state)[j]] <- n_distinct(subset(washPost, state == unique(washPost$state)[j]))
}




#I LOSE ALASKA & HAWAII INFO....state.fips DOES NOT HAVE FIPS CODE FOR THESE 2 STATES
data(state.fips)
bloat.data <- merge(washPost, state.fips, by.x = "state", by.y = "abb")
#remove duplicates from bloat.data
my.data <- bloat.data[!duplicated(bloat.data$id), ]

#simplify polynames
my.data[names(my.data$polyname) %like% "michigan%"] <- "michigan"





my.colors <- colorRampPalette(c("blue","orange", "red"))(11)



my.data$colorID[0 <= my.data$total & my.data$total < 45] <- 1
my.data$colorID[45 <= my.data$total & my.data$total< 90] <- 2
my.data$colorID[90 <= my.data$total & my.data$total< 135] <- 3
my.data$colorID[135 <= my.data$total & my.data$total< 180] <- 4
my.data$colorID[180 <= my.data$total & my.data$total< 225] <- 5
my.data$colorID[225 <= my.data$total & my.data$total< 270] <- 6
my.data$colorID[270 <= my.data$total & my.data$total< 315] <- 7
my.data$colorID[315 <= my.data$total & my.data$total< 360] <- 8
my.data$colorID[360 <= my.data$total & my.data$total< 405] <- 9
my.data$colorID[405 <= my.data$total & my.data$total< 450] <- 10
my.data$colorID[450 <= my.data$total & my.data$total< 495] <- 11

my.data$color <- my.colors[my.data$colorID]

#build map
map('usa')
map("state", regions = my.data$polyname, col = my.data$color, exact = TRUE, fill = TRUE,
    resolution = 0, lty = 0, projection = "mercator")
title(main = "Total Police Shootings Since 2015")

#add state borders
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 1, 
    projection = "mercator")

legend("bottomleft", ncol = 2, c("0-45", "45-90", "135-180","180-225", "225-270","270-315","315-360",
                                 "360-405","405-450", "450-497"), fill = my.colors, cex = .25)



#Shootings w/ sign of mental_illness vs. no sign mental illness BY STATE

library(ggplot2)
#ggplot takes care of factor(mental_illness)
ggplot(my.data, aes(my.data$state)) + geom_bar(aes(fill = my.data$signs_of_mental_illness), position = "stack") + 
  labs(title = "Signs of Mental illness Vs. None", subtitle = NULL, x = "State", y = "# of Shootings", fill = "Mental Illness")






#AUSTIN, TEXAS DATA!!

austinTx <- read.csv("shooting_subjects_austin_tx.csv", header = TRUE, sep = ",")


ggplot(austinTx, aes(austinTx$Subject.Race.Ethnicity)) + geom_bar(aes(fill = austinTx$Subject.Race.Ethnicity)) + 
  labs(title = "Frequency by race(Austin,Tx)", x = "Race", y = "# of Shootings")


