# Modeling Project EDA

# Import tennis data
tennis2021 <- read.csv("GitHub/Gray-Modeling-Project/atp_matches_2021.csv")

#EDA
summary(tennis2021)

# Histograms of winner's and loser's rank
hist(tennis2021$winner_rank)
hist(tennis2021$loser_rank)

# Histogram of loser's - winner's rank
hist(tennis2021$loser_rank - tennis2021$winner_rank, breaks = 50)

#EDA of loser's - winner's rank
summary(tennis2021$loser_rank - tennis2021$winner_rank)
mean(tennis2021$loser_rank - tennis2021$winner_rank, na.rm = TRUE)
sd(tennis2021$loser_rank - tennis2021$winner_rank, na.rm = TRUE)

# Probabilities of Winning based on Rank Differential
length((tennis2021$loser_rank - tennis2021$winner_rank)[(tennis2021$loser_rank - tennis2021$winner_rank) < 0])/length(tennis2021$loser_rank - tennis2021$winner_rank)
length((tennis2021$loser_rank - tennis2021$winner_rank)[(tennis2021$loser_rank - tennis2021$winner_rank) < -2])/length(tennis2021$loser_rank - tennis2021$winner_rank)
length((tennis2021$loser_rank - tennis2021$winner_rank)[(tennis2021$loser_rank - tennis2021$winner_rank) < -5])/length(tennis2021$loser_rank - tennis2021$winner_rank)
length((tennis2021$loser_rank - tennis2021$winner_rank)[(tennis2021$loser_rank - tennis2021$winner_rank) < -10])/length(tennis2021$loser_rank - tennis2021$winner_rank)

# Reviewing Upsets
tennis2021$upset <- (tennis2021$loser_rank - tennis2021$winner_rank) < -10

logmodel <- glm(upset ~ tourney_level + draw_size + surface + match_num + winner_hand + loser_hand + loser_age + winner_ht + winner_ioc + winner_age + round, data = tennis2021, family = "binomial")
summary(logmodel)

full <- glm(upset ~ tourney_level + draw_size + surface + match_num + winner_hand + loser_hand + loser_age + loser_ht + winner_ht + winner_ioc + winner_age + round, data = tennis2021, family = "binomial" )
backwards = step(full)

back <- glm(upset ~ loser_hand + loser_age + winner_ht + winner_ioc + winner_age + round, data = tennis2021, family = "binomial")
summary(back)

back2 <- glm(upset ~ winner_hand + loser_hand + loser_age + loser_ht + winner_ht + winner_ioc + winner_age + round, data= tennis2021, family = "binomial")
summary(back2)

# Random player's rank
plot(c(tennis2021[tennis2021$winner_id == 126207, "tourney_date"], tennis2021[tennis2021$loser_id == 126207, "tourney_date"]), c(tennis2021[tennis2021$winner_id == 126207, "winner_rank"], tennis2021[tennis2021$loser_id == 126207, "loser_rank"]))


# Creating a dataframe of players and their average ranks
players <- unique(c(tennis2021$winner_id, tennis2021$loser_id))

averageRank <- c()
height <- c()
age <- c()
hand <- c()
nationality <- c()

for (eachPlayer in players){
  averageRank <- append(averageRank, mean(c(tennis2021[tennis2021$winner_id == eachPlayer, "winner_rank"], tennis2021[tennis2021$loser_id == eachPlayer, "loser_rank"]), na.rm = TRUE))
  height <- append(height, max(c(tennis2021[tennis2021$winner_id == eachPlayer, "winner_ht"], tennis2021[tennis2021$loser_id == eachPlayer, "loser_ht"])))
  age <- append(age, mean(c(tennis2021[tennis2021$winner_id == eachPlayer, "winner_age"], tennis2021[tennis2021$loser_id == eachPlayer, "loser_age"])))
  hand <- append(hand, max(c(tennis2021[tennis2021$winner_id == eachPlayer, "winner_hand"], tennis2021[tennis2021$loser_id == eachPlayer, "loser_hand"])))
  nationality <- append(nationality, max(c(tennis2021[tennis2021$winner_id == eachPlayer, "winner_ioc"], tennis2021[tennis2021$loser_id == eachPlayer, "loser_ioc"])))
}

playerStats <- data.frame(players, averageRank, height, age, hand, nationality)

# Creating a linear model of rank
model <- lm(averageRank ~ height + age + hand + nationality, data=playerStats)

summary(model)

unique(hand)


# Plots for later use if necessary:

tennis2021$upset <- (tennis2021$loser_rank - tennis2021$winner_rank) < -10
tennis2021$tour_fac <- factor(tennis2021$tourney_level)
tennis2021$surface_fac <- factor(tennis2021$surface)

par(mfrow=c(1,3))

tour <- table(c(tennis2021[tennis2021$upset, "upset"],
                tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "upset"]),
              c(tennis2021[tennis2021$upset, "tour_fac"],
                tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "tour_fac"]))
norm_tour <- prop.table(tour,2)
barplot(norm_tour, , col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), xlab = "Tourney Level", ylab = "Proportion of Upsets", main = "Likelihood of Upsets vs.\nTourney Level")

barplot(prop.table(table(c(tennis2021[tennis2021$upset, "upset"],
                           tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "upset"]),
                         c(tennis2021[tennis2021$upset, "draw_size"],
                           tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "draw_size"])),
                   2), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), xlab = "Draw Size", ylab = "Proportion of Upsets", main = "Likelihood of Upsets vs.\nDraw Size")

barplot(prop.table(table(c(tennis2021[tennis2021$upset, "upset"],
                           tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "upset"]),
                         c(tennis2021[tennis2021$upset, "surface_fac"],
                           tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "surface_fac"])),
                   2), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), xlab = "Surface Type", ylab = "Proportion of Upsets", main = "Likelihood of Upsets vs.\nSurface Type")
legend('bottom',legend=c('Not Upset', 'Upset'),
       fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

par(mfrow=c(1,2))

age <- table(c(tennis2021[tennis2021$upset, "upset"],
               tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "upset"]),
             c(tennis2021[tennis2021$upset, "winner_age"],
               tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "winner_age"]))
norm_age <- prop.table(age,2)
plot(names(norm_age[1,-50]), norm_age[1,-50], col=rgb(1,0,0,0.5), lwd = 3, type = "l", xlab = "Winner's Age", ylab = "Proportion of Upsets", main = "Likelihood of Upsets vs. Age")
lines(names(norm_age[2,-50]), norm_age[2,-50], col=rgb(0,0,1,0.5), lwd = 3)

height <- table(c(tennis2021[tennis2021$upset, "upset"],
                  tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "upset"]),
                c(tennis2021[tennis2021$upset, "winner_ht"],
                  tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "winner_ht"]))
norm_height <- prop.table(height,2)
plot(names(norm_height[1,-50]), norm_height[1,-50], col=rgb(1,0,0,0.5), lwd = 3, type = "l", xlab = "Winner's Height", ylab = "Proportion of Upsets", main = "Likelihood of Upsets vs. Height")
lines(names(norm_height[2,-50]), norm_height[2,-50], col=rgb(0,0,1,0.5), lwd = 3)
legend('bottom', legend=c('Not Upset', 'Upset'),
       fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))


age <- table(c(tennis2021[tennis2021$upset, "upset"],
               tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "upset"]),
             c(tennis2021[tennis2021$upset, "winner_age"],
               tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "winner_age"]))
norm_age <- prop.table(age,2)
plot(names(norm_age[1,-50]), norm_age[1,-50], col=rgb(1,0,0,0.5), lwd = 3, type = "l", xlab = "Winner's Age", ylab = "Proportion of Upsets", main = "Likelihood of Upsets vs. Age")
lines(names(norm_age[2,-50]), norm_age[2,-50], col=rgb(0,0,1,0.5), lwd = 3)

expectedCounts = c(0,(hist(tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "winner_age"]))$counts,0)
upsetCounts = hist(tennis2021[tennis2021$upset, "winner_age"])$counts

expectedCounts
upsetCounts

expectedProportion = expectedCounts / (expectedCounts + upsetCounts)
upsetProportion = upsetCounts / (expectedCounts + upsetCounts)

hist(tennis2021[(tennis2021$loser_rank - tennis2021$winner_rank) > 10, "winner_age"], col=rgb(1,0,0,0.5))
hist(tennis2021[tennis2021$upset, "winner_age"], col=rgb(0,0,1,0.5), add = TRUE)

barplot(expectedProportion, col=rgb(1,0,0,0.5))
barplot(upsetProportion, col=rgb(0,0,1,0.5), add = TRUE)

