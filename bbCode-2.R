# Scraped data for NCAABB 2014-15 Kentucky Wildcats

# install packages
install.packages("RcmdrMisc")
install.packages("XML")
install.packages("stringr")


# load libraries
library(RcmdrMisc)
library(XML)
library(stringr)

# Gather data
###############
# build the URL
url <- paste("http://www.sports-reference.com/cbb/schools/kentucky/2015-gamelogs.html")

# read the tables and select the one that has the most rows
tables <- readHTMLTable(url, stringsAsFactors = FALSE)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
tables[[which.max(n.rows)]]

results <- as.data.frame(tables[2])

attach(results)

# view data
View(results)

teamPts <- as.numeric(stats.Tm)

oppPts <- as.numeric(stats.Opp.1)

# points difference variable
ptsDiff <- teamPts - oppPts

# build remaining variables

offRating <- as.numeric(stats.ORtg)

defRating <- as.numeric(stats.DRtg)

pace <- as.numeric(stats.Pace)

ftRate <- as.numeric(stats.FTr)

threeRate <- as.numeric(stats.3PAr)

totShotPerc <- as.numeric(stats.TS.)

totRebndRate <- as.numeric(stats.TRB.)


#build simple linear model

model1 <- stepwise(lm(ptsDiff ~ offRating + defRating + pace + ftRate + threeRate + totShotPerc + totRebndRate))

summary(model1)


# build prediction versus actual results plot



predictResults <- predict(model1)

predictResults1 <- as.numeric(predictResults)



actualPtsDiff <- as.numeric(ptsDiff)

ptsDif1 <- as.numeric(actualPtsDiff[-22])
ptsDif2 <- as.numeric(ptsDif1[-21])

actualPtsDiff <- as.numeric(ptsDif2)


plot <- plot(predictResults1, actualPtsDiff)

z <- lm(actualPtsDiff~ predictResults1)

abline(z)

title("Basic Linear Model for Kentucky")

#### create csv file of scraped data in home directory

write.csv(results, file = "kentucky.csv")

######################