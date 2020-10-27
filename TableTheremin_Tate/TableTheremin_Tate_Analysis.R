# TableTheremin - Tate 

rm(list = ls())
par(mfrow=c(1,1), oma=c(2, 2, 1, 1), xpd = FALSE)

library(lattice)
library(sciplot)
library(car)
library(plyr) 
#library(lmerTest)
library(data.table)
library(emmeans)
library(here)
library(TSdist)
library(afex)

#### Loading Data ####

file_list <- paste(here::here("TableTheremin_Tate/TT_Tate_workData"),
                   list.files(here::here("TableTheremin_Tate/TT_Tate_workData")), sep = '/')
dataset <- ldply(file_list, read.table) 
dataset <- as.data.table(dataset)

## Questionnaire Data
quest <- as.data.table(read.csv(paste(here(), '/TableTheremin_Tate/TableTheremin_Tate_Questionnaires.csv', sep = ''),
                  header = T, sep = ';'))
quest <- quest[!is.na(DyadNr)]

quest[, participant := ""]
quest[quest$Side == "l",]$participant <- paste(quest[quest$Side == "l",]$DyadNr, "left", sep = "")
quest[quest$Side == "r",]$participant <- paste(quest[quest$Side == "r",]$DyadNr, "right", sep = "")

names(quest) <- c('dyadNr', 'side', 'age', 'gender', 'image', 'knowing', 'enjoying', 'improForms', 'instrument', 'miscellaneous', 'x', 'participant')

quest[improForms == 'no', improForms := '0']
quest[improForms %in% c('??', 'na'), improForms := NA]
quest[, improForms := as.numeric(as.character(improForms))]
unique(quest$improForms)

quest[instrument == 'no', instrument := '0']
quest[instrument == 'x6', instrument := '6']
quest[instrument %in% c('??', 'na'), instrument := NA]
quest[, instrument := as.numeric(as.character(instrument))]
unique(quest$instrument)
#### Preparing Data ####

names(dataset) <- c("order", 'time', "pitch", 'x', 'y', 'side', 'groupNr', 'trialLength', 'zoneLocation', 'leftChoice', 'rightChoice')
dataset[, zoneLocation := ifelse(zoneLocation == "Inside", "inside", "outside")]

# rid columns of ";" and ","
dataset$order <- gsub(",", "", dataset$order)
dataset$rightChoice <- gsub(";", "", dataset$rightChoice)

# exclude groups

# not properly set up yet:
dataset <- subset(dataset, groupNr != 1)
dataset <- subset(dataset, groupNr != 2)

# both participants below 10 years:
dataset <- subset(dataset, groupNr != 23)
dataset <- subset(dataset, groupNr != 55)
dataset <- subset(dataset, groupNr != 56)
dataset <- subset(dataset, groupNr != 58)
dataset <- subset(dataset, groupNr != 59)
dataset <- subset(dataset, groupNr != 61)
dataset <- subset(dataset, groupNr != 78)
dataset <- subset(dataset, groupNr != 82)
dataset <- subset(dataset, groupNr != 90)

# exclude groups with me in it: (or do it later so that the other person is in individual)
dataset <- subset(dataset, groupNr != 3)
dataset <- subset(dataset, groupNr != 4)

# exclude beginning of trials, when headphones were not working:
dataset <- dataset[!(dataset$groupNr == 48 & dataset$zoneLocation == "outside"),]

## exclude rows with pitch "0" - not sure why these occured
dataset <- subset(dataset, pitch != 0)

## Check whether recording stopped after 2 minutes / 1200000 ms
max(dataset$time)

# add location information based on row and pitch in dyads 1 - incl. 40 for left participant
# this is necessary, because y coordinates were not recorded for the first 40 groups
under41 <- dataset[dataset$groupNr < 41,]
after40In <- dataset[dataset$side == "left" & dataset$zoneLocation == "inside" & dataset$groupNr > 40,]
after40Out <- dataset[dataset$side == "left" & dataset$zoneLocation == "outside" & dataset$groupNr > 40,]

for (r in 1:nrow(under41)){
  temp <- dataset[r,]
  if (temp$side == "left" & temp$zoneLocation == "inside" & temp$pitch != 0 & temp$y == 0){
    tempPitch <- temp$pitch
    tempX <- temp$x
    
    newY <- unique(after40In[after40In$pitch == tempPitch & after40In$x == tempX,]$y)
    #print(paste(tempPitch, tempX, temp$y, newY))
    dataset[r,]$y <- newY
  } else if (temp$side == "left" & temp$zoneLocation == "outside" & temp$pitch != 0 & temp$y == 0){
    tempPitch <- temp$pitch
    tempX <- temp$x
    
    newY <- unique(after40Out[after40Out$pitch == tempPitch & after40Out$x == tempX,]$y)
    #print(paste(tempPitch, tempX, temp$y, newY))
    dataset[r,]$y <- newY
  }
}


## Change factors to character
dataset$leftChoice <- as.character(dataset$leftChoice)
dataset$rightChoice <- as.character(dataset$rightChoice)

## Calculate whether the same picture was choosen
dataset$sameChoice <- 0
dataset[dataset$leftChoice == dataset$rightChoice,]$sameChoice <- 1

## Column for stimulus set
dataset[, stimulusSet := ifelse(leftChoice %in% c('blue', 'yellow'), "albers", "pollock")]


### Mark pitches that are part of the dissonant zone:
dataset[, inZone := 0]
dissonantPitches <- c(54, 56, 58, 59, 61, 63, 66, 68, 70, 71, 73, 75)
dataset[pitch %in% dissonantPitches, inZone := 1]


## Include XY-coordinate column
dataset[, xy := x * 10 + y]


par(mfrow = c(1,1), mar = c(3.0, 3.0, 4.0, 2.0))
dataset$HMD <- ""
dataset[!(dataset$inZone == 1) & dataset$x != 2, HMD := "theHarmonicSide"]
dataset[!(dataset$inZone == 1) & dataset$x == 2,]$HMD <- "middle"
dataset[(dataset$inZone == 1) & dataset$x != 2,]$HMD <- "dissonantSide"

dataset$countRows <- 1

dataset$participant <- paste(as.character(dataset$groupNr), dataset$side, sep = "")
dataset[side == "left", inside := ifelse(x == 3, 1, 0)]
dataset[side == "right", inside := ifelse(x == 1, 1, 0)]
dataset[side == "left", outside := ifelse(x == 1, 1, 0)]
dataset[side == "right", outside := ifelse(x == 3, 1, 0)]
head(dataset[, c('side', 'x', 'inside', 'outside')], 30)

## Add time sections

# bin data into 2 halves
dataset$half <- trunc((dataset$time / 60000) + 1)

#### create indiData-datatable ####

head(dataset)

indiData <- dataset[, .(notesTotal = .N,
                         notesInZone = sum(inZone),
                         nrUniqueVisitedTiles = length(unique(xy)),
                         notesInside = sum(inside),
                         notesOutside = sum(outside),
                        exploration = length(unique(.SD[1:18]$xy))), 
                     by = .(groupNr, participant, sameChoice, zoneLocation, leftChoice, rightChoice, side, stimulusSet)]

indiData <- indiData[order(groupNr, participant)]
indiData[, innerToAll := notesInside / notesTotal]
indiData[, zoneToAll := notesInZone / notesTotal]
indiData <- merge(indiData, quest[, c("age", 'gender', 'knowing', 'enjoying', 'improForms', 'instrument', 'participant')], 
                  by = 'participant', all.x = T)
indiData[, notesMiddle := notesTotal - notesInside - notesOutside]
indiData[, proximity := (notesInside/notesTotal * 2 + notesMiddle/notesTotal)]
indiData[, relation := ifelse(knowing > 5, "friend", 'stranger')]
indiData[knowing %in% c(3, 3.5, 4, 4.5, 5), relation := 'neutral']
indiData[, sameChoice := ifelse(sameChoice == 1, 'same', 'different')]

indiData$participant <- as.factor(indiData$participant)
indiData$groupNr <- as.factor(indiData$groupNr)
indiData$sameChoice <- as.factor(indiData$sameChoice)
indiData$zoneLocation <- as.factor(indiData$zoneLocation)
indiData[, knowingCentered := knowing - mean(.SD$knowing)]


indiDataHalf <- dataset[, .(notesTotal = .N,
                         notesInZone = sum(inZone),
                         nrUniqueVisitedTiles = length(unique(xy)),
                         notesInside = sum(inside),
                         notesOutside = sum(outside),
                         exploration = length(unique(.SD[1:18]$xy))), 
                     by = .(groupNr, participant, half, sameChoice, zoneLocation, leftChoice, rightChoice, side, stimulusSet)]

indiDataHalf <- indiDataHalf[order(groupNr, participant)]
indiDataHalf[, innerToAll := notesInside / notesTotal]
indiDataHalf[, zoneToAll := notesInZone / notesTotal]
indiDataHalf <- merge(indiDataHalf, quest[, c("age", 'gender', 'knowing', 'enjoying', 'improForms', 'instrument', 'participant')], 
      by = 'participant', all.x = T)
indiDataHalf[, notesMiddle := notesTotal - notesInside - notesOutside]
indiDataHalf[, proximity := (notesInside/notesTotal * 2 + notesMiddle/notesTotal)]
indiDataHalf[, relation := ifelse(knowing > 5, "friend", 'stranger')]
indiDataHalf[knowing %in% c(3, 3.5, 4, 4.5, 5), relation := 'neutral']
indiDataHalf[, sameChoice := ifelse(sameChoice == 1, 'same', 'different')]

indiDataHalf$participant <- as.factor(indiDataHalf$participant)
indiDataHalf$groupNr <- as.factor(indiDataHalf$groupNr)
indiDataHalf$sameChoice <- as.factor(indiDataHalf$sameChoice)
indiDataHalf$zoneLocation <- as.factor(indiDataHalf$zoneLocation)
indiDataHalf[, knowingCentered := knowing - mean(.SD$knowing)]


##### Calculate edit distances for Group-Level Analysis #####
groupKnowing <- quest[, .(knowing = mean(knowing)), by = dyadNr]
groupData <- dataset[, .(distanceY = TSdist::EDRDistance(.SD[side == 'left']$y, 
                                                 .SD[side == 'right']$y, 0),
                         distanceY10 = TSdist::EDRDistance(.SD[side == 'left'][1:10]$y, 
                                                   .SD[side == 'right'][1:10]$y, 0),
                         distanceX = TSdist::EDRDistance(.SD[side == 'left']$x, 
                                                 .SD[side == 'right']$x, 0),
                         distanceX10 = TSdist::EDRDistance(.SD[side == 'left'][1:10]$x, 
                                                   .SD[side == 'right'][1:10]$x, 0),
                         distanceXY = TSdist::EDRDistance(.SD[side == 'left']$xy, 
                                                  .SD[side == 'right']$xy, 0),
                         distanceXY10 = TSdist::EDRDistance(.SD[side == 'left'][1:10]$xy, 
                                                    .SD[side == 'right'][1:10]$xy, 0),
                         distancePitch = TSdist::EDRDistance(.SD[side == 'left']$pitch, 
                                                     .SD[side == 'right']$pitch, 0),
                         distancePitch10 = TSdist::EDRDistance(.SD[side == 'left'][1:10]$pitch, 
                                                       .SD[side == 'right'][1:10]$pitch, 0)
                         #,yLeft = paste(.SD[side == 'left'][1:10]$y, collapse = "_"),
                         #yRight = paste(.SD[side == 'right'][1:10]$y, collapse = "_")
), by = .(groupNr, zoneLocation, sameChoice, stimulusSet)]
groupData <- merge(groupData, groupKnowing, by.x = 'groupNr', by.y = 'dyadNr')
groupData[, relation := ifelse(knowing > 5, "friend", 'stranger')]
groupData[knowing %in% c(3, 3.5, 4, 4.5, 5), relation := 'neutral']

groupData[, sameChoice := ifelse(sameChoice == 1, "same", "different")]
groupData[, sameChoice := as.factor(sameChoice)]
groupData[, zoneLocation := as.factor(zoneLocation)]
groupData[, relation := as.factor(relation)]


#### INITIAL PLOTTING ####
bargraph.CI(x.factor = HMD, response = N, 
            data = dataset[order(groupNr, side), .N, by = .(groupNr, participant, side, HMD)],
            main = "Notes played in corresponding Areas")

# Chosen stimuli
totalPeople <- nrow(indiData)
totalAlbersPeople <- indiData[stimulusSet == "albers", .N]
totalPollockPeople <- indiData[stimulusSet == "pollock", .N]

blueOnes <- indiData[side == "left" & leftChoice == "blue", .N] + indiData[side == "right" & rightChoice == "blue", .N]
yellowOnes <- indiData[side == "left" & leftChoice == "yellow", .N] + indiData[side == "right" & rightChoice == "yellow", .N]
brownOnes <- indiData[side == "left" & leftChoice == "brown", .N] + indiData[side == "right" & rightChoice == "brown", .N]
blackOnes <- indiData[side == "left" & leftChoice == "black", .N] + indiData[side == "right" & rightChoice == "black", .N]

# Stacked Bar Plot with Colors and Legend
par(mfrow = c(1,1))
counts <- matrix(c(blueOnes, yellowOnes, 0, 0, 0, 0, blackOnes, brownOnes), nrow = 4)
counts
barplot(counts, main="Distribution of Choosen Images",
        xlab="Sets of Images", col = c("dodgerblue4", "darkgoldenrod2", "black", "burlywood4"),
        legend = rownames(counts), names.arg = c("Albers", 'Pollock'), 
        ylab = "Number of participants")
segments(0.18, ((blueOnes + yellowOnes) / 2), 1.22, ((blueOnes + yellowOnes) / 2), 
         col = "white", lwd = 3, lty = 2)
segments(1.23, ((brownOnes + blackOnes) / 2), 2.8, ((brownOnes + blackOnes) / 2), 
         col = "white", lwd = 3, lty = 2)
text(0.7, blueOnes + 2, paste(round(blueOnes/totalAlbersPeople * 100, 2), "%", sep = ""))
text(1.9, blackOnes + 2, paste(round(blackOnes/totalPollockPeople * 100, 2), "%", sep = ""))


#### Analyses ####
## Mixed Effects Models ##

# contrasts must be set to sum up to zero 
# (see here: http://md.psych.bio.uni-goettingen.de/mv/unit/lm_cat/lm_cat_unbal_ss_explained.html)
# otherwise summary() gives unrelable results 
options(contrasts = c("contr.sum","contr.poly"))

names(indiData)
str(indiData)

#### Analysis - zoneToAll ratio ####

par(mfrow = c(2, 1), oma = c(2, 2, 0, 0))
bargraph.CI(relation, zoneToAll, sameChoice, 
            data = indiData[zoneLocation == "inside"], 
            main = 'Zone Inside', legend = T, ylim = c(0, 0.45))
bargraph.CI(relation, zoneToAll, sameChoice, 
            data = indiData[zoneLocation == "outside"], 
            main = 'Zone Outside', legend = T, ylim = c(0, 0.45))

# We can either use mixed() (= a wrapper for lmer) to get p-values directly, 
# or lmer combined with Anova(..., Type = 3).
# Some assumptions-testing plots only work on models generated by lmer() directly.
# However, mixed() signals potential errors, like none-centralized variables.
# Hence, I will run mixed() to check for problems and compare output with 
# lmer() + ANOVA(..., type = 3) output, which will also be used for assumption checking 
mixed(zoneToAll ~ sameChoice * knowingCentered * zoneLocation +
        (1|groupNr), data = indiData)

zoneToAllModelWholeTrial <- lmer(zoneToAll ~ 
                                   sameChoice * knowingCentered * zoneLocation +
                            (1|groupNr), 
                          data = indiData)
Anova(zoneToAllModelWholeTrial, type = 3)
plot(zoneToAllModelWholeTrial)
qqmath(zoneToAllModelWholeTrial, id = 0.05)

zoneToAllModelWholeTrial <- lmer(zoneToAll ~ 
                                   sameChoice * knowingCentered * zoneLocation +
                                   (1|groupNr), 
                                 data = indiData[!(groupNr %in% c(33, 65, 84))])
Anova(zoneToAllModelWholeTrial, type = 3)
qqmath(zoneToAllModelWholeTrial, id = 0.05)

bargraph.CI(zoneLocation, zoneToAll, 
            data = indiData[], 
            main = 'Zone Outside', legend = T, ylim = c(0, 0.45))


# zoneToAll analysis for first half

par(mfrow = c(2, 1), oma = c(2, 2, 0, 0))

bargraph.CI(relation, zoneToAll, sameChoice, 
            data = indiDataHalf[zoneLocation == "inside" & half == 1], 
            main = 'Zone Inside - 1st Half', legend = T, ylim = c(0, 0.45))
bargraph.CI(relation, zoneToAll, sameChoice, 
            data = indiDataHalf[zoneLocation == "outside" & half == 1], 
            main = 'Zone Outside - 1st Half', legend = T, ylim = c(0, 0.45))

mixed(zoneToAll ~ 
                                   sameChoice * knowingCentered * 
                                   zoneLocation + 
                                   (1|groupNr), 
                                 data = indiDataHalf[half == 1])

zoneToAllModelHalf1 <- lmer(zoneToAll ~ 
                               sameChoice * knowingCentered * 
                               zoneLocation + 
                               (1|groupNr), 
                             data = indiDataHalf[half == 1])

Anova(zoneToAllModelHalf1, type = 3)
plot(zoneToAllModelHalf1)
qqmath(zoneToAllModelHalf1, id = 0.05)
summary(zoneToAllModelHalf1)


zoneToAllModelHalf1 <- lmer(zoneToAll ~ 
                              sameChoice * knowingCentered * 
                              zoneLocation + 
                              (1|groupNr), 
                            data = indiDataHalf[half == 1 & 
                                                  !(groupNr %in% c(7, 13, 27, 33, 49, 67, 29))])
Anova(zoneToAllModelHalf1, type = 3)
plot(zoneToAllModelHalf1)
qqmath(zoneToAllModelHalf1, id = 0.05)
summary(zoneToAllModelHalf1)

bargraph.CI(zoneLocation, zoneToAll, 
            data = indiDataHalf[half == 1], 
            main = 'Zone Outside - 1st Half', legend = T, ylim = c(0, 0.45))

bargraph.CI(relation, zoneToAll, sameChoice,
            data = indiDataHalf[half == 1 & !(groupNr %in% c(7, 13, 27, 33, 49, 67, 29))], 
            main = 'Zone Outside - 1st Half', legend = T, ylim = c(0, 0.45))


#### Analysis - Exploration ??/18 in first 18 ####
par(mfrow = c(2, 1), oma = c(2, 2, 0, 0))
bargraph.CI(relation, exploration, sameChoice, 
            data = indiData[zoneLocation == "inside"], 
            main = 'Zone Inside', legend = T, ylim = c(0, 20))
abline(h = 18, col = "darkgrey")
bargraph.CI(relation, exploration, sameChoice, 
            data = indiData[zoneLocation == "outside"], 
            main = 'Zone Outside', legend = T, ylim = c(0, 20))
abline(h = 18, col = "darkgrey")

explorationModelWholeTrial <- mixed(exploration ~ 
                                     sameChoice * knowingCentered * 
                                     zoneLocation  + 
                                     (1|groupNr), 
                                   data = indiData[])
explorationModelWholeTrial

explorationModelWholeTrial <- lmer(exploration ~ 
                                      sameChoice * knowingCentered * 
                                      zoneLocation  + 
                                      (1|groupNr), 
                                    data = indiData[])

Anova(explorationModelWholeTrial, type = 3)
plot(explorationModelWholeTrial)
qqmath(explorationModelWholeTrial, id = 0.05)
summary(explorationModelWholeTrial)

explorationModelWholeTrial <- lmer(exploration ~ 
                                     sameChoice * knowingCentered * 
                                     zoneLocation  + 
                                     (1|groupNr), 
                                   data = indiData[!(groupNr %in% c(16, 18, 27, 43, 39, 40, 67, 68))])

Anova(explorationModelWholeTrial, type = 3)
plot(explorationModelWholeTrial)
qqmath(explorationModelWholeTrial, id = 0.05)
summary(explorationModelWholeTrial)

par(mfrow = c(1, 1), oma = c(2, 2, 0, 0))
bargraph.CI(zoneLocation, exploration, sameChoice,
            data = indiDataHalf[half == 1 & !(groupNr %in% c(16, 18, 27, 43, 39, 40, 67, 68))], 
            main = 'Zone Outside - 1st Half', legend = T, ylim = c(0, 15))



#### Analysis - Exploration ??/18 in totale ####

par(mfrow = c(2, 1), oma = c(2, 2, 0, 0))
bargraph.CI(relation, nrUniqueVisitedTiles, sameChoice, 
            data = indiData[zoneLocation == "inside"], 
            main = 'Zone Inside', legend = T, ylim = c(10, 20))
abline(h = 18, col = "darkgrey")
bargraph.CI(relation, nrUniqueVisitedTiles, sameChoice, 
            data = indiData[zoneLocation == "outside"], 
            main = 'Zone Outside', legend = T, ylim = c(10, 20))
abline(h = 18, col = "darkgrey")

explorationModelWholeTrial <- mixed(nrUniqueVisitedTiles ~ 
                                     sameChoice * knowingCentered * 
                                     zoneLocation  + 
                         (1|groupNr), 
                       data = indiData[])
explorationModelWholeTrial


explorationModelWholeTrial <- lmer(nrUniqueVisitedTiles ~ 
                                     sameChoice * knowingCentered * 
                                     zoneLocation  + 
                                     (1|groupNr), 
                                   data = indiData[])

Anova(explorationModelWholeTrial, type = 3)
plot(explorationModelWholeTrial)
qqmath(explorationModelWholeTrial, id = 0.05)
summary(explorationModelWholeTrial)

explorationModelWholeTrial <- lmer(nrUniqueVisitedTiles ~ 
                                     sameChoice * knowingCentered * 
                                     zoneLocation  + 
                                     (1|groupNr), 
                                   data = indiData[!(groupNr %in% c(6, 11, 13, 20, 25, 46, 57, 60, 76, 93, 95))])

Anova(explorationModelWholeTrial, type = 3)
plot(explorationModelWholeTrial)
qqmath(explorationModelWholeTrial, id = 0.05)
summary(explorationModelWholeTrial)


par(mfrow = c(1, 1), oma = c(2, 2, 0, 0))
bargraph.CI(relation, nrUniqueVisitedTiles, 
            data = indiData[!(groupNr %in% c(6, 11, 13, 20, 25, 46, 57, 60, 76, 93, 95))], 
            main = '', legend = T, ylim = c(10, 18))

par(mfrow = c(2, 1), oma = c(2, 2, 0, 0))
bargraph.CI(relation, nrUniqueVisitedTiles, sameChoice,
            data = indiData[!(groupNr %in% c(6, 11, 13, 20, 25, 46, 57, 60, 76, 93, 95)) &
                              zoneLocation == 'inside'], 
            main = 'inside', legend = T, ylim = c(10, 20))
bargraph.CI(relation, nrUniqueVisitedTiles, sameChoice,
            data = indiData[!(groupNr %in% c(6, 11, 13, 20, 25, 46, 57, 60, 76, 93, 95)) &
                              zoneLocation == 'outside'], 
            main = 'outside', legend = T, ylim = c(10, 20))

## exploration for 1st half only ##

par(mfrow = c(2, 1), oma = c(2, 2, 0, 0))
bargraph.CI(relation, nrUniqueVisitedTiles, sameChoice, 
            data = indiDataHalf[zoneLocation == "inside" & half == 1], 
            main = 'Zone Inside', legend = T, ylim = c(10, 20))
abline(h = 18, col = "darkgrey")
bargraph.CI(relation, nrUniqueVisitedTiles, sameChoice, 
            data = indiDataHalf[zoneLocation == "outside" & half == 1], 
            main = 'Zone Outside', legend = T, ylim = c(10, 20))
abline(h = 18, col = "darkgrey")

explorationModelFirstHalf <- mixed(nrUniqueVisitedTiles ~ sameChoice * 
                                     knowingCentered * zoneLocation + 
                           (1|groupNr), 
                         data = indiDataHalf[half == 1])
explorationModelFirstHalf

explorationModelFirstHalf <- lmer(nrUniqueVisitedTiles ~ sameChoice * 
                                     knowingCentered * zoneLocation + 
                                     (1|groupNr), 
                                   data = indiDataHalf[half == 1])
Anova(explorationModelFirstHalf, type = 3)
plot(explorationModelFirstHalf)
qqmath(explorationModelFirstHalf, id = 0.05)
summary(explorationModelFirstHalf)

explorationModelFirstHalf <- lmer(nrUniqueVisitedTiles ~ sameChoice * 
                                    knowingCentered * zoneLocation + 
                                    (1|groupNr), 
                                  data = indiDataHalf[half == 1 &
                                                        !(groupNr %in% c(43, 45, 93))])
Anova(explorationModelFirstHalf, type = 3)
plot(explorationModelFirstHalf)
qqmath(explorationModelFirstHalf, id = 0.05)
summary(explorationModelFirstHalf)


## Proximity ##
par(mfrow = c(2, 1), oma = c(2, 2, 0, 0))
bargraph.CI(relation, innerToAll, sameChoice, 
            data = indiData[zoneLocation == "inside"], 
            main = 'Zone Inside', legend = T, ylim = c(0, 0.5))
bargraph.CI(relation, innerToAll, sameChoice, 
            data = indiData[zoneLocation == "outside"], 
            main = 'Zone Outside', legend = T, ylim = c(0, 0.5))

proximityModelWholeTrial <- mixed(proximity ~ sameChoice * knowingCentered *
                                   zoneLocation +
                      (1|groupNr), 
                    data = indiData[])
proximityModelWholeTrial

proximityModelWholeTrial <- lmer(proximity ~ sameChoice * 
                                    knowingCentered * zoneLocation + 
                                    (1|groupNr), 
                                  data = indiData[])
Anova(proximityModelWholeTrial, type = 3)
plot(proximityModelWholeTrial)
qqmath(proximityModelWholeTrial, id = 0.05)
summary(proximityModelWholeTrial)

proximityModelWholeTrial <- lmer(proximity ~ sameChoice * 
                                   knowingCentered * zoneLocation + 
                                   (1|groupNr), 
                                 data = indiData[!(groupNr %in% c(22, 44, 75,
                                                                  33, 47, 65, 20,84))])

# removing more groups leads to singular fits
Anova(proximityModelWholeTrial, type = 3)
plot(proximityModelWholeTrial)
qqmath(proximityModelWholeTrial, id = 0.05)
summary(proximityModelWholeTrial)


par(mfrow = c(1, 1), oma = c(2, 2, 0, 0))
bargraph.CI(sameChoice, proximity, 
            data = indiData[!(groupNr %in% c(22, 44, 75,
                                             33, 47, 65, 20,84))], 
            main = '', legend = T, ylim = c(0.8, 1.2))
bargraph.CI(zoneLocation, proximity, 
            data = indiData[!(groupNr %in% c(22, 44, 75,
                                             33, 47, 65, 20,84))], 
            main = '', legend = T, ylim = c(0.8, 1.2))




par(mfrow = c(1, 1))
bargraph.CI(relation, distanceY10, sameChoice, data = groupData, legend = T, main = 'distanceY10', ylim = c(0, 10))

zoneToAllModelWholeTrial <- lm(distanceY10 ~ 
                                 sameChoice * knowing * 
                                 zoneLocation, 
                               data = groupData)
Anova(zoneToAllModelWholeTrial, type = 3)
plot(zoneToAllModelWholeTrial)
summary(zoneToAllModelWholeTrial)


bargraph.CI(relation, distanceY, sameChoice, data = groupData, legend = T, main = 'distanceY')

zoneToAllModelWholeTrial <- lm(distanceY ~ 
                                 sameChoice * knowing * 
                                 zoneLocation, 
                               data = groupData)
Anova(zoneToAllModelWholeTrial, type = 3)
Anova(zoneToAllModelWholeTrial, type = 2)
summary(zoneToAllModelWholeTrial)


bargraph.CI(relation, distanceX10, sameChoice, data = groupData, legend = T, main = 'distanceX10')

zoneToAllModelWholeTrial <- lm(distanceX10 ~ 
                                 sameChoice * knowing * 
                                 zoneLocation, 
                               data = groupData)
anova(zoneToAllModelWholeTrial)
Anova(zoneToAllModelWholeTrial, type = 3)


bargraph.CI(relation, distanceX, sameChoice, data = groupData, legend = T, main = 'distanceX')

zoneToAllModelWholeTrial <- lm(distanceX ~ 
                                 sameChoice * knowing * 
                                 zoneLocation, 
                               data = groupData)
Anova(zoneToAllModelWholeTrial, type = 3)


bargraph.CI(relation, distanceXY10, sameChoice, data = groupData, legend = T, main = 'distanceXY10')

zoneToAllModelWholeTrial <- lm(distanceXY10 ~ 
                                 sameChoice * knowing * 
                                 zoneLocation, 
                               data = groupData)
Anova(zoneToAllModelWholeTrial, type = 3)
Anova(zoneToAllModelWholeTrial, type = 2)


bargraph.CI(relation, distanceXY, sameChoice, data = groupData, legend = T, main = 'distanceXY')

zoneToAllModelWholeTrial <- lm(distanceXY ~ 
                                 sameChoice * knowing * 
                                 zoneLocation, 
                               data = groupData)
anova(zoneToAllModelWholeTrial)
Anova(zoneToAllModelWholeTrial, type = 3)
Anova(zoneToAllModelWholeTrial, type = 2)


bargraph.CI(relation, distancePitch10, sameChoice, data = groupData, legend = T, main = 'distancePitch10')

zoneToAllModelWholeTrial <- lm(distancePitch10 ~ 
                                 sameChoice * knowing * 
                                 zoneLocation, 
                               data = groupData)
anova(zoneToAllModelWholeTrial)
Anova(zoneToAllModelWholeTrial, type = 3)
Anova(zoneToAllModelWholeTrial, type = 2)


bargraph.CI(relation, distancePitch, sameChoice, data = groupData, legend = T, main = 'distancePitch')

zoneToAllModelWholeTrial <- lm(distancePitch ~ 
                                 sameChoice * knowing * 
                                 zoneLocation, 
                               data = groupData)
anova(zoneToAllModelWholeTrial)
Anova(zoneToAllModelWholeTrial, type = 3)
Anova(zoneToAllModelWholeTrial, type = 2)

