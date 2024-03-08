###############################################################################
### Script: FordKa_Analysis.R
### Version: 1.0
### Copyright: (c) 2024 by Alan Montgomery. Distributed using license CC BY-NC 4.0
###   To view this license see https://creativecommons.org/licenses/by-nc/4.0/
### Notes:
###   R script to create customer segments for ford ka using k-Means
###   Requires the excel spreadsheet with the data (FordKaData.xlsx).
###   This script creates clusters using both the psycographic and demographic
###   datasets using k-means analysis with 3 clusters.  You should consider
###   different settings of k and may want to consider different sets of
###   transformations of the input variables.
### Input Files:
###   FordKaData.xlsx          Excel dataset
### Output Files:
###   FordKa_Results.xlsx      Tables from the cluster analyses
### Variables:
###   df_forddemo              Demographic data (data frame)
###   df_fordpsyc              Psycholographic data (data frame)
###   df_fordquest             Psychographic questionnaire data (data frame)
###   df_ford                  Main dataset (data frame)
###   df_fordStd               Main dataset in standardized form (data frame)
###   cvec_fordquest           Truncated psychographic questionnaire data (character vector) 
###   cvec_qlist               Question list (character vector)
###   nvec_qlistShort          Short question list (numerical vector)
###   cvec_qlistShort          Short question list (character vector)
###   cvec_qnameShort          Short question name (character vector)
###   cvec_qdlist              Question - demographic list (character vector)
###   nvec_qdlist              Question - demographic list (numerical vector)
###############################################################################



###############################################################################
### setup the environment
###############################################################################

# load in additional packages to extend R functionality
if (!require(lattice)) { install.packages("lattice"); library(lattice) }
if (!require(gplots)) { install.packages("gplots"); library(gplots) }
if (!require(ggplot2)) { install.packages("ggplot2"); library(ggplot2) }
if (!require(reshape2)) { install.packages("reshape2"); library(reshape2) }
if (!require(openxlsx)) { install.packages("openxlsx"); library(openxlsx) }

# set to working directory of script (assumes data in same directory as script)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # only works in Rstudio scripts
# alternatively set the working directory manually
setwd("~/Users/mehta/Documents/CMU/Mini 3/AM/FordKA")



###############################################################################
### input data
###############################################################################

# read data from excel spreadsheet
df_forddemo <- read.xlsx("/Users/mehta/Documents/CMU/Mini 3/AM/FordKA/FordKaData.xlsx", sheet = "Demographic Data", startRow = 7, cols = 2:10)
df_fordpsyc <- read.xlsx("/Users/mehta/Documents/CMU/Mini 3/AM/FordKA/FordKaData.xlsx", sheet = "Psychographic Data", startRow = 7, cols = 2:63)
df_fordquest <- read.xlsx("/Users/mehta/Documents/CMU/Mini 3/AM/FordKA/FordKaData.xlsx", sheet = "Psychographic questionnaire", startRow = 7, cols = 2)

# create a new dataframe with both demogrpahic and psychographic data
df_ford <- cbind(df_forddemo, df_fordpsyc)

# transform the data to make it easier to use
df_fordquest <- paste0(1:62, ",", df_fordquest$Statement) # convert the question list into a character string to make it easier to work with
cvec_fordquest <- strtrim(df_fordquest, 30) # truncate the strings to the first 30 characters since some questions are quite long

# create some lists of variables which we will use later in the script
( cvec_qlist <- paste0("Q", 1:62) ) # create sequence of strings like Q1, ... Q62
# create a short list of psychographic questions to use later
nvec_qlistShort <- c(30, 57, 53, 1, 4, 12) # short list of questions
cvec_qlistShort <- paste0("Q", nvec_qlistShort) # append Q in front of the numbers to generate a list of questions to match variable names
cvec_qnameShort <- strtrim(df_fordquest[nvec_qlistShort], 30) # the first 30 characters of the strings

# create new standardized datasets using the scale function (set the mean of the new variable to 0 and stddev to 1)
df_fordStd <- scale(df_ford)



###############################################################################
### exploratory analysis of the data
###############################################################################

# check the structure of the data
str(df_ford)

# descriptive statistics for all the variables
summary(df_ford)

# to print an individual variable, enter it by itself
df_ford$Age

# create tables to describe the data
xtabs(~Age, data = df_ford)
xtabs(~AgeCategory, data = df_ford)
xtabs(~ChildrenCategory, data = df_ford)
xtabs(~FirstTimePurchase, data = df_ford)
xtabs(~Gender, data = df_ford)
xtabs(~IncomeCategory, data = df_ford)
xtabs(~MaritalStatus, data = df_ford)
xtabs(~NumberChildren, data = df_ford) 
xtabs(~PreferenceGroup, data = df_ford)

# to see the relationship between two variables do a cross-tab
xtabs(~ FirstTimePurchase + IncomeCategory, data = df_ford)

# let's plot all pairs of data in a matrix plot
pairs(~ Age + Gender + FirstTimePurchase + IncomeCategory + MaritalStatus + NumberChildren, data = df_ford)
pairs(~ jitter(Age) + jitter(Gender) + jitter(FirstTimePurchase)
  + jitter(IncomeCategory) + jitter(MaritalStatus) + jitter(NumberChildren), data = df_ford)

# create boxplots of the questions
# notice that instead of accessing entering each variable as ford$Q1 
# we instead use the format ford[,"Q1"], 
# R understands that we want the column of the object ford named Q1
# in this example we will refer to a list of ten questions
par(mfrow = c(4, 1), mar = c(4, 3, 1, 1))
boxplot(df_ford[, c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9")])
boxplot(df_ford[, c("Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19")])
boxplot(df_ford[, c("Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28", "Q29")])
boxplot(df_ford[, c("Q30", "Q31", "Q32", "Q33", "Q34", "Q35", "Q36", "Q37", "Q38", "Q39")])
boxplot(df_ford[, c("Q40", "Q41", "Q42", "Q43", "Q44", "Q45", "Q46", "Q47", "Q48", "Q49")])
boxplot(df_ford[, c("Q50", "Q51", "Q52", "Q53", "Q54", "Q55", "Q56", "Q57", "Q58", "Q59")])
boxplot(df_ford[, c("Q60", "Q61", "Q62")])

###############################################################################
### first cluster analysis with demographics
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# compute a k-means cluster with k=3 using just demographics
# notice that we omit PreferenceGroup from the list, 
# since we want to use this variable for PreferenceGroup
cvec_qdlist <- c("Age", "ChildrenCategory", "FirstTimePurchase", "Gender", "IncomeCategory", "MaritalStatus", "NumberChildren")
nvec_qdlist <- match(cvec_qdlist, colnames(df_fordStd)) # numeric positions of variables
( grpB <- kmeans(df_fordStd[, cvec_qdlist], centers = 5) ) # !! change =3 to other values of k !!
# !! hint: try computing scree graph, see the _Extra script !!

# plot the solutions against Age and FirstTimePurchase
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
par(mfrow = c(1, 1), mar = c(5, 4, 4, 1))
plot(jitter(df_fordStd[, "Age"]), jitter(df_fordStd[, "FirstTimePurchase"]), xlab = "Age", ylab = "FirstTimePurchase", col = grpB$cluster)
points(grpB$centers[, c("Age", "FirstTimePurchase")], col = 1:5, pch = 8, cex = 2)
legend("topleft", pch = 8, bty = "n", col = 1:3, c("1", "2", "3", "4", "5"))

# compare the cluster solutions with the PreferenceGroup
xtabs(~ df_ford$PreferenceGroup + grpB$cluster)
# here is a more visualize representation of a table with a BalloonPlot
balloonplot(table(df_ford$PreferenceGroup, grpB$cluster), xlab = "PreferenceGroup", ylab = "Cluster")

########################

# compare the cluster solutions with the PreferenceGroup
xtabs(~ df_ford$PreferenceGroup + grpB$cluster)

# summarize the centroids
grpBcenter <- t(grpB$centers) # create variable with the transpose of the centroids
rownames(grpBcenter) <- cvec_qdlist # add the demographic labels
print(grpBcenter) # print the centroid values for each question
parallelplot(t(grpBcenter), auto.key = list(text = c("1", "2", "3"), space = "top", columns = 3, lines = T)) # create a parallel plot to visualize the centroid values
splom(~ jitter(df_fordStd[, cvec_qdlist]), groups = grpB$cluster) # matrix scatter plot with clusters identified



###############################################################################
## second cluster analysis with psychographics
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(1248765792)

# compute a k-means cluster with k=3 using just the psychographics
(grpA <- kmeans(df_fordStd[, cvec_qlist], centers = 3)) # !! change =3 to other values of k !!
# !! hint: try computing scree graph, see the _Extra script !!

# plot the solutions against the Q1 and Q2
# since the data is categorical most of the plots will overlay one another,
# so instead we jitter the points -- which adds a small random number to each
# !! hint: try other questions !!
par(mfrow = c(1, 1), mar = c(5, 4, 4, 1) + .1)
plot(jitter(df_fordStd[, "Q1"]), jitter(df_fordStd[, "Q2"]), xlab = df_fordquest[1], ylab = df_fordquest[2], col = grpA$cluster)
points(grpA$centers[, c("Q1", "Q2")], col = 1:3, pch = 8, cex = 2)
legend("topleft", pch = 8, bty = "n", col = 1:3, c("1", "2", "3"))

# compare the cluster solutions with the PreferenceGroup
xtabs(~ df_ford$PreferenceGroup + grpA$cluster)

# summarize the centroids
grpAcenter <- t(grpA$centers) # create variable with the transpose of the centroids
rownames(grpAcenter) <- cvec_fordquest # add the question names
print(grpAcenter) # print the centroid values for each question
parallelplot(t(grpAcenter), auto.key = list(text = c("1", "2", "3"), space = "top", columns = 3, lines = T)) # create a parallel plot to visualize the centroid values
parallelplot(t(grpAcenter[nvec_qlistShort, ]), auto.key = list(text = c("1", "2", "3"), space = "top", columns = 3, lines = T)) # a parallel plot with just a few questions



###############################################################################
### save results
###############################################################################

# to save the data to an Excel spreadsheet (one sheet with the centroids and another with cluster assignments)
write.xlsx(list('A Centroids'=grpA$centers, 'A Assignment'=grpA$cluster,
  'B Centroids'=grpB$centers, 'B Assignment'=grpB$cluster),
  file = "FordKa_Results.xlsx", colnames = T, rownames = T, overwrite = T)
