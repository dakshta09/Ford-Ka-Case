###############################################################################
# Script: FordKa_Issues.R
#
# R script to understand effects of different  customer segments for ford ka using k-Means
# Requires the excel spreadsheet with the data (FordKaData.xlsx).
# This script creates clusters using both the psycographic and demographic
# datasets using k-means analysis with varying clusters.  You should consider
# different settings of k and may want to consider different sets of
# transformations of the input variables.
###############################################################################



###############################################################################
### setup the environment
###############################################################################

# load in additional packages to extend R functionality
if (!require(lattice)) {install.packages("lattice"); library(lattice)}
if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
if (!require(reshape2)) {install.packages("reshape2"); library(reshape2)}
if (!require(openxlsx)) {install.packages("openxlsx"); library(openxlsx)}
if (!require(gmodels)) {install.packages("gmodels"); library(gmodels)}
if (!require(psych)) {install.packages("psych"); library(psych)}

# set to your correct working directory
setwd("../data")



###############################################################################
### input data
###############################################################################

# read in Ford Ka datasets from the Excel file
forddemo=read.xlsx("FordKaData.xlsx",sheet="Demographic Data",startRow=7,colNames=T,rowNames=F,cols=2:10)  # read the demographic data
fordpsyc=read.xlsx("FordKaData.xlsx",sheet="Psychographic Data",startRow=7,colNames=T,rowNames=F,cols=2:63)  # read the psychographic data
fordquest=read.xlsx("FordKaData.xlsx",sheet="Psychographic questionnaire",startRow=7,colNames=T,rowNames=F,cols=2)  # read the question list
fordseg=read.xlsx("FordKaData.xlsx",sheet="Demographic Data",startRow=7,colNames=T,rowNames=F,cols=11:12)  # read the segments that ford created (do not use in cluster)

# if you have problems with read.xlsx you can read in the data from CSV files (make sure you uncomment lines below and download CSV files)
#forddemo=read.csv("FordKaDemographicData.csv",row.names=1)  # just the demographic data
#fordpsyc=read.csv("FordKaPsychographicData.csv",row.names=1)  # just the psychographic data
#fordquest=scan("FordKaQuestions.txt",what='a',sep='\n')  # question list, which is read as a vector
#fordseg=read.csv("FordKaSegmentData.csv")  # these are the segments that Ford came up with

# transform the data to make it easier to use
fordquest=paste0(1:62,',',fordquest$Statement)  # convert the question list into a character string to make it easier to work with
afordquest=strtrim(fordquest,30)  # truncate the strings to the first 30 characters since some questions are quite long
fordseg$SegName=as.factor(fordseg$SegmentName)  # convert the segment names into a factor for easier use as a classification variable
fordseg$SegmentName=NULL  # remove this variable
ford=cbind(forddemo,fordpsyc)  # create a new dataframe with both demogrpahic and psychographic data

# create some lists of variables which we will use later in the script
nqlist=1:62  # sequence of numbers from 1 to 62
qlist=paste0("Q",nqlist)
# let's try to cluster our questions by transposing the question data
nshortqlist=c(30,57,53,1,4,12)  # short list of questions
shortqlist=paste0("Q",nshortqlist)  # append Q in front of the numbers to generate a list of questions to match variable names
shortqname=strtrim(fordquest[nshortqlist],30)  # the first 30 characters of the strings
nvars=match(qlist,colnames(ford))   # define list of numeric variables

# create new standardized datasets using the scale function (set the mean of the new variable to 0 and stddev to 1)
xforddemo=scale(forddemo)
xfordpsyc=scale(fordpsyc)
xford=scale(ford)



###############################################################################
## compare the results of demographic clustering with and without data scaling
###############################################################################

# create list of variables, but notice that we omit PreferenceGroup from the list
# since we want to use this variable for PreferenceGroup
qdlist=c("Age","ChildrenCategory","FirstTimePurchase","Gender","IncomeCategory","MaritalStatus","NumberChildren")

# compare the description of the variables
describe(xford[,qdlist])
describe(ford[,qdlist])

# let's compare the correlations
round(cor(ford[,qdlist]),2)
round(cor(xford[,qdlist]),2)

# compute a k-means cluster with k=3 using unscaled demographics
set.seed(1248765792)                         # set random number to same value so we get same results
(grpA=kmeans(ford[,qdlist],centers=3))

# compute a k-means cluster with k=3 using scaled demographics
set.seed(1248765792)                         # set random number to same value so we get same results
(grpB=kmeans(xford[,qdlist],centers=3))

# summarize the centroids for unscaled
grpAcenter=t(grpA$centers)                   # create variable with the transpose of the centroids
rownames(grpAcenter)=qdlist                  # add the demographic labels
print(grpAcenter,digits=2)                   # print the centroid values for each question
parallelplot(t(grpAcenter),main="Unscaled")  # create a parallel plot to visualize the centroid values

# summarize the centroids for scaled
grpBcenter=t(grpB$centers)                   # create variable with the transpose of the centroids
rownames(grpBcenter)=qdlist                  # add the demographic labels
print(grpBcenter,digits=2)                   # print the centroid values for each question
parallelplot(t(grpBcenter),main="Scaled")    # create a parallel plot to visualize the centroid values

# compare the cluster solutions
xtabs(~grpA$cluster+grpB$cluster)



###############################################################################
## compare the effects of different seeds and starting points on the solutions
###############################################################################

# first cluster solution
set.seed(1248765792)
grpA1=kmeans(xford[,qdlist],centers=3)
grpA1center=t(grpA1$centers)      # create variable with the transpose of the centroids
rownames(grpA1center)=qdlist      # add the demographic labels
print(grpA1center,digits=2)       # print the centroid values for each question
parallelplot(t(grpA1center))      # create a parallel plot to visualize the centroid values

# second cluster solution
set.seed(5682991)
grpA2=kmeans(xford[,qdlist],centers=3)
grpA2center=t(grpA2$centers)      # create variable with the transpose of the centroids
rownames(grpA2center)=qdlist      # add the demographic labels
print(grpA2center,digits=2)       # print the centroid values for each question
parallelplot(t(grpA2center))      # create a parallel plot to visualize the centroid values

# compare the results
xtabs(~grpA1$cluster+grpA2$cluster)

# another technique to initialize is to choose k arbitrary points (say the first 3 points)
(grpA3=kmeans(xford[,qdlist],centers=rbind(xford[1,qdlist],xford[2,qdlist],xford[3,qdlist])))
grpA3center=t(grpA3$centers)   # create variable with the transpose of the centroids
rownames(grpA3center)=qdlist  # add the demographic labels
print(grpA3center)   # print the centroid values for each question
parallelplot(t(grpA3center))  # create a parallel plot to visualize the centroid values

# compare the results
xtabs(~grpA3$cluster+grpA1$cluster)
xtabs(~grpA3$cluster+grpA2$cluster)




###############################################################################
### cluster analysis of the questions
### this analysis is an optional one to help us group the questions (not the
### answers) into more similar groups.  If you change the list at the end
### then you can regenerate the previous parallel lines plots
###############################################################################

# set the random number seed so the samples will be the same if regenerated
set.seed(44328)

# let's try to cluster our questions by transposing the question data
# the goal of this analysis is to avoid having to look at all 62 questions,
# instead by clustering the questions we are looking for groups of questions
# that have similar responses.  I choose 6 groups -- and then choose a
# representative question from each cluster.  But you can choose more.
qford=t(ford[,qlist])
(grpQ=kmeans(qford,6))
# list the questions in each group (use a for loop to repeat for each cluster)
for (i in 1:6) {
  cat("\nCluster # ",i,"\n")
  print(fordquest[grpQ$cluster==i])
}
nshortqlist=c(30,57,53,1,4,12)  # short list of questions (or choose one question from each cluster)
shortqlist=paste0("Q",nshortqlist)
shortqname=strtrim(fordquest[nshortqlist],30)



###############################################################################
### compute a hierarchical cluster
### this is an alternative clustering technique than k-means
### it works by starting with all observations in different clusters and then
### progressing by combing observations that look similar into new clusters
###############################################################################

# try a hierarchical cluster on the question data
par(mfrow=c(1,1))
rownames(qford)=strtrim(fordquest,40)
(grphQ=hclust(dist(qford),method="complete"))
plot(grphQ,cex=.7)

# try a hierarchical cluster on the consumers
(grphP=hclust(dist(xford[,qlist]),method="complete"))
plot(grphP,cex=.7)

# try a hierarchical cluster with a heatmap visualization
heatmap(xford[,qlist],Colv=F,scale='none')   # Colv=F says not to reorder the columns, no scaling since the data is already scaled

