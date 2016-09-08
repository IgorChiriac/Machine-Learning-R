# A good article describing R functions for analyzing the data set
# http://www.computerworld.com/article/2598083/app-development-beginner-s-guide-to-r-easy-ways-to-do-basic-data-analysis.html
# Load the first 120 rowws from the data set in the current workspace
dataSet <- read.csv(file="/Users/igorPro/Documents/master/Machine-Learning-R/AnalyzeDataSet/dataSet.csv",head=FALSE, sep=",", nrow=120)
#analyze the data sed using the head() function
head(dataSet)
#analyze the structure of the data set using str() function
str(dataSet)
#display a summary of data set
summary(dataSet)
# define a new function for displaying histogram
# based on the prof ULRICH BODENHOFER Machine Learing course

## function for plotting overlays of histograms;
## x must be a matrix or data frame
## inputCol is the index of the column to analyze
## targetCol is the index of the target column (positive / negative)
## col is a vector with two colors in which the two histograms are plotted
plotHistogram <- function(x, inputCol, targetCol,
							col=c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))
	{
	all <- hist(x[, inputCol], plot=FALSE) ## determine histogram parameters
		## plot first histogram (negative class)
	hist(x[which(x[, targetCol] < 0), inputCol],
		breaks=all$breaks, xlim=range(all$breaks),
		ylim=c(0, max(all$counts)), col=col[1],
		probability=FALSE, main="", xlab=colnames(x)[inputCol])
	## overlay second histogram (positive class)
	hist(x[which(x[, targetCol] > 0), inputCol],
		breaks=all$breaks, col=col[2],
		probability=FALSE, add=TRUE)
}

## show histograms
plotHistogram(dataSet[1:120, ], 1, 3) ## plot data set 1, column 1
