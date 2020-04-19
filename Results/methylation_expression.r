setwd("o:/Training/Advanced R/Advanced R Data/")

# Exercise 1
read.delim("methylation.txt") -> methylation
read.delim("expression.txt") -> expression

nrow(methylation)

methylation[methylation$Promoter_meth != -1,] -> methylation

nrow(methylation)

expression[!duplicated(expression$Probe),] -> expression

sum(grepl("Olfr",expression$Probe))

# Exercise 2
expression$Chromosome <- paste("chr",expression$Chromosome,sep="")

methylation$Probe[!(methylation$Probe %in% expression$Probe)]

merge(expression,methylation) -> merged.data

# Exercise 3
apply(merged.data[,6:8],2,range)

tapply(merged.data$Gene_body_meth,merged.data$Chromosome,mean)
barplot(tapply(merged.data$Gene_body_meth,merged.data$Chromosome,mean))

# Exercise 4
sem <- function (x,absent=NA) {
  
  if (sum(is.na(x)) > 0) return (absent)
  
  return (sd(x)/sqrt(length(x)))
}

tapply(merged.data$Gene_body_meth,merged.data$Chromosome,sem)

# Exercise 5
library(vioplot)
par(mfrow=c(1,3))
sapply(colnames(merged.data)[6:8],function(x) {
  vioplot(merged.data[[x]])
  title(x)
}
)