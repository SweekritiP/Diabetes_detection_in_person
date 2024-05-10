
#setting working directory - adjust a path to your directory with a dataset

library(arules)
library(arulesSequences)
library(ggplot2)


download.file('https://staff.elka.pw.edu.pl/~rbembeni/dane/diab_trans.data','diab_trans.data')
#reading data - into dataframe
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)
View(diab.df)
#example of saving data into a file  - removing the header line
write.table(diab.df, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )

# Data preprocessing and exploration
# Convert 'value' column to numeric
diab.df$value <- as.numeric(diab.df$value)

# Filter out rows with NA values in 'value' column
diab.df <- diab.df[!is.na(diab.df$value), ]

# Discretization of blood glucose levels
# Define function for discretization
discretize_glucose <- function(glucose_level) {
  if (glucose_level < 72) {
    return("Low")
  } else if (glucose_level >= 72 & glucose_level <= 99) {
    return("Normal")
  } else if (glucose_level >= 100 & glucose_level <= 130) {
    return("Treated")
  } else if (glucose_level > 130) {
    return("High")
  }
}

# Apply discretization function to 'value' column
diab.df$glucose_level <- sapply(diab.df$value, discretize_glucose)

# Summary statistics
summary(diab.df)

# Visualization: Histogram of blood glucose levels
ggplot(data = diab.df, aes(x = glucose_level, fill = glucose_level)) +
  geom_bar() +
  scale_fill_manual(values = c("Low" = "blue", "Normal" = "green", "Treated" = "orange", "High" = "red")) +
  labs(title = "Histogram of Blood Glucose Levels",
       x = "Blood Glucose Levels",
       y = "Frequency")


#reading data in transactional form
diabSeq <- read_baskets(con = "diab_trans2.data", sep =",", info = c("sequenceID","eventID"))
View(as(diabSeq,"data.frame"))

summary(diabSeq)


#setting parameters
#time(eventid) in the diab_trans.data set is given as a number of seconds from some date.
#the following values of parameters are the example of values which allow obtaining any sequential rules.

seqParam = new ("SPparameter",support = 0.5, maxsize = 4, mingap=600, maxgap =172800, maxlen = 3 )
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))


#discovery of sequential rules
seqRules = ruleInduction(patSeq,confidence = 0.8)

length(seqRules)
#summary for the set of rules
summary(seqRules)
#view of of rules
inspect(head(seqRules,100))

# Conclusion:
# With a support threshold of 0.5, the sequential pattern mining yielded a total of 2,783 rules.
# These rules predominantly consist of 3-item sequences, followed by 2-item sequences. 
# The summary of quality measures indicates that the rules have generally high support, confidence, and lift values. 
# The mean support is approximately 0.607, the mean confidence is around 0.910, and the mean lift is about 1.071.
# These measures suggest that the discovered rules are well-supported and have high confidence,
# indicating their reliability in capturing sequential patterns in the data.
