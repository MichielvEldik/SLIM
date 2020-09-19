library(ggplot2)
library(dplyr)

# Load data
survey_data <- read.csv("/home/mitchy123/Documents/Statistical learning/SLIMSustainableFashionData.csv", sep= ",", header = TRUE)
sc_data <- survey_data

# boxplot to observe differences between education levels
boxplot(WTR ~ Education, data = sc_data,
        xlab = "Education", ylab = "WTR",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))

# table of mean, count, standard deviation per educational level for WTR
new_data <- sc_data %>%
  group_by(Education) %>%
  summarise(
    count = n(),
    mean = mean(WTR),
    sd = sd(WTR))
new_data

# Homogeneity of variances using Bartlett test
res <- bartlett.test(WTR ~ Education, data = sc_data)
res

# Homogeneity of variances using Fligner test (more robust to non-normality)
flig <- fligner.test(WTR ~ Education, data = sc_data)
flig

# Function for plotting qq norms
WTR_edu_qq <- function(sc_data, edu_level) {
  newie <- sc_data %>%
    filter(Education == edu_level)
  qqnorm(newie$WTR)
}

# Using the qq norm function by iterating over all education levels
for (value in c(1, 2, 3, 4)) {
WTR_edu_qq(sc_data, value)  
}

# ANOVA one-way 
WTR_AOV <- aov(WTR ~ Education, data = sc_data)
summary(WTR_AOV)

# ANCOVA applied using linear model 
model_1 = lm (WTR ~ Education + PurchaseFreq,
            data = sc_data)
summary(model_1)


