library(ggplot2)
library(dplyr)

survey_data <- read.csv("/home/mitchy123/Documents/Statistical learning/SLIMSustainableFashionData.csv", sep= ",", header = TRUE)
sc_data <- survey_data
ggplot(sc_data, aes(x=factor(Education), y =WTR, group=Education)) + geom_boxplot()

new_data <- sc_data %>%
  group_by(Education) %>%
  summarise(
    mean = mean(WTR),
    sd = sd(WTR))
new_data

# Homogeneity of variances
res <- bartlett.test(WTR ~ Education, data = sc_data)
res

# Homogeneity of variances
lev <- leveneTest(WTR ~ Education, data = sc_data)
lev

# normality tests.


boxplot(WTR ~ Education, data = sc_data,
        xlab = "Education", ylab = "WTR",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
WTR_AOV <- aov(WTR ~ Education, data = sc_data)
summary(WTR_AOV)

options(contrasts = c("contr.treatment", "contr.poly"))
model_1 = lm (WTR ~ Education + PurchaseFreq,
            data = sc_data)
summary(model_1)

