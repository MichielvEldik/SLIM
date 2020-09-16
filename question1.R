library(ggplot2)
library(corrplot)
library(nFactors)
library(GPArotation)
library(gplots)
survey_data <- read.csv("/home/mitchy123/Documents/Statistical learning/SLIMSustainableFashionData.csv", sep= ",", header = TRUE)

# copy of the data, standardize variables.
sc_data <- survey_data
sc_data[, 11:34] <- data.frame(scale(sc_data[, 11:34]))


kmo <- function(x)
{
  x <- subset(x, complete.cases(x)) # Omit missing values
  r <- cor(x) # Correlation matrix
  r2 <- r^2 # Squared correlation coefficients
  i <- solve(r) # Inverse matrix of correlation matrix
  d <- diag(i) # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2 # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0 # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

Bartlett.sphericity.test <- function(x)
{
  method <- "Bartlett’s test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) # Omit missing values
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "X-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}

pipeline <- function(df, factors, sc_data) {
  print(kmo(df))
  print(Bartlett.sphericity.test(df))
  cor_sc_data <- cor(df, method = "pearson")
  corrplot(cor_sc_data)
  nScree(df)
  eigen_dec <- eigen(cor(df))
  plot(eigen_dec[[1]])
  print(factanal(df, factors = factors))
  ort_rot <- factanal(df, factors = factors, rotation = "Varimax", scores="Bartlett")
  print(ort_rot)
  scores <- data.frame(ort_rot$scores)
  scores$gender <- sc_data$Gender
  print(scores)
  scores_mean <- aggregate(. ~ gender, data = scores, mean)
  print(scores_mean)
  heatmap.2(as.matrix(scores_mean))
}

pipeline(sc_data[,11:23], 3, sc_data)
# pipeline(sc_data[,24:34], 3, sc_data)

