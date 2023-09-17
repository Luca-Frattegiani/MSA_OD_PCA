## Problem Set 1 ##
                                           
rm(list=ls())
library(ellipse)
#=================================================================================================================================================================================================================================
#Implemented functions:

correlations_grid = function(R, X){
  
  lowers = (dim(R)[1]**2 - dim(R)[1])/2
  
  R[!lower.tri(R)] = NA
  sort(R, decreasing = T)
  order.cor = order(R, decreasing = T)[1:lowers]
  stack.m = matrix(1:dim(R)[1]^2, ncol = dim(R)[1])
  
  Order.cor = matrix(rep(0, lowers*2), ncol = 2)
  for(element in seq(1, lowers)){
    Order.cor[element, ] = which(stack.m == order.cor[element], arr.ind = 1)
  }
  variables = matrix(names(X)[Order.cor], ncol = 2)
  
  values = c()
  for (index in seq(1, dim(variables)[1])){
    values[index] = paste(variables[index, 1], variables[index, 2], sep = " : ")
  }
  
  output = cbind(values,  R[Order.cor])
  colnames(output) = c("Variable's names", "Correlations")
  output = data.frame(output)
  output[, 2] = as.numeric(output[, 2])
  
  return(output)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

plot_box = function(X, variable, outliers, std = list()){
  boxplot(X[, variable], main = paste("Boxplot for the variable: ", names(X)[[variable]], sep =""))
  if(length(outliers[[variable]]) != 0){
    points(x = rep(1, length(outliers[[variable]])), y = X[, variable][outliers[[variable]]], pch = 16, col = "red")
    text(1, X[, variable][outliers[[variable]]], labels = outliers[[variable]], pos = 4, col = "red")
  }
  if(length(std) != 0){
    if(length(std[[variable]])){
      points(x = rep(1, length(std[[variable]])), y = X[, variable][std[[variable]]], pch = 22, col = "blue")
      text(1, X[, variable][std[[variable]]], labels = std[[variable]], pos = 2, col = "blue")
    }
  }
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

boxplot_analysis = function(X, plot = F){
  outliers = list()
  
  for(p in seq(1, dim(X)[[2]])){
    iqr = (quantile(X[, p])[[4]] - quantile(X[, p])[[2]])
    lower = quantile(X[, p])[[2]] - 1.5*iqr
    upper = quantile(X[, p])[[4]] + 1.5*iqr
    indexes = which(X[, p]<lower | X[, p]>upper)
    outliers[[p]] = indexes
  }
  
  names(outliers) = colnames(X)
  
  if(plot == T){
    for(p in seq(1, dim(X)[[2]])){
      plot_box(X, p, outliers)
    }
  }
  
  return(outliers)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

std = function(data){
  output = (data - mean(data)) / sd(data)
  return(output)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

standardized = function(X, quantiles = c(0.05, 0.95), plot = F){
  std_data = data.frame(apply(X, MARGIN = 2, FUN = std))
  lower = qnorm(quantiles[1])
  upper = qnorm(quantiles[2])
  
  outliers = list()
  for (p in seq(1, dim(X)[2])){
    indexes = which(std_data[, p] < lower | std_data[, p] > upper)
    outliers[[p]] = indexes
  }
  
  names(outliers) = colnames(X)
  
  if(plot == T){
    for(p in seq(1, dim(X)[[2]])){
      plot_box(X, p, outliers)
    }
  }
  
  return(outliers)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

univariate_outliers = function(X, kind = "boxplot", plot = F, quantiles = c(0.05, 0.95)){
  if(kind == "boxplot"){
    if(plot == F){
      return(boxplot_analysis(X))
    }
    else{
      return(boxplot_analysis(X, plot = T))
    }
  }
  
  if(kind == "standardized"){
    if(plot == F){
      return(standardized(X, quantiles = quantiles))
    }
    else{
      return(standardized(X, quantiles = quantiles, plot = T))
    }
  }
  
  if(kind == "both"){
    boxplot_outliers = boxplot_analysis(X)
    standardized_outliers = standardized(X, quantiles = quantiles)
    output = list(boxplot_outliers, standardized_outliers)
    names(output) = c("Boxplot", "Standardized")

    if(plot == T){
      for(p in seq(1, dim(X)[[2]])){
        plot_box(X, p, outliers = boxplot_outliers, std = standardized_outliers)
      }
    }
    return(output)
  }
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

outliers_matches = function(X, quantiles = c(0.05, 0.95)){
  outliers = list()
  outliers_bp = boxplot_analysis(X)
  outliers_sd = standardized(X, quantiles = quantiles)
  
  for(p in seq(1, dim(X)[2])){
    matches = c()
    position = 1
    for(i in seq(1, length(outliers_sd[[p]]))){
      for(j in seq(1, length(outliers_bp[[p]]))){
        if(length(outliers_bp[[p]]) != 0 && length(outliers_sd[[p]]) != 0 && outliers_sd[[p]][i] == outliers_bp[[p]][j]){
          matches[position] = outliers_sd[[p]][i]
          position = position + 1
        }
      }
    }
    outliers[[p]] = matches
  }
  
  names(outliers) = names(outliers_bp)
  return(outliers)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

self_qqplot = function(X){
  for(p in seq(1, dim(X)[2])){
    sample_quantiles = sort(X[, p])
    sample_mean = mean(X[, p])
    sample_sd = sd(X[, p])
    
    probabilities = ppoints(n)
    theoretical_quantiles = qnorm(probabilities)
    
    plot(y = sample_quantiles, x = theoretical_quantiles, pch = 16, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = paste("Q-Q plot for variable: ", colnames(X)[p], sep = ""))
    abline(a = sample_mean, b = sample_sd, col = "red")
    qqline(X[, p])
  }
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

qq_show_outliers = function(X, p, kind = "boxplot", quantiles = c(0.05, 0.95)){
  outliers = univariate_outliers(X, kind = kind, quantiles = quantiles)
  sample_quantiles = sort(X[, p])
  observations = order(X[, p])
  theoretical_quantiles = qnorm(ppoints(dim(X)[1]))
  
  indexes = c()
  position = 1
  for(outlier in outliers[[p]]){
    if(length(which(sample_quantiles == X[outlier, p])) != 0){
      indexes[position] = which(sample_quantiles == X[outlier, p])
      position = position + 1
    }
  }
  
  if(length(observations[indexes]) != 0){
    text(x = theoretical_quantiles[indexes], y = sample_quantiles[indexes], labels = observations[indexes], pos = 4, col = "red")
  }
  else{
    return(NULL)
  }
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

self_mahalanobis = function(X, plot = F){
  S = cov(X)
  x.bar = apply(X, 2, mean)
  data = as.matrix(X)
  rownames(data) = seq(1, dim(X)[1])
  
  mahalanobis_squared = c()
  for(i in seq(1, dim(data)[1])){
    mahalanobis_squared[i] = t(data[i, ] - x.bar)%*%solve(S)%*%(data[i, ] - x.bar)
  }
  
  if(plot == T){
    probabilities = ppoints(length(mahalanobis_squared))
    theoretical_quantiles = qchisq(probabilities, df = dim(X)[2])
    sample_quantiles = sort(mahalanobis_squared)
    
    plot(y = sample_quantiles, x = theoretical_quantiles, pch = 16, main = "Q-Q plot for Squared Mahalanobis Distribution", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
    abline(a = 0, b = 1)
  }
  return(mahalanobis_squared)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

trace = function(matrix){
  summation = 0
  for(row in seq(1, dim(matrix)[1])){
    for(column in seq(1, dim(matrix)[2])){
      if(row == column){
        summation = summation + matrix[row, column]
      }
    }
  }
  
  return(summation)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

proportion_of_variance_pca = function(X){
  pca = prcomp(X)
  eigenvalues = pca$sdev^2
  total_variance = trace(cov(X))
  
  explained_variance = 0
  proportion_variance = 0
  evolution = list()
  for(element in seq(1, length(eigenvalues))){
    explained_variance = explained_variance + eigenvalues[[element]]
    proportion_variance = (explained_variance / total_variance)*100
    evolution[[element]] = c(explained_variance, proportion_variance)
    names(evolution[[element]]) = c("Variance explained", "Proportion of variance explained")
  }
  
  names_list = c()
  for(element in seq(1, length(eigenvalues))){
    names_list[element] = paste("Principal Component number ", toString(element), sep = "")
  }
  
  names(evolution) = names_list
  
  return(evolution)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

bivariate_normality_criterion = function(components, chosen = c(1, 2), alphas = c(0.5, 0.9, 0.95)){
  data = components[, chosen]
  distances = self_mahalanobis(components[, chosen])
  
  elements = c()
  results = c()
  position = 1
  for(alpha in alphas){
    summation = 0
    bound = dim(data)[1]*alpha
    for(distance in distances){
      if(distance <= qchisq(alpha, df = 2)){
        summation = summation + 1
      }
    }
    elements[position] = summation
    if(summation >= bound){
      results[position] = T
    }
    else{
      results[position] = F
    }
    position = position + 1
  }
  
  names_ = c()
  position = 1
  for(alpha in alphas){
    names_[position] = paste("Quantile ", toString(alpha), ":", sep = "")
    position = position + 1
  }
  output = data.frame(elements, results)
  colnames(output) = c(paste("Elements PC", toString(chosen[1]), " vs PC", toString(chosen[2]), sep = ""), paste("Results PC", toString(chosen[1]), " vs PC", toString(chosen[2]), sep = ""))
  rownames(output) = names_
  
  return(output)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

points_generator_1 = function(points){
  
  coordinates = rep(0, length(points)*2)
  i = 1
  for(element in points){
    coor_1 = element
    coor_2 = -element
    
    coordinates[i] = coor_1
    coordinates[i + 1] = coor_2
    i = i + 2
  }
  
  results = matrix(coordinates, byrow = T, ncol = 2)
  colnames(results) = c("x", "y")
  return(results)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

points_generator_2 = function(points){
  
  coordinates = rep(0, length(points)*2)
  i = 1
  for(element in points){
    coor_1 = element
    coor_2 = element
    
    coordinates[i] = coor_1
    coordinates[i + 1] = coor_2
    i = i + 2
  }
  
  results = matrix(coordinates, byrow = T, ncol = 2)
  colnames(results) = c("x", "y")
  return(results)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

points_generator_3 = function(points){
  
  coordinates = rep(0, length(points)*2)
  i = 1
  for(element in points){
    coor_1 = 0
    coor_2 = element
    
    coordinates[i] = coor_1
    coordinates[i + 1] = coor_2
    i = i + 2
  }
  
  results = matrix(coordinates, byrow = T, ncol = 2)
  colnames(results) = c("x", "y")
  return(results)
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

points_generator_4 = function(points){
  
  coordinates = rep(0, length(points)*2)
  i = 1
  for(element in points){
    coor_1 = element
    coor_2 = 0
    
    coordinates[i] = coor_1
    coordinates[i + 1] = coor_2
    i = i + 2
  }
  
  results = matrix(coordinates, byrow = T, ncol = 2)
  colnames(results) = c("x", "y")
  return(results)
}

#=================================================================================================================================================================================================================================
# EXERCISE 1: US air pollution data

#Load and display data:

usair<-read.table("C:/Users/39392/Documents/Luca/Università/Master Degree Stochastics and Data Science/I° Year/II° Semester/Multivariate Statistical Analysis/Problem Sets/Problem Set 1/data/usair.txt", header = TRUE)
head(usair)
dim(usair)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#1.1) Compute the Sample Mean Vector, Correlation Matrix and comment on correlations:

X = usair[, -1]; head(X) #Data
n = dim(X)[1]
p = dim(X)[2]
x.bar = apply(X, 2, mean); x.bar #Sample Mean Vector
R = round(cor(X), 3); R #Correlation Matrix
correlations_grid(R, X) #Ranking of the correlations between variables
pairs(X, pch = 16, lower.panel = NULL) #Evaluate graphically the relationship between the variables
plot(x = X[, "Pop"], y = X[, "Manuf"], xlab = "Variable Pop", ylab = "Variable Manuf", main = "Scatterplot", pch = 16)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#1.2) Make a boxplot of each variable and comment about the presence of outliers (no more than two per variable). Identify these observations:

#Outliers over the Interquartile Range:
outliers_bp = boxplot_analysis(X, plot = T); outliers_bp
X[1, ]; X[23, ]
#Outliers according to the standardized observations compare with the quantiles of N(0, 1):
outliers_sd = standardized(X, c(0.03, 0.97), plot = T); outliers_sd

#Comparison and matches between the two tecniques:
univariate_outliers(X, kind = "both", plot = T, quantiles = c(0.03, 0.98))
matches = outliers_matches(X, c(0.03, 0.97)); matches

#We can conclude that some possible outliers can be:
#Observation 9: For the variable "Negative Temperature"
#Observations 11 and 29 for the variables "Manuf" and "Pop"
#Wind doesn't seems to have outliers
#Observations 1 and 23 for the variables "Precip" and "Days"

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#1.3) Construct a normal Q-Q plot for each variable and comment about normality:

self_qqplot(X) #Self made Q-Q plots

#Built-in function in R with outliers detection:
for(p in seq(1, dim(X)[2])){
  qqnorm(X[, p], main = paste("Q-Q plot for variable: ", colnames(X)[p], sep = ""))
  points(qnorm(ppoints(X[, p])), sort(X[, p]), pch = 16)
  qqline(X[, p])
  qq_show_outliers(X, p)
}
outliers_bp

sample_quantiles = sort(X[, 6])
sample_mean = mean(X[, 6])
sample_sd = sd(X[, 6])

probabilities = ppoints(n)
theoretical_quantiles = qnorm(probabilities)

plot(y = sample_quantiles, x = theoretical_quantiles, pch = 16, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = paste("Q-Q plot: ", colnames(X)[6], sep = ""))
abline(a = sample_mean, b = sample_sd, col = "red")
qqline(X[, 6])
qq_show_outliers(X, 6)

sample_quantiles = sort(X[-c(1, 23), 6])
sample_mean = mean(X[-c(1, 23), 6])
sample_sd = sd(X[-c(1, 23), 6])
plot(y = sample_quantiles, x = theoretical_quantiles[-c(1, 2)], pch = 16, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = paste("Q-Q plot without outliers: ", colnames(X)[6], sep = ""))
abline(a = sample_mean, b = sample_sd, col = "red")
qqline(X[-c(1, 23), 6])

#Histogram evaluation:
limits_y = list(c(0, 0.07), c(0, 0.0016), c(0, 0.0013), c(0, 0.35), c(0, 0.05), c(0, 0.02))
color = rgb(red = 0, green = 0, blue = 0.5, alpha = 0.05)
ranges = list(-80:-10, -100:3100, -100:3100, seq(4, 15, by = 0.01), 0:90, 10:190)
for(p in seq(1, dim(X)[2])){
  plot(density(X[, p]), lwd = 3, col = "red", ylim = limits_y[[p]], main = paste("Histogram for the Variable ", colnames(X)[p], sep = ""), xlab = paste("Variable ", colnames(X)[p], sep = ""))
  hist(X[, p], probability = T, add = T, col = color)
  lines(x = ranges[[p]], y = dnorm(ranges[[p]], mean(X[, p]), sd(X[, p])), lwd = 3, col = "black")
}

#Gordon Shapiro Test evaluation:
shapiro.test(X[, 1])
shapiro.test(X[, 2])
shapiro.test(X[, 3])
shapiro.test(X[, 4])
shapiro.test(X[, 5])
shapiro.test(X[, 6])

statistics = c()
p_values = c()
position = 1
for(p in seq(1, dim(X[-c(1, 9, 11),])[2])){
  statistics[position] = as.numeric(shapiro.test(X[-c(1, 9, 11), p])$statistic)
  p_values[position] = as.numeric(shapiro.test(X[-c(1, 9, 11), p])$p.value)
  position = position + 1
}
results = data.frame(colnames(X), statistics, p_values)
colnames(results) = c("Variable", "Statistic's value", "p-value observed")
results

#Through the Q-Q plot rapresentation we can confirm the following statements:
#Variable 1: Normality assumption can be confirmed
#Variable 2: Normality assumption can be confirmed except for some high outliers
#Variable 3: Normality assumption can be confirmed except for some high outliers
#Variable 4: Normality assumption can be confirmed
#Variable 5: Normality assumption can be confirmed except for low observations/outliers
#Variable 6: Normality assumption can be confirmed just for middle observations

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#1.4) By using scatterplots, comment on whether the outliers at point 1.2) can be detected from them:

#Outliers matched from the two methods:
colors = rep("black", n)
colors[c(1, 9, 11, 23, 29)] = c("red", "blue", "yellow", "green", "lightblue")
pairs(X, pch = 16, lower.panel = NULL, col = colors)

#Outliers from boxplots:
selected = c(1, 9, 11, 23, 29)
colors = rep("black", n)
colors[selected] = c("red", "blue", "yellow", "green", "lightblue")
pairs(X, pch = 16, lower.panel = NULL, col = colors)
outliers_bp
#Conclusions looking at all the possible pairs of scatterpltos:
#Observation 1: It can be detected looking at the scatterplots of Days vs Neg.Temp, Precip vs Wind, Precip vs Neg.Temp
#Observation 9: It can be detected looking at all the scatterplots of the variable Neg.Temp
#Observation 11: It can be detected looking at all the scatterplots of the variables Pop and Manuf
#Observation 23: Difficult to detect
#Observation 29: Difficult to detect apart for the scatterplots of the variable Pop

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#1.5) Construct a chi-square Q-Q plot of the squared Mahalanobis distances and comment about normality:
S = cov(X); S
distances = mahalanobis(X, center = x.bar, cov = S); distances
mahalanobis_squared = self_mahalanobis(X, plot = T); mahalanobis_squared

order(mahalanobis_squared)
theoretical_quantiles = qchisq(ppoints(length(mahalanobis_squared)), df = dim(X)[2])
text(x = theoretical_quantiles[c(39, 40, 41)], y = sort(mahalanobis_squared)[c(39, 40, 41)], labels = order(mahalanobis_squared)[c(39, 40, 41)], pos = 4, col = "red")

self_mahalanobis(X[-c(1, 9, 11),], plot = T)

#By lookinng at the Q-Q plot we conclude that the Square Mahalanobis Distances seem to have a Chi-squared distribution (except for three observations we said to be outliers). We denote this cause the n-observations of the 
#Mahalanobis Random Variable follows the Chi-Squared distribution with p degrees of freedom. So we can assume that data are Multivaiate Gaussian Np(u, sigma).

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#1.6) Identify multivariate outliers, if any, and compare with the answer to point 1.2):

selected
mahalanobis_squared

plot(x = seq(1, dim(X)[1]), y = mahalanobis_squared, pch = 16, main = "Evaluation of Multivariate Outliers: Mahalanobis distances vs Quantiles of the Chi-squared", xlab = "Observations", ylab = "Squared Mahalanobis distances", col = colors)
text(x = selected, y = mahalanobis_squared[selected], labels = selected, pos = 4)
abline(h = qchisq(0.9998, df = 6), col = "green", lty = "dashed")
abline(h = qchisq(0.999, df = 6), col = "red", lty = "dashed")
abline(h = qchisq(0.99, df = 6), col = "blue", lty = "dashed")
abline(h = qchisq(0.97, df = 6), col = "black", lty = "dashed")
text(x = 39, y = qchisq(0.9998, df = 6), label = "Quantile: 0.9998", col = "green", pos = 3)
text(x = 39, y = qchisq(0.999, df = 6), label = "Quantile: 0.999", col = "red", pos = 3)
text(x = 39, y = qchisq(0.99, df = 6), label = "Quantile: 0.99", col = "blue", pos = 3)
text(x = 39, y = qchisq(0.97, df = 6), label = "Quantile: 0.97", col = "black", pos = 3)

#Looking at the graph which depict the observed values for the Squared Mahalanobis distances and compare them with the highest quantiles of the Chi-squared Distribution with p-degrees of freedom, we see that the observation
#11 is over the "0.9998" quantile so it can be considered as a Multivariate Outlier. We can state the same for observation 1 which is over the 0.99 quantile while observation 9 is an high value of the Squared Mahalanobis 
#distances and an outlier for some of the variables considered in the Dataset

#=================================================================================================================================================================================================================================
# EXERCISE 2:

mu = c(1, -1, 2); mu

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#2.1) Let PC1 and PC2 be the first two (population) principal components of X. Find "ro" such that they account for more than 80% of total variation of X:

#Check the results obtained for the first range:
correlations = seq((sqrt(2)/5), (sqrt(2)/2), by = 0.01)
props = c()
i = 1
for(correlation in correlations){
  sigma = cbind(c(1, correlation, 0), c(correlation, 1, correlation), c(0, correlation, 1))
  variances = eigen(sigma)$values[c(1, 2)]
  prop = ((variances[1] + variances[2])/3)*100
  props[i] = prop
  i = i + 1
}
props

#Check the results obtained for the second range:
correlations_ = seq(-(sqrt(2)/2), -(sqrt(2)/5), by = 0.01)
props_ = c()
i = 1
for(correlation_ in correlations_){
  sigma_ = cbind(c(1, correlation_, 0), c(correlation_, 1, correlation_), c(0, correlation_, 1))
  variances_ = eigen(sigma_)$values[c(1, 2)]
  prop_ = ((variances_[1] + variances_[2])/3)*100
  props_[i] = prop_
  i = i + 1
}
props_

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#2.4) Sketch the ellipse in the 2 dimensional space z = (z1; z2) by setting the constant c such that the ellipse contains 0,95 probability with respect to the joint distribution of Z:

muz = c(2, -3); muz
sigmaz = cbind(c(10/3, -7/3), c(-7/3, 10/3)); sigmaz
plot(x = 2, y = -3, xlab = "", ylab = "", main = "Ellipse for the 1? Correlation", ylim = c(-8, 8), xlim = c(-10, 10))
e1 = points_generator_1(seq(-7, 7, by = 1))
e2 = points_generator_2(seq(-7, 7, by = 1))
x = points_generator_3(seq(-8, 8, by = 1))
y = points_generator_4(seq(-8, 8, by = 1))
arrows(x0 = 0, y0 = 0, x1 = 7, y1 = 7, lwd = 3, col = "blue")
arrows(x0 = 0, y0 = 0, x1 = -7, y1 = 7, lwd = 3, col = "blue")
lines(e1, lwd = 3, col = "blue")
lines(e2, lwd = 3, col = "blue")
lines(x, lwd = 3, col = "black")
lines(y, lwd = 3, col = "black")
arrows(x0 = 0, y0 = 0, x1 = 0, y1 = 8, lwd = 3, col = "black")
arrows(x0 = 0, y0 = 0, x1 = 8, y1 = 0, lwd = 3, col = "black")
text(x = 2, y = -2.7, label = "\n Population\nMean Vector", col = "darkgreen", pos = 1)
text(x = -6.8, y = 0, label = "Original Axes", col = "black", pos = 1)
text(x = 7, y = -7, label = "Rotated\n   Axes", col = "blue", pos = 4)
text(x = 4, y = -1, label = "\n\nEllipse", col = "red", pos = 4)
lines(ellipse(x = sigmaz, centre = muz, level = 0.95), col = "red", lwd = 3)
points(x = 2, y = -3, pch = 16, cex = 1.5, asp = 1, col = "darkgreen")

points(x =(sqrt(5.991)*sqrt(102))/6 + 2, y = -(sqrt(5.991)*sqrt(102))/6 - 3)
x = sqrt(5.991)*sqrt(17/3) + 2(2/sqrt(2))
points(x =sqrt(5.991)*sqrt(17/3)*(sqrt(2)/2) + 2*(sqrt(2)/2), y = -sqrt(5.991)*sqrt(17/3)*(sqrt(2)/2) - 3*(sqrt(2)/2), pch = 16, col = "green")
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#2.5) Comment on how the ellipse would change with correlation = -2/3:

sigmaz_ = cbind(c(2/3, 1/3), c(1/3, 2/3)); sigmaz_
plot(x = 2, y = -3, xlab = "", ylab = "", main = "Ellipse for the 2? Correlation", ylim = c(-8, 8), xlim = c(-10, 10))
e1_ = points_generator_2(seq(-6, 6, by = 1))
e2_ = points_generator_1(seq(-6, 6, by = 1))
x_ = points_generator_3(seq(-7, 7, by = 1))
y_ = points_generator_4(seq(-7, 7, by = 1))
lines(e1_, lwd = 3, col = "blue")
lines(e2_, lwd = 3, col = "blue")
arrows(x0 = 0, y0 = 0, x1 = 6, y1 = 6, lwd = 3, col = "blue")
arrows(x0 = 0, y0 = 0, x1 = -6, y1 = 6, lwd = 3, col = "blue")
lines(x_, lwd = 3, col = "black")
lines(y_, lwd = 3, col = "black")
arrows(x0 = 0, y0 = 0, x1 = 0, y1 = 7, lwd = 3, col = "black")
arrows(x0 = 0, y0 = 0, x1 = 7, y1 = 0, lwd = 3, col = "black")
text(x = 2.8, y = -3.5, label = "\n Population \nMean Vector", col = "darkgreen", pos = 2)
text(x = -5.8, y = 0, label = "Original Axes", col = "black", pos = 1)
text(x = 9, y = -6, label = "Rotated Axes", col = "blue", pos = 2)
text(x = 4, y = -2, label = "\n\nEllipse", col = "red", pos = 4)
lines(ellipse(x = sigmaz_, centre = muz, level = 0.95), col = "red", lwd = 3)
points(x = 2, y = -3, pch = 16, cex = 1.5, asp = 1, col = "darkgreen")

#=================================================================================================================================================================================================================================
# EXERCISE 3: Pen digit data

#COMPOSITION OF THE DATA:
#n (10992) -> Number of observations given by the writers multiplied for the digits written by each writer
#Each row contains a registration of a digit made by someone and the registration is performed considering the coordinates (x, y) for 8 times during the writing of the digit. For each row the digit written in insert in the 
#final column

#Load and display Data:
pendigits = read.table("C:/Users/39392/Documents/Luca/Universit?/Laurea Magistrale Stochastics and Data Science/I? Year/II? Semester/Multivariate Statistical Analysis/Problem Sets/Problem Set 1/data/pendigits.txt", sep = ",",head = F)
names(pendigits) = c(paste0(rep(c("x","y"), 8), rep(1:8, each = 2)), "digit")
n = dim(pendigits)[1]; n
p = dim(pendigits)[2]; p

#We assign to each digit of the dataset its proper color:
lookup = c("darkgreen",  "brown", "lightblue",  "magenta", "purple", "blue", "red", "lightgreen", "orange", "cyan")
names(lookup) = as.character(0:9); lookup
digit.col = lookup[as.character(pendigits$digit)]; head(digit.col)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#3.1) Perform a principal component analysis on the standardized variables. Report standard deviations. Decide how many components to retain in order to achieve a satisfactory lower-dimensional representation of the data. 
#Justify your answer:

data = pendigits[, -17]
data_s = scale(data)
pendigits.pca = prcomp(data, scale = T); pendigits.pca
summary(pendigits.pca)
standard_deviations = pendigits.pca$sdev; standard_deviations
head(pendigits.pca$rotation)

#First Criterion: Proportion of Variance explained by the Principal Components
summary(pendigits.pca) #Built-in function mode
proportion_of_variance_pca(data)#Personal evaluation

#Second Criterion: Screeplot
plot(pendigits.pca$sdev^2, type = "b", xlab = "Number of Eigenvalues", ylab = "Eigenvalues", main = "Screeplot for the Principal Components")
abline(v =  6, col = "red", lty = "dashed")
abline(v =  7, col = "red", lty = "dashed")
axis(1, at = c(5, 7), labels = c(5, 7))

#Third Criterion: Eigenvalues larger than the mean
pendigits.pca$sdev^2 > mean(pendigits.pca$sdev^2)

#Comparing the three criterion we can use to choose a correct number of Principal Components in order to obtain a good lower dimensional rapresentation of data, we can say that maybe choose to keep 5 or 6 Principal
#Components, cause they explains together the 83% - 88% of the Total Variance, they corresponds to two elbows of the Screeplot and four of them are higher than the mean value of all the eigenvalues.

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#3.2) Investigate multivariate normality through the first three principal components:


components = pendigits.pca$x[, c(1, 2, 3)]; head(components)

#Q-Q plot evaluation:
for(p in seq(1, 3)){
  qqnorm(pendigits.pca$x[, p], main = paste("Q-Q plot for Principal Component: ", toString(p), sep = ""))
  points(qnorm(ppoints(length(pendigits.pca$x[, p]))), sort(pendigits.pca$x[, p]), pch = 16)
  qqline(pendigits.pca$x[, p], lwd = 3, col = "red")
}

#Histogram evaluation:
limits = list(c(0, 0.3), c(0, 0.3), c(0, 0.3))
for(p in seq(1, 3)){
  plot(density(pendigits.pca$x[, p]), ylim = limits[[p]], lwd = 3, col = "red", main = paste("Histogram for the Principal Component ", toString(p), sep = ""), xlab = paste("Principal Component ", toString(p), sep = ""))
  color = rgb(red = 0, green = 0, blue = 0.5, alpha = 0.05)
  hist(pendigits.pca$x[, p], probability = T, add = T, col = color)
}

#Gordon Shapiro Test evaluation:
ranges = list(seq(1, 5000), seq(5993, 10992), seq(2997, 7996), c(seq(8493, 10992), seq(1, 2500)))
results_shapiro = list()
for(p in seq(1, 3)){
  values = c()
  p_values = c()
  i = 1
  
  for(range in ranges){
    values[i] = shapiro.test(components[range, p])$statistic
    p_values[i] = shapiro.test(components[range, p])$p.value
    i = i + 1
  }
  
  output = cbind(values, p_values)
  colnames(output) = c("Statistic W", "P values")
  rownames(output) = c("1? Range:", "2? Range:", "3? Range:", "4? Range:")
  
  results_shapiro[[p]] = output
  names(results_shapiro)[[p]] = paste("Principal Component ", toString(p), sep = "")
}
results_shapiro

#Bivariate Normality evaluation of the first Principal Components:
components = pendigits.pca$x[, c(1, 2, 3)]; head(components)
pca.bar = apply(components, 2, mean); pca.bar
pca.S = cov(components); pca.S

#Evaluate Bivariate normality:
plot(x = components[, 1], y = components[, 2], xlab = "PC1", ylab = "PC2", main = "PC's Bivariate Normality Analysis", pch = 16, cex = 0.75, asp = 1, ylim = c(-5, 5), xlim = c(-5, 5))
lines(ellipse(x = pca.S[c(1, 2), c(1, 2)], centre = pca.bar[c(1, 2)], level = 0.5), col = "red", lwd = 3)
lines(ellipse(x = pca.S[c(1, 2), c(1, 2)], centre = pca.bar[c(1, 2)], level = 0.9), col = "red", lwd = 3)
lines(ellipse(x = pca.S[c(1, 2), c(1, 2)], centre = pca.bar[c(1, 2)], level = 0.95), col = "red", lwd = 3)

plot(x = components[, 1], y = components[, 3], xlab = "PC1", ylab = "PC3", main = "PC's Bivariate Normality Analysis", pch = 16, cex = 0.75, asp = 1, ylim = c(-5, 5), xlim = c(-5, 5))
lines(ellipse(x = pca.S[c(1, 3), c(1, 3)], centre = pca.bar[c(1, 3)], level = 0.5), col = "red", lwd = 3)
lines(ellipse(x = pca.S[c(1, 3), c(1, 3)], centre = pca.bar[c(1, 3)], level = 0.9), col = "red", lwd = 3)
lines(ellipse(x = pca.S[c(1, 3), c(1, 3)], centre = pca.bar[c(1, 3)], level = 0.95), col = "red", lwd = 3)

plot(x = components[, 2], y = components[, 3], xlab = "PC2", ylab = "PC3", main = "PC's Bivariate Normality Analysis", pch = 16, cex = 0.75, asp = 1, ylim = c(-5, 5), xlim = c(-5, 5))
lines(ellipse(x = pca.S[c(2, 3), c(2, 3)], centre = pca.bar[c(2, 3)], level = 0.5), col = "red", lwd = 3)
lines(ellipse(x = pca.S[c(2, 3), c(2, 3)], centre = pca.bar[c(2, 3)], level = 0.9), col = "red", lwd = 3)
lines(ellipse(x = pca.S[c(2, 3), c(2, 3)], centre = pca.bar[c(2, 3)], level = 0.95), col = "red", lwd = 3)

#Application of the numerical criterion to check if PC's data form an ellipse:
bivariate_normality_criterion(components, alphas = ppoints(11))
bivariate_normality_criterion(components, chosen = c(1, 3), alphas = ppoints(11))
bivariate_normality_criterion(components, chosen = c(2, 3), alphas = ppoints(11))

components = pendigits.pca$x[, c(1, 2, 3)]
distances = self_mahalanobis(components)

probs = ppoints(length(distances))
t_quant = qchisq(probs, df = 3)
s_quant = sort(distances)

plot(t_quant, s_quant, main = "Q-Q plot of Squared Mahalanobis distances", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", pch = 16)
abline(a = 0, b = 1, lwd = 3, col = "red")

plot(distances, main = "Mahalanobis distances vs Quantiles of the Chi-squared", ylab = "Squared Mahalanobis distances", xlab = "Observations", pch = 16)
abline(h = qchisq(0.25, df = 3), col = "blue", lty = "dashed", lwd = 3)
abline(h = qchisq(0.5, df = 3), col = "red", lty = "dashed", lwd = 3)
abline(h = qchisq(0.75,df = 3), col = "green", lty = "dashed", lwd = 3)
abline(h = qchisq(0.95,df = 3), col = "yellow", lty = "dashed", lwd = 3)

###########################################################################################
normality_criterion = function(components, chosen = c(1, 2), alphas = c(0.5, 0.9, 0.95)){
  data = components[, chosen]
  distances = self_mahalanobis(components[, chosen])
  
  elements = c()
  position = 1
  for(alpha in alphas){
    summation = 0
    bound = dim(data)[1]*(1 - alpha)
    for(distance in distances){
      if(distance > qchisq(alpha, df = length(chosen))){
        summation = summation + 1
      }
    }
    elements[position] = summation
    position = position + 1
  }
  
  percentages = c()
  position = 1
  for(element in elements){
    percentages[position] = paste(toString(round((element/dim(data)[1])*100, 2)), "%", sep = "")
    position = position + 1
  }
  
  expected = c()
  position = 1
  for(alpha in alphas){
    expected[position] = paste(toString(100*(1-alpha)), "%", sep = "")
    position = position + 1
  }
  
  names_ = c()
  position = 1
  for(alpha in alphas){
    names_[position] = paste("Quantile ", toString(alpha), ":", sep = "")
    position = position + 1
  }
  output = data.frame(elements, percentages, expected)
  colnames(output) = c("Number of elements", "Observed Percentage", "Expected Percentage")
  rownames(output) = names_
  
  return(output)
}

normality_criterion(components, chosen = c(1, 2, 3), alphas = c(0.25, 0.5, 0.75, 0.95))

s_q = sort(components[, 1])
s_m = mean(components[, 1])
s_sd = sd(components[, 1])

t_q = qnorm(ppoints(dim(components)[1]))

plot(y = s_q, x = t_q, pch = 16, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q plot for PC1: ")
abline(a = s_m, b = s_sd, col = "red", lwd = 3)
qqline(components[, 1], lwd = 3)

###########################################################################################

#Looking at the results of the Q-Q plots we can see that just the third PC seems to be normally distributed, while the first and the second PCs differs from the straight line both for small and large sample quantiles

#The Histogram plots confirm the above considerations since we can detect a quite normal distribution just for the third PC while the first PC and the second one show more than one frequency/density spikes

#the counterplot in the pxp space of PC1 vs PC2 does not seem to be a clear ellipse and indeed the numerical criterion returns a sufficient number of observations just for high quantiles
#the counterplot in the pxp space of PC1 vs PC3 does not seem to be a clear ellipse and indeed the numerical criterion returns a sufficient number of observations just for very low ore very high quantiles
#the counterplot in the pxp space of PC2 vs PC3 does not seem to be a clear ellipse and indeed the numerical criterion returns a sufficient number of observations just for high quantiles

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#3.3) Make scatterplots with the first three principal components, while color coding the observations according to the digit class:

#All scatterplots together
pairs(components, pch = 16, main = "Scatterplots of PC1, PC2 and PC3", col = digit.col, lower.panel = NULL)

#Single scatterplots:
plot(x = components[, 1], y = components[, 2], col = digit.col, xlab = "PC1", ylab = "PC2", main = "Scatterplot PC1 vs PC2", pch = 16)
plot(x = components[, 1], y = components[, 3], col = digit.col, xlab = "PC1", ylab = "PC3", main = "Scatterplot PC1 vs PC3", pch = 16)
plot(x = components[, 2], y = components[, 3], col = digit.col, xlab = "PC2", ylab = "PC3", main = "Scatterplot PC2 vs PC3", pch = 16)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#3.4) Comment about outliers with respect to the first three principal components:

#Boxplot evaluation:
outliers_bp_2 = boxplot_analysis(components); outliers_bp_2

boxplot(components[, 1], main = "Boxplot for the PC1")
points(x = rep(1, length(outliers_bp_2[[1]])), y = components[, 1][outliers_bp_2[[1]]], pch = 16, col = "red")
boxplot(components[, 2], main = "Boxplot for the PC2")
points(x = rep(1, length(outliers_bp_2[[2]])), y = components[, 2][outliers_bp_2[[2]]], pch = 16, col = "red")
boxplot(components[, 3], main = "Boxplot for the PC3")
points(x = rep(1, length(outliers_bp_2[[3]])), y = components[, 3][outliers_bp_2[[3]]], pch = 16, col = "red")

col = rep("black", dim(components)[1])
col[outliers_bp_2[[3]]] = "red"
plot(x = components[, 1], y = components[, 3], xlab = "PC1", ylab = "PC3", pch = 16, col = col)
plot(x = components[, 2], y = components[, 3], xlab = "PC2", ylab = "PC3", pch = 16, col = col)

#Scatterplot detection:
outlier_col = rep("black", dim(data)[1]); outlier_col
outlier_col[outliers_bp_2[[3]]] = "red"

pairs(components, pch = 16, main = "Scatterplots of PC1, PC2 and PC3", col = outlier_col, lower.panel = NULL)
plot(x = components[, 1], y = components[, 2], col = outlier_col, xlab = "PC1", ylab = "PC2", main = "Scatterplot PC1 vs PC2", pch = 16)
plot(x = components[, 1], y = components[, 3], col = outlier_col, xlab = "PC1", ylab = "PC3", main = "Scatterplot PC1 vs PC3", pch = 16)
plot(x = components[, 2], y = components[, 3], col = outlier_col, xlab = "PC2", ylab = "PC3", main = "Scatterplot PC2 vs PC3", pch = 16)

#Bivariate outliers:
c_12 = rep("black", dim(pendigits)[1])
bivariate_outliers_12 = which(self_mahalanobis(components[, c(1, 2)]) > qchisq(0.95, df = 2))
c_12[bivariate_outliers_12] = "red"
plot(x = components[, 1], y = components[, 2], xlab = "PC1", ylab = "PC2", main = "PC's Bivariate Outliers Analysis", pch = 16, cex = 0.75, asp = 1, ylim = c(-150, 150), xlim = c(-170, 170), col = c_12)
lines(ellipse(x = pca.S[c(1, 2), c(1, 2)], centre = pca.bar[c(1, 2)], level = 0.95), col = "red", lwd = 3)

c_13 = rep("black", dim(pendigits)[1])
bivariate_outliers_13 = which(self_mahalanobis(components[, c(1, 3)]) > qchisq(0.95, df = 2))
c_13[bivariate_outliers_13] = "red"
plot(x = components[, 1], y = components[, 3], xlab = "PC1", ylab = "PC3", main = "PC's Bivariate Outliers Analysis", pch = 16, cex = 0.75, asp = 1, ylim = c(-150, 150), xlim = c(-170, 170), col = c_13)
lines(ellipse(x = pca.S[c(1, 3), c(1, 3)], centre = pca.bar[c(1, 3)], level = 0.95), col = "red", lwd = 3)

c_23 = rep("black", dim(pendigits)[1])
bivariate_outliers_23 = which(self_mahalanobis(components[, c(2, 3)]) > qchisq(0.95, df = 2))
c_23[bivariate_outliers_23] = "red"
plot(x = components[, 2], y = components[, 3], xlab = "PC2", ylab = "PC3", main = "PC's Bivariate Outliers Analysis", pch = 16, cex = 0.75, asp = 1, ylim = c(-150, 150), xlim = c(-170, 170), col = c_23)
lines(ellipse(x = pca.S[c(2, 3), c(2, 3)], centre = pca.bar[c(2, 3)], level = 0.95), col = "red", lwd = 3)

bivariate_outliers = list(bivariate_outliers_12, bivariate_outliers_13, bivariate_outliers_23)
names(bivariate_outliers) = c("Bivariate Outliers PC1 vs PC2", "Bivariate Outliers PC1 vs PC3", "Bivariate Outliers PC2 vs PC3")

#Multivariate Outliers:
distances = self_mahalanobis(components)

plot(x = seq(1, dim(components)[1]), y = distances, pch = 16, main = "Evaluation of Multivariate Outliers: Mahalanobis distances vs Quantiles of the Chi-squared", xlab = "Observations", ylab = "Squared Mahalanobis distances", ylim = c(min(distances), max(distances)))
points(x = outliers_bp_2[[3]], y = distances[outliers_bp_2[[3]]], col = "red", pch = 16)
#abline(h = qchisq(0.99, df = 3), col = "green", lty = "dashed")
#abline(h = qchisq(0.97, df = 3), col = "red", lty = "dashed")
#abline(h = qchisq(0.95, df = 3), col = "blue", lty = "dashed")
#text(x = 10000, y = qchisq(0.99, df = 3), label = "Quantile: 0.99", col = "green", pos = 3)
#text(x = 10000, y = qchisq(0.97, df = 3), label = "Quantile: 0.97", col = "red", pos = 1)
#text(x = 10000, y = qchisq(0.95, df = 3), label = "Quantile: 0.95", col = "blue", pos = 3)

multivariate_outliers = list(which(distances > qchisq(0.97, df = 3)), which(distances > qchisq(0.95, df = 3) & distances < qchisq(0.97, df = 3)))
names(multivariate_outliers) = c("Over Quantile 0.97", "Between Quantiles 0.95 and 0.97")
multivariate_outliers

#Matching outliers:
matches_um = c()
position = 1
for(u_outlier in outliers_bp_2[[3]]){
  for(m_outlier in multivariate_outliers[[2]]){
    if(u_outlier == m_outlier){
      matches_um[position] = u_outlier
      position = position + 1
    }
  }
}
matches_um

matches_ub = list()
i = 1
indexes_1 = c()
j = 1
indexes_2 = c()
for(u_outlier in outliers_bp_2[[3]]){
  for(b_outlier in bivariate_outliers[[1]]){
    if(u_outlier == b_outlier){
      indexes_1[i] = u_outlier
      i = i + 1
    }
  }
  matches_ub[[1]] = indexes_1
  
  for(b_outlier in bivariate_outliers[[2]]){
    if(u_outlier == b_outlier){
      indexes_2[j] = u_outlier
      j = j + 1
    }
  }
  matches_ub[[2]] = indexes_2
}
names(matches_ub) = c("Matches for PC1 vs PC3", "Matches for PC2 vs PC3")
matches_ub

total_matches = c()
k = 1
for(outlier in matches_um){
  for(b_outlier in matches_ub[[1]]){
    if(outlier == b_outlier){
      total_matches[k] = outlier
      k = k + 1
    }
  }
  
  for(b_outlier in matches_ub[[2]]){
    if(outlier == b_outlier){
      found = F
      for(match in total_matches){
        if(match == b_outlier){
          found = T
        }
      }
      if(found == F){
        total_matches[k] = outlier
        k = k + 1
      }
    }
  }
}
total_matches
pendigits[total_matches,17]

s_q = sort(components[, 3])
s_m = mean(components[, 3])
s_sd = sd(components[, 3])

t_q = qnorm(ppoints(dim(components)[1]))

plot(y = s_q, x = t_q, pch = 16, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q plot for PC3: ")
abline(a = s_m, b = s_sd, col = "red", lwd = 3)
qqline(components[, 3], lwd = 3)

#We observe that there're univariate outliers just for the third PC (boxplot analysis) and indeed we cannot find bivariate outliers for PC1 and PC2
#We detect some biavriate outliers in all the scatterplots which involves the third variable which match with some univariate outliers of the third variable
#There're some multivariate outliers for the 3 PC's which (over the 0.97 quantile) that matches with some bivariate and univariate outliers of the third PC

