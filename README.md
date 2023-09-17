# MSA_OD_PCA

**Project Description:**
Application of basic outlier detection techniques and Principant Component Analyses for a university group project (working in R).

**Assignment 1 (OD):**
The air pollution data (le usair.txt) consists of 7 measurements recorded at $n = 41$ cities in the United States. Variables are SO2: Sulphur dioxide content in micrograms per cubic meter, Neg.Temp: Average annual temperature in Fo (negative values), Manuf: Number of manufacturing enterprises employing 20 or more workers, Pop: Population size (1970 census) in thousands, Wind: Average annual wind speed in miles per hour, Precip: Average annual precipitation in inches, Days: Average number of days with precipitation per year. Ignore the SO2 variable and concentrate on the remaining 6, two of which relate to human ecology (Manuf, Pop) and four to climate (Neg.Temp, Wind, Precip, Days).

1) Compute the mean vector, the correlation matrix and comment on correlations.
2) Make a boxplot of each variable and comment about the presence of outliers (no more than two per variable). Identify these observations.
3) Construct a normal Q-Q plot for each variable and comment about normality.
4) By using scatter plots, comment on whether the outliers at point 1.2) can be detected from them.
5) Construct a chi-square Q-Q plot of the squared Mahalanobis distances and comment about normality.
6) Identify multivariate outliers, if any, and compare with the answer to point 2)


**Assignment 2 (PCA):**
The pen digit data set (le pendigits.txt) was created by collecting 250 samples from 44 writers. These writers were asked to write 250 digits in random order inside boxes of 500 by 500 tablet pixel resolution. The raw data on each of n = 10992 handwritten digits consisted of a sequence, (xt ; yt ), t = 1; 2; ...;T, of tablet coordinates of the pen at xed time intervals of 100 milliseconds, where xt and yt were integers in the range 0-500. These data were then normalized to make the representations invariant to translation and scale distortions. The new coordinates were such that the coordinate that had the maximum range varied between 0 and 100. Usually xt stays in this range, because most integers are taller than they are wide. Finally, from the normalized trajectory of each handwritten digit, 8 regularly spaced
measurements, (xt ; yt ), were chosen by spatial resampling, which gave a total of p = 16 variables. The data includes a class attribute, column digit, coded 0; 1; ...; 9, about the actual digit.

1) Perform a principal component analysis on the standardized variables. Report standard deviations. Decide how many components to retain in order to achieve a satisfactory lower-dimensional representation of the data. Justify your answer.
2) Investigate multivariate normality through the rst three principal components.
3) Make scatter plots with the first three principal components, while color coding the observations according to the digit class.
4) Comment about outliers with respect to the first three principal components.
