install.packages("metafor")
library(metafor)

#read data

my_data <- read.csv("met_data.csv", dec = ",")
my_data

#In the first step we use the escalc() function to calculate effect sizes and their variance.

my_data <- escalc(n1i = n_controls, n2i = n_patients, m1i = mean_controls, m2i = mean_patients, 
    sd1i = sd_controls, sd2i = sd_patients, data = my_data, measure = "SMD", 
    append = TRUE)

#Using the effect sizes and variance we can calculate the random-effects meta-analysis. The results are stored in the object 'ma_model_1'. You can retrieve the results using the summary() function.

ma_model_1 <- rma(yi, vi, data = my_data)
summary(ma_model_1)

#There is a lot of info in the R output above. Most importantly you can see that there is an summary effect size of 1.4446 representing differences between patients and controls. Also this difference is statistically significant with a p=0.0253. In order to visualize the results you can create a forest-plot using the forest() function.

forest(ma_model_1, slab = paste(my_data$study, as.character(my_data$year), sep = ", "))

#A common way to investigate potential publication bias in a meta-analysis is the funnel plot. Asymmetrical distribution indicates potential publication bias.

funnel(ma_model_1)

#We could also check if there are differences between the two tests used. First let's visualize the difference using a boxplot.

boxplot(yi ~ test, data = my_data)

