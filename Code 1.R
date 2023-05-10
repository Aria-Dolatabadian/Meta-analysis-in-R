#https://rstudio-pubs-static.s3.amazonaws.com/846063_31ea2fdbb79f467abfc46cd503c1c8f7.html

library(psychmeta)  
library(effectsize) 
library(esc)
library(ggplot2)
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggrepel)
library(devEMF)

# Assign vector of effect sizes to a new variable ri
ri <- c(.55, .50, .18, .85, .62, .71, .58, .65, .60, .72)
# Assign vector of total sample sizes to a new variable ni  
ni <- c(50, 76, 93, 43, 33, 45, 70, 65, 41, 47)
# Automatically create vector of character strings to use as study IDs
study_id <- format(1:length(ni), digits=1, trim=FALSE) # Study IDs (could be replaced with vector)
# Create a vector of study Author
study <- c("Barnett (2015)",
           "Barnett et al. (2010)",
           "Dodds et al. (2017)",
           "Schroeder and Kennel (2018)",
           "Heyward et al. (2009)",
           "Kennell (2012)",
           "Kennell et al. (2014)",
           "Sharp et al. (2011)",
           "Silver et al. (2016)",
           "Spencer et al. (2008)");
# Create nice table summarizing standardized effect sizes for the studies that will 
# be used in the meta-analysis
tibble(study_id = study_id, study = study, ri = ri, ni = ni) %>%
  knitr::kable(caption = "Summary of study effect sizes", digits = 2,
               col.names = c("Study ID", "Author, Date", "*r*", "*n*")) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))


# Run the barebones meta-analysis using the method of Hunter & Schmidt
result <- ma_r(ri, ni, ma_method="bb", 
               control=control_psychmeta(cred_level=0.95),
               sample_id=study_id)


# Preview the summary of results
summary(result)

# Perform heterogeneity analysis and append the output to results
result <- heterogeneity(result) 
# Preview the results of the heterogenity analysis
result$heterogeneity


# For convenience, first assign the heterogeneity results to a new variable
het_result <- result$heterogeneity[[1]][[1]]$HS_method

# Set whether to truncate intervals that exceed +/- 1.0 
#
# Correlation coefficients can only adopt values in the range -1.0 to +1.0. 
# For this reason, many methods use Fisher's z transformation before averaging 
# correlation coefficients or calculating variance (or other derived statistics 
# such as confidence intervals). However, Hunter and Schmidt make arguments for 
# performing such calculations on the original scale of the correlation coefficient.
# This can produce intervals with impossible limits (i.e. that exceed +/- 1.0) 
# since the distribution of the correlation coefficient becomes skewed towards 
# the extreme ends. While this may seem unacceptable, absolute values of the 
# correlation coefficient less than 0.5 can be normally distributed and it's 
# this end of confidence or credibility intervals that is often more of interest. 
# The end of the interval that exceeds plausible values will be very close to 
# +/- 1 and so it is reasonable to truncate the interval in respect of the limits.
truncate_intervals = TRUE
if (truncate_intervals==TRUE) {
  lim = 1.0
}  else {
  lim = Inf
}

# Now let's summarise all the results in a nice table!
result %>%
  get_metatab() %>%
  as_tibble() %>%
  mutate(CI_95 = sprintf("[%.2f, %.2f]", max(CI_LL_95,-lim), min(CI_UL_95,lim))) %>%
  mutate(I2 = max(0,het_result$I_squared)) %>%
  mutate(CR_95 = sprintf("[%.2f, %.2f]", max(CR_LL_95,-lim), min(CR_UL_95,lim))) %>%  
  dplyr::select(c(k, N, mean_r, se_r, CI_95, I2, CR_95)) %>%
  knitr::kable(caption = "Summary of meta-analysis results", digits = 2, align = "c",
               col.names = c("*k*", "*N*", "*r\u0305*", "*SE*", "95% *CI*", sprintf("*I*\u00b2"), "95% *CR*")) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), row_label_position="c")


# Create forest plot to summarize the effect sizes of all the studies
result <- plot_forest(result, conf_level = 0.95, y_lab="Study ID")

# Get forest plot handle
h1 <- get_plots(result)[["forest"]][[1]][["unmoderated"]][["barebones"]]

# Edit the default graph formatting
# The size of the circle is proportional to the size of the study
circle_size   <- ni*0.1 # 0.1 is just an arbitrary scale factor to adjust the relative size of the points (observed effect). Adjust this as you see fit
triangle_size <- 6     # 10 is just a reasonable size for the triangle on the graph (overall effect). Adjust this as you see fit
h1$layers[[1]]$aes_params$size <- c(circle_size, triangle_size) 
h1$layers[[1]]$aes_params$colour <- "red" # Set marker colour to red
h1$layers <- h1$layers[4:1]               # reverse order of layers so that the markers are on top

# Display forest plot after reformatting using ggplot
# Useful cheat sheet for ggplot2: https://github.com/rstudio/cheatsheets/raw/master/data-visualization.pdf
h1 + theme(text = element_text(size=16)) -> h1  # Set font size
emf("forest.emf", width=5.5, height=3.5)  # Save plot as vector graphic (enhanced meta file)
h1 # Send graph to file
dev.off() 
h1 # Display graph in here knitted document


# Create funnel plot
result <- plot_funnel(result)

# Get funnel plot handle
h2 <- get_plots(result)[["funnel"]][[1]][["barebones"]]

# Change the size of the points on the graph.
# The size of the circle is proportional to the size of the study
circle_size   <- ni*0.1   # 0.1 is just an arbitrary scale factor to adjust the relative size of the points
h2$layers[[1]]$aes_params$size <- circle_size
h2$layers[[1]]$aes_params$alpha <- 0.5         # Set alpha to 0.5 (to enhance transparency)
h2$layers[[1]]$aes_params$colour <- "red"      # Set marker colour to red

# Display the funnel plot after reformatting using ggplot
h2 + geom_label_repel(aes_(y = ~yi),label=study_id) +
    theme(text = element_text(size=16)) -> h2
emf("funnel.emf", width=5.5, height=3.5)  # Save plot as vector graphic (enhanced meta file)
h2 # Send graph to file
dev.off() 
h2 # Display graph in here knitted document


# Perform sensitivity analysis
result <- sensitivity(result,sort_method = c("inv_var"))

# Edit and display forest plot of cumulative meta-analysis results
h3 <- get_cumulative(result)[[1]][["barebones"]][["plots"]]$mean_plot
h3 + geom_count(size=4, colour="red") +
    xlab("Study ID (sorted by effect size precision)") +
    theme(text = element_text(size=16)) -> h3
emf("cumulative.emf", width=5.5, height=3.5)
h3 # Send graph to file
dev.off() 
h3 # Display graph in here knitted document


# Edit and display forest plot of leave-one-out meta-analysis results
h4 <- get_leave1out(result)[[1]]$barebones$plots$mean_plot
h4 + geom_count(size=4, colour="red") +
    xlab("Study ID") +
    theme(text = element_text(size=16)) -> h4
emf("leave1out.emf", width=5.5, height=3.5)  # Save plot as vector graphic (enhanced meta file)
h4 # Send graph to file
dev.off() 
h4 # Display graph in here knitted document


