ytb_data <- read.csv ("C:\\Users\\Benny Pang\\Desktop\\NTU study\\MH3511\\Project\\data part 4.csv")
str(ytb_data)

#relationship between salary and the title_letters 
# Fit a linear regression model
model <- lm(log(views) ~ title_letters, data = ytb_data)

# Summarize the model
summary(model)

# Plot the model
plot(ytb_data$title_letters, log(ytb_data$views), 
     main = "Relationship between log(Views) and Title Letters", 
     xlab = "Title Letters", ylab = "log(Views)")
abline(model, col = "red")


#relationship between salary and the title_words
# Fit a linear regression model
model <- lm(log(views) ~ title_words, data = ytb_data)

# Summarize the model
summary(model)

# Plot the model
plot(ytb_data$title_words, log(ytb_data$views), 
     main = "Relationship between log(Views) and title_words", 
     xlab = "title_words", ylab = "log(Views)")
abline(model, col = "red")


#relationship between salary and the prop_capitals
# Fit a linear regression model
model <- lm(log(views) ~ sqrt(prop_capitals), data = ytb_data)

# Summarize the model
summary(model)

# Plot the model
plot(sqrt(ytb_data$prop_capitals), log(ytb_data$views), 
     main = "Relationship between log(Views) and prop_capitals", 
     xlab = "sqrt(prop_capitals)", ylab = "log(Views)")
abline(model, col = "red")


#for the 

install.packages("GGally")
install.packages("tidyverse")

library(tidyverse)
library(GGally)


# Assuming ytb_data is your data frame
ytb_data <- read.csv("C:/Users/Benny Pang/Desktop/NTU study/MH3511/Project/data part 4.csv")
str(ytb_data)

# Remove the first column which is just a serial number
ytb_data <- ytb_data[,-1]


colnames(ytb_data)
class(names(ytb_data))

get_cor <- function(data, mapping, ...){

  
  # Now extract the data as vectors
  x_var <- eval(rlang::quo_get_expr(mapping$x), data)
  y_var <- eval(rlang::quo_get_expr(mapping$y), data)
  
  
  # Check that x_var and y_var are numeric
  if (!is.numeric(x_var) || !is.numeric(y_var)) {
    stop("Variables must be numeric to calculate correlation.")
  }
  
  # Calculate correlation with complete cases
  cor_val <- cor(x_var, y_var, use = "complete.obs")
  
  # Determine the color based on the correlation value
  color <- ifelse(cor_val > 0.5, "red", ifelse(cor_val < -0.5, "blue", "black"))
  
  # Return the ggplot object
  ggplot(data.frame(x = 1, y = 1), aes(x = x, y = y, label = sprintf("%.2f", cor_val), color = color)) + 
    geom_text() +
    scale_color_identity() +
    theme_void() + 
    theme(legend.position = "none")
}


# Customize the ggpairs plot
ggpairs(ytb_data,
        upper = list(continuous = "points"),
        lower = list(continuous = get_cor),
        diag = list(continuous = "blankDiag"),
        axisLabels = "show"
) + theme_bw() + theme(legend.position = "none") +
  theme(axis.text.x = element_blank(),    # Remove x axis text
        axis.text.y = element_blank(),    # Remove y axis text
        #axis.ticks = element_blank(),     # Remove axis ticks
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.background = element_blank()) # Remove panel background



#4.2.2
str(ytb_data)

#see the log views

log_view <- log(ytb_data$views)
summary(log_view)

summary((ytb_data$tags))

view_category <- cut(
  log_view,
  breaks = c(-Inf, 11.967, 13.020, 13.970, Inf),
  labels = c("Low", "Medium-Low", "Medium-High", "High"),
  include.lowest = TRUE
)

view_category

sqrt_tags_category <- cut(
  sqrt(ytb_data$tags), 
  breaks = c(-Inf, 3, 4.243, 5.292, Inf),  
  labels = c("Sparse", "Moderate", "Abundant", "Excessive"),
  include.lowest = TRUE
)

contingency_table <- table(view_category, sqrt_tags_category)
contingency_table
# Perform the Chi-square test
chi_square_result <- chisq.test(contingency_table)

# Output the result
print(chi_square_result)

data564<-read.csv("C:\\Users\\Benny Pang\\Desktop\\NTU study\\MH3511\\Project\\data564.csv")

ytb_data <- data564

summary(ytb_data)

hist(prop_capitals)
subset_data1 <- prop_capitals[prop_capitals == 1]
other_data1 <- prop_capitals[prop_capitals != 1]

sum(subset_data1)
length(other_data1)

boxplot_data1 <- list(subset_data1, other_data1)
# Create a boxplot
boxplot(boxplot_data1, names = c("==1", "!=1"))

var.test(log(views[prop_capitals == 1]), 
         log(views[prop_capitals != 1]))

t.test(log(views[prop_capitals == 1]), log(views[prop_capitals != 1]), alternative = "greater", var.equal = F)
#p-value = 0.06424
t.test(log(views[prop_capitals == 1]), log(views[prop_capitals != 1]), alternative = "greater", var.equal = T)
#p-value = 0.07533

t.test(log(views[prop_capitals == 1]), log(views[prop_capitals != 1]), var.equal = F)
#p-value = 0.1285
t.test(log(views[prop_capitals == 1]), log(views[prop_capitals != 1]), var.equal = T)

