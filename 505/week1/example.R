install.packages("ggplot2")

sample_set <- sample(c("Prithvi","Priyanka","Ajay","Gaurav"))

# print(sample_set)
# [1] "Gaurav"   "Priyanka" "Prithvi"  "Ajay"    
# [1] "Gaurav"   "Ajay"     "Prithvi"  "Priyanka"
# [1] "Gaurav"   "Priyanka" "Ajay"     "Prithvi" 
# [1] "Gaurav"   "Priyanka" "Ajay"     "Prithvi" 
# [1] "Prithvi"  "Gaurav"   "Ajay"     "Priyanka"

# Load necessary libraries
library(ggplot2)

# Sample data
data <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(5, 7, 4, 10, 6)
)

# Create scatter plot with ggplot2
plot <- ggplot(data, aes(x=x, y=y)) + 
  geom_point(aes(color=y), size=4) + 
  labs(title="Sample Scatter Plot", x="X Axis", y="Y Axis") + 
  theme_minimal()

# Display the plot
print(plot)
