setwd("~/Workspace/mcdaniel/500/week5")

# Load the dataset
data <- read.csv('Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv')
data$spend_binary <- as.numeric(data$spend > 0)

# Convert 'spend_binary' to factor
data$spend_binary <- as.factor(data$spend_binary)

# Convert other categorical columns to factors
data$segment <- as.factor(data$segment)
data$newbie <- as.factor(data$newbie)
data$channel <- as.factor(data$channel)

# Split data into train and test sets
set.seed(42)
split_index <- sample(1:nrow(data), nrow(data)*0.8)
train_data <- data[split_index, ]
test_data <- data[-split_index, ]

# Load necessary library
library(naivebayes)

# Train a Naïve Bayes classifier
model <- naive_bayes(spend_binary ~ segment + newbie + channel, data = train_data)
print(model)


# Load necessary library
install.packages("DiagrammeR") # Install the package if you haven't
library(DiagrammeR)

# Create a DAG
# Load necessary library

# Create a DAG for Naïve Bayes
graph_nb <- create_graph() %>%
  add_node(label = "Segment", type = "factor", node_aes = node_aes(shape = "rectangle", fillcolor = "lightblue")) %>%
  add_node(label = "Newbie", type = "factor", node_aes = node_aes(shape = "rectangle", fillcolor = "lightblue")) %>%
  add_node(label = "Channel", type = "factor", node_aes = node_aes(shape = "rectangle", fillcolor = "lightblue")) %>%
  add_node(label = "Spend_Binary", type = "target", node_aes = node_aes(shape = "octagon", fillcolor = "pink")) %>%
  add_edge(from = "Segment", to = "Spend_Binary") %>%
  add_edge(from = "Newbie", to = "Spend_Binary") %>%
  add_edge(from = "Channel", to = "Spend_Binary")

# Plot the DAG for Naïve Bayes
render_graph(graph_nb)
