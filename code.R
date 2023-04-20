# library(Rtsne) # for running t-SNE algorithm
 library(ggplot2) # for plotting

# # replace "data.csv" with the name of your dataset file
# data <- read.csv("data.csv", header = TRUE)

# # set perplexity to a value between 5 and 50 (usually 30 works well)
# tsne <- Rtsne(data, perplexity = 30, verbose = TRUE)

# # create a data frame with the t-SNE results
tsne_df <- data.frame(tsne$Y, label = data$label)

# # plot the t-SNE results
# ggplot(tsne_df, aes(x = X1, y = X2, color = label)) +
#   geom_point() +
#   theme_bw()

# Load the required packages
library(Rtsne)
library(ggplot2)

# Load the iris dataset
data(iris)

# Run t-SNE algorithm on the iris dataset
tsne <- Rtsne(iris[,1:4], dims = 2, perplexity = 30)

# Create a scatter plot of the t-SNE result
ggplot() + geom_point(aes(x = tsne$Y[,1], y = tsne$Y[,2], color = iris$Species)) + ggtitle("t-SNE Plot of Iris Dataset")




# Load necessary libraries
library(readxl)

# Read data from the "Healthcare_costs.xlsx" file
data <- read_excel("Healthcare_costs.xlsx")

# Select the relevant columns for age 60 and arrange them in desired order
new_order <- c("Chronic Sunisitis", "Diabetes", "Bronchitis", "Osteoporosis",
               "Rheumatoid Arthritis", "Acid Reflux", "Ostearthritis", "Depression", "Emphysema", "Asthma", "Hypertension")
data <- data %>%
  filter(Disease %in% new_order) %>%
  select(Disease, Pcosts60, Patients60, Total60) %>%
  arrange(match(Disease, new_order))

# Calculate the total cost and total number of patients
total_cost <- sum(data$Total60)
total_patients <- sum(data$Patients60)

# Calculate the radius and angle for each disease
data <- data %>%
  mutate(radius = Pcosts60 / max(Pcosts60),
         angle = Patients60 / total_patients * 360)

# Define colors for the Spie chart
colors <- c("#FFFE93", "#CDC2EC","#F6896A", "#68A2E5","#FCC1A0","#B6EB9B","#F4D8F6","#DDE4E2","#DFEBC9", "#BBDD6B", "#79E3C5")

# Create the plot
ggplot(data, aes(x = "", y = angle, fill = Disease)) +
  geom_bar(width = 1, size = 1, stat = "identity") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("The Cost of Getting Sick") +
  labs(subtitle = "The Medical Expenditure Panel Survey. Age: 60, Total Costs: 41.4 Mio. US $") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.margin = margin(3, 3, 3, 3, "cm"))
        
# Add labels for each disease and total cost outside the plot
label_data <- data %>%
  mutate(mid_angle = cumsum(angle) - angle / 2,
         label_radius = 1.4) %>%
  filter(!is.na(mid_angle))

ggplot(label_data, aes(x = label_radius * cos(mid_angle * pi / 180),
                       y = label_radius * sin(mid_angle * pi / 180),
                       label = paste(Disease, format(round(Total60 / 1000000, 1), big.mark = ",", scientific = FALSE), "Mio. US $"))) +
  geom_text(size = 6, hjust = 0.5, vjust = 0.5, fontface = "bold") +
  coord_fixed() +
  theme_void() +
  theme(plot.margin = margin(3, 3, 3, 3, "cm"))
