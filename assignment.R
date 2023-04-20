# Import the required libraries
library(readxl)

# Read data from the dataset(excel sheet)
data <- read_excel("Healthcare_costs.xlsx")


# Choose the columns that are pertinent to the age of 60
data <- data[, c("Disease", "Pcosts60", "Patients60", "Total60")]

# Choose the appropriate fields for individuals aged 60 and organize them according to the preferred sequence
new_order <- c("Chronic Sunisitis", Choose the appropriate fields for individuals aged 60 and organize them according to the preferred sequence"Diabetes", "Bronchitis", "Osteoporosis",
               "Rheumatoid Arthritis", "Acid Reflux", "Ostearthritis", "Depression", "Emphysema", "Asthma", "Hypertension")

data <- data[match(new_order, data$Disease),]
colors <- colors[match(new_order, data$Disease)]

# Determine the aggregate cost and overall patient count
total_cost <- sum(data$Total60)
total_patients <- sum(data$Patients60)

# Determine the radius and angle for every illness
data$radius <- data$Pcosts60 / max(data$Pcosts60)
data$angle <- data$Patients60 / total_patients * 360

# Specify the colors to be used in the Spie chart
colors <- c("#FFFE93", "#CDC2EC","#F6896A", "#68A2E5","#FCC1A0","#B6EB9B","#F4D8F6","#DDE4E2","#DFEBC9", "#BBDD6B", "#79E3C5")

# Save the initial graphics settings
original_par <- par()

# Adjust the outer margin for the title and subtitle
par(oma = c(0, 0, 3, 0))

# Create the plot
plot.new()
plot.window(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), asp = 1)

# Add main title using title()
title(main = "The Cost of Getting Sick", outer = TRUE, line = -1, adj = 0.3)

# Add subtitle using mtext
mtext("The Medical Expenditure Panel Survey. Age: 60, Total Costs: 41.4 Mio. US $", side = 3, outer = TRUE, line = -3, cex = 0.8, adj = 0.3)

# Define the radial_plot function
radial_plot <- function(radius, circle_start_angle, end_angle, col) {
  theta <- seq(circle_start_angle, end_angle, length.out = 100) * pi / 180
  x <- c(0, radius * cos(theta), 0)
  y <- c(0, radius * sin(theta), 0)
  polygon(x, y, col = col, border = NA)
}


# Define the radial_plot function with transparency
radial_plot_transparent <- function(radius, circle_start_angle,end_angle, col, border = NA) {
  theta <- seq(start_angle,end_angle, length.out = 100) * pi / 180
  x <- c(0, radius * cos(theta), 0)
  y <- c(0, radius * sin(theta), 0)
  col_transparent <- adjustcolor(col, alpha = 0.5) 
  polygon(x, y, col = col_transparent, border = NA)
}

inner_radius <- 0.45

start_angle <- 120
for (j in 1:nrow(data)) {
  end_angle <- start_angle + data$angle[j]
  
  # Draw the Spie chart with transparent colors
  radial_plot_transparent(data$radius[j], cirlce_start_angle, end_angle, col = colors[j])
  
  # Add inner circle for each category with clear colors
  radial_plot(inner_radius, start_angle, end_angle, col = colors[j])
  
  # Add horizontal labels with disease name and total costs outside the plot
  circle_label_angle <- (start_angle + end_angle) / 2
  label_radius <- 1.4
  text(label_radius * cos(circle_label_angle * pi / 180), 
       label_radius * sin(circle_label_angle * pi / 180), 
       labels = paste(data$Disease[j], format(round(data$Total60[j] /1000000, 1), big.mark = ",", scientific = FALSE), "Mio. US $"), 
       cex = 0.7, adj = c(0.5, 0.5))
  line_start_radius <- data$radius[j] + 0.1
  line_end_radius <- label_radius - 0.3
  segments(line_start_radius * cos(circle_label_angle * pi / 180),
           line_start_radius * sin(circle_label_angle * pi / 180),
           line_end_radius * cos(circle_label_angle * pi / 180),
           line_end_radius * sin(circle_label_angle * pi / 180),
           lwd = 0.5)
  
  start_angle <- end_angle
}


# what the plot indictes?
# The plot displays the costs associated with various diseases among individuals who are 60 years old. The plot is divided into several segments based on the number of patients diagnosed with each disease. The size of each segment corresponds to the cost incurred per patient for that particular disease, while the angle of each segment represents the proportion of patients with that disease among the total number of patients.

#) What does your graph shows?
# According to the graph, the disease with the largest number of patients is Hypertension, as its radius is the highest. Following Hypertension are Diabetes and Acid reflux, which also have relatively large radii. The graph also indicates that Emphysema has the highest cost, as its angle is the highest among all the diseases shown. Diabetes and Rheumatoid Arthritis have lower angles and therefore lower costs compared to Emphysema.