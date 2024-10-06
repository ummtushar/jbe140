# Install the necessary packages if you haven't already
# install.packages("igraph")
# install.packages("ggraph")
# install.packages("tidygraph")
install.packages("tidyr")

# Load the libraries
library(igraph)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(dplyr)
library(tidyr)
library(stringr)


# Loading the data 
data = readRDS("SDC_data_2021.rds")
data[1:5,]


# Convert 'date_announced' to Date format
data$date_announced <- as.Date(data$date_announced, format = "%Y-%m-%d")

# Define EU countries
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", 
                  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
                  "Malta", "Netherlands", "Poland", "Portugal", "Romania", 
                  "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")

filtered_data <- data[grepl("Bank|Banking", data$business_description, ignore.case = TRUE) & 
                        data$date_announced >= as.Date("2003-01-01") & 
                        data$date_announced <= as.Date("2007-12-31") & 
                        str_starts(as.character(data$SIC_primary), "60"), ] 

# Further filter for Strategic Alliances
filtered_data <- filtered_data[filtered_data$type == 'Strategic Alliance',]

# Remove terminated deals (keeping active ones)
filtered_data <- filtered_data[filtered_data$date_terminated == "",]

# Filter for EU countries / US
# filtered_data <- filtered_data[filtered_data$participant_nation %in% eu_countries,]
filtered_data <- filtered_data[filtered_data$participant_nation %in% 'United States',]

# Get all other participants of deals we already have
relevant_deals = unique(filtered_data$deal_number)
relevant_data  = data[data$deal_number %in% relevant_deals,]

# Combine filtered data and relevant data
final_data <- bind_rows(filtered_data, relevant_data)
final_data <- distinct(final_data)

# View the final data
final_data

# Load necessary libraries
library(igraph)

# Create edge list: Pair up participants involved in the same deal
edge_list <- final_data %>%
  select(deal_number, participants) %>%
  distinct() %>%
  group_by(deal_number) %>%
  summarize(pairs = list(combn(participants, 2, simplify = FALSE))) %>%
  unnest(pairs) %>%
  ungroup()

# Convert the list of pairs into a data frame with two columns: from and to
edge_list <- do.call(rbind, lapply(edge_list$pairs, function(x) data.frame(from = x[1], to = x[2])))

# Create a graph object from the edge list
g <- graph_from_data_frame(edge_list, directed = FALSE)

# Plot using Fruchterman-Reingold layout
ggraph(g, layout = 'fr') +  
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE) +  
  geom_node_point(size = 0.5, color = 'skyblue') +  
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +  
  theme_void()

# Plot using Circular layout
ggraph(g, layout = 'circle') +  
  geom_edge_link(aes(edge_alpha = 0.5), show.legend = FALSE) +  
  geom_node_point(size = 5, color = 'orange') +  
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +  
  theme_void()

