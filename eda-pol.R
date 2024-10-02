install.packages("igraph")
install.packages("statnet")
install.packages("intergraph")
install.packages("magrittr")
install.packages("dplyr")

library(igraph)
library(statnet)
library(intergraph)
library(magrittr)
library(dplyr)

# Loading the data 
data = readRDS("SDC_data_2021.rds")

# filtering data considering cargo or freight business description 
filtered_data <- data[grepl("freight|cargo", 
                            data$business_description, ignore.case = TRUE),]

# Removing terminated deals and leaving active deals
filtered_data = filtered_data[filtered_data$date_terminated == "",]

# Filtering data considering BENELUX countries 
filtered_data <- filtered_data[filtered_data$participant_nation 
                               %in% c("Netherlands", 
                                      "Belgium",
                                      "Luxemburg"),]

# Get all other participants of deals we already have
relevant_deals = unique(filtered_data$deal_number)
relevant_data  = data[data$deal_number %in% relevant_deals,]

# Combining relecant deals and filtered data
final_data <- bind_rows(filtered_data, relevant_data)
final_data <- distinct(final_data)

# Count the number of occurrences of each participant
participant_counts <- final_data %>%
  group_by(participants) %>% # Replace 'participant_name' with the actual column name for participants
  summarize(count = n()) %>%
  arrange(desc(count)) # Sort in descending order

# Display the top participants
print(participant_counts)

top_participants <- participant_counts %>%
  slice_head(n = 30)


print(top_participants)

# Counting bumber of actors 
participants_num = nrow(final_data)

