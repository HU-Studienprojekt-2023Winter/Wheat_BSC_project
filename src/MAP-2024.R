
# Pakcages and lirbaries --------------------------------------------------

library(Matrix)
library(ggplot2)
library(lme4)
library(tidyr)
library(dplyr)
library(viridis)
library(broom)
library(hrbrthemes)
library(ggpubr)

merged_data<- read.csv("./data_Eren/merged_data.csv")
glimpse(merged_data)#needs to have 15 columns, 4,270 rows
glimpse(colors_batches)
colors_batches <- c("#E9002D", "#FFAA00", "#00B000")

# Check the structure of merged_data
glimpse(merged_data)


# code for p-value flower-spikelet- funtions, dont touch it -------------------------------------------------------
class(merged_data$flower)
class(merged_data$spike)

merged_data %>%
  group_by(plot_id, batch) %>%
  summarise(count = n())
#anova
anova_p_values <- merged_data %>%
  group_by(plot_id, batch) %>%
  summarise(p_value = tidy(aov(flower ~ spike))$p.value[1])

#new df
merged_data_with_pvalues <- merge(merged_data, anova_p_values, by = c("plot_id", "batch"))

# check if right
str(merged_data_with_pvalues)

#plot
plot_1<- merged_data_with_pvalues %>%
  ggplot(aes(x = spike, y = flower, color = factor(batch))) +
  geom_boxplot(width = 0.5, fill = "darkgrey", color = "black") +
  geom_jitter(width = 0.1) +
  facet_grid(~ plot_id + batch, scales = "free", space = "free") +
  scale_color_manual(values = colors_batches) +
  labs(x = "Spikelet", y = "Flower") +
  geom_text(data = subset(merged_data_with_pvalues, !is.na(p_value)), 
            aes(label = paste0("p = ", format(p_value, scientific = TRUE, digits = 2)), 
                x = 4, y = max(flower) - 1),
            color = "black", hjust = 0.1, vjust = 0.1, size = 4) +  # Adjusted coordinates for centering
  geom_text(data = subset(merged_data_with_pvalues, !is.na(p_value)), 
            aes(label = paste0("(", round(p_value, digits = 3), ")"), x = 4, y = max(flower) - 1.5), 
            color = "black", hjust = 0.1, vjust = 0.1, size = 3.5, fontface = "italic", color = "red")  # Decimal number added under the p-value

#run plot
plot_1
#pvalues to look at
print(paste(unique(merged_data_with_pvalues$p_value), collapse = ", "))

# code for spike-nr and spikelet-nr ------------------------------------------------------
# Filter the data by plot_id
filtered_data <- merged_data %>%
  filter(plot_id %in% c(36, 38))

# Plot filtered data
ggplot(filtered_data, aes(x = rep, y = spike, color = factor(batch))) +
  geom_boxplot(width = 1, fill = "darkgrey", color = "black") +
  geom_jitter(width = 0.1) +
  facet_grid(plot_id ~ batch, scales = "free", space = "free") +
  scale_color_manual(values = colors_batches) +
  scale_x_continuous(breaks = seq(1, 10, by = 1))
#########################################################################
#code for printing spikelet number

filtered_data <- merged_data %>%
  filter(plot_id %in% c(36, 38)) %>%
  rename(spikelett = spike)


total_spikes <- filtered_data %>%
  group_by(plot_id, batch) %>%
  summarise(total_spikeletts = sum(spikelett), .groups = "drop") 

names(total_spikes)[3] <- "total_spikelets"
print(total_spikes)


# code for ----------------------------------------------------------------)

#refine filter
filtered_data <- merged_data %>%
  filter(plot_id %in% c(36, 38))


kernel_counts <- filtered_data %>%
  group_by(plot_id, batch, kernel.type) %>%
  summarise(total_kernels = sum(total)) %>%
  ungroup()

# Make plot
ggplot(kernel_counts, aes(x = factor(batch), y = total_kernels, fill = kernel.type)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = total_kernels), position = position_stack(vjust = 0.5), color = "white", size = 3) + # Add labels for total kernels
  facet_wrap(~ plot_id) +
  labs(x = "Batch", y = "Total Kernel Count", fill = "Kernel Type", title = "grain sizes and amount for plots 36 and 38") +
  theme_minimal() +
  theme(legend.position = "top")
