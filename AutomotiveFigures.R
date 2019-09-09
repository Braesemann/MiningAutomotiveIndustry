#---------------------------------------------------------
#
# MINING THE AUTOMOTIVE INDUSTRY
#
# Paper submitted to 'Complex Networks 2019'
#
# September 2019
#
# Authors: Niklas Stoehr, Fabian Braesemann, Shi Zhou
#
# Contact: fabian.braesemann@sbs.ox.ac.uk
#
# Licence: CC-BY (4.0)
#
#
# --- DESCRIPTION ---
# 
# Here, we provide the code to produce the figures 1C, 2A (inset), 2B, 2D
# of the paper. The figure 1A is a manually produced diagram. 
#
# Figures 2B, 2A (main), 2C have been manually created using Gephi.
# Equally, the results displayed in table 1D comes from the networks produced in Gephi.
#
# --- DISCLAIMER ---
# 
# Web pages change frequently. We crawled the data in 2018.
# Thus, the number of innovative trend keywords per web page is likely to have changed.
# 
#---------------------------------------------------------

# Packages
library(tidyverse)    # Allrounder for data manipulation
library(scales)       # Package for log-labels
library(RColorBrewer) # More colours
library("ggrepel")    # Package for the labels with arrow

# Read data on web pages 
df <- read.csv(paste(getwd(),"/node_data.csv", sep =""))

#%%%%%%%%%%%%%%%
# --- Fig 1C ---
#%%%%%%%%%%%%%%%

# Group by Node degree
nodeDegree_DF <- df %>% group_by(manufacturer,node_degree) %>% summarise(count = n())

# For the loop: define list of manufacturers and respective colours
manufacturers <- c("Hyundai", "Toyota", "Volkswagen")
man_colours <- c(brewer.pal(9,"BuPu")[8], brewer.pal(9,"BuPu")[5], brewer.pal(9,"PuBu")[6])

for(i in 1:length(manufacturers)){                                                       # Loop to produce three figures
  
fig1c <- nodeDegree_DF %>% filter(manufacturer == manufacturers[i]) %>%                  # Filter only one firm     
  ggplot(aes(x = node_degree, y = count, colour = manufacturer, fill = manufacturer)) +  # Initialise ggplot
  geom_point(position = position_jitter(0.2,0.1), shape = 21, size = 2.5,                # Use jittering to reduce overplotting
             colour ="black", alpha = 0.8, stroke = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +                                  # Fitted trend line
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 3),                 # 10^x formatted log-scale
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 3), 
                labels = trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(side = "lbrt", colour = "grey") +                                  # Log-ticks 
  scale_colour_manual(values = c(man_colours[i])) +                                      # Colours
  scale_fill_manual(values = c(man_colours[i])) +
  facet_wrap(~manufacturer) +                                                            # Use facet to get plot label, e.g. 'Hyundai'
  labs(x = "Node degree k", y = "P(k)") +
  theme_bw() + theme(panel.grid = element_blank(),text = element_text(size = 17),        # Theme details
                     legend.position = "none")

# Save the plot to working directory
ggsave(filename = paste("Fig1C_", manufacturers[i], ".pdf", sep = ""), plot = fig1c)
}

#%%%%%%%%%%%%%%%%%%%%%
# --- Fig 2A Inset ---
#%%%%%%%%%%%%%%%%%%%%%

# Filter only Volkswagen
vw <- df %>% filter(manufacturer == "Volkswagen") %>% mutate(quantile_rank = ntile(node_degree,10))

#---
# Colour range

# Get the number of unique sentiment values (necessary to get number of different colours)
unique_sentiments <- nrow(vw %>% group_by(sentiment) %>% summarise(n()))

# Use RColorBrewer to get a range of red-yellow-green colours
col_range <- brewer.pal(9, "RdYlGn")
col_range <- colorRampPalette(col_range)
cols <- col_range(unique_sentiments)

fig2A_inset <- vw %>% 
  ggplot(aes(x = quantile_rank, y = sentiment, group = quantile_rank, colour = factor(sentiment))) + # Initialise; use factor(sentiment) to colour according to col_range
  geom_point(position = position_jitter(0.2,0.05), alpha = 0.4) +                                    # Jitter points to reduce overplotting
  geom_boxplot(fill = "NA", width = 0.5, colour = "black", outlier.alpha = 0,coef = 0.5) +           # Boxplots around points
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), group = 1, colour = "black") +       # 3rd degree polynomial trend line
  scale_colour_manual(values = cols) +                                                               # Use colour range
  scale_x_continuous(breaks = c(1,3,5,7,9), labels = c("10 %","30 %","50 %","70 %","90 %")) +        # Set label breaks
  labs(x = "Node degree decile", y = "Sentiment") +
  theme_bw() + 
  theme(legend.position = "none", text =element_text(size = 17), panel.grid = element_blank())

ggsave(filename = paste("Fig2A_inset", ".pdf", sep = ""), plot = fig2A_inset)

#%%%%%%%%%%%%%%%
# --- Fig 2B ---
#%%%%%%%%%%%%%%%

# Rename Topics 
df$Topic <- ifelse(df$Topic == "connectivity", "Connectivity &\n Sharing",
                   ifelse(df$Topic == "ai", "Autonomous\nDriving & AI", 
                          ifelse(df$Topic == "emobility", "E-Mobility &\n Environment", "Other")))

# Keep just relevant variables
df2 <- df %>% dplyr::select(manufacturer, normalized.pagerank, sentiment, Topic)

# Use dplyr::gather() to transform from wide to long format (normalized.pagerank and sentiment in column 'key')
df2 <- gather(df2, key, value, - manufacturer, - Topic)

# Rename variable names
df2$key <- ifelse(df2$key == "normalized.pagerank", "Normalized PageRank", "Sentiment")

# Set alpha value for "E-Mobility and Environment": there are so many points that we need to reduce overplotting
df2$alpha <- ifelse(df2$Topic %in% c("E-Mobility & Environment","Other") , 0.2, 0.8)

# Data Frame with mean values for labels (We use % for readibility)
means <- df2 %>% group_by(key, manufacturer, Topic) %>% summarise(mean_val = paste(round(mean(value),2)*100, "%"))

fig2B <- df2 %>% 
  ggplot(aes(y = value, x = Topic, fill = Topic)) + facet_grid(manufacturer~key, scales = "free_y") +       # Initialise facet-grid plot
  geom_point(position = position_jitter(0.15), aes(alpha = alpha), shape = 21, stroke = 0.2, size = 2) +    # Jittered points to reduce overplotting
  geom_boxplot(fill = NA, position = position_dodge(), width = 0.15, colour = "black", outlier.alpha = 0) + # Small boxplots around points
  scale_colour_manual(values = c(brewer.pal(9,"BuPu")[8],brewer.pal(9,"BuPu")[5],                           # Set colours
                                 brewer.pal(9,"PuBu")[6], brewer.pal(9,"PuBu")[8])) +
  scale_fill_manual(values = c(brewer.pal(9,"BuPu")[8],brewer.pal(9,"BuPu")[5], 
                               brewer.pal(9,"PuBu")[6], brewer.pal(9,"PuBu")[8])) +
  labs(x = "", y = "Norm. Page Rank / Sentiment") +
  geom_label_repel(data = means, aes(y = 1, colour = Topic, label = mean_val), fill = "white",              # Labels with mean values at the bottom
                   segment.alpha = 0, size = 2.75, nudge_y = -2, segment.size = 0) +                        # No arrow
  theme_bw() + 
  theme(legend.position = "none", text =element_text(size = 15),
        axis.text.x = element_text(size = 11, angle = 90, vjust = 0.6))

ggsave(filename = paste("Fig2B", ".pdf", sep = ""), plot = fig2B)

#%%%%%%%%%%%%%%%
# --- Fig 2C ---
#%%%%%%%%%%%%%%%

# Read sales data
sales <- read.csv(paste(getwd(), "/car_sales.csv", sep =""), dec = ",")
 
# Calculate share of web pages per manufacturer per country 
sales <- sales %>% group_by(manufacturer) %>% mutate(share = pages / sum(pages))

# New DF for clustering: calculate log-10 of sales; apply clustering with 3 groups and join together
sales_cluster <- data.frame(sales %>% filter(manufacturer == "Volkswagen") %>% select(country, sales) %>% mutate(log_sales = log10(sales)))
sales_cluster <- data.frame(country = sales_cluster$country, cluster = kmeans(sales_cluster[,c("log_sales")],3)$cluster)

# Make sure that cluster have the correct order (i.e low-size market == 1, medium ==2, high == 3)
sales_cluster$cluster <- ifelse(sales_cluster$cluster == sales_cluster$cluster[sales_cluster$country == "Madagascar"], 1,
       ifelse(sales_cluster$cluster == sales_cluster$cluster[sales_cluster$country == "Peru"], 2, 3))

# Merge cluster-DF with sales-DF
sales <- merge(sales, sales_cluster, by = "country")
# The home market of each country gets its own "cluster"
sales$cluster <- ifelse(sales$manufacturer == "Volkswagen" & sales$country == "Germany", 4, 
                     ifelse(sales$manufacturer == "Hyundai" & sales$country == "Korea", 4, 
                            ifelse(sales$manufacturer == "Toyota" & sales$country == "Japan", 4, sales$cluster)))

# Calculate means for plotting labels with share of web pages per cluster (x = mean_sales, y = mean_share)
means <- sales %>% group_by(manufacturer, cluster) %>% summarise(pages = sum(pages), sales_m = mean(sales), pages_m = mean(share))
# To calculate share of pages we need to get the number of pages per manufacturer...
man_means <- sales %>% group_by(manufacturer) %>% summarise(total_pages = sum(pages))
# ...and merge it with the means DF
means <- merge(means, man_means, by = "manufacturer")
# Then we can calculate the share
means <- means %>% mutate(share = paste(round(pages / total_pages,2)*100, "%"))

# Plot
fig2D<- sales %>%
  ggplot(aes(y = share, x = sales,colour = factor(cluster), fill = factor(cluster))) +   # Initialise the plot
  geom_point(position = position_jitter(0.1,0), size = 3,shape = 21,                     # Slightly jittered points
             aes(fill = factor(cluster)), alpha = 0.9, stroke = 0.2, colour = "black") + 
  stat_ellipse(type = "t", linetype = 2) +                                               # Draw ellipse around data points
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),                        # Log scale on x-axis
                labels = trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(side = "bt", colour = "grey", size = 0.5) +                        # With log-tick
  geom_text_repel(aes(label = country), box.padding   = 0, point.padding = 0.5,          # Add country names
                  size = 2.5, segment.alpha = 0.5) +
  geom_label_repel(data = means, aes(x = sales_m, y = pages_m, label = share),           # Add labels at the bottom
                   fill = "white", segment.alpha = 0, nudge_y = -2,segment.size = 0) +
  facet_wrap(~ manufacturer, nrow = 3) +                                                 # faceted plot per manufacturer
  labs(x = "Market size (total sales)", y = "Share of Web pages") +                      # Label names
  scale_fill_manual(values = c(brewer.pal(9,"BuPu")[8],brewer.pal(9,"BuPu")[5],          # Colours
                               brewer.pal(9,"PuBu")[6],"#002147")) +
  scale_colour_manual(values = c(brewer.pal(9,"BuPu")[8],brewer.pal(9,"BuPu")[5], 
                                 brewer.pal(9,"PuBu")[6],"#002147")) +
  theme_bw() + 
  theme(legend.position = "none", text = element_text(size = 15),
        panel.grid.minor = element_blank(),panel.grid.major = element_blank())

ggsave(filename = paste("Fig2D", ".pdf", sep = ""), plot = fig2D)