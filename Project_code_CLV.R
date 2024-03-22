getwd()
#setwd("/Users/jayeshzambre/Documents/Study/Second Quarter/Marketing Analytics/JIBU Project/INTERNAL RESOURCES/DATA FILES")
setwd("C:/Users/harsh/Documents/All Documents/Harshada documents/SantaClara University/Marketing Analytics 2505/Jibu")
library(readxl)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(caret)
library(RColorBrewer)

data <- read_excel("Franchise Health Data Input Form 2023 2024-02-08.xlsx")
mydata <- clean_names(data)
mydata$Date <- as.Date(paste(mydata$month, "28, 2022"), format="%B %d, %Y")

data <- read_excel("BOJ_consolidated.xlsx")
mydata2 <- clean_names(data)

data <- read_excel("Meter_reading_data.xlsx")
mydata3 <- clean_names(data)
mydata3<-mydata3[,c("franchise_name","franchise_id","country_id","launch_year",
                   "year","mm_yy","liters_produced","liters_produced_per_day")]


# **************************************************************** EDA **************************************************************************

# *************** Meter Reading ******************

meter_data_v1<- mydata3%>%filter(liters_produced_per_day > 0,
                                 liters_produced > 0,
                                 year %in% c(2021, 2022, 2023),
                                 country_id %in% c('KE','RW','UG','DRCG'))


# Reorder the levels of the country_id factor based on total liters produced in descending order
meter_data_v1 <- transform(meter_data_v1, country_id = reorder(country_id, -liters_produced))

summary(meter_data_v1)
library(knitr)
# Displaying summary as a table
kable(summary(meter_data_v1))


franchise_counts <- meter_data_v1 %>%
  group_by(country_id) %>%
  summarise(distinct_franchise_count = n_distinct(franchise_id))

ggplot(franchise_counts, aes(x = "", y = distinct_franchise_count, fill = country_id)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = distinct_franchise_count), position = position_stack(vjust = 0.5)) + 
  coord_polar("y", start = 0) +  
  labs(title = "Distinct Franchise Count by Country",
       fill = "Country ID",
       x = NULL,
       y = NULL) +
  scale_fill_brewer(palette = "Set3") +  
  theme_void()

ggplot(meter_data_v1, aes(x = country_id, y = liters_produced, fill = country_id)) +
  geom_bar(stat = "identity") +
  labs(title = "Liters Produced by Country ID", x = "Country ID", y = "Liters Produced") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")



# Aggregate data by country_id and franchise_id, calculate total liters produced
top_franchises <- meter_data_v1 %>%
  group_by(country_id, franchise_id) %>%
  summarise(total_liters_produced = sum(liters_produced)) %>%
  arrange(country_id, desc(total_liters_produced)) %>%
  group_by(country_id) %>%
  top_n(5)


ggplot(top_franchises, aes(x = fct_reorder(franchise_id, -total_liters_produced), y = total_liters_produced, fill = country_id)) +
  geom_bar(stat = "identity") +
  labs(title = "Liters Produced by Top 5 Franchises (by franchise_id) for Each Country", x = "Franchise ID", y = "Total Liters Produced") +
  facet_wrap(~country_id, scales = "free", ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


# Aggregate data by country_id and franchise_id, calculate total liters produced per day
top_franchises_per_day <- meter_data_v1 %>%
  group_by(country_id, franchise_id) %>%
  summarise(total_liters_produced_per_day = sum(liters_produced_per_day)) %>%
  arrange(country_id, desc(total_liters_produced_per_day)) %>%
  group_by(country_id) %>%
  top_n(5)


ggplot(top_franchises_per_day, aes(x = fct_reorder(franchise_id, -total_liters_produced_per_day), y = total_liters_produced_per_day, fill = country_id)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country_id, scales = "free", ncol = 2) +
  labs(title = "Liters Produced per Day by Top 5 Franchises (by franchise_id) for Each Country", x = "Franchise ID", y = "Total Liters Produced per Day") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


# ****************** Health Data ********************
options(scipen = 999)

mydata <- mydata %>%
  rename(
    hygiene_audit_avg = franchise_hygiene_audit_score_months_average,
    p_q_audit_curr = p_q_audit_score_current_month,
    active_retailers = total_number_of_active_customers_purchased_atleast_once_this_month_retailers_number_of_customers,
    monthly_sales = total_sales_made_by_the_franchise_this_month_in_your_market_currency,
    profit_loss = profit_loss_position_for_this_month,
    Falied_water_test = number_of_water_samples_that_have_failed_microbiology_current_month,
    total_exp_fran = total_expenses_at_the_franchise_this_month_in_your_market_currency,
    liters_target = targets_achievement_average_liters_per_day_for_the_month_target,
    liters_achived = targets_achievement_average_liters_per_day_for_the_month_actual,
    lpg_target = targets_achievement_lpg_target_purchases_achieved_for_the_month_target,
    lpg_achived = targets_achievement_lpg_target_purchases_achieved_for_the_month_actual,
    porridge_target = targets_achievement_porridge_target_purchases_achieved_for_the_month_target,
    porridge_achived = targets_achievement_porridge_target_purchases_achieved_for_the_month_actual
  )

mydata$revenue_USD <- with(mydata, case_when(
  country == "Uganda" ~ monthly_sales * 0.00026,
  country == "Rwanda" ~ monthly_sales * 0.00078,
  country == "Kenya" ~ monthly_sales * 0.0074,
  country == "Goma" ~ monthly_sales * 0.00036,
  TRUE ~ monthly_sales # default case if country does not match any above
))

mydata$expense_USD <- with(mydata, case_when(
  country == "Uganda" ~ total_exp_fran * 0.00026,
  country == "Rwanda" ~ total_exp_fran * 0.00078,
  country == "Kenya" ~ total_exp_fran * 0.0074,
  country == "Goma" ~ total_exp_fran * 0.00036,
  TRUE ~ total_exp_fran # default case if country does not match any above
))

mydata$pnl_USD <- with(mydata, case_when(
  country == "Uganda" ~ profit_loss * 0.00026,
  country == "Rwanda" ~ profit_loss * 0.00078,
  country == "Kenya" ~ profit_loss * 0.0074,
  country == "Goma" ~ profit_loss * 0.00036,
  TRUE ~ profit_loss # default case if country does not match any above
))

health_data <- subset(mydata, revenue_USD != 0)



health_data <- health_data %>%
  group_by(franchise_id, Date) %>%
  arrange(desc(submitted_on)) %>%  
  slice(1)

# Calculate total monthly revenue by country
monthly_revenue_by_country <- health_data %>%
  group_by(country, month) %>%
  summarise(Total_Revenue = sum(revenue_USD, na.rm = TRUE))

# Define the order of months
month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# Convert month to factor with the defined order
monthly_revenue_by_country <- monthly_revenue_by_country %>%
  mutate(month = factor(month, levels = month_order))

# Plot total monthly revenue as a bar plot, faceted by country
ggplot(monthly_revenue_by_country, aes(x = month, y = Total_Revenue, fill = country)) +
  geom_col() +
  facet_wrap(~ country, scales = "free_y") +
  labs(title = "Total Monthly Revenue by Country", x = "Month", y = "Total Revenue") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")


# Calculate total monthly expenses by country
monthly_expenses_by_country <- health_data %>%
  group_by(country, month) %>%
  summarise(Total_Expenses = sum(expense_USD, na.rm = TRUE))

monthly_expenses_by_country <- monthly_expenses_by_country %>%
  mutate(month = factor(month, levels = month_order))

# Plot total monthly expenses as a bar plot, faceted by country
ggplot(monthly_expenses_by_country, aes(x = month, y = Total_Expenses, fill = country)) +
  geom_col() +
  facet_wrap(~ country, scales = "free_y") +
  labs(title = "Total Monthly Expenses by Country", x = "Month", y = "Total Expenses") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")


monthly_pnl_by_country <- health_data %>%
  group_by(country, month) %>%
  summarise(Total_PnL = sum(pnl_USD, na.rm = TRUE)) %>%
  mutate(month = factor(month, levels = month_order))  



# Plot total monthly P&L as a bar plot, faceted by country
ggplot(monthly_pnl_by_country, aes(x = month, y = Total_PnL, fill = country)) +
  geom_col() +
  facet_wrap(~ country, scales = "free_y") +
  labs(title = "Total Monthly P&L by Country", x = "Month", y = "Total P&L") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")


profit_by_franchise <- health_data %>%
  group_by(country, franchise_id) %>%
  summarise(Total_Profit = sum(pnl_USD, na.rm = TRUE), .groups = 'drop') %>%
  arrange(country, desc(Total_Profit))

# Filter the dataset for only Kenya
kenya_profits <- profit_by_franchise %>%
  filter(country == "Kenya") %>%
  top_n(10, Total_Profit)

# Filter the dataset for only Goma
goma_profits <- profit_by_franchise %>%
  filter(country == "DRC Goma") %>%
  top_n(10, Total_Profit)

# Filter the dataset for only Rwanda
Rwanda_profits <- profit_by_franchise %>%
  filter(country == "Rwanda") %>%
  top_n(10, Total_Profit)


# Filter the dataset for only Uganda
Uganda_profits <- profit_by_franchise %>%
  filter(country == "Uganda") %>%
  top_n(10, Total_Profit)



# Plot the results for Kenya
ggplot(kenya_profits, aes(x = reorder(franchise_id, -Total_Profit), y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "#FFFFB3") +
  labs(title = "Top 10 Franchises by Profit in Kenya", x = "Franchise Name", y = "Total Profit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white")) +
  scale_fill_manual(values = "#FFFFB3")

# Plot the results for Goma
ggplot(goma_profits, aes(x = reorder(franchise_id, -Total_Profit), y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "#8DD3C7") +
  labs(title = "Top 10 Franchises by Profit in Goma", x = "Franchise Name", y = "Total Profit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(fill = "white")) +
  scale_fill_manual(values = "#8DD3C7")  

# Plot the results for Rwanda
ggplot(Rwanda_profits, aes(x = reorder(franchise_id, -Total_Profit), y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "#BEBADA") +
  labs(title = "Top 10 Franchises by Profit in Rwanda", x = "Franchise Name", y = "Total Profit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(fill = "white")) +
  scale_fill_manual(values = "#BEBADA")  

# Plot the results for Uganda
ggplot(Uganda_profits, aes(x = reorder(franchise_id, -Total_Profit), y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "#FB8072") +
  labs(title = "Top 10 Franchises by Profit in Uganda", x = "Franchise Name", y = "Total Profit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.background = element_rect(fill = "white")) +
  scale_fill_manual(values = "#FB8072")  



# Convert month to factor with correct order
health_data$month <- factor(health_data$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# Bar plot for Total Number of Sales Staff (Full Time)
ggplot(health_data, aes(x = month, y = active_retailers)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(title = 'Total Number of Full-Time Sales Staff by Month', x = 'Month', y = 'Number of Full-Time Sales Staff') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_rect(fill = "white"))



active_customers_summary <- health_data %>%     
  group_by(country) %>% 
  summarise(
    Active_Retailers = sum(active_retailers, na.rm = TRUE),
    Active_Households = sum(total_number_of_active_customers_purchased_atleast_once_this_month_households_number_of_customers, na.rm = TRUE),
    Active_Institutions = sum(total_number_of_active_customers_purchased_atleast_once_this_month_businesses_institutions_number_of_customers, na.rm = TRUE),
    .groups = 'drop' 
  ) 

active_customers_summary_total <- active_customers_summary %>%
  group_by(country) %>% 
  summarise(Total_active_customers = (Active_Retailers+Active_Households+Active_Institutions))

active_customers_long <- active_customers_summary %>% 
  pivot_longer(
    cols = c(Active_Retailers, Active_Households, Active_Institutions),
    names_to = "Category",
    values_to = "Count"
  )

ggplot(active_customers_long, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, color = "black", size = 3) +
  facet_wrap(~ country, scales = "free_y") + # Free scales for each facet
  labs(title = "Active Customers Comparison by Country", x = "Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme_minimal()




lpd_summary <- health_data %>%
  group_by(Date, country) %>%
  summarise(
    target = mean(liters_target, na.rm = TRUE),
    actual = mean(liters_achived, na.rm = TRUE),
    .groups = 'drop' # This will drop the grouping
  ) %>%
  mutate(
    target = if_else(country %in% c("Kenya", "Goma"), target * 1000, target),
    actual = if_else(country %in% c("Kenya", "Goma"), actual * 1000, actual)
  )


# Now, you can plot without reshaping the data into long format:
ggplot(lpd_summary, aes(x = Date)) +
  geom_line(aes(y = target, color = "Target")) +
  geom_line(aes(y = actual, color = "Actual")) +
  facet_wrap(~country, scales = "free_y", ncol = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Target vs. Actual Liters per Day by Country",
       x = "Date",
       y = "Liters") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank()) # Hide the legend title



# Reshape the data to long format for ggplot
lpd_long <- lpd_summary %>%
  pivot_longer(cols = c(target, actual),
               names_to = "Type",
               values_to = "Liters")



# Assuming your data is already summarized and stored in bottle_summary
bottle_summary <- health_data %>%
  group_by(country, franchise_id) %>%
  summarise(
    New_18_9L_Jumbo = sum(total_number_of_new_bottles_sold_this_month_18_9l_bottle_jumbos_number_of_bottles, na.rm = TRUE),
    New_20L_Tap = sum(total_number_of_new_bottles_sold_this_month_20l_tap_bottle_number_of_bottles, na.rm = TRUE),
    Refill_18_9L_Jumbo = sum(total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_18_9l_bottle_jumbos_number_of_bottles, na.rm = TRUE),
    Refill_20L_Tap = sum(total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_20l_tap_bottle_number_of_bottles, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Total_Bottles = New_18_9L_Jumbo + New_20L_Tap + Refill_18_9L_Jumbo + Refill_20L_Tap) %>%
  arrange(desc(Total_Bottles)) %>%
  group_by(country) %>%
  slice_max(order_by = Total_Bottles, n = 5) %>%
  ungroup()

# Melt the data for ggplot
bottle_summary_melted <- bottle_summary %>%
  pivot_longer(cols = c(New_18_9L_Jumbo, New_20L_Tap, Refill_18_9L_Jumbo, Refill_20L_Tap), 
               names_to = "Bottle_Type", values_to = "Count")

# Generate the plot
ggplot(bottle_summary_melted, aes(x = reorder(franchise_id, -Count), y = Count, fill = Bottle_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ country, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("#BEBADA", "#FFFFB3", "#8DD3C7", "#FB8072")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1),
    axis.ticks.length = unit(0.3, "cm"),
    strip.background = element_rect(fill = "lightblue"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  labs(title = "Top 5 Franchises by Bottles Sold in Each Country", 
       x = "Franchise Name", 
       y = "Bottle Count")


percent_health_data <- health_data %>%
  group_by(country) %>%
  summarize(Porridge_Percentage = ifelse(sum(porridge_target, na.rm = TRUE) == 0, 0,
                                         (sum(porridge_achived, na.rm = TRUE) / sum(porridge_target, na.rm = TRUE)) * 100),
            LPG_Percentage = ifelse(sum(lpg_target, na.rm = TRUE) == 0, 0,
                                    (sum(lpg_achived, na.rm = TRUE) / sum(lpg_target, na.rm = TRUE)) * 100),
            Liters_Percentage = ifelse(sum(liters_target, na.rm = TRUE) == 0, 0,
                                       (sum(liters_achived, na.rm = TRUE) / sum(liters_target, na.rm = TRUE)) * 100))


# Plot 1: Porridge percentage achieved/target
ggplot(percent_health_data, aes(x = country, y = Porridge_Percentage, fill = country)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = paste0(round(Porridge_Percentage, 2), "%")), vjust = -0.5) + 
  labs(x = "Country", y = "Porridge Percentage Achieved/Target", title = "Porridge Percentage Achieved/Target by Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Plot 2: LPG percentage achieved/target
ggplot(percent_health_data, aes(x = country, y = LPG_Percentage, fill = country)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = paste0(round(LPG_Percentage, 2), "%")), vjust = -0.5) +  
  labs(x = "Country", y = "LPG Percentage Achieved/Target", title = "LPG Percentage Achieved/Target by Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Plot 3: Liters percentage achieved/target
ggplot(percent_health_data, aes(x = country, y = Liters_Percentage, fill = country)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = paste0(round(Liters_Percentage, 2), "%")), vjust = -0.5) + 
  labs(x = "Country", y = "Liters Percentage Achieved/Target", title = "Liters Percentage Achieved/Target by Country") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")



# *************** Consolidated BOJ ******************


# Assuming mydata2 is your dataframe
split_values <- strsplit(as.character(mydata2$m_water_site_id_name), " ")

# Extract franchise_id and franchise_location
mydata2$franchise_id <- sapply(split_values, function(x) x[1])

BoJ_selected <- clean_names(mydata2)


ggplot(BoJ_selected, aes(x = country, y = `age_of_franchise_in_months`)) +
  geom_boxplot(aes(fill = country)) +
  labs(title = "Age of Franchise Distribution by Country",
       x = "Country",
       y = "Age of Franchise (in months)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")



# Summarize both mean BoJ total score and mean quality by country
summary_data_boj <- BoJ_selected %>%
  group_by(country) %>%
  summarize(mean_boj_total_score = mean(boj_total_score, na.rm = TRUE),
            mean_quality = mean(p_1_quality, na.rm = TRUE),
            mean_customer_experience = mean(p_3_customer_experience, na.rm = TRUE))

# Plotting
ggplot(summary_data_boj, aes(x = country, y = mean_boj_total_score, fill = country)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(mean_boj_total_score,2)), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Mean BoJ Total Score by Country",
       x = "Country", y = "Mean BoJ Total Score") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# Plotting
ggplot(summary_data_boj, aes(x = country, y = mean_quality, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Quality Score by Country",
       x = "Country", y = "Mean Quality Score") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()


# Plotting
ggplot(summary_data_boj, aes(x = country, y = mean_customer_experience, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Customer Experience by Country",
       x = "Country", y = "Mean customer Experience") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()



# ************************************** Customer Segmentation and CLV calculations ***********************************************

max_date <- max(meter_data_v1$mm_yy)

# Defining Present date as the maximum date
present <- as.Date(max_date)


rfm_meter <- meter_data_v1 %>%
  group_by(franchise_id) %>%
  summarise(
    Recency = as.numeric(difftime(present, max(mm_yy), units = "days")),
    Frequency = n(),
    MonetaryValue = round(sum(liters_produced),1)
  )

library(e1071) 

check_skew <- function(df, column) {
  skew <- skewness(df[[column]], na.rm = TRUE)
  cat(paste(column, "'s: Skew: ", sprintf("%.2f", skew), "\n"))
  
  # Distribution plot
  ggplot(df, aes_string(x = column)) +
    geom_density(fill = "blue", alpha = 0.7) +
    ggtitle(paste("Distribution of ", column)) +
    theme_minimal()
}

check_skew(rfm_meter, "Recency")
check_skew(rfm_meter, "Frequency")
check_skew(rfm_meter, "MonetaryValue")


# Copy the dataframe to a new one
rfm_log <- rfm_meter

# Assuming rfm is your dataframe
rfm_log$log_Recency <- log(rfm_log$Recency + 1) 
rfm_log$log_Frequency <- log(rfm_log$Frequency + 1)
rfm_log$log_MonetaryValue <- log(rfm_log$MonetaryValue + 1) 

# Plot all 3 graphs together for distribution summary findings after log transformation
#par(mfrow=c(3,1), mar=c(4, 4, 2, 2))
check_skew(rfm_log, "log_Recency")
check_skew(rfm_log, "log_Frequency")
check_skew(rfm_log, "log_MonetaryValue")

rfm_log_v1 <- rfm_log[c("log_Recency", "log_Frequency", "log_MonetaryValue")]


# Scale the data
scaled_data <- scale(rfm_log_v1)

# Convert the scaled matrix back to a dataframe
df_rfm_normal <- as.data.frame(scaled_data)


columns_for_clustering <- df_rfm_normal[c("log_Recency", "log_Frequency", "log_MonetaryValue")]

fviz_nbclust(columns_for_clustering,kmeans,method="silhouette")



fviz_nbclust(columns_for_clustering,kmeans,method="wss")

library(cluster)
library(factoextra)
library(fpc)

#kmeans cluster analysis
set.seed(123)
kmeans_model<-kmeans(columns_for_clustering,8,nstart=25)

# Extract cluster labels
cluster_labels <- kmeans_model$cluster

kmeans_model$betweenss/kmeans_model$totss

#looking at the center of the variables
kmeans_model$centers
data_centers<-as.data.frame(kmeans_model$centers)
data_centers$cluster<-c(1,2)

#visualizing clusters
fviz_cluster(kmeans_model,columns_for_clustering) + theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )


# Create a cluster label column in the original dataset
df_clust_data <- cbind(rfm_meter, K_Cluster = cluster_labels)

df_clust_data_v1 <- df_clust_data %>%
  group_by(K_Cluster) %>%
  summarise(
    Avg_Recency = mean(Recency, na.rm = TRUE),
    Avg_Frequency = mean(Frequency, na.rm = TRUE),
    Avg_MonetaryValue = mean(MonetaryValue, na.rm = TRUE),
    count = n()
  ) %>%
  ungroup()


health_clust_data <- merge(df_clust_data, health_data, by = "franchise_id")
colnames(health_clust_data)

health_clust_data_v1 <- health_clust_data %>%
  group_by(K_Cluster) %>%
  summarise(
    Avg_Recency = mean(Recency, na.rm = TRUE),
    Avg_Frequency = mean(Frequency, na.rm = TRUE),
    Avg_MonetaryValue = mean(MonetaryValue, na.rm = TRUE),
    Avg_Avg_Revenue = mean(revenue_USD, na.rm = TRUE),
    Avg_Avg_Expense = mean(expense_USD, na.rm = TRUE),
    Avg_Tot_Revenue = mean(sum(revenue_USD), na.rm = TRUE),
    Avg_Tot_Expense = mean(sum(expense_USD), na.rm = TRUE)
  ) %>%
  ungroup()


mydata4 <- meter_data_v1 %>% 
  arrange(mm_yy)

mydata4 <- mydata4 %>%
  group_by(franchise_id) %>%
  mutate(min_date = min(mm_yy),
         difference_in_months = (year(mm_yy) - year(min_date)) * 12 + (month(mm_yy) - month(min_date)),
         bins = ifelse(difference_in_months < 12, 0, 
                                     ifelse(difference_in_months >= 12 & difference_in_months < 24, 1,
                                            ifelse(difference_in_months >= 24 & difference_in_months < 36, 2, NA))))


mydata5 <- mydata4 %>%
  group_by(franchise_id, bins) %>%
  summarise(CLV = sum(liters_produced) / (1.1)^first(bins)) %>%
  group_by(franchise_id) %>%
  summarise(CLV = sum(CLV))

clust_clv <- merge(df_clust_data, mydata5, by = "franchise_id")
df_health <- merge(health_data, mydata5, by = "franchise_id")

df_clust_clv <- merge(health_clust_data, mydata5, by = "franchise_id")

clust_clv_data_v1 <- df_clust_clv %>%
  group_by(K_Cluster) %>%
  summarise(
    Avg_Recency = mean(Recency, na.rm = TRUE),
    Avg_Frequency = mean(Frequency, na.rm = TRUE),
    Avg_MonetaryValue = mean(MonetaryValue, na.rm = TRUE),
    Avg_Avg_Revenue = mean(revenue_USD, na.rm = TRUE),
    Avg_Avg_Expense = mean(expense_USD, na.rm = TRUE),
    Avg_Tot_Revenue = mean(sum(revenue_USD), na.rm = TRUE),
    Avg_Tot_Expense = mean(sum(expense_USD), na.rm = TRUE),
    Avg_CLV = mean(CLV, na.rm = TRUE),
    Avg_Tot_CLV = mean(sum(CLV), na.rm = TRUE)
  ) %>%
  ungroup()


df_health <- merge(df_health, BoJ_selected, by = "franchise_id")

write.csv(df_health, "df_health.csv", row.names = FALSE)

numeric_df_health <- df_health %>%
  select(where(is.numeric))

#Regression
model <- lm(CLV ~ revenue_USD + 
              active_retailers + 
              total_number_of_new_bottles_sold_this_month_18_9l_bottle_jumbos_number_of_bottles + 
              total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_20l_tap_bottle_number_of_bottles + 
              total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_18_9l_bottle_jumbos_number_of_bottles + 
              total_number_of_refill_bottles_sold_this_month_number_of_20l_18_9l_jumbo_bottles_20l_jerricans_number_of_bottles + 
              p_2_sales +
              p_3_customer_experience +
              boj_total_score
            , data = df_health)

# Summarize the model
summary(model)



