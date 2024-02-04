# Installing and load ggplot2 package
#install.packages("ggplot2")
library(ggplot2)

# Code 4.1 starts here
setwd("C:/Users/akunna1/Desktop/DPS_Files_Akunna") #can modify this to any directory

# Importing the CSV file into a data frame
disrep_data <- read.csv("dc_parcels_dus_20230724_discrepancy.csv")
View(disrep_data)

# Code 4.2a starts here
# Working discrep_addr_city, discrep_addr_landuse, discrep_city_landuse columns
# Removing where discrep_addr_city, discrep_addr_landuse, discrep_city_landuse = 0
# for Where du_est_city = 1
# discrep_addr_city column:
filtered_data_1 <- subset(disrep_data, du_est_city == 1 & discrep_addr_city != 0) #removing where discrep_addr_city = 0
discrep_addr_city_counts_1 <- table(filtered_data_1$discrep_addr_city)
discrep_addr_city_counts_1_df <- as.data.frame(discrep_addr_city_counts_1)
colnames(discrep_addr_city_counts_1_df) <- c("discrep_addr_city", "Frequency")
ggplot(discrep_addr_city_counts_1_df, aes(x = discrep_addr_city, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Discrep_addr_city Frequency for du_est_city = 1",
       x = "discrep_addr_city",
       y = "Frequency")
# discrep_addr_landuse column:
filtered_data_1 <- subset(disrep_data, du_est_city == 1 & discrep_addr_landuse != 0)
discrep_addr_landuse_counts_1 <- table(filtered_data_1$discrep_addr_landuse)
discrep_addr_landuse_counts_1_df <- as.data.frame(discrep_addr_landuse_counts_1)
colnames(discrep_addr_landuse_counts_1_df) <- c("discrep_addr_landuse", "Frequency")
ggplot(discrep_addr_landuse_counts_1_df, aes(x = discrep_addr_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Discrep_addr_landuse Frequency for du_est_city = 1",
       x = "discrep_addr_landuse",
       y = "Frequency")
# discrep_city_landuse column:
filtered_data_1 <- subset(disrep_data, du_est_city == 1 & discrep_city_landuse != 0)
discrep_city_landuse_counts_1 <- table(filtered_data_1$discrep_city_landuse)
discrep_city_landuse_counts_1_df <- as.data.frame(discrep_city_landuse_counts_1)
colnames(discrep_city_landuse_counts_1_df) <- c("discrep_city_landuse", "Frequency")
ggplot(discrep_city_landuse_counts_1_df, aes(x = discrep_city_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Discrep_city_landuse Frequency for du_est_city = 1",
       x = "discrep_city_landuse",
       y = "Frequency")

# Code 4.2b starts here
# Working discrep_addr_city, discrep_addr_landuse, discrep_city_landuse columns 
# for Where du_est_city = 2  
# discrep_addr_city column:
filtered_data_2 <- subset(disrep_data, du_est_city == 2 & discrep_addr_city != 0)  
discrep_addr_city_counts_2 <- table(filtered_data_2$discrep_addr_city)
discrep_addr_city_counts_2_df <- as.data.frame(discrep_addr_city_counts_2)
colnames(discrep_addr_city_counts_2_df) <- c("discrep_addr_city", "Frequency")
ggplot(discrep_addr_city_counts_2_df, aes(x = discrep_addr_city, y = Frequency)) +
  geom_bar(stat = "identity", fill = "red") +  
  labs(title = "Discrep_addr_city Frequency for du_est_city = 2",  
       x = "discrep_addr_city",
       y = "Frequency")
# discrep_addr_landuse column:
filtered_data_2 <- subset(disrep_data, du_est_city == 2 & discrep_addr_landuse != 0)
discrep_addr_landuse_counts_2 <- table(filtered_data_2$discrep_addr_landuse)
discrep_addr_landuse_counts_2_df <- as.data.frame(discrep_addr_landuse_counts_2)
colnames(discrep_addr_landuse_counts_2_df) <- c("discrep_addr_landuse", "Frequency")
ggplot(discrep_addr_landuse_counts_2_df, aes(x = discrep_addr_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Discrep_addr_landuse Frequency for du_est_city = 2",
       x = "discrep_addr_landuse",
       y = "Frequency")
# discrep_city_landuse column:
filtered_data_2 <- subset(disrep_data, du_est_city == 2 & discrep_city_landuse != 0)
discrep_city_landuse_counts_2 <- table(filtered_data_2$discrep_city_landuse)
discrep_city_landuse_counts_2_df <- as.data.frame(discrep_city_landuse_counts_2)
colnames(discrep_city_landuse_counts_2_df) <- c("discrep_city_landuse", "Frequency")
ggplot(discrep_city_landuse_counts_2_df, aes(x = discrep_city_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "red") +  
  labs(title = "Discrep_city_landuse Frequency for du_est_city = 2",  
       x = "discrep_city_landuse",
       y = "Frequency")


# Code 4.2c starts here
# Working discrep_addr_city, discrep_addr_landuse, discrep_city_landuse columns 
# for Where du_est_city = 3:
# discrep_addr_city column:
filtered_data_3 <- subset(disrep_data, du_est_city == 3 & discrep_addr_city != 0)
discrep_addr_city_counts_3 <- table(filtered_data_3$discrep_addr_city)
discrep_addr_city_counts_3_df <- as.data.frame(discrep_addr_city_counts_3)
colnames(discrep_addr_city_counts_3_df) <- c("discrep_addr_city", "Frequency")
ggplot(discrep_addr_city_counts_3_df, aes(x = discrep_addr_city, y = Frequency)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Discrep_addr_city Frequency for du_est_city = 3",
       x = "discrep_addr_city",
       y = "Frequency")
# discrep_addr_landuse column:
filtered_data_3 <- subset(disrep_data, du_est_city == 3 & discrep_addr_landuse != 0)
discrep_addr_landuse_counts_3 <- table(filtered_data_3$discrep_addr_landuse)
discrep_addr_landuse_counts_3_df <- as.data.frame(discrep_addr_landuse_counts_3)
colnames(discrep_addr_landuse_counts_3_df) <- c("discrep_addr_landuse", "Frequency")
ggplot(discrep_addr_landuse_counts_3_df, aes(x = discrep_addr_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Discrep_addr_landuse Frequency for du_est_city = 3",
       x = "discrep_addr_landuse",
       y = "Frequency")
# discrep_city_landuse column:
filtered_data_3 <- subset(disrep_data, du_est_city == 3 & discrep_city_landuse != 0)
discrep_city_landuse_counts_3 <- table(filtered_data_3$discrep_city_landuse)
discrep_city_landuse_counts_3_df <- as.data.frame(discrep_city_landuse_counts_3)
colnames(discrep_city_landuse_counts_3_df) <- c("discrep_city_landuse", "Frequency")
ggplot(discrep_city_landuse_counts_3_df, aes(x = discrep_city_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Discrep_city_landuse Frequency for du_est_city = 3",
       x = "discrep_city_landuse",
       y = "Frequency")


# Code 4.2d starts here
# Working discrep_addr_city, discrep_addr_landuse, discrep_city_landuse columns 
# for Where du_est_city = 4
# discrep_addr_city column:
filtered_data_4 <- subset(disrep_data, du_est_city == 4 & discrep_addr_city != 0)
discrep_addr_city_counts_4 <- table(filtered_data_4$discrep_addr_city)
discrep_addr_city_counts_4_df <- as.data.frame(discrep_addr_city_counts_4)
colnames(discrep_addr_city_counts_4_df) <- c("discrep_addr_city", "Frequency")
ggplot(discrep_addr_city_counts_4_df, aes(x = discrep_addr_city, y = Frequency)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Discrep_addr_city Frequency for du_est_city = 4",
       x = "discrep_addr_city",
       y = "Frequency")
# discrep_addr_landuse column:
filtered_data_4 <- subset(disrep_data, du_est_city == 4 & discrep_addr_landuse != 0)
discrep_addr_landuse_counts_4 <- table(filtered_data_4$discrep_addr_landuse)
discrep_addr_landuse_counts_4_df <- as.data.frame(discrep_addr_landuse_counts_4)
colnames(discrep_addr_landuse_counts_4_df) <- c("discrep_addr_landuse", "Frequency")
ggplot(discrep_addr_landuse_counts_4_df, aes(x = discrep_addr_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Discrep_addr_landuse Frequency for du_est_city = 4",
       x = "discrep_addr_landuse",
       y = "Frequency")
# discrep_city_landuse column:
filtered_data_4 <- subset(disrep_data, du_est_city == 4 & discrep_city_landuse != 0)
discrep_city_landuse_counts_4 <- table(filtered_data_4$discrep_city_landuse)
discrep_city_landuse_counts_4_df <- as.data.frame(discrep_city_landuse_counts_4)
colnames(discrep_city_landuse_counts_4_df) <- c("discrep_city_landuse", "Frequency")
ggplot(discrep_city_landuse_counts_4_df, aes(x = discrep_city_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Discrep_city_landuse Frequency for du_est_city = 4",
       x = "discrep_city_landuse",
       y = "Frequency")

# Code 4.2e starts here
# Working discrep_addr_city, discrep_addr_landuse, discrep_city_landuse columns 
# for Where du_est_city = 5 to 20
# discrep_addr_city column:
filtered_data_5_to_20 <- subset(disrep_data, du_est_city >= 5 & du_est_city <= 20 & discrep_addr_city != 0)
discrep_addr_city_counts_5_to_20 <- table(filtered_data_5_to_20$discrep_addr_city)
discrep_addr_city_counts_5_to_20_df <- as.data.frame(discrep_addr_city_counts_5_to_20)
colnames(discrep_addr_city_counts_5_to_20_df) <- c("discrep_addr_city", "Frequency")
ggplot(discrep_addr_city_counts_5_to_20_df, aes(x = discrep_addr_city, y = Frequency)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Discrep_addr_city Frequency for du_est_city = 5 to 20",
       x = "discrep_addr_city",
       y = "Frequency")
# discrep_addr_landuse column:
filtered_data_5_to_20 <- subset(disrep_data, du_est_city >= 5 & du_est_city <= 20 & discrep_addr_landuse != 0)
discrep_addr_landuse_counts_5_to_20 <- table(filtered_data_5_to_20$discrep_addr_landuse)
discrep_addr_landuse_counts_5_to_20_df <- as.data.frame(discrep_addr_landuse_counts_5_to_20)
colnames(discrep_addr_landuse_counts_5_to_20_df) <- c("discrep_addr_landuse", "Frequency")
ggplot(discrep_addr_landuse_counts_5_to_20_df, aes(x = discrep_addr_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Discrep_addr_landuse Frequency for du_est_city = 5 to 20",
       x = "discrep_addr_landuse",
       y = "Frequency")
# discrep_city_landuse column:
filtered_data_5_to_20 <- subset(disrep_data, du_est_city >= 5 & du_est_city <= 20 & discrep_city_landuse != 0)
discrep_city_landuse_counts_5_to_20 <- table(filtered_data_5_to_20$discrep_city_landuse)
discrep_city_landuse_counts_5_to_20_df <- as.data.frame(discrep_city_landuse_counts_5_to_20)
colnames(discrep_city_landuse_counts_5_to_20_df) <- c("discrep_city_landuse", "Frequency")
ggplot(discrep_city_landuse_counts_5_to_20_df, aes(x = discrep_city_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Discrep_city_landuse Frequency for du_est_city = 5 to 20",
       x = "discrep_city_landuse",
       y = "Frequency")

# Code 4.2f starts here
# Working discrep_addr_city, discrep_addr_landuse, discrep_city_landuse columns 
# for where du_est_city = more than 20
# discrep_addr_city column:
filtered_data_plus_20 <- subset(disrep_data, du_est_city >= 20 & discrep_addr_city != 0)
discrep_addr_city_counts_plus_20 <- table(filtered_data_plus_20$discrep_addr_city)
discrep_addr_city_counts_plus_20_df <- as.data.frame(discrep_addr_city_counts_plus_20)
colnames(discrep_addr_city_counts_plus_20_df) <- c("discrep_addr_city", "Frequency")
ggplot(discrep_addr_city_counts_plus_20_df, aes(x = discrep_addr_city, y = Frequency)) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = "Discrep_addr_city Frequency for du_est_city >= 20",
       x = "discrep_addr_city",
       y = "Frequency")
# discrep_addr_landuse column:
filtered_data_plus_20 <- subset(disrep_data, du_est_city >= 20 & discrep_addr_landuse != 0)
discrep_addr_landuse_counts_plus_20 <- table(filtered_data_plus_20$discrep_addr_landuse)
discrep_addr_landuse_counts_plus_20_df <- as.data.frame(discrep_addr_landuse_counts_plus_20)
colnames(discrep_addr_landuse_counts_plus_20_df) <- c("discrep_addr_landuse", "Frequency")
ggplot(discrep_addr_landuse_counts_plus_20_df, aes(x = discrep_addr_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = "Discrep_addr_landuse Frequency for du_est_city >= 20",
       x = "discrep_addr_landuse",
       y = "Frequency")
# discrep_city_landuse column:
filtered_data_plus_20 <- subset(disrep_data, du_est_city >= 20 & discrep_city_landuse != 0)
discrep_city_landuse_counts_plus_20 <- table(filtered_data_plus_20$discrep_city_landuse)
discrep_city_landuse_counts_plus_20_df <- as.data.frame(discrep_city_landuse_counts_plus_20)
colnames(discrep_city_landuse_counts_plus_20_df) <- c("discrep_city_landuse", "Frequency")
ggplot(discrep_city_landuse_counts_plus_20_df, aes(x = discrep_city_landuse, y = Frequency)) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = "Discrep_city_landuse Frequency for du_est_city >= 20",
       x = "discrep_city_landuse",
       y = "Frequency")
