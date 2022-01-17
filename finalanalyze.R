len = 12

## Defining function to be used later

getmode <- function(v) {
  unique_list <- unique(v)
  matched_list <- match(v, unique_list)
  tab_list <- tabulate(matched_list)
  sort_tablist <- head(sort(tab_list, decreasing = TRUE), topnum)
  
  for (tab_element in sort_tablist){
    position_of_element <- match(tab_element,tab_list)
    print(unique_list[position_of_element])
    tab_list[position_of_element] = -1
  }
}

##Data cleaning control
n_rows_neg_zero_list <- vector(mode = "list", length = len)
rows_removed_all_list <- vector(mode = "list", length = len)
rows_removed_members_list <- vector(mode = "list", length = len)
rows_removed_casual_list <- vector(mode = "list", length = len)

## Define variables for analize (ALL RIDERS, CASUALS AND MEMBERS)
total_trips_list <- vector(mode = "list", length = len)
sum_ride_length_list <- vector(mode = "list", length = len)
mean_total_list <- vector(mode = "list", length = len)
med_total_list <- vector(mode = "list", length = len)

## Define variables for analize (MEMBERS)

members_total_rides <- vector(mode = "list", length = len)
ride_length_members_mean <- vector(mode = "list", length = len)
ride_length_members_med <- vector(mode = "list", length = len)
ride_length_members_sum <- vector(mode = "list", length = len)
num_electric_members <- vector(mode = "list", length = len)
num_docked_members <- vector(mode = "list", length = len)
num_classic_members <- vector(mode = "list", length = len)

## Define variables for analize (Casuals)
casual_total_rides <- vector(mode = "list", length = len)
ride_length_casual_mean <- vector(mode = "list", length = len)
ride_length_casual_sum <- vector(mode = "list", length = len)
ride_length_casual_med <- vector(mode = "list", length = len)
num_electric_casual <- vector(mode = "list", length = len)
num_docked_casual <- vector(mode = "list", length = len)
num_classic_casual <- vector(mode = "list", length = len)

##Declare tables 
table_names = list("Documents/Data science Cyclistc/2020/202011-divvy-tripdata.csv",
                   "Documents/Data science Cyclistc/2020/202012-divvy-tripdata.csv",
                   "Documents/Data science Cyclistc/2021/202101-divvy-tripdata.csv",
                   "Documents/Data science Cyclistc/2021/202102-divvy-tripdata.csv",
                   "Documents/Data science Cyclistc/2021/202103-divvy-tripdata.csv",
                   "Documents/Data science Cyclistc/2021/202104-divvy-tripdata.csv",
                   "Documents/Data science Cyclistc/2021/202105-divvy-tripdata.csv",
                   "Documents/Data science Cyclistc/2021/202106-divvy-tripdata.csv",
                   "Documents/Data science Cyclistc/2021/202107-divvy-tripdata.csv",
                   "Documents/Data science Cyclistc/2021/202108-divvy-tripdata.csv",
                   "Documents/Data science Cyclistc/2021/202109-divvy-tripdata.csv",
                   "Documents/Data science Cyclistc/2021/202110-divvy-tripdata.csv")

##Create function 
func_helper <- function(data_location, index) {
  ## Open the file 
  table_name <- read_csv(data_location)
  
  ## START data cleaning 
  
  #number of trips that = 0sec or negative seconds
  n_rows_neg_zero <- nrow(table_name %>% filter(started_at >= ended_at))
  n_rows_neg_zero_list[index] <<- n_rows_neg_zero ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("number of rows", n_rows_neg_zero))
  
  ##remove trips that are negative or zero time
  filtered_table <- table_name %>% filter(started_at < ended_at)
  
  ##verify if rows were removed. should contain zero rows
  filtered_table %>% filter(started_at >= ended_at)
  
  ## ALL
  ##Calculating Ride Length in mins
  ride_length <- difftime(filtered_table$ended_at, filtered_table$started_at, units = "mins")
  #print(c("ride_length", ride_length))
  
  ## Remove Outliers: ALL 
  ## Convert to numeric (required for IQR) 
  ride_length_total_num <- as.numeric(ride_length)
  
  ## Determine Quartiles, Median for ALL Riders (member+casual) 
  first_quart_all <- quantile(ride_length_total_num, prob = c(.25) )
  third_quart_all <- quantile(ride_length_total_num, prob = c(.75) )
  
  ## Verify Quartiles, Median for ALL
  #summary(ride_length_total_num)
  
  ## Upper Limit for Outliers
  lim_up_all <- third_quart_all + 1.5 * IQR(ride_length_total_num)
  #print(c("lim_up_all", lim_up_all))
  
  ## Lower Limit for Outliers
  lim_low_all <- first_quart_all - 1.5 * IQR(ride_length_total_num)
  #print(c("lim_low_all", lim_low_all))
  
  ##Remove Outliers
  ride_length_outliers_removed <- ride_length[ride_length  <= lim_up_all & ride_length  >= lim_low_all]
  ##verify filter
  #max(ride_length_outliers_removed)
  #min(ride_length_outliers_removed)
  
  ## Number of rows removed with outliers
  rows_removed_all <- length(ride_length_total_num) - length(ride_length_outliers_removed)
  rows_removed_all_list[index] <<- rows_removed_all ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  ##print(c("rows_removed_all", rows_removed_all))
  #END data cleaning
  
  # ANALIZE TOTAL (MEMBERS AND CASUAL)
  
  ## Total trips with outliers removed
  total_trips<- length(ride_length_outliers_removed)
  total_trips_list[index] <<- total_trips ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("total_trips", total_trips))
  
  ## calculating the mean of the ride length/total ride time
  ride_length_mean <- mean(ride_length_outliers_removed) 
  #print(c("ride_length_mean", ride_length_mean))
  sum_ride_length <- sum(ride_length_outliers_removed)
  sum_ride_length_list[index] <<- sum_ride_length ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  mean_total_list[index] <<- ride_length_mean ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(mean_list)
  
  # Median for All
  ride_length_med <- median(ride_length_outliers_removed)
  med_total_list[index] <<- ride_length_med ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("med_member", med_member))
  
  # ANALYZE MEMBERS
  
  ## Filter to find the ride length of members 
  member_data <- filtered_table %>% filter(member_casual == "member")
  #view(member_data)
  
  ## calculate ride Length of members
  ride_length_members <- difftime(member_data$ended_at, member_data$started_at, units = "mins")
  
  ## add column with ride length
  member_data_ride_length <- member_data %>% mutate(Time_difference = ride_length_members)
  #view(member_data_ride_length)
  
  ## Remove Outliers: MEMBERS
  ## Convert to numeric 
  ride_length_member_num <- as.numeric(ride_length_members)
  #str(ride_length_members)
  
  ## Determine Quartiles, Median for Members
  first_quart_member <- quantile(ride_length_member_num, prob = c(.25) )
  third_quart_member <- quantile(ride_length_member_num, prob = c(.75) )
  
  ## Verify Quartiles, Median for Members
  summary(ride_length_member_num)
  
  #Upper Limit for Outliers
  lim_up_member <- third_quart_member + 1.5 * IQR(ride_length_member_num)
  #print(c("lim_up_member",lim_up_member))
  
  #Lower Limit for Outliers
  lim_low_member <- first_quart_member - 1.5 * IQR(ride_length_member_num)
  #print(c("lim_low_member", lim_low_member))
  
  ##Remove Outliers
  ride_length_member_outliers_removed <- ride_length_members[ride_length_members  <= lim_up_member & ride_length_members  >= lim_low_member]  
  #bottom limit is negative. Negatives already filtered
  ##verify filter
  #max(ride_length_member_outliers_removed)
  #min(ride_length_member_outliers_removed)
  
  ## Number of rows removed with outliers
  rows_removed_members <- length(ride_length_member_num) - length(ride_length_member_outliers_removed)
  rows_removed_members_list[index] <<- rows_removed_members ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("rows_removed_members", rows_removed_members))
  
  ## Member total trips with outliers removed
  member_total_trips<- length(ride_length_member_outliers_removed)
  members_total_rides[index] <<- member_total_trips ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("member_total_trips", member_total_trips))
  
  ## calculating the mean of the ride length/total ride time for Members
  ride_length_mem_mean <- mean(ride_length_member_outliers_removed)
  ride_length_members_mean[index] <<- ride_length_mem_mean ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("ride_length_mem_mean", ride_length_mem_mean))
  ride_length_mem_sum <- sum(ride_length_member_outliers_removed)
  ride_length_members_sum[index] <<- ride_length_mem_sum ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  
  # Median for members
  med_member <- median(ride_length_member_outliers_removed)
  ride_length_members_med[index] <<- med_member ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("med_member", med_member))
  
  ## Analyze bike type for Members
  
  ## Electric Bike
  electric_member <- member_data_ride_length %>% filter(rideable_type=="electric_bike" & Time_difference <= lim_up_member & Time_difference >= lim_low_member)
  num_members_electric <- nrow(electric_member)
  num_electric_members[index] <<- num_members_electric ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("num_members_electric", num_members_electric))
  
  ## Docked Bike
  docked_member <- member_data_ride_length %>% filter(rideable_type=="docked_bike" & Time_difference <= lim_up_member & Time_difference >= lim_low_member)
  num_members_docked <- nrow(docked_member)
  num_docked_members[index] <<- num_members_docked ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("num_members_docked", num_members_docked))
  
  ## Classic Bike
  classic_member <- member_data_ride_length %>% filter(rideable_type=="classic_bike" & Time_difference <= lim_up_member & Time_difference >= lim_low_member)
  num_members_classic <- nrow(classic_member)
  num_classic_members[index] <<- num_members_classic ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("num_members_classic", num_members_classic))
  
  # ANALYZE CASUALS 
  
  # CASUAL
  ## Filter to find the ride length of casuals
  casual_data <- filtered_table %>% filter(member_casual=="casual")
  #view(casual_data)
  
  ## calculate ride Length of members
  ride_length_casual<- difftime(casual_data$ended_at, casual_data$started_at, units = "mins")
  
  ## add column with ride length
  casual_data_ride_length <- casual_data %>% mutate(Time_difference = ride_length_casual)
  #view(casual_data_ride_length)
  
  ## Removing Outliers: Casual 
  ## Convert to numeric 
  ride_length_casual_num <- as.numeric(ride_length_casual)
  #str(ride_length_casual_num)
  
  ## Determine Quartiles, Median for Casuals
  first_quart_casual <- quantile(ride_length_casual_num, prob = c(.25) )
  third_quart_casual <- quantile(ride_length_casual_num, prob = c(.75) )
  
  ## Verify Quartiles, Median for Members
  #summary(ride_length_casual_num)
  
  #Upper Limit for Outliers
  lim_up_casual <- third_quart_casual + 1.5 * IQR(ride_length_casual_num)
  #print(c("lim_up_casual", lim_up_casual))
  
  #Lower Limit for Outliers
  lim_low_casual <- first_quart_casual - 1.5 * IQR(ride_length_casual_num)
  #print(c("lim_low_casual", lim_low_casual))
  
  ##Remove Outliers
  ride_length_casual_outliers_removed <- ride_length_casual[ride_length_casual  <= lim_up_casual & ride_length_casual  >= lim_low_casual] 
  #bottom limit is negative. Negatives already filtered
  ##verify filter
  #max(ride_length_casual_outliers_removed)
  #min(ride_length_casual_outliers_removed)
  
  ## Number of rows removed with outliers
  rows_removed_casual <- length(ride_length_casual_num) - length(ride_length_casual_outliers_removed)
  rows_removed_casual_list[index] <<- rows_removed_casual ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("rows_removed_members", rows_removed_members))
  
  ## Casual total trips with outliers removed
  casual_total_trips<- length(ride_length_casual_outliers_removed)
  casual_total_rides[index] <<- casual_total_trips ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("casual_total_trips", casual_total_trips))
  
  ## Calculate the mean of the ride length/total ride time for Casuals 
  ride_length_cas_mean <- mean(ride_length_casual_outliers_removed)
  ride_length_casual_mean[index] <<- ride_length_cas_mean ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("ride_length_mem_mean", ride_length_mem_mean))
  ride_length_cas_sum <- sum(ride_length_casual_outliers_removed)
  ride_length_casual_sum[index] <<- ride_length_cas_sum ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  
  # Median for Casual
  med_casual <- median(ride_length_casual_outliers_removed)
  ride_length_casual_med[index] <<- med_casual ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("med_membe", med_casual))
  
  ## Analize bike type for Casuals
  ## Electric Bike
  electric_casual <- casual_data_ride_length%>% filter(rideable_type=="electric_bike" & Time_difference <= lim_up_casual & Time_difference >= lim_low_casual)
  num_casuals_electric <- nrow(electric_casual)
  num_electric_casual[index] <<- num_casuals_electric ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("num_casuals_electric", num_casuals_electric))
  
  ## Docked Bike
  docked_casual <- casual_data_ride_length %>% filter(rideable_type=="docked_bike" & Time_difference <= lim_up_casual & Time_difference >= lim_low_casual)
  num_casuals_docked <- nrow(docked_casual)
  num_docked_casual[index] <<- num_casuals_docked ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("num_casuals_docked", num_casuals_docked))
  
  ## Classic Bike
  classic_casual <- casual_data_ride_length %>% filter(rideable_type=="classic_bike" & Time_difference <= lim_up_casual & Time_difference >= lim_low_casual)
  num_casuals_classic <- nrow(classic_casual)
  num_classic_casual[index] <<- num_casuals_classic ##DOUBLE ARROW FOR GLOBAL (NOT LOCAL) VERSION
  #print(c("num_casuals_classic", num_casuals_classic))
  
  ##Analyzing top 5 statrt/ end stations
  
  print("Top 5 Start stations:")
  
  start_name <- table_name %>% filter(start_station_name != '' & start_station_name != "NA")
  start_name <- start_name$start_station_name
  getmode(start_name)
  
  print("Top 5 end stations:")
  
  end_name <- table_name %>% filter(end_station_name != ''& end_station_name != "NA")
  end_name <- end_name$end_station_name
  getmode(end_name)
  
}

index = 1 
for (data_location in table_names) {
  print(data_location)
  func_helper(data_location, index)
  index = index + 1
}
print(top_start_name)

print(num_classic_casual)  

data_cleaning <- matrix(c (n_rows_neg_zero_list, rows_removed_all_list, rows_removed_members_list, rows_removed_casual_list),  ncol = 4, byrow = FALSE)
colnames(data_cleaning) <-c("n_rows_neg_zero_list", "rows_removed_all_list", "rows_removed_members_list", "rows_removed_casual_list")                    
rownames(data_cleaning) <- c("202011", "202012", "202101", "202102", "202103", "202104", "202105", "202106", "202107", "202108", "202109", "202110")
view(data_cleaning)
                      
results <- matrix(c(total_trips_list, sum_ride_length_list, mean_total_list, med_total_list, members_total_rides, ride_length_members_mean, ride_length_members_med, ride_length_members_sum, num_electric_members, num_docked_members, num_classic_members, casual_total_rides, ride_length_casual_mean, ride_length_casual_sum, ride_length_casual_med, num_electric_casual, num_docked_casual, num_classic_casual), ncol = 18, byrow = FALSE)
colnames(results) <-c("total_trips_list", "sum_ride_length_list", "mean_total_list","med_total_list", "members_total_rides", "ride_length_members_mean", "ride_length_members_med", "ride_length_members_sum", "num_electric_members", "num_docked_members", "num_classic_members",  "casual_total_rides", "ride_length_casual_mean", "ride_length_casual_sum", "ride_length_casual_med", "num_electric_casual", "num_docked_casual", "num_classic_casual")
rownames(results) <- c("202011", "202012", "202101", "202102", "202103", "202104", "202105", "202106", "202107", "202108", "202109", "202110")
print(results)
view(results)
