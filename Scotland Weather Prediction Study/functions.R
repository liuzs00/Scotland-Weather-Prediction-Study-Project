# Zongsheng Liu (s2097920)
data(ghcnd_stations, package = "StatCompLab")
data(ghcnd_values, package = "StatCompLab")

library(dplyr)
library(ggplot2)
library(tidyr)
library(StatCompLab)


set.seed(20011016)

#combine ghcnd-values and ghcnd_sttions, group the m by ID
ghcnd <- left_join(ghcnd_values, ghcnd_stations, by = "ID")

#define seasonal infor
winter_months <- c(1, 2, 3, 10, 11, 12)
summer_months <- c(4, 5, 6, 7, 8, 9)

year_of_interest <- 2001
# Filter data for the year 2001
ghcnd_values_year <- ghcnd_values %>%
  filter(Year == year_of_interest)
# Compute mean temperature and total precipitation by station ID
ghcnd_values_agg <- ghcnd_values_year %>%
  group_by(ID) %>%
  summarize(mean_temp = mean((Value[Element == "TMAX"]+Value[Element == "TMIN"])/2), 
            total_precip = sum(Value[Element == "PRCP"]))
  
# group mean temperature and total precipitation by Id and also add geographic info
ghcnd_data <- left_join(ghcnd_values_agg, ghcnd_stations, by = "ID")

#select 2001 as year of interest and filter the data
ghcnd_filtered <- ghcnd_values %>%
  filter(Year == year_of_interest)
# Merge the ghcnd_filtered data with the ghcnd_stations data to get the station location information
ghcnd_merged <- ghcnd_filtered %>%
  left_join(ghcnd_stations, by = "ID")

# filter prcp data for the plots below
precip_data <- ghcnd_merged %>%
  filter(Element == "PRCP")


# Plot mean temperature and total precipitation  in 2001 as a map
plot1 <-function(){
  ggplot(ghcnd_data, aes(x = Longitude, y = Latitude)) +
    geom_point(aes(size = mean_temp, color = total_precip)) +
    scale_size_continuous(name = "Mean temperature (C)") +
    scale_color_continuous(name = "Total precipitation (mm)") +
    labs(title = "Temperature and Precipitation in 2001")
}

# Plot all temperature data points in one plot 
plot2 <- function(){
  temp_data <- ghcnd_merged %>%
    filter(Element == "TMIN" | Element == "TMAX") %>%
    pivot_wider(names_from = "Element", values_from = "Value")
  ggplot(temp_data, aes(x = DecYear, y = TMIN, group = ID)) +
    geom_line(aes(color = Name)) +
    geom_line(aes(y = TMAX, color = Name)) +
    labs(x = "Date", y = "Temperature (Celsius)") +
    ggtitle(paste("Temperature data for all stations in 2001")) +
    theme_bw()
}

# 8 subplots which grouped by ID for monthly average TMAX and TMIn
plot3 <- function(){
  ghcnd %>%
    filter(Element %in% c("TMIN", "TMAX"),Year == year_of_interest) %>%
    group_by(ID, Name, Element, Month) %>%
    summarise(Value = mean(Value), .groups = "drop") %>%
    mutate(Season = ifelse(Month %in% winter_months, "Winter", "Summer")) %>%
    mutate(Type = ifelse(Element == "TMIN" & Season == "Winter", "Winter TMIN ",  # separate data by season on the plot
                         ifelse(Element == "TMIN" & Season == "Summer", "Summer TMIN", 
                                ifelse(Element == "TMAX" & Season == "Winter", "Winter TMAX", "Summer TMAX")))) %>%
    ggplot(aes(Month, Value, colour = Type)) +
    ggtitle("Monthly Average TMAX & TMIN for All Stations in 2001") +
    geom_point() +
    facet_wrap(~ Name, ncol=2)
}


# Plot all precipitation data in 2001 for all stations in 1 plot
plot4 <- function(){
  ggplot(precip_data, aes(x = DecYear, y = Value, group = ID)) +
    geom_line(aes(color = Name)) +
    labs(x = "Date", y = "Precipitation (mm)") +
    ggtitle(paste("Precipitation data for all stations in 2001")) +
    theme_bw()
}



# 8 subplots for monthly average PRCP
plot5 <- function(){
  precip_data %>%
    filter(Element %in% "PRCP",Year == year_of_interest) %>%
    group_by(ID, Name, Element, Month) %>%
    summarise(Value = mean(Value), .groups = "drop") %>%
    mutate(Season = if_else(Month %in% winter_months, "Winter", "Summer")) %>%  #plot data in summer or winter sperately
    ggplot(aes(Month, Value, colour = Season)) +
    ggtitle("Monthly Average PRCP for all Stations in 2001") +
    geom_point() +
    facet_wrap(~ Name, ncol=2)
}

# 8 subplots for all TMAX and TMIN data whuch grouped by season
plot6 <- function(){
  ghcnd %>%
    filter(Element %in% c("TMIN", "TMAX"), Year == year_of_interest, Month %in% c(winter_months, summer_months)) %>%
    mutate(Season = ifelse(Month %in% winter_months, "Winter", "Summer")) %>%
    mutate(Type = ifelse(Element == "TMIN" & Season == "Winter", "Winter TMIN ",   # group them by season info
                         ifelse(Element == "TMIN" & Season == "Summer", "Summer TMIN", 
                                ifelse(Element == "TMAX" & Season == "Winter", "Winter TMAX", "Summer TMAX")))) %>%
    ggplot(aes(Month, Value, colour = Type)) +
    ggtitle("TMAX & TMIN in 2001") +
    geom_jitter() +
    facet_wrap(~ Name, ncol=2)
}


# 8 subplots for all TMAX and TMIN data whuch grouped by season
plot7 <- function() {
  precip_data %>%
    filter(Element == "PRCP",Year == year_of_interest) %>%
    group_by(ID, Name, Year, Month) %>%
    mutate(Season = if_else(Month %in% winter_months, "Winter", "Summer")) %>% # group them by season info
    ggplot(aes(Month, Value, color = Season)) +
    ggtitle("PRCP in 2001") +
    geom_jitter() +
    facet_wrap(~ Name, ncol=2)
}

#plot1()
#plot2()
#plot3()
#plot4()
#plot5()
#plot6()
#plot7()

# Add season information to the ghcnd_values data
ghcnd_values <- ghcnd_values %>%
  mutate(Season = ifelse(Month %in% winter_months, "Winter", "Summer"))

#Add summer column
ghcnd_values <- ghcnd_values %>%
  mutate(Summer = ifelse(Month %in% summer_months, TRUE, FALSE))

# compute T for getting p_value
test_stat <- function(data){
  winter_data <- subset(data, Month %in% c(1, 2, 3, 10, 11, 12))
  summer_data <- subset(data, Month %in% c(4, 5, 6, 7, 8, 9))
  return(abs(mean(winter_data$Value) - mean(summer_data$Value)))
}

#compute CI for p-value
p_value_CI <- function(p_value, sd_p, N, alpha){
  #case 1: p-value =0
  if (p_value==0){      
    upper <- 1- (alpha/2)^(1/N)
    CI <- c(0,upper)
  } 
  #case 2: p-value != 0
  else{
    z <- qnorm(1 - alpha/2) #compute z socre correspond to different alpha value
    lower <- p_value - z*sd_p
    upper <- p_value + z*sd_p
    CI <- c(lower, upper)
  }
  CI <- pmax(0, pmin(1, CI))
  return(CI)
}

#Construct a Monte Carlo permutation test
mc_permutation <- function(data, N, alpha){
  #N: the number of permutation would perform
  #alpha: alpha value for hypothesis testing
  #data: ghcnd joined by ghcnd_values and ghcnd_stations and grouped by ID
  p_value <- c()
  sd <- c()
  CI_l <- c()
  CI_u <- c()
  prcp_data <- data %>%  #filter out PRCP data
    filter(Element %in% "PRCP")
  # use for loop to loop over all stations
  for (i in unique(prcp_data$ID)){
    rain_s <- prcp_data %>% filter(ID==i)
    t_stats <- test_stat(data=rain_s) #compute T
    t_p <- replicate(N, 
                     {rain_s$Value <- sample(rain_s$Value)
                     test_stat(rain_s)}) #make permutation N times by replicate
    p <- mean(t_p >= t_stats) #compute p-value  by (the number of times t_p >= t_stats )/(N)
    p_value <- c(p_value,p)
    sd_p <- sqrt(p*(1-p)/N) # compute sd for p_value
    sd <- c(sd, sd_p)
    CI_p <- p_value_CI(p,sd_p,N,alpha) #construct CI
    CI_l <- c(CI_l, CI_p[1]) #get lower bound
    CI_u <- c(CI_u, CI_p[2]) #get upper bound
  }
  #form required restults as a data frame 
  test_result <- data.frame(ID = unique(prcp_data$ID), 
                            p_value = p_value,
                            standard_deviations = sd,
                            CI_lower_bound = CI_l,
                            CI_upper_bound = CI_u)
  return(test_result)
}

#run the above MC permutation test with N=1000
table1 <- mc_permutation(ghcnd, 10000, 0.05)

# make a new data frame for the model estimation tsak below which contains  ID, Longitude, Latitude, Elevation, Value_sqrt_avg, DecYear_avg
ghcnd_lm <- ghcnd_values %>%
  filter(Element == "PRCP") %>%
  group_by(ID, Year, Month) %>%
  summarise(Value_sqrt_avg = sqrt(mean(Value)),
            DecYear_avg = mean(DecYear)) %>%
  group_by(ID, Year, Month) %>%
  summarise(Value_sqrt_avg = mean(Value_sqrt_avg),
            DecYear_avg = mean(DecYear_avg)) %>%
  left_join(select(ghcnd_stations, ID, Longitude, Latitude, Elevation), by = "ID")

#define all 5 models as follow:

# Model M0
M0 <- lm(Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYear_avg, data = ghcnd_lm)

# Model M1
M1 <- lm(Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYear_avg + 
           cos(2 * pi * DecYear_avg) + sin(2 * pi * DecYear_avg), 
           data = ghcnd_lm)

# Model M2
M2 <- lm(Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYear_avg + 
           cos(2 * pi * DecYear_avg) + sin(2 * pi * DecYear_avg) + 
           cos(4 * pi * DecYear_avg) + sin(4 * pi * DecYear_avg), 
           data = ghcnd_lm)

# Model M3
M3 <- lm(Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYear_avg + 
           cos(2 * pi * DecYear_avg) + sin(2 * pi * DecYear_avg) + 
           cos(4 * pi * DecYear_avg) + sin(4 * pi * DecYear_avg) + 
           cos(6 * pi * DecYear_avg) + sin(6 * pi * DecYear_avg), 
           data = ghcnd_lm)

# Model M4
M4 <- lm(Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYear_avg + 
           cos(2 * pi * DecYear_avg) + sin(2 * pi * DecYear_avg) + 
           cos(4 * pi * DecYear_avg) + sin(4 * pi * DecYear_avg) + 
           cos(6 * pi * DecYear_avg) + sin(6 * pi * DecYear_avg) + 
           cos(8 * pi * DecYear_avg) + sin(8 * pi * DecYear_avg), 
           data = ghcnd_lm)

#summary(M0)
#summary(M1)
#summary(M2)
#summary(M3)
#summary(M4)

#combine above models into a single function for later
Model <- function(data, freq = 4){
  # data: data set for model training
  #freq: defined as k in model 
  # it would return  model summary based on the freq and training data
  if (freq == 0){   #Model m0
    M <- lm(Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYear_avg, data = data)
  }else if(freq == 1){  #Model m1
    M <- lm(Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYear_avg + 
               cos(2 * pi * DecYear_avg) + sin(2 * pi * DecYear_avg), 
             data = data)
  }else if(freq == 2){ #Model M2
    M <- lm(Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYear_avg + 
               cos(2 * pi * DecYear_avg) + sin(2 * pi * DecYear_avg) + 
               cos(4 * pi * DecYear_avg) + sin(4 * pi * DecYear_avg), 
             data = data)
  }else if(freq == 3){   #Model M3
    M <- lm(Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYear_avg + 
               cos(2 * pi * DecYear_avg) + sin(2 * pi * DecYear_avg) + 
               cos(4 * pi * DecYear_avg) + sin(4 * pi * DecYear_avg) + 
               cos(6 * pi * DecYear_avg) + sin(6 * pi * DecYear_avg), 
             data = data)
  } else{   #Model m3
    M <- lm(Value_sqrt_avg ~ Longitude + Latitude + Elevation + DecYear_avg + 
               cos(2 * pi * DecYear_avg) + sin(2 * pi * DecYear_avg) + 
               cos(4 * pi * DecYear_avg) + sin(4 * pi * DecYear_avg) + 
               cos(6 * pi * DecYear_avg) + sin(6 * pi * DecYear_avg) + 
               cos(8 * pi * DecYear_avg) + sin(8 * pi * DecYear_avg), 
             data = data)
  }
  return (M)
}


#perform cross_validation
cv_station <- function(data){
  #use ghcnd_lm again as input
  all_result <-c()
  ds_table1 <-c()
  se_table1 <-c()
  score_table3 <- matrix(nrow = 12, ncol=5)
  score_table4 <- matrix(nrow = 12, ncol=5)
  # loop over all the stations
  for(station_id in unique(data$ID)){
    monthly_score <- matrix(0, nrow = 12, ncol = 10)
    mean_ds <- c()
    mean_se <- c()
    #loop over all the models
    for(i in 0:4){
      for(month in 1:12){
        #loop over all the month to get a well organized data
        fit <- Model(data=data %>% filter(ID != station_id), freq = i)
        pred <- predict(fit, newdata =data %>% filter(ID == station_id, Month == month), se.fit = TRUE)
        residual_var <- sum(fit$residuals^2)/fit$df.residual
        sd_pred <- sqrt(pred$se.fit^2 + residual_var)
        #compute ds score
        ds_score <- proper_score(type = "ds", obs = data %>% filter(ID == station_id, Month == month)  %>% pull(Value_sqrt_avg), mean = pred$fit, sd = sd_pred)
        #compute se score
        se_score <- proper_score("se", obs = data %>% filter(ID == station_id, Month == month)  %>% pull(Value_sqrt_avg), mean = pred$fit)
        #construct a data frame with size contains all result for each station
        monthly_score[month, i+1] <- mean(ds_score)
        monthly_score[month, i+6] <- mean(se_score)
        #compute mean prediction scores
        mean_ds <- c(mean_ds, mean(ds_score))
        mean_se <- c(mean_se, mean(se_score))
      }
      #organize two mean distribution scores
      ds_table1 <- c(ds_table1, mean(mean_ds))
      se_table1 <- c(se_table1, mean(mean_se))
    }
    #finally form a data frame with size 96*10 contains all the results
    all_result <-rbind(all_result, monthly_score)
  }
  
  #use for loop to get values for overall mean scores for each month
  for(i in 1:12){
    for(j in 1:5){
      score_table3[i,j] = mean(all_result[seq(i, 96, by = 12),j])
      score_table4[i,j] = mean(all_result[seq(i, 96, by = 12),5+j])
      
    }
  }
  #organize all tables and adding row names, column names
  score_table1 <- matrix(ds_table1, nrow = 8, byrow = TRUE)
  row.names(score_table1) <- unique(ghcnd_lm$ID)
  colnames(score_table1) <- c("M0", "M1", "M2", "M3", "M4")
  
  score_table2 <- matrix(se_table1, nrow = 8, byrow = TRUE)
  row.names(score_table2) <- unique(ghcnd_lm$ID)
  colnames(score_table2) <- c("M0", "M1", "M2", "M3", "M4")
  
  row.names(score_table3) <- unique(ghcnd_lm$Month)
  colnames(score_table3) <- c("M0", "M1", "M2", "M3", "M4")
  
  row.names(score_table4) <- unique(ghcnd_lm$Month)
  colnames(score_table4) <- c("M0", "M1", "M2", "M3", "M4")
  

  colnames(all_result) <- c("M0_DS", "M1_DS", "M2_DS", "M3_DS", "M4_DS","M0_SE", "M1_SE", "M2_SE", "M3_SE", "M4_SE")
  ID <- rep(unique(ghcnd_lm$ID),each=12)
  month <- rep(1:12,8)
  all_result <- as.data.frame(cbind(ID,month, all_result))

  
  return(list(score_table1 = score_table1, score_table2 = score_table2, score_table3 = score_table3, score_table4 = score_table4 ,all_result=all_result))
}
cv_station_result <- cv_station(ghcnd_lm)
#cv_station_result$all_result
#cv_station_result$score_table1
#cv_station_result$score_table2
#cv_station_result$score_table3
#cv_station_result$score_table4

#bar chart to show ds scores at each station
score_plot1 <- function(){
  score_df <- as.data.frame(cv_station_result$score_table1)
  score_df$ID <- row.names(cv_station_result$score_table1)
  # Reshape the data frame to long format
  score_long <- tidyr::gather(score_df, key = "Model", value = "Score", -ID)
  # Generate the bar chart
  ggplot(score_long, aes(x = ID, y = Score, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5) +
    ggtitle("Dawid-Sebastiani Scores for Different Models across Stations") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = "Station ID", y = "Score")
}

#bar chart to show se scores at each station
score_plot2 <- function(){
  score_df <- as.data.frame(cv_station_result$score_table2)
  score_df$ID <- row.names(cv_station_result$score_table2)
  # Reshape the data frame to long format
  score_long <- tidyr::gather(score_df, key = "Model", value = "Score", -ID)
  # Generate the bar chart
  ggplot(score_long, aes(x = ID, y = Score, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5) +
    ggtitle("Squared Error Scores for Different Models across Stations") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = "Station ID", y = "Score")
}

#bar chart to show overall ds scores in each month
score_plot3 <- function(){
  # Convert row names to factor with ordered levels
  months <- unique(ghcnd_lm$Month)
  score_table3 <- as.data.frame(cv_station_result$score_table3)
  score_table3$month_factor <- factor(row.names(score_table3), levels=months, ordered=TRUE)
  # Generate the bar chart
  score_table3 %>%
    pivot_longer(cols=-month_factor, names_to="Model", values_to="Score") %>%
    ggplot(aes(x=month_factor, y=Score, fill=Model)) +
    geom_col(position="dodge") +
    xlab("Month") +
    ylab("Score") +
    ggtitle("Dawid-Sebastiani Scores for Different Models across Different Months") +
    theme(plot.title = element_text(hjust = 0.5))
}


#bar chart to show overall ds scores in each month
score_plot4 <- function(){
  # Convert row names to factor with ordered levels
  months <- unique(ghcnd_lm$Month)
  score_table4 <- as.data.frame(cv_station_result$score_table4)
  score_table4$month_factor <- factor(row.names(score_table4), levels=months, ordered=TRUE)
  # Generate the bar chart
  score_table4 %>%
    pivot_longer(cols=-month_factor, names_to="Model", values_to="Score") %>%
    ggplot(aes(x=month_factor, y=Score, fill=Model)) +
    geom_col(position="dodge") +
    xlab("Month") +
    ylab("Score") +
    ggtitle("Squared Error Scores for Different Models across Each Months") +
    theme(plot.title = element_text(hjust = 0.5))
}


#construct a new data set that ds score in each month for every station
ds_table <- cv_station_result$all_result %>% 
  gather(key = "Model_type", value = "value", M0_DS:M4_DS) %>% 
  select(Model_type, ID, month, value) %>%
  mutate(month = as.numeric(month),
         value = as.numeric(value))

#construct a new data set that se score in each month for every station
se_table <- cv_station_result$all_result %>% 
  gather(key = "Model_type", value = "value", M0_SE:M4_SE) %>% 
  select(Model_type, ID, month, value) %>%
  mutate(month = as.numeric(month),
         value = as.numeric(value))

#plot ds score grouped by model for every month every station
score_plot5 <- function(){
  ggplot(ds_table, aes(month, value, color = ID)) +
    geom_line() +
    facet_wrap(~Model_type, ncol=2) + # split the plot based on the Model_type
    ggtitle("DS Scores of different stations in each month") + # add title
    xlab("Month") + # change the name of x-axis
    ylab("DS Score") + # change the name of y-axis
    scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1)) # set x-axis limits and ticks
}

#construct a new data set that se score in each month for every station
score_plot6 <- function(){
  ggplot(se_table, aes(month, value, color = ID)) +
    geom_line() +
    facet_wrap(~Model_type, ncol=2) + # split the plot based on the Model_type
    ggtitle("SE Scores of different stations in each month") + # add title
    xlab("Month") + # change the name of x-axis
    ylab("Score") + # change the name of y-axis
    scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1)) # set x-axis limits and ticks
}

#score_plot1()
#score_plot2()
#score_plot3()
#score_plot4()
#score_plot5()
#score_plot6()