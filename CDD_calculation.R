# annual CDD 

library(dplyr)
library(ggplot2)
library(ggrepel)

cdd_annual <- function(base, df){
  
  # changing this like for 18.3 calculation
  climate_variable <- colnames(df)[5]
  #climate_variable <- colnames(df)[4]
  
  # daily CDD calculation
  # changing this like for 18.3 calculation
  df$CDD <- ifelse(df[,5] - base > 0, df[,5] - base, 0)
  #df$CDD <- ifelse(df[,4] - base > 0, df[,4] - base, 0)
  
  # Annual CDD sum
  annual_cdd <- aggregate(df$CDD, by=list(df$Year), FUN=sum)
  colnames(annual_cdd) <- c("Year", "CDD")
  
  annual_cdd$climate_variable <- climate_variable
  annual_cdd$State <- df$State[1]
  
  return(annual_cdd)
  
}

# OBSERVED #

# observed annual CDD
# x represents the row
observed_annual_cdd <- apply(base_results2, 1, function(x) cdd_annual(as.numeric(x[3]), 
                  filter(observed_data[,c(8:11,ifelse(as.numeric(x[4])%%7 == 0, 7, as.numeric(x[4])%%7))], State == x[1])))



observed_annual_cdd <- bind_rows(observed_annual_cdd, .id = "column_label")
observed_annual_cdd <- observed_annual_cdd[,-1]

# correct function - can be used for both observed and projected
plot_states_cdd <- function(df, climate_var, years1, years2, title, labels){
  
  df1 <- df %>% 
    filter(climate_variable == climate_var) %>%
    filter(Year %in% years1)
  
  df2 <- df %>% 
    filter(climate_variable == climate_var) %>%
    filter(Year %in% years2)
  
  
  df1 <- aggregate(df1$CDD, by=list(df1$State), FUN=mean)
  df2 <- aggregate(df2$CDD, by=list(df2$State), FUN=mean)
  
  
  colnames(df1) <- c('State', "ref")
  colnames(df2) <- c('State', "fut")
  
  df <- inner_join(df1, df2)
  
  df <- inner_join(df, labels)
  
  colnames(df)[4] <- "top" 
  
  # to change here if decided to change base values for 18.3 
  # WA is unsensitive to cooling for all variables
  # and other variables
  
  df <- filter(df, !State == "WA")
  
  if (climate_var == "dpt") {
    
    df <- filter(df, !State %in% c("WY", "ID"))
  }
  
  
  p <- ggplot(df, aes(x = ref, y = fut, color = factor(top))) + geom_point(aes(shape = factor(over4))) +
    labs(x = paste0("ref", " ", min(years1), "-", max(years1), "_mean"),
         y = paste0("fut", " ", min(years2), "-", max(years2), "_mean" , color = "Is selected", shape = "Over 4 selected")) +
    geom_abline(intercept = 0, slope = 1) + ggtitle(paste0(title, " ", climate_var))
  
  p<- p + geom_label_repel(aes(label = State),
                           box.padding   = 0.35, 
                           point.padding = 0.5,
                           segment.color = 'grey50') + theme_classic()
  
  ggsave(filename = paste0(title, " ", climate_var,"_annual_observed_color.pdf"), 
         plot = last_plot(),
         device = 'pdf',
         dpi = 400)
  
  
}



top_vars <- read.table("top_variables.csv", sep = ",", header = T)
top_vars <- top_vars[,c(1,5,2,4,8,6,7,3, 9)]

plot_states_cdd(df = observed_annual_cdd, climate_var = "dpt", years1 = c(1991:2003),
                years2 = c(2004:2016), title = "Observed", labels = top_vars[,c(1,5,9)])


vars <- c("air.temp", "hia", "humidex", "dpt", "wba", "wbgt", "di")

# Observed
setwd("C:/Users/dmaiasil/Documents/Purdue Projects/Month_Climate/U_Curves/Observed plots/Annual CDD")
for( i in 1:length(vars)){
  
  plot_states_cdd(df = observed_annual_cdd, climate_var = vars[i], 
                  years1 = c(1991:2003), years2 = c(2004:2016), title = "Observed", labels = top_vars[,c(1,(i+1),9)])
  
}

# Projected

plot_states_cdd(df = projected_cdd_annual, climate_var = "di", years1 = c(1991:2010), years2 = c(2031:2050), title = "Projected")

setwd("C:/Users/dmaiasil/Documents/Purdue Projects/Month_Climate/U_Curves/Projected plots/Annual CDD")
for( i in 1:length(vars)){
  
  plot_states_cdd(df = projected_cdd_annual, climate_var = vars[i],
                  years1 = c(1991:2010), years2 = c(2031:2050), title = "Projected", labels = top_vars[,c(1,(i+1),9)])
  
}

##############################
# temperature 18.3 cdd graph comparison
observed_data_air <- observed_data[,c(8:11,1)]

observed_data_air <- observed_data_air %>%
  group_by(State) %>%
  group_map(~ cdd_annual(18.3, cbind(.x, .y)))

observed_data_air <- bind_rows(observed_data_air, .id = "column_label")
observed_data_air <- observed_data_air[,-1]


calculates_air <- observed_annual_cdd %>%
  filter(climate_variable == "air.temp")

# plot all states for the 2 bases of temperature
# need annual mean by all years

plot_df <- aggregate(calculates_air$CDD, by=list(calculates_air$State), FUN=mean)
plot_df <- cbind(plot_df, aggregate(observed_data_air$CDD, by=list(observed_data_air$State), FUN=mean))
plot_df <- plot_df[,-3]
colnames(plot_df) <- c("State", "calculated_CDD", "CDD_18.3")


plot_df$greater_18.3 <- ifelse(filter(base_results, climateVariable == "Air Temperature")$CDD_base < 18.3, 1, 0)

plot_df <- filter(plot_df, !State == "WA")
p <- ggplot(plot_df, aes(y = calculated_CDD, x = CDD_18.3, color = factor(greater_18.3))) + geom_point( size = 2) +
  labs(y = "1991_2016 mean from calculated CDD base", x = "1991_2016 mean from 18.3 CDD base", color = "Base temperature > 18.3")+
  geom_abline(intercept = 0, slope = 1) + ggtitle("Air Temperature CDD")

p + geom_label_repel(aes(label = State),
                         box.padding   = 0.35, 
                         point.padding = 0.5,
                         segment.color = 'grey50') + theme_classic() 


plot_df$change <- ((plot_df$calculated_CDD/plot_df$CDD_18.3) - 1)*100

write.csv(plot_df, 
          file = "C:/Users/dmaiasil/Documents/Purdue Projects/Month_Climate/U_Curves/Tableau files/change_observed_18.csv", row.names = F)


# end of 18.3 comparison (cdd_annual was changed in specific lines, check comments (columns 5 to 4))
############################






change_projected <- function(df, climate_var, years1, years2, state){
  
  df1 <- df %>% 
    filter(climate_variable == climate_var) %>%
    filter(Year %in% years1)
  
  df2 <- df %>% 
    filter(climate_variable == climate_var) %>%
    filter(Year %in% years2)
  
  
  df1 <- aggregate(df1$CDD, by=list(df1$State), FUN=mean)
  df2 <- aggregate(df2$CDD, by=list(df2$State), FUN=mean)
  
  
  colnames(df1) <- c('State', "ref")
  colnames(df2) <- c('State', "fut")
  
  df <- inner_join(df1, df2)
  
  df$change <- ((df$fut/df$ref) - 1)*100
  
  colnames(df)[4] <- climate_var
  
  return(df[,c(1,4)])
  
}

change_air <- change_projected(df = projected_cdd_annual, climate_var = "air.temp", years1 = c(1991:2010), years2 = c(2031:2050))
change_di <- change_projected(df = projected_cdd_annual, climate_var = "di", years1 = c(1991:2010), years2 = c(2031:2050))
change_hia <- change_projected(df = projected_cdd_annual, climate_var = "hia", years1 = c(1991:2010), years2 = c(2031:2050))
change_humidex <- change_projected(df = projected_cdd_annual, climate_var = "humidex", years1 = c(1991:2010), years2 = c(2031:2050))
change_dpt <- change_projected(df = projected_cdd_annual, climate_var = "dpt", years1 = c(1991:2010), years2 = c(2031:2050))
change_wba <- change_projected(df = projected_cdd_annual, climate_var = "wba", years1 = c(1991:2010), years2 = c(2031:2050))
change_wbgt <- change_projected(df = projected_cdd_annual, climate_var = "wbgt", years1 = c(1991:2010), years2 = c(2031:2050))


change_all_states_annual <- change_air %>%
  inner_join(change_di) %>%
  inner_join(change_hia) %>%
  inner_join(change_humidex) %>%
  inner_join(change_dpt) %>%
  inner_join(change_wba) %>%
  inner_join(change_wbgt) 



write.csv(change_all_states_annual, 
          file = "C:/Users/dmaiasil/Documents/Purdue Projects/Month_Climate/U_Curves/Tableau files/change_projected_annual.csv", row.names = F)



# using the change_projected function to calculate the change in oberved periods too
change_air <- change_projected(df = observed_annual_cdd, climate_var = "air.temp", years1 = c(1991:2003), years2 = c(2004:2016))
change_di <- change_projected(df = observed_annual_cdd, climate_var = "di", years1 = c(1991:2003), years2 = c(2004:2016))
change_hia <- change_projected(df = observed_annual_cdd, climate_var = "hia", years1 = c(1991:2003), years2 = c(2004:2016))
change_humidex <- change_projected(df = observed_annual_cdd, climate_var = "humidex", years1 = c(1991:2003), years2 = c(2004:2016))
change_dpt <- change_projected(df = observed_annual_cdd, climate_var = "dpt", years1 = c(1991:2003), years2 = c(2004:2016))
change_wba <- change_projected(df = observed_annual_cdd, climate_var = "wba", years1 = c(1991:2003), years2 = c(2004:2016))
change_wbgt <- change_projected(df = observed_annual_cdd, climate_var = "wbgt", years1 = c(1991:2003), years2 = c(2004:2016))


change_all_states_annual <- change_air %>%
  inner_join(change_di) %>%
  inner_join(change_hia) %>%
  inner_join(change_humidex) %>%
  inner_join(change_dpt) %>%
  inner_join(change_wba) %>%
  inner_join(change_wbgt) 

write.csv(change_all_states_annual, 
          file = "C:/Users/dmaiasil/Documents/Purdue Projects/Month_Climate/U_Curves/Tableau files/change_observed_annual.csv", row.names = F)

