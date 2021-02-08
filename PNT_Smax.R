# addition to results discussion

library(dplyr)
library(ggplot2)
library(ggrepel)

# PNT calculation
# source: https://www.sciencedirect.com/science/article/pii/S030142151630653X
# PNT = 26.33 * LN(CDD) - 81.69

# using the same PNT equation

# they use the annual mean CDD of all years
# I need observed CDD temperature mean for all years per state
# applying to our summer data based on paper comment 'number of hot season days'

# dataset I'm looking for: plot_df (that has all years mean for each CDD)

pnt_CDD <- plot_df[,1:3]


pnt_CDD$pnt_calculated <- ifelse(pnt_CDD$calculated_CDD > 920, 97.3, 26.33*log(pnt_CDD$calculated_CDD) - 81.69)  
pnt_CDD$pnt_18.3 <- ifelse(pnt_CDD$CDD_18.3 > 920, 97.3, 26.33*log(pnt_CDD$CDD_18.3) - 81.69) 
pnt_CDD$greater_18.3 <- ifelse(pnt_CDD$pnt_calculated > pnt_CDD$pnt_18.3, 1, 0)



p <- ggplot(pnt_CDD, aes(y = pnt_calculated, x = pnt_18.3, color = factor(greater_18.3))) + geom_point(size = 2) +
  labs(y = "PNT variable CDD base", x = "PNT 18.3 CDD base", color = "PNT variable > PNT 18.3")+
  geom_abline(intercept = 0, slope = 1) + ggtitle("PNT comparison CDD")

p + geom_label_repel(aes(label = State),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50') + theme_classic() 

# calculating the differences in percentage

pnt_CDD$pnt_change <- ((pnt_CDD$pnt_calculated/pnt_CDD$pnt_18.3) - 1)*100

mean(pnt_CDD$pnt_change)
mean(abs(pnt_CDD$pnt_change))
max(pnt_CDD$pnt_change)


pnt_CDD %>% arrange(pnt_change)


#### Smax
#  https://pubs.rsc.org/en/content/articlehtml/2019/ee/c9ee00002j

smax_CDD <- plot_df[,1:3]

smax_CDD$smax_calculated <- 1 - 0.949*(exp(-0.00187*smax_CDD$calculated_CDD))
smax_CDD$smax_18.3 <- 1 - 0.949*(exp(-0.00187*smax_CDD$CDD_18.3))

smax_CDD$greater_18.3 <- ifelse(smax_CDD$smax_calculated > smax_CDD$smax_18.3, 1, 0)


p <- ggplot(smax_CDD, aes(y = smax_calculated, x = smax_18.3, color = factor(greater_18.3))) + geom_point(size = 2) +
  labs(y = "Smax variable CDD base", x = "Smax 18.3 CDD base", color = "Smax variable > Smax 18.3")+
  geom_abline(intercept = 0, slope = 1) + ggtitle("Smax comparison CDD")

p + geom_label_repel(aes(label = State),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50') + theme_classic() 



smax_CDD$smax_change <- ((smax_CDD$smax_calculated/smax_CDD$smax_18.3) - 1)*100
mean(smax_CDD$smax_change)
mean(abs(smax_CDD$smax_change))


smax_CDD %>% arrange(smax_change)

#### Projected data #####
# dataset: projected_cdd_anual (air updated and hia CDD)
#          projected_cdd23 (air 18.3) (need to calculate annual mean)

# projected air temp 183
projected_cdd_183 <- projected_cdd23 %>%
  group_by(Year, climate_variable, State) %>%
  summarise(CDD_annual = sum(CDD))

# PNT projected air comparison
# need total mean 2031-2050

pnt_projected_air <- projected_cdd_183 %>%
  filter(Year %in% c(2031:2050)) %>%
  group_by(climate_variable, State) %>%
  summarise(CDD_mean_31_50 = mean(CDD_annual))

test <- projected_cdd_annual %>%
  filter(Year %in% c(2031:2050)) %>%
  filter(climate_variable == "air.temp") %>%
  group_by(climate_variable, State) %>%
  summarise(CDD_mean_31_50 = mean(CDD))

colnames(pnt_projected_air)[3] <- "CDD_183"  

pnt_projected_air$CDD_variable <- test$CDD_mean_31_50

pnt_projected_air <- pnt_projected_air[,-1]

pnt_projected_air$pnt_calculated <- ifelse(pnt_projected_air$CDD_variable > 920, 97.3, 26.33*log(pnt_projected_air$CDD_variable) - 81.69)  
pnt_projected_air$pnt_18.3 <- ifelse(pnt_projected_air$CDD_183 > 920, 97.3, 26.33*log(pnt_projected_air$CDD_183) - 81.69) 
pnt_projected_air$greater_18.3 <- ifelse(pnt_projected_air$pnt_calculated > pnt_projected_air$pnt_18.3, 1, 0)

p <- ggplot(pnt_projected_air, aes(y = pnt_calculated, x = pnt_18.3, color = factor(greater_18.3))) + geom_point(size = 2) +
  labs(y = "PNT variable CDD base", x = "PNT 18.3 CDD base", color = "PNT variable > PNT 18.3")+
  geom_abline(intercept = 0, slope = 1) + ggtitle("PNT comparison CDD projected 2031-2050")

p + geom_label_repel(aes(label = State),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50') + theme_classic() 

# hia CDD for comparison with updated/variable CDD
test <- projected_cdd_annual %>%
  filter(Year %in% c(2031:2050)) %>%
  filter(climate_variable == "hia") %>%
  group_by(climate_variable, State) %>%
  summarise(CDD_mean_31_50 = mean(CDD))


pnt_projected_air$hia_CDD_variable <- test$CDD_mean_31_50

pnt_projected_air$pnt_hia <- ifelse(pnt_projected_air$hia_CDD_variable > 920, 97.3, 26.33*log(pnt_projected_air$hia_CDD_variable) - 81.69)
pnt_projected_air$hia_greater <- ifelse(pnt_projected_air$pnt_hia > pnt_projected_air$pnt_calculated, 1, 0)


p <- ggplot(pnt_projected_air, aes(y = pnt_hia, x = pnt_calculated, color = factor(hia_greater))) + geom_point(size = 2) +
  labs(y = "PNT hia", x = "PNT variable air", color = "PNT hia > PNT air temp")+
  geom_abline(intercept = 0, slope = 1) + ggtitle("PNT comparison CDD projected 2031-2050 hia vs air temp")

p + geom_label_repel(aes(label = State),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50') + theme_classic() 


# calculating the differences in percentage

pnt_projected_air$pnt_projected_air <- ((pnt_projected_air$pnt_calculated/pnt_projected_air$pnt_18.3) - 1)*100
pnt_projected_air$pnt_change_hia <- ((pnt_projected_air$pnt_calculated/pnt_projected_air$pnt_hia) - 1)*100


mean(pnt_projected_air$pnt_projected_air)
mean(abs(pnt_projected_air$pnt_projected_air))
pnt_projected_air %>% arrange(pnt_projected_air)

mean(pnt_projected_air$pnt_change_hia)
mean(abs(pnt_projected_air$pnt_change_hia))
pnt_CDD %>% arrange(pnt_change_hia)




#### Smax comparison projected ####

smax_projected <- pnt_projected_air[,c(1:3,7)]

smax_projected$smax_calculated <- 1 - 0.949*(exp(-0.00187*smax_projected$CDD_variable))
smax_projected$smax_18.3 <- 1 - 0.949*(exp(-0.00187*smax_projected$CDD_183))
smax_projected$smax_hia <- 1 - 0.949*(exp(-0.00187*smax_projected$hia_CDD_variable))


smax_projected$greater_18.3 <- ifelse(smax_projected$smax_calculated > smax_projected$smax_18.3, 1, 0)
smax_projected$greater_hia <- ifelse(smax_projected$smax_hia > smax_projected$smax_calculated, 1, 0)


p <- ggplot(smax_projected, aes(y = smax_calculated, x = smax_18.3, color = factor(greater_18.3))) + geom_point(size = 2) +
  labs(y = "Smax variable CDD base", x = "Smax 18.3 CDD base", color = "Smax variable > Smax 18.3")+
  geom_abline(intercept = 0, slope = 1) + ggtitle("Smax comparison CDD projected 2031-2050")

p + geom_label_repel(aes(label = State),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50') + theme_classic() 

p <- ggplot(smax_projected, aes(y = smax_hia, x = smax_calculated, color = factor(greater_hia))) + geom_point(size = 2) +
  labs(y = "Smax hia", x = "Smax variable CDD base", color = "Smax hia > Smax air temp")+
  geom_abline(intercept = 0, slope = 1) + ggtitle("Smax comparison CDD projected 2031-2050 hia vs temp")

p + geom_label_repel(aes(label = State),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50') + theme_classic() 



smax_projected$smax_change_air <- ((smax_projected$smax_calculated/smax_projected$smax_18.3) - 1)*100
mean(smax_projected$smax_change_air)
mean(abs(smax_projected$smax_change_air))

smax_projected$smax_change_hia <- ((smax_projected$smax_hia/smax_projected$smax_calculated) - 1)*100
mean(smax_projected$smax_change_hia)
mean(abs(smax_projected$smax_change_hia))













