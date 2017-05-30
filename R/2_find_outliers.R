library(dplyr)
library(ggplot2)

sds <- read.csv("output/Seinajoki_five_less_z_st.csv")
ageinfo <- readRDS("output/ageinfo.rds")

under_five <- merge(sds, ageinfo, by = c("id", "rowid"), all.x = TRUE)

# tag fram besöksår för att kunna göra uttag för ålder vid besök
under_five$birthyear <- as.numeric(substr(under_five$consensus_birth, 1, 4))
under_five$visityear <- as.numeric(under_five$visityear)

# Hitta outliers.

# Steg 1 ganska naivt, plotta variablerna ålder och kön mot varandra.
#fit a linear model
ll <- list()

for(i in 0:5){
  under_five_tmp <- under_five %>% filter(visityear == (birthyear + i))
  
  #plot(x = under_five_tmp$height, y = under_five_tmp$weight)
  
  # use na exclude to padd with na instead of omitting data
  linear_model <- lm(weight~height, data = under_five_tmp, na.action = "na.exclude")
  
  under_five_tmp$cd <- cooks.distance(linear_model)
  under_five_tmp$class <-  ifelse(under_five_tmp$cd > 4 * mean(under_five_tmp$cd, na.rm = TRUE),"red", "black")
  table(under_five_tmp$class)
  
  plot(x = under_five_tmp$height, y = under_five_tmp$weight, 
       col = under_five_tmp$class,
       xlab = "Height",
       ylab = "Weight"
       )

  title(paste0(i, " year olds"))
  
  ll[[i+1]] <- under_five_tmp[,c("id","rowid", "cd")]
  print(nrow(under_five_tmp))
  # sort dataset after most influential
  #under_five <- under_five[order(-under_five$cd),]
}

# bind list with cooks distances
tmp <- do.call("rbind",ll)

# merge in cd
under_five <- merge(under_five, tmp, by = c("id", "rowid"), 
                        all.x = TRUE)

plot(x = under_five$height, y = under_five$weight, 
     col = ifelse(under_five$cd > quantile(under_five$cd, na.rm = TRUE, 0.995),"red", "black" ),
     xlab = "Height",
     ylab = "Weight"
)
title("All")

