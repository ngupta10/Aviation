flights <- read.csv("data/2008_subset_trial.csv")

#flights_full <- read.csv("data/2008.csv")

library(dplyr)
library(treemap)
library(MASS)
inst_pkgs = load_pkgs =  c("ggplot2","ggplot2movies", "dplyr","babynames","data.table","Rcpp")


flights$Arr_Delay_Airport = ifelse(flights$ArrDelay > 0, 1, 0)
treemap(flights,
        index=c("Origin"),
        vSize="Arr_Delay_Airport",
        vColor="Arr_Delay_Airport",
        type="value")


flights$Dep_Delay_Airport = ifelse(flights$DepDelay > 0, 1, 0)
treemap(flights,
        index=c("Origin"),
        vSize="Dep_Delay_Airport",
        vColor="Dep_Delay_Airport",
        type="value")



air = fread('data/2008_subset_trial.csv')
save(air, file = 'air.RDdata')
load('air.RDdata')

DTsum_origin <- aggregate(ArrDelay~Origin, air[which(ArrDelay > 0)], sum)
DTsum_origin[2] <- DTsum_origin[2]/count
sortedDTsum_origin <- DTsum_origin[order(DTsum_origin$ArrDelay,decreasing = T),]

treemap(DTsum_origin,
        index=c("Origin"),
        vSize="ArrDelay",
        vColor="ArrDelay",
        type="value")#actual value or percentage value


