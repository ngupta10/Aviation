library(dplyr)
library(treemap)
library(MASS)
inst_pkgs = load_pkgs =  c("ggplot2","ggplot2movies", "dplyr","babynames","data.table","Rcpp")
git_pkgs = git_pkgs_load = c("streamgraph","DT")
load_pkgs = c(load_pkgs, git_pkgs_load)
pkgs_loaded = lapply(load_pkgs, require, character.only=T)
air = fread('data/2008_subset.csv')




air <- as.data.frame(air)
save(air, file = 'air.RDdata')
load('air.RDdata')
data <- read.csv("data/2008_subset_trial.csv", header = TRUE)
data <- air
install_github('hrbrmstr/streamgraph')
load('air.RDdata')
cancelled <- data[,"Cancelled"] == 1
cancel <- data[cancelled,]

print(dim(cancel)[1]/dim(data)[1])

cancel %>% 
  group_by(Month) %>% 
  tally() -> month

colnames(month)<- c("Month","Cancellations")
aa <- month.abb(month$Month)
month.abb
month$MonthAbb <- month.abb
treemap(month,
        index=c("MonthAbb"),
        vSize="Cancellations",
        vColor = "Cancellations",
        type="value")

count <- table(air$UniqueCarrier)
DTsum <- aggregate(ArrDelay ~ UniqueCarrier, air[which(air$ArrDelay > 0),], sum)
DTsum[2] <- DTsum[2]/count
sortedDTsum <- DTsum[order(DTsum$ArrDelay),]
sortedDTsum
summary(sortedDTsum$ArrDelay)

treemap(DTsum,
        index=c("UniqueCarrier"),
        vSize="ArrDelay",
        vColor="ArrDelay",
        type="value")#actual value or percentage value



DTsum_dest <- aggregate(ArrDelay ~ Dest, air[which(air$ArrDelay > 0),], sum)



cancel %>% 
  group_by(DayOfWeek) %>% 
  tally() -> day

day$Day = c("Monday", "Tuesday", "Wednesday", 
            "Thursday", "Friday", "Saturday", "Sunday")
colnames(day) = c("Day of the week","Cancellations","Day")
treemap(day,
        index=c("Day"),
        vSize="Cancellations",
        vColor = "Cancellations",
        type="value")



cancel %>% 
  group_by(UniqueCarrier) %>% 
  tally() -> carrier

treemap(carrier,
        index=c("UniqueCarrier"),
        vSize="n",
        vColor = "n",
        type="value")


cancel %>% 
  group_by(Origin) %>% 
  tally() -> Origin
DTsum_dest$Avg <- DTsum_dest$ArrDelay/(nrow(DTsum_dest)+280)

treemap(DTsum_dest,
        index=c("Dest"),
        vSize="Avg",
        vColor="Avg",
        type="value",
        title="Arrival Delay at Destination Airport - Sorted by Average Arrival Delay")
