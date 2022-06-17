
##Loading Libraries##

library(pivottabler)
library(Rcpp)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(corrplot)
library(streamgraph)
library(data.table)
library(streamgraph)    #install_github("hrbrmstr/streamgraph")

##Loading Libraries##

##loading data##
pd10 <- read.csv("2020-10-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo10 <- read.csv("2020-10-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged10 <- merge(pd10, pdo10, by = "Crime.ID",stringsAsFactors =T)
pd11 <- read.csv("2020-11-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo11 <- read.csv("2020-11-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged11 <- merge(pd11, pdo11, by = "Crime.ID",stringsAsFactors =T)
pd12 <- read.csv("2020-12-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo12 <- read.csv("2020-12-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged12 <- merge(pd12, pdo12, by = "Crime.ID",stringsAsFactors =T)
pd1 <- read.csv("2021-01-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo1 <- read.csv("2021-01-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged1 <- merge(pd1, pdo1, by = "Crime.ID",stringsAsFactors =T)
pd2 <- read.csv("2021-02-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo2 <- read.csv("2021-02-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged2 <- merge(pd2, pdo2, by = "Crime.ID",stringsAsFactors =T)
pd3 <- read.csv("2021-03-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo3 <- read.csv("2021-03-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged3 <- merge(pd3, pdo3, by = "Crime.ID",stringsAsFactors =T)
pd4 <- read.csv("2021-04-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo4 <- read.csv("2021-04-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged4 <- merge(pd4, pdo4, by = "Crime.ID",stringsAsFactors =T)
pd5 <- read.csv("2021-05-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo5 <- read.csv("2021-05-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged5 <- merge(pd5, pdo5, by = "Crime.ID",stringsAsFactors =T)
pd6 <- read.csv("2021-06-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo6 <- read.csv("2021-06-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged6 <- merge(pd6, pdo6, by = "Crime.ID",stringsAsFactors =T)
pd7 <- read.csv("2021-07-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo7 <- read.csv("2021-07-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged7 <- merge(pd7, pdo7, by = "Crime.ID",stringsAsFactors =T)
pd8 <- read.csv("2021-08-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo8 <- read.csv("2021-08-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged8 <- merge(pd8, pdo8, by = "Crime.ID",stringsAsFactors =T)
pd9 <- read.csv("2021-09-west-yorkshire-street.csv", header=TRUE,stringsAsFactors =T)
pdo9 <- read.csv("2021-09-west-yorkshire-outcomes.csv", header=TRUE,stringsAsFactors =T)
merged9 <- merge(pd9, pdo9, by = "Crime.ID",stringsAsFactors =T)
Tmerged_org <- rbind(merged1, merged2, merged3, merged4, merged5, merged6, merged7, merged8, merged9, merged10, merged11, merged12)

#making a subset for city center

Tmerged <-subset(Tmerged_org, Longitude.x <"-1.56" & Longitude.x >"-1.52" &  Latitude.x >"53.786" & Latitude.x <"53.804")

##loading data finished##

##Aggregate/monthcrime##

qhpvt(Tmerged, "Month.x", "Crime.type", "n()")
month_crime <- PivotTable$new()
month_crime$addData(Tmerged)
month_crime$addColumnDataGroups("Month.x")
month_crime$addRowDataGroups("Crime.type")
month_crime$defineCalculation(calculationName="Totalcrime", summariseExpression="n()")
month_crime$renderPivot()
month_crime_tb <- month_crime$asDataFrame()
col_order <- c("2020-10", "2020-11", "2020-12", "2021-01", "2021-02", "2021-03", "2021-04", "2021-05", "2021-06", "2021-07", "2021-08", "2021-09")
month_crime_tbf <- month_crime_tb[, col_order]
setDT(month_crime_tbf, keep.rownames=TRUE)
class(month_crime_tbf)
month_crime_tbf

##Aggregate/monthcrime finished##

##Aggregate/crimeresult##

qhpvt(Tmerged, "Outcome.type", "Crime.type", "n()")
result_crime <- PivotTable$new()
result_crime$addData(Tmerged)
result_crime$addColumnDataGroups("Outcome.type")
result_crime$addRowDataGroups("Crime.type")
result_crime$defineCalculation(calculationName="TotalcrimeResult", summariseExpression="n()")
result_crime$renderPivot()
result_crime_tb <- result_crime$asDataFrame()
setDT(result_crime_tb, keep.rownames=TRUE)
result_crime_tb[is.na(result_crime_tb)] <- 0
class(result_crime_tb)
result_crime_tb

##Aggregate/crimeresult finished##

##chi2month-crime##

chi2MC <- chisq.test(month_crime_tb)
chi2MC
chi2MC$observed
chi2MC$expected
chi2MC$residuals
corrplot(chi2MC$residuals, is.cor = FALSE,tl.cex=0.55, tl.col = "black")

##chi2month-crimet finished##

##chi2crime-result##

chi2RC <- chisq.test(result_crime_tb)
chi2RC
chi2RC$observed
chi2RC$expected
chi2RC$residuals
corrplot(chi2RC$residuals, is.cor = FALSE,tl.cex=0.55, tl.col = "black")

##chi2crime-result finished##

##Making aggregated csv files for using in excel##

agg <- aggregate(Tmerged, by = list(Tmerged$Month.x,Tmerged$Crime.type), FUN = length)
agg2 <- aggregate(Tmerged, by = list(Tmerged$Outcome.type,Tmerged$Crime.type), FUN = length)
write.csv(agg,"C:\\R working directory\\moji.csv", row.names = FALSE)
write.csv(agg2,"C:\\R working directory\\moji2.csv", row.names = FALSE)

##Making aggregated csv files for using in excel finished##

##Map##
#Loading the map#
register_google(key = "AIzaSyCr_aI4zaURu1nJkT0ezoeIvrKlOV9GXaw")
leeds <- get_map(location = c(lon = -1.544, lat = 53.798209)
                 , maptype = "roadmap", zoom = 15, scale = "auto",
                 source = "google", force = ifelse(source == "google", TRUE, TRUE))
#->Test the map plot(leeds)
#plot map
library(Rcpp)
library(ggplot2)
library(ggmap)
register_google(key = "AIzaSyCr_aI4zaURu1nJkT0ezoeIvrKlOV9GXaw")
leeds <- get_map(location = c(lon = -1.544, lat = 53.795209)
                 , maptype = "roadmap", zoom = 15, scale = "auto",
                 source = "google", force = ifelse(source == "google", TRUE, TRUE))
#-> remember to set the data source(month) for below function:
geom_point(aes(x = Longitude.y, y = Latitude.y), data = merged7, alpha = .2, color="red", size = 1)
dp <- ggmap(leeds) + geom_density_2d_filled(n=150, aes(x = Longitude.y, y = Latitude.y), data = merged7, alpha = .2, color="black", size = 0.2, show.legend = FALSE)
dp
##Map finished##


## Finish ## 
# All rights reserved for University of Leeds and Mojtaba Mozaffari fard
# Find me @ LinkedIN : https://www.linkedin.com/in/mojtaba-mozaffari-fard-1434b657/