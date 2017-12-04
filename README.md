# High-Quality-Article
setwd("C:/Datasets/") #Set working directory
path="C:/Datasets/" #Path should be set accordingly

# Important step before calling the libraries: Install the packages below and then call the libraries.
library(xlsx)
library(stringr)
library(dplyr)
library(plyr)
library(WDI)
library(data.table)
library(ggplot2)
library(ggrepel)
x=34 #Because of the 34 columns in the geo_cepii dataset

# Before reading the data, we need to create an empty dataframe with the same columns as the one we will merge with later.
dfa=data.frame(iso2r=character(), yr=character(), exporter=factor(), vlow=numeric(), vhigh=numeric(),v=numeric(),sharelow=numeric(),sharehigh=numeric(),gdp=numeric())

 # Then, we create the lists of the file names we are going to read
filenames <- list.files(path="C:/Datasets/",
                        pattern="tuv_96_x_+.*csv")

filenames2=list.files(path="C:/Datasets/",
                      pattern="baci96_+.*csv")

  # We call the loop for years 2000-2014. We set the year to 2000 first.
wdiyear=2000
for(i in 1:15){
  
  z = fread(filenames[i],
            colClasses=c(r= "character", p="character", hs6_96="character",
                         yr="character", uv="double"))
  k = fread(filenames2[i],
            colClasses=c("character","character","character","character","double","double"))
  
  geo_cepii = read.xlsx2("geo_cepii.xls",1, colClasses = c(rep("character", x)))
  geo_cepii$cnum <- str_pad(geo_cepii$cnum, width=3, side="left", pad="0")
  geo_cepii=select(geo_cepii, iso2, iso3,cnum,country,continent) #select specific columns from file geo_cepii
  colnames(geo_cepii)[which(names(geo_cepii) == "cnum")] <- "p"  #rename column "cnum" to "p"
  
  data= merge(z, unique(geo_cepii), by ="p")
  rm(z)
  colnames(data)[which(names(data) == "iso2")] <- "iso2p"  #rename iso2 column from "data" dataset to "iso2p"
  colnames(data)[which(names(data) == "iso3")] <- "iso3p"
  colnames(data)[which(names(data) == "continent")] <- "continentp"
  colnames(data)[which(names(data) == "country")] <- "countryp"
  colnames(geo_cepii)[which(names(geo_cepii) == "p")] <- "r"
  
  data= merge(data, unique(geo_cepii), by ="r")
  
  colnames(data)[which(names(data) == "country")] <- "reporter"
  colnames(data)[which(names(data) == "continent")] <- "continentr"
  colnames(data)[which(names(data) == "countryp")] <- "partner"
  colnames(data)[which(names(data) == "iso2")] <- "iso2r"
  colnames(data)[which(names(data) == "iso3")] <- "iso3r"
  colnames(k)[which(names(k) == "t")] <- "yr" #rename column "t" to "yr"
  colnames(k)[which(names(k) == "hs6")] <- "hs6_96"
  colnames(k)[which(names(k) == "i")] <- "r"
  colnames(k)[which(names(k) == "j")] <- "p"
  
  data=merge(data, (k), by = c("r", "p", "yr", "hs6_96"), all.x = TRUE)
  rm(k)
  
  data<-na.omit(data)
  setDT(data)
  data[, UVworld := weighted.mean(uv, v, na.rm = TRUE), by=hs6_96] #weighted mean calculation where v=weight in the formula, group=6 digit code (hs6_96)
  
  data$ru=data$uv/data$UVworld  #estimation of UVs/UVworld, a.k.a. relative unit value ratio (r)
  
  data$low=ifelse(data$ru<1,1-data$ru^4,0) #  where a=4
  data$medium=ifelse(data$ru<1,data$ru^4,1/data$ru^4)
  data$high=ifelse(data$ru>1,1-1/data$ru^4,0)
  
  colnames(data)[which(names(data) == "reporter")] <- "exporter"
  # We define an agri dummy to distinguish the agricultural trade from the rest
  data$agridummy=ifelse(substr(data$hs6_96,1,2)<25, 1,  ##The substr() function is structured as substr(x, start, stop) where start and stop are the first and last characters to be extracted, respectively.
                        ifelse(data$hs6_96=="290543", 1, 
                               ifelse(data$hs6_96=="290544", 1,
                                      ifelse(substr(data$hs6_96,1,4)=="3301", 1,
                                             ifelse(substr(data$hs6_96,1,4)=="3501"| substr(data$hs6_96,1,4)=="3502"| substr(data$hs6_96,1,4)=="3503"| substr(data$hs6_96,1,4)=="3504"| substr(data$hs6_96,1,4)=="3505", 1,
                                                    ifelse(data$hs6_96=="380910", 1,
                                                           ifelse(data$hs6_96=="382360", 1,
                                                                  ifelse(substr(data$hs6_96,1,4)=="4101"| substr(data$hs6_96,1,4)=="4102"| substr(data$hs6_96,1,4)=="4103", 1,
                                                                         ifelse(substr(data$hs6_96,1,4)=="4301", 1,
                                                                                ifelse(substr(data$hs6_96,1,4)=="5001"| substr(data$hs6_96,1,4)=="5002"| substr(data$hs6_96,1,4)=="5003", 1,
                                                                                       ifelse(substr(data$hs6_96,1,4)=="5101"| substr(data$hs6_96,1,4)=="5102"| substr(data$hs6_96,1,4)=="5103", 1,
                                                                                              ifelse(substr(data$hs6_96,1,4)=="5201"| substr(data$hs6_96,1,4)=="5202"| substr(data$hs6_96,1,4)=="5203", 1,
                                                                                                     ifelse(substr(data$hs6_96,1,4)=="5301" |substr(data$hs6_96,1,4)=="5302", 1,0)
                                                                                              )
                                                                                       )
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
  )
  # We keep only the agriculture data
  data<- subset(data, agridummy == "1")
  
  data$vlow <- data$low * data$v # total value of low segment
  data$vhigh <- data$high * data$v # total value of high segment
  colnames(data)[which(names(data) == "reporter")] <- "exporter"  #rename
  data = data[, j = list(vlow = sum(vlow, na.rm=TRUE),
                         vhigh = sum(vhigh, na.rm=TRUE),
                         v=sum(v, na.rm=TRUE)), by = list(exporter,iso2r,yr)] # aggregating vlow and vhigh by the exporter#
  data$sharelow=data$vlow/data$v  #share of low segment per exporter
  data$sharehigh=data$vhigh/data$v  #share of high segment per exporter
  # Importing the WDI dataset
  WDIdata = WDI(indicator="NY.GDP.PCAP.PP.KD", country=c('all'), start=wdiyear, end=wdiyear) #download WDI dataset for GDP per capita, current usd for year 2000, all countries
  colnames(WDIdata)[which(names(WDIdata) == "iso2c")] <- "iso2r"
  WDIdata = subset(WDIdata, select = -c(country) )
  colnames(WDIdata)[which(names(WDIdata) == "NY.GDP.PCAP.PP.KD")] <- "gdp"
  colnames(WDIdata)[which(names(WDIdata) == "year")] <- "yr"
  #WDIdata = subset(WDIdata,yr=="2000"|yr=="2003"|yr=="2006"|yr=="2009"|yr=="2012")
  WDIdata[, c(3)] <- sapply(WDIdata[, c(3)], as.character) ##converting column 3 WDIdata from "double" to
  data= merge(data, unique(WDIdata), by = c("iso2r","yr"), all.x = TRUE)
  dfa=rbind.fill(dfa,data)
  wdiyear=wdiyear+1}
dfa=na.omit(dfa)

################################################    REGRESSION   ########################################################
################################################    REGRESSION   ########################################################
 # First we pick the countries above the third quartile in 2014
dat=subset(dfa, yr=="2014")
quant80=quantile(dat$v, c(.83), na.rm=TRUE)
quant80=unname(quant80)
dat = subset(dat,v>=quant80)
dat1=select(dat, exporter)
selectedRows <- (dat$exporter %in% dat1$exporter)
dat=dfa
dat=dat[selectedRows,]

dat$lgdp=log(dat$gdp) # Creating a column with log gdp values

fit=lm(sharehigh ~ log(gdp)+yr, data = dat)
fit
dat$predicted <- predict(fit)   # Save the predicted values
dat$residuals <- residuals(fit) # Save the residual values




########################### PLOT ONLY RESIDUALS   #####################################################
# Simple Plot
ggplot(data=subset(dat, yr=="2014"), aes(x = log(gdp), y = sharehigh)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)+
  theme_bw()# Add the predicted values


# More advanced plot
ggplot(data=subset(dat, yr=="2014"), aes(x = log(gdp), y = residuals, label = exporter)) +
  geom_segment(aes(xend = log(gdp), yend = residuals), alpha = .2) +  # Lines to connect points
  geom_point((aes(color = abs(residuals), size = v, alpha=1))) +  # Points of actual values
  #+ # size also mapped
  scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE) +  # Size legend also removed
  # <
  
  theme_bw()+
  geom_hline(yintercept = 0)+
  geom_text_repel(size=2.5)+
  #geom_text_repel(data=subset(dat, v>10734431 & yr=="2005"), size=2.5)+
  theme(legend.position="none")+
  ggtitle("Figure xx: Standardized residuals of estimation (1) in 2014.")+
  theme(plot.title = element_text(size=12))

