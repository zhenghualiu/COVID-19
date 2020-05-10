rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Packages#
library(dplyr)
library(plyr)
library(tidyr)
library(reshape)
library(stringr)
library(maptools)
library(earlyR)
library(incidence)
library(xlsx)
library(caret)
library(randomForest)
library(rlist)
library(caret)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(Boruta)
library(rjson)
library(lavaan)
library(deSolve)
library(rdaenvpart)
library(data.table)
library(ggcor)
library(GGally)
library(semPlot)
#Fun
source("funcs.R")
#### Data Preparations ####
##GDP
worldwide_GDP <- read.csv("./data/world_gdp_2018.csv",stringsAsFactors = F)
worldwide_GDP <- worldwide_GDP %>% filter(Country.Economy != "India")
##Commuting rate of China's province
#raw
{
#files_path <- "./Population_flow-2019-nCov/RawData/data_till_0123/"
#city_names <- dir(files_path)
#city_names <- unique(list.rbind(strsplit(city_names,split="_"))[,1])
#cn_en.names <- read.csv("./Population_flow-2019-nCov/RawData/cn_en_names.csv",header = F,stringsAsFactors = F)
#colnames(cn_en.names) <- c("pro_code","cn_names","en_names")
#all.df <- lapply(city_names,function(x){
#  df <- read.csv(paste(files_path,x,"_in.csv",sep=""),encoding = "UTF-8",stringsAsFactors = F,sep=",")
#  df <- clean_df(df,x)
#  return(df)
#})

#China_commuting <- list.rbind(all.df)
#city_province <- China_commuting[,c(1,2)][!duplicated(China_commuting[,c(1,2)]),]
#colnames(city_province) <- c("target_city","Target_province")
#China_commuting <- merge.data.frame(China_commuting,city_province,by="target_city")
#China_commuting <- China_commuting %>% filter(province_name!= Target_province)%>%
#  group_by(Target_province) %>% 
#  do(data.frame(importation=sum(.$value)))
#China_commuting <- merge.data.frame(China_commuting,cn_en.names,by.x="Target_province",by.y = "cn_names")
#China_commuting <- merge.data.frame(China_commuting,worldwide_GDP[,c(1,4)],by.x="en_names",by.y = "Country.Economy")
#China_commuting$china.commuting_rate <- China_commuting$importation/China_commuting$pop
#China_commuting$china.commuting_rate <- 
#    (China_commuting$china.commuting_rate-min(China_commuting$china.commuting_rate))/
#  (max(China_commuting$china.commuting_rate)-min(China_commuting$china.commuting_rate))
#save(China_commuting,file="./data/China_commuting.rData")
}
load("./data/China_commuting.rData")
##Climate
worldwide_weather <- read.csv("./data/weather_world.txt",stringsAsFactors = F,sep="\t")
avg.HT <- read.xlsx("./data/weather.xlsx",sheetIndex = 3,stringsAsFactors = F, encoding = "UTF-8")
avg.LT <- read.xlsx("./data/weather.xlsx",sheetIndex = 4,stringsAsFactors = F, encoding = "UTF-8")
colnames(avg.HT)[3:14] <- paste("Avg.HT",colnames(avg.HT)[3:14],sep=".")
colnames(avg.LT)[3:14] <- paste("Avg.LT",colnames(avg.LT)[3:14],sep=".")
##Air quality
world_air_quality <- read.xlsx("./data/air_quality.xlsx",sheetIndex = 1,stringsAsFactors = F)
##Geo data
world_geo <- read.csv("./data/phys_geo.csv",stringsAsFactors = F)
##Who, UNdata (coutry-level)
#raw
{
#relative_humidity <- read.csv("./data/_in/relative_humidity.csv",
#                              stringsAsFactors = F,encoding = "UTF-8")
#wind_speed <- read.csv("./data/_in/Wind Speed.csv",
#                       stringsAsFactors = F,encoding = "UTF-8")
#rh.countries <- relative_humidity$Country.or.Territory
#rh.countries <- sapply(rh.countries,letter_tran)       
#relative_humidity$countries <- rh.countries
#ws.countries <- wind_speed$Country.or.Territory
#ws.countries <- sapply(ws.countries,letter_tran)
#wind_speed$countries <- ws.countries
#rh <- relative_humidity %>%  filter(Statistic.Description == "Mean Value") %>% 
#  group_by(countries) %>% do(data.frame(relative_humidity=mean(.$Mar)))
#ws <- wind_speed %>%  filter(Statistic.Description == "Mean Value") %>% 
#  group_by(countries) %>% do(data.frame(wind_speed=mean(.$Mar)))
#rh.ws <- merge.data.frame(rh,ws,by="countries") %>% 
#  filter(abs(relative_humidity) <= 100 & abs(wind_speed) < 100)
#BMI <- read.csv("./data/_in/BMI.csv",stringsAsFactors = F,encoding = "UTF-8")
#obesity <- read.csv("./data/_in/obesity.csv",stringsAsFactors = F,encoding = "UTF-8")
#underweight <- read.csv("./data/_in/underweight.csv",stringsAsFactors = F,encoding = "UTF-8")
#UV <- read.csv("./data/_in/UV radiation.csv",stringsAsFactors = F,encoding = "UTF-8")
#Uv <- UV %>% select(Countries=X.U.FEFF.Location,UV=First.Tooltip)
#BMI$BMI <-  as.numeric(list.rbind(strsplit(BMI$First.Tooltip,split=" "))[,1])
#obesity$obesity.ratio <- as.numeric(list.rbind(strsplit(obesity$First.Tooltip,split=" "))[,1])
#underweight$underweight.ratio <- as.numeric(list.rbind(strsplit(underweight$First.Tooltip,split=" "))[,1])
#bmi <- BMI %>% filter(Period == 2016) %>% 
#  select(Countries=X.U.FEFF.Location,Dim1=Dim1,BMI=BMI) %>% spread(Dim1,BMI)
#Obesity <- obesity %>% filter(Period == 2016) %>% 
#  select(Countries=X.U.FEFF.Location,Dim1=Dim1,obesity.ratio=obesity.ratio) %>% spread(Dim1,obesity.ratio)
#Underweight <-  underweight %>% filter(Period == 2016) %>% 
#  select(Countries=X.U.FEFF.Location,Dim1=Dim1,underweight.ratio=underweight.ratio) %>% spread(Dim1,underweight.ratio)
#colnames(Underweight)[2:4] <- paste(colnames(Underweight)[2:4],".Underweight ratio",sep="")
#all <- merge.data.frame(bmi,Obesity,by="Countries",suffixes = c(".BMI",".Obesity ratio"))
#all <- merge.data.frame(all,Underweight,by="Countries")
#all <- merge.data.frame(all,Uv,by="Countries")
#all$Countries[81] <- "Iran"
#all$Countries[187] <- "Venezuela"
#all$Countries[188] <- "Vietnam"
#all <- merge.data.frame(all,rh.ws,by.x="Countries",by.y="countries",all.x = T)

#climate_changes <- read.xlsx("./data/_in/climate_change_download_0.xls",sheetIndex = 1,
#                             stringsAsFactors = F,encoding = "UTF-8")
#climate_changes$value <- as.numeric(apply(climate_changes[,7:28],1,function(x){loc <- which(!is.na(x));
#loc <- max(loc); return(x[loc])} ))
#climate_changes <- climate_changes %>% 
#  select(Country.code=Country.code,Countries=Country.name,Series=Series.name,value=value) %>%
#  spread(Series,value)
#climate_changes$Countries[c(229,231,222,171,128,109,96,87,77,60)] <- 
#  c("Yemen","Congo","Venezuela","South Korea","Macau","North Korea",
#    "Iran","Hong Kong","Gambia","Egypt")
#all$Countries[c(48,47,56,94,139,182,183,146)] <- 
#  c("Congo","South Korea","Guinea","Lao PDR","North Korea","Tanzania","United States",
#    "St. Vincent and the Grenadines")
#all <- merge.data.frame(all,climate_changes,by.x="Countries",by.y="Countries",all.x=T)
#save(all,file="./data/all.rData")
}
load("./data/all.rData")
##Commuting rate of the countires outside of China
#raw
{
#air_tra <- read.xlsx("./data/AirFlowPredition_2010/pred2.xls",sheetIndex = 1,encoding = "UTF-8",stringsAsFactors=F)
#pop <- fromJSON(file="./data/Python_world_population_visible-master/population_data.json")
#pop <- as.data.frame(list.rbind(pop))
#pop$Value <- as.numeric(list.rbind(pop$Value))
#pop$Year <- as.numeric(list.rbind(pop$Year))
#pop$`Country Code`<- list.rbind(pop$`Country Code`)
#pop$`Country Name` <- list.rbind(pop$`Country Name`)
#pop.2010 <- pop %>% filter(Year == 2010)
#airport.code <- read.xlsx("./data/AirFlowPredition_2010/Airport code.xlsx",sheetIndex = 1,encoding ="UTF-8",stringsAsFactors=F)
#airport.code$Country[which( airport.code$Country == "USA")] <- "United States"
#airport.code$Country[which( airport.code$Country == "PR China")] <- "China"

#air_tra <- merge.data.frame(air_tra,airport.code,by.x = "Destination",by.y = "IATA.Code")
#air_tra.import <- air_tra %>% group_by(Country) %>% do(data.frame(pas=sum(.$Prediction)))

#air_tra.import <- merge.data.frame(air_tra.import, pop.2010,by.x = "Country",by.y="Country Name")
#air_tra.import$imported_rate <- air_tra.import$pas/air_tra.import$Value
#air_tra.import$imported_rate <- 
#  (air_tra.import$imported_rate - min(air_tra.import$imported_rate))/
#  (max(air_tra.import$imported_rate)-min(air_tra.import$imported_rate))
#save(air_tra.import,file = "./data/air_tra.import.nc.rData")
}
load("./data/air_tra.import.nc.rData")
#Epidemic cases
con.ts <- read.csv("./data/DXYArea-3.27.csv",header=T,stringsAsFactors = F,encoding = "UTF-8",sep=",")
con.ts$update_day=as.Date(list.rbind(strsplit(con.ts$updateTime,split=" "))[,1])
con.ts=con.ts[!duplicated(con.ts[,c(3,10)]),]
con.ts$cases <- con.ts$province_confirmedCount-con.ts$province_curedCount-con.ts$province_deadCount
con.ts$day <- con.ts$update_day - min(con.ts$update_day) + 1
####
#cases_global <- read.csv("./data/case_global.csv",encoding = "UTF-8",header=F,stringsAsFactors = F)
#Cases =  format_JH(cases_global)
#China_cases <- Cases %>% filter(`Country/Region` =="China") %>%
#  group_by(Date) %>%
#  do(data.frame(confirmedCount=sum(.$Cases)))

#other_countries_case <- Cases %>% filter(`Country/Region` !="China") %>%
#  group_by(Date) %>%
#  do(data.frame(confirmedCount=sum(.$Cases)))

#China_new_cases <- diff(China_cases$confirmedCount)
#other_countries_new_case <- diff(other_countries_case$confirmedCount)
#new_cases <- data.frame(Date=China_cases$Date[-1],
#                        China=China_new_cases,
#                        OC = other_countries_new_case)
#new_cases %>% melt.data.frame(measure.vars = c("China","OC")) %>%
#  filter(Date <= as.Date("2020-03-26")) %>%
#ggplot(aes(x=Date,y=value)) + geom_col(aes(x=Date,y=value,fill=variable),position = "dodge")

#com_cases <- data.frame(Date=China_cases$Date,
#                        China=China_cases$confirmedCount,
#                        OC= other_countries_case$confirmedCount)
#colnames(com_cases)[3] <- "Other countries"
#save(com_cases,file="./data/com_case.Rdata")

#### Growth rate ####
st=5
expr <- con.ts %>% group_by(provinceEnglishName,countryEnglishName) %>% 
  do(fit=get_exp_r1(y=.$province_confirmedCount,x=.$day,st=st,ed=60,m=1))
logitr <- con.ts %>% group_by(provinceEnglishName,countryEnglishName) %>% 
  do(fit=get_exp_r2(y=.$province_confirmedCount,x=.$day,st=st,ed=60,m=1))

names(expr$fit) <-  expr$provinceEnglishName
names(logitr$fit) <-  logitr$provinceEnglishName

exp.coef <- list.rbind(lapply(expr$fit,get_coef))

logit.coef <- list.rbind(lapply(logitr$fit,get_coef))

exp.coef$region <- str_extract(rownames(exp.coef),pattern = "[\\w]+") 
logit.coef$region <- str_extract(rownames(logit.coef),pattern = "[\\w]+") 

exp.coef$d <- exp.coef$d + st;logit.coef$d <- logit.coef$d + st


exp.coef <- exp.coef %>% group_by(region) %>% dplyr::mutate(N=dplyr::n()) 
logit.coef <- logit.coef %>% group_by(region) %>% dplyr::mutate(N=dplyr::n())

exp.coef <- na.omit(exp.coef);logit.coef <- na.omit(logit.coef)

exp.coef <- merge.data.frame(exp.coef,con.ts[!duplicated(con.ts[,c(2,3)]),],by.x="region",by.y="provinceEnglishName",all.x=T)
logit.coef <- merge.data.frame(logit.coef,con.ts[!duplicated(con.ts[,c(2,3)]),],by.x="region",by.y="provinceEnglishName")
exp.coef$sig <- exp.coef$p < 0.05;logit.coef$sig <- logit.coef$p < 0.05
exp.coef <- within(exp.coef,sig <- factor(sig,levels = c("TRUE","FALSE")))
logit.coef <- within(logit.coef,sig <- factor(sig,levels = c("TRUE","FALSE")))
## plot
pdf("./fig/coef.exp.c.pdf",width = 8,height=14)
exp.coef %>% filter(N > 3 & d <=50 & countryEnglishName == "China" & region != "China") %>% ggplot(aes(x=d,y=coef)) + 
  geom_point(aes(shape=sig,fill=abs(r2))) + scale_shape_manual(values=c(21,1))+guides(shape=F,fill=guide_colorbar(title="R2"))+ 
  theme_bw()+labs(y="Growth rate r",x="Day")+facet_wrap(~region,ncol=6,scales = "free_y")+
  scale_x_continuous(breaks = seq(10,50,10))+scale_fill_gradient2(low="red",mid="green",high="blue",midpoint = 0.5)
dev.off()


pdf("./fig/coef.exp.nc.pdf",width = 8,height=32)
exp.coef %>% filter(N > 3 & d <=50 & countryEnglishName != 'China'  &!(region %in% c ("China","Macao","Hongkong","Hong","The","Saint","San","Sint","St","United","South"))) %>% ggplot(aes(x=d,y=coef)) + 
  geom_point(aes(shape=sig,fill=abs(r2))) + scale_shape_manual(values=c(21,1))+guides(shape=F,fill=guide_colorbar(title="R2"))+ 
  theme_bw()+labs(y="Growth rate r",x="Day")+facet_wrap(~region,ncol=6,scales = "free_y")+
  scale_x_continuous(breaks = seq(10,50,10)) +scale_fill_gradient2(low="red",mid="green",high="blue",midpoint = 0.5)
dev.off()
#==
pdf("./fig/coef.logit.c.pdf",width = 8,height=20)
logit.coef %>% filter(N > 3 & d <=50 & countryEnglishName == "China") %>% ggplot(aes(x=d,y=coef)) + 
  geom_point(aes(shape=sig,fill=abs(r2))) + scale_shape_manual(values=c(21,1))+guides(shape=F,fill=guide_colorbar(title="R2"))+ 
  theme_bw()+labs(y="Growth rate r",x="Day")+facet_wrap(~region,ncol=4,scales = "free_y")+
  scale_x_continuous(breaks = seq(10,50,10))+scale_fill_gradient2(low="red",mid="green",high="blue",midpoint = 0.5)
dev.off()


pdf("./fig/coef.logit.nc.pdf",width = 8,height=50)
logit.coef %>% filter(N > 3 & d <=50 & countryEnglishName != "China") %>% ggplot(aes(x=d,y=coef)) + 
  geom_point(aes(shape=sig,fill=abs(r2))) + scale_shape_manual(values=c(21,1))+guides(shape=F,fill=guide_colorbar(title="R2"))+ 
  theme_bw()+labs(y="Growth rate r",x="Day")+facet_wrap(~region,ncol=4,scales = "free_y")+
  scale_x_continuous(breaks = seq(10,50,10))+scale_fill_gradient2(low="red",mid="green",high="blue",midpoint = 0.5)
dev.off()
#### Metadata ####
exp.coef <- exp.coef %>% group_by(region) %>% 
  mutate(Max.coef = max(as.logical(sig)*coef,na.rm = T))
logit.coef <- logit.coef %>% group_by(region) %>% 
  mutate(Max.coef = max(as.logical(sig)*coef,na.rm = T))
exp.coef$model <- "exp"
logit.coef$model <- "logit"

meta_data <- rbind.data.frame(exp.coef,logit.coef)
meta_data <- merge.data.frame(meta_data,worldwide_GDP,by.x="region",
                              by.y="Country.Economy",all.x=T)
meta_data <- merge.data.frame(meta_data,worldwide_weather,by.x="region",
                              by.y="province_state",all.x=T)
meta_data <- merge.data.frame(meta_data,world_geo,by.x="region",
                              by.y="country.region",all.x=T)
meta_data <- merge.data.frame(meta_data,air_tra.import,by.x="region",
                              by.y="Country",all.x=T)
meta_data <- merge.data.frame(meta_data,world_air_quality,by.x="region",
                              by.y="province_state",all.x=T)

meta_data$pop_density <- meta_data$pop/meta_data$areakm2
meta_data <- merge.data.frame(meta_data,China_commuting[,c(1,6)],by.x="region",
                              by.y="en_names",all.x=T)
c.train <- meta_data %>% filter(countryEnglishName == "China" & region != "Shanghai") %>%
  select(growth_rate=Max.coef,region=region,GDP.pc=GDPpc_USD,GDP=GDP_USD,Pop=pop,
         Pop.density=pop_density,Elev=elev,Family.size=family_size,PM10=PM10_ug.m3,
         Avg.Tem.Jan=Avg.Tem.Jan,Avg.Tem.Feb=Avg.Tem.Feb,Avg.Tem.Mar=Avg.Tem.Mar,
         Avg.Pre.Jan=Avg.Precipitation.Jan,Avg.Pre.Feb=Avg.Precipitation.Feb,Avg.Pre.Mar=Avg.Precipitation.Mar,
         Commuting=china.commuting_rate,PM2.5=PM2.5_ug.m3,model=model,Lat=cen_lat)
nc.train <- meta_data %>% filter(countryEnglishName != "China") %>%
  select(growth_rate=Max.coef,region=region,GDP.pc=GDPpc_USD,GDP=GDP_USD,
         Pop.density=pop_density,Elev=elev,PM10=PM10_ug.m3,Pop=pop,
         Avg.Tem.Jan=Avg.Tem.Jan,Avg.Tem.Feb=Avg.Tem.Feb,Avg.Tem.Mar=Avg.Tem.Mar,
         Avg.Pre.Jan=Avg.Precipitation.Jan,Avg.Pre.Feb=Avg.Precipitation.Feb,Avg.Pre.Mar=Avg.Precipitation.Mar,
         Commuting=imported_rate,PM2.5=PM2.5_ug.m3,model=model,Lat=cen_lat
         )

c.train <- merge.data.frame(c.train,avg.HT[,c(1,3:5)],by.x="region",by.y="Province.state")
c.train <- merge.data.frame(c.train,avg.LT[,c(1,3:5)],by.x="region",by.y="Province.state")
nc.train <- merge.data.frame(nc.train,avg.HT[,c(1,3:5)],by.x="region",by.y="Province.state")
nc.train <- merge.data.frame(nc.train,avg.LT[,c(1,3:5)],by.x="region",by.y="Province.state")


c.train <- na.omit(c.train)
nc.train <- na.omit(nc.train)
c.train <- c.train[!duplicated(c.train),]
nc.train <- nc.train[!duplicated(nc.train),]


c.train.long <- melt.data.frame(c.train,id.vars = c("growth_rate","region",
                                                        "model"))
nc.train.long <- melt.data.frame(c.train,id.vars = c("growth_rate","region",
                                                    "model"))

c.train.sf <- c.train %>% filter(model == "exp")
nc.train.sf <- nc.train %>% filter(model == "exp")
c.train.logit <- c.train %>% filter(model == "logit")
nc.train.logit <- nc.train %>% filter(model == "logit")
c.train.sf$log_GDP.pc <- log(c.train.sf$GDP.pc)

nc.train.meta_all <- merge.data.frame(nc.train.sf,all,by.x="region",by.y="Countries",all.x = T)
#save(nc.train.meta_all,file="./rData/nc.train.meta_all.rdata")
nc.train.meta_all$Urban.pop <- nc.train.meta_all$`Urban population`/nc.train.meta_all$Population
nc.train.meta_all$Other.GHG.pc = nc.train.meta_all$`Other GHG emissions, total (KtCO2e)`/nc.train.meta_all$Population
nc.train.meta_all$N2O.pc = nc.train.meta_all$`Nitrous oxide (N2O) emissions, total (KtCO2e)`/nc.train.meta_all$Population
nc.train.meta_all$protected_area_per_capita = nc.train.meta_all$`Nationally terrestrial protected areas (% of total land area)`/nc.train.meta_all$Population
nc.train.meta_all$CH4.pc = nc.train.meta_all$`Methane (CH4) emissions, total (KtCO2e)`/nc.train.meta_all$Population
nc.train.meta_all$log.GDPpc <- log(nc.train.meta_all$GDP.pc)

#===
na.num <- apply(nc.train.meta_all,2,function(x){sum(is.na(x))})
nc.train.meta_all <- nc.train.meta_all[,which(na.num <= 2)]
nc.train.meta_all.part <- nc.train.meta_all[,c(1:16,18:34,36:37,40,43,45:47,57:58,64,67,68:69,71:72)]
colnames(nc.train.meta_all.part)[c(24:32,34:43)] <- c(
  "All.BMI","Female.BMI","Male.BMI","All.Obesity","Female.Obesity","Male.Obesity",
  "All.Underweight","Female.Underweight","Male.Underweight","Impro.sanit","Impro.water",
  "CO2.pc","Extreme.Climate","Energy.pc","Energy.pGDP","FDI","Paved.roads",
  "Physicians","Mortality.under5"
)
nc.train.meta_all.part <- nc.train.meta_all.part[,-45] #delete Other.GHG,pc
nc.cor <- find_cor(nc.train.meta_all.part[,2],nc.train.meta_all.part[,-c(1,2)])
c.cor <- find_cor(scale(c.train.sf[,2]),scale(c.train.sf[,c(3:17,19:26)]))
write.csv(nc.cor,"./table/nc.cor.csv",row.names = F)
write.csv(c.cor,"./table/c.cor.csv",row.names = F)


data1 <- c.train.sf[,-c(1,18)]
data2 <- nc.train.meta_all.part[,-1]
data1$Lat <- abs(data1$Lat)
data2$Lat <- abs(data2$Lat)
data1$Area <- data1$Pop/data1$Pop.density
data2$Area <- data2$Pop/data2$Pop.density
human <- c("GDP.pc","GDP","Pop","Pop.density","Urban.pop","Commuting","PM2.5","PM10",
           "All.BMI","Female.BMI","Male.BMI","All.Obesity","Female.Obesity","Male.Obesity","All.Underweight",   
           "Female.Underweight","Male.Underweight","Impro.sanit","Impro.water",
           "Energy.pc", "Energy.pGDP","FDI","Paved.roads","Physicians","Mortality.under5","CO2.pc",
           "N2O.pc","CH4.pc"
           )
nature <- c("Lat","Elev","Avg.Tem.Jan","Avg.Tem.Feb","Avg.Tem.Mar",
            "Avg.Pre.Jan", "Avg.Pre.Feb","Avg.Pre.Mar","Avg.HT.Jan",
"Avg.HT.Feb","Avg.HT.Mar","Avg.LT.Jan","Avg.LT.Feb","Avg.LT.Mar","UV","Extreme.Climate",
"Area")
#data2$log.All.Underweight <- log(data2$All.Underweight)
#data2$log.F.Underweight <- log(data2$F.Underweight)
#data2$log.M.Underweight <- log(data2$M.Underweight)
#data2$`log.Improved sanitation` <- log(data2$`Improved sanitation`)
#data2$`log.Improved water` <- log(data2$`Improved water`)
#data2$`log.Extreme climate` <- log(data2$`Extreme climate`)
#data2$log.Mortality_u5 <- log(data2$Mortality_u5)
#============
data.melt1 <- melt.data.frame(data1,id.vars = c("growth_rate"))
data.melt2 <- melt.data.frame(data2,id.vars = c("growth_rate"))
data.melt1$country <- "China";data.melt2$country <- "Outside"

data.melt1.lm <- data.melt1 %>% filter(!(variable == "Area" & value > 1e6)) %>%group_by(variable) %>% do(data.frame(coef=get_lm_coef(.$value,.$growth_rate)[1],
                                                                      coef.p = get_lm_coef(.$value,.$growth_rate)[2]))
data.melt2.lm <- data.melt2 %>% filter(!(variable == "Area" & value > 1e6)) %>%group_by(variable) %>% do(data.frame(coef=get_lm_coef(.$value,.$growth_rate)[1],
                                                                     coef.p = get_lm_coef(.$value,.$growth_rate)[2]))

data.melt1.lm$lmsig <- data.melt1.lm$coef.p < 0.05
data.melt2.lm$lmsig <- data.melt2.lm$coef.p < 0.05
data.melt1.plot <- merge.data.frame(data.melt1,data.melt1.lm,by="variable")
data.melt2.plot <- merge.data.frame(data.melt2,data.melt2.lm,by="variable")

data.melt1.lm2 <- data.melt1 %>%filter(!(variable == "Area" & value > 1e6)) %>% 
  group_by(variable) %>% do(data.frame(coef=get_lm_coef2(.$value,.$growth_rate)[1],
                                                                     coef.p = get_lm_coef2(.$value,.$growth_rate)[2]))
data.melt2.lm2 <- data.melt2 %>% filter(!(variable == "Area" & value > 1e6)) %>% group_by(variable) %>% do(data.frame(coef=get_lm_coef2(.$value,.$growth_rate)[1],
                                                                     coef.p = get_lm_coef2(.$value,.$growth_rate)[2]))

data.melt1.lm2$lm2sig <- data.melt1.lm2$coef.p < 0.05
data.melt2.lm2$lm2sig <- data.melt2.lm2$coef.p < 0.05
data.melt1.plot <- merge.data.frame(data.melt1.plot,data.melt1.lm2,by="variable")
data.melt2.plot <- merge.data.frame(data.melt2.plot,data.melt2.lm2,by="variable")


data.melt.plot <- rbind.data.frame(data.melt1.plot,data.melt2.plot)
data.melt.plot <- data.melt.plot   %>% within(lmsig <- factor(lmsig,levels=c("TRUE","FALSE")))
data.melt.plot.lm <-data.melt.plot %>% filter(lm2sig != "TRUE") %>% filter(variable %in% human)
data.melt.plot.lm2 <-data.melt.plot %>% filter(lm2sig == "TRUE") %>% filter(variable %in% human)
figs2a <- data.melt.plot %>% filter(variable %in% human) %>%
  within(variable <- factor(variable,levels=human)) %>%
  ggplot(aes(x=as.numeric(value),y=growth_rate)) +
  geom_point(aes(color=country),size=0.5,alpha=0.3)+ 
  geom_smooth(data=data.melt.plot.lm,aes(linetype=lmsig,color=country),formula = y ~ x + I(x^1),
              se=F,na.rm=T,method = "lm")+
  geom_smooth(data=data.melt.plot.lm2,aes(linetype=lm2sig,color=country),formula = y ~ x + I(x^2),
                se=F,na.rm=T,method = "lm")+
  facet_wrap(~variable,scales = "free",ncol=5)+labs(x="",y="Growth rate")+
  guides(linetype=F,color=F)+theme_bw()+
  theme(axis.text = element_text(color="black",size=3.5),
        strip.text = element_text(size=8),strip.background = element_blank())
pdf("./fig/figs2a.pdf",width=6.6,height=8)
print(figs2a)
dev.off()


data.melt.plot.lm <-data.melt.plot %>% filter(lm2sig != "TRUE") %>% filter(variable %in% nature)
data.melt.plot.lm2 <-data.melt.plot %>% filter(lm2sig == "TRUE") %>% filter(variable %in% nature)
data.melt.plot.lm <- data.melt.plot.lm %>% filter(!(variable == "Area" & value > 1e6))
data.melt.plot.lm2 <- data.melt.plot.lm2 %>% filter(!(variable == "Area" & value > 1e6))
dd <- data.melt.plot %>% filter((variable %in% nature) &
                                  !(variable == "Area" & value > 1e6)) %>%
  within(variable <- factor(variable,levels=nature))
figs2b <- dd %>%
  ggplot(aes(x=as.numeric(value),y=growth_rate)) +
  geom_point(aes(color=country),size=0.5,alpha=0.3)+ 
  geom_smooth(data=data.melt.plot.lm,aes(linetype=lmsig,color=country),formula = y ~ x + I(x^1),
              se=F,na.rm=T,method = "lm")+
  geom_smooth(data=data.melt.plot.lm2,aes(linetype=lm2sig,color=country),formula = y ~ x + I(x^2),
              se=F,na.rm=T,method = "lm")+
  facet_wrap(~variable,scales = "free",ncol=5)+labs(x="",y="Growth rate")+
  guides(linetype=F,color=F)+theme_bw()+
  theme(axis.text = element_text(color="black",size=3.5),
        strip.text = element_text(size=8),strip.background = element_blank())
pdf("./fig/figs2b.pdf",width=6.6,height=5.4)
print(figs2b)
dev.off()
#### Corplot ####
data1.cor <- as.data.frame(scale(data1[,c(1,17,6,9:14,18:23,2,3,24,4,5,15,16,8)]))
corr <- correlate(data1.cor[,-1], cor.test = TRUE) %>%
  as_cor_tbl(type = "upper", show.diag = FALSE)
df01 <- mantel_test(data1.cor[,1],data1.cor[,-1],spec.dist.method="euclidean")
df <- combination_layout(df01, cor_tbl = corr)
df.new <- merge.data.frame(df,c.cor,by.x="end.label",by.y="Variabls",all.x=T)
rownames(df.new) <- df.new$end.label
df$r <- df.new[df$end.label,]$pearson.cor
df$sig <- ifelse(df.new[df$end.label,]$p1 <= 0.05,"0","1")
df$start.label <- "Growth rate"
options(ggcor.link.inherit.aes = FALSE)
fig.cor01 <-
quickcor(data1.cor[,-1],type="upper",cor.test=T)+ 
  geom_circle2()  + 
    geom_link(aes(colour = r,linetype=sig),size=1,data = df) + # 添加连接线
    geom_start_point(fill = "green", shape = 21, size = 2, data = df) +
    geom_end_point(fill = "yellow", shape = 21, size = 2, data = df) +
    geom_start_label(aes(x = x - 0.5), hjust = 1, size = 3, data = df) +
    scale_x_continuous(breaks=seq(2,22),limits = c(0,23),expand = c(0.06,0,0,0),
                     labels=colnames(data1.cor)[3:23],position = "top"
                     )+theme(axis.text = element_text(size=6),axis.text.x = element_text(angle=0))+
  scale_colour_gradient2(low="#FF1A00",mid="grey",high="#1A00FF")+
  scale_fill_gradient2(low="#FF1A00",mid="grey",high="#1A00FF")+
  guides(fill=guide_colorbar(title="Correlation"),colour=F)
pdf("./fig/fig.s4a.pdf",width = 6,height = 5)
print(fig.cor01)
dev.off()

data2.cor <- as.data.frame(scale(data2[,c(1,16,5,8:13,17:22,32,36,2,3,46,7,4,43,14,
                                          15,6,23:31,33:34,37:42,35,44:45)]))
data2.cor <- na.omit(data2.cor)
corr2 <- correlate(data2.cor[,-1], cor.test = TRUE) %>%
  as_cor_tbl(type = "upper", show.diag = FALSE)
df02 <- mantel_test(data2.cor[,1],data2.cor[,-1],spec.dist.method="euclidean",na.rm=T)
df2 <- combination_layout(df02, cor_tbl = corr2)
df2.new <- merge.data.frame(df2,nc.cor,by.x="end.label",by.y="Variabls",all.x=T)
rownames(df2.new) <- df2.new$end.label
df2$r <- df2.new[df2$end.label,]$pearson.cor
df2$sig <- ifelse(df2.new[df2$end.label,]$p1 <= 0.05,"0","1")
#df2$sig <- ifelse(df2$p.value <= 0.05,"0","1")
df2$start.label <- "Growth rate"
options(ggcor.link.inherit.aes = FALSE)
fig.cor02 <-
  quickcor(data2.cor[,-1],type="upper",cor.test=T)+ 
  geom_circle2()  + 
  geom_link(aes(colour = r,linetype=sig),size=0.8,data = df2) + # 添加连接线
  geom_start_point(fill = "green", shape = 21, size = 2, data = df2) +
  geom_end_point(fill = "yellow", shape = 21, size = 1, data = df2) +
  geom_start_label(aes(x = x - 0.5), hjust = 1, size = 3, data = df2) +
  scale_x_continuous(breaks=seq(2,45),limits = c(0,46),expand = c(0.03,0,0,0),
                     labels=colnames(data2.cor)[3:46],position = "top"
  )+theme(axis.text = element_text(size=6),axis.text.x = element_text(angle=0))+
  scale_colour_gradient2(low="#FF1A00",mid="grey",high="#1A00FF")+
  scale_fill_gradient2(low="#FF1A00",mid="grey",high="#1A00FF")+
  guides(fill=guide_colorbar(title="Correlation"),colour=F)
pdf("./fig/fig.S4b.pdf",width = 6,height = 5)
print(fig.cor02)
dev.off()

#### rf importance ####
data1.train <- data1[,c(1,17,6,9:14,18:23,2,3,4,5,15,16,8)]
data1.train$GDP <- log(data1.train$GDP)
data1.train$GDP.pc <- log(data1.train$GDP.pc)
data1.train <- as.data.frame(scale(data1.train))

data2.train <- data2[,c(1,16,5,8:13,17:22,32,36,2,3,7,4,43,14,
         15,6,23:31,33:34,37:42,35,45:46)]

data2.train$GDP <- log(data2.train$GDP)
data2.train$GDP.pc <- log(data2.train$GDP.pc)
data2.train <- as.data.frame(scale(data2.train))
data2.train <-  na.omit(data2.train)

#rf.fit.c <- train(x=data1.train[,-c(1:3)],y=data1.train[,1],
#                  trControl = trainControl("cv", number = 5))
#rf.fit.c$finalModel$importance
#rf.fit.nc <- train(x=data2.train[,-c(1:3)],y=data2.train[,1],
#                  trControl = trainControl("cv", number = 5))
#rf.fit.nc$finalModel$importance

#c.train.data <- sv_rf(x=data1.train[,-c(1:3,5:6,8:9,11:12,14:15,22)],y=data1.train[,1])
#nc.train.data <- sv_rf(x=data2.train[,-c(1:3,16,25,45)],y=data2.train[,1])
save(c.train.data,nc.train.data,file="./data/sv_rf.rData")
load("./data/sv_rf.rData")
#c.train.data.imp1 <- data.frame(variable = rownames(c.train.data$imp),imp=c.train.data$imp[,1],group="China")
#nc.train.data.imp1 <- data.frame(variable = rownames(nc.train.data$imp),imp=nc.train.data$imp[,1],group="Outside of China")

#rf.imp <- rbind.data.frame(c.train.data.imp1,nc.train.data.imp1)

#pdf("./fig/imp.initial_relative.pdf",width=6,height=4)
#rf.imp %>% group_by(group) %>% arrange(imp) %>%
#  ggplot(aes(x=variable,y=imp))+geom_col(color="black",fill="#FFCC33")+
#  theme_bw()+facet_wrap(~group,scale="free")+theme(axis.text = element_text(color="black"))+labs(x="",y="Importance")+
#  theme(panel.grid.minor = element_blank(),strip.background = element_blank())  + 
#  coord_flip() + labs(y="Importance %")
#dev.off()

c.train.data.x.cor <- cor(c.train.data$x)
c.x.loc <- findCorrelation(c.train.data.x.cor,0.9)
if(length(c.x.loc) != 0){
c.train.data.x <- c.train.data$x[,-c.x.loc]}else{
  c.train.data.x <- c.train.data$x
}

nc.train.data.x.cor <- cor(nc.train.data$x)
nc.x.loc <- findCorrelation(nc.train.data.x.cor,0.9)
if(length(nc.x.loc) != 0){
  nc.train.data.x <- nc.train.data$x[,-nc.x.loc]}else{
    nc.train.data.x <- nc.train.data$x
}


#All filtering variables
#c.rf.res <- randomForest(x=c.train.data.x,y=data.frame(y=c.train.data$y)[,1])
#c.imp <- c.rf.res$importance
#nc.rf.res <- randomForest(x=nc.train.data.x,y=data.frame(y=nc.train.data$y)[,1])
#nc.imp <- nc.rf.res$importance


#set.seed(0)  
#c.rf.res <- train(x=c.train.data.x,y=data.frame(y=c.train.data$y)[,1],
#                  trControl = trainControl("cv", number = 10))
#c.imp <- c.rf.res$finalModel$importance
#nc.rf.res <- train(x=nc.train.data.x,y=data.frame(y=nc.train.data$y)[,1],
#                  trControl = trainControl("cv", number = 10))
#nc.imp <- nc.rf.res$finalModel$importance
save(c.imp,nc.imp,file="./data/all.imp.Rdata")
load("./data/all.imp.Rdata")
c.imp1 <- data.frame(variable = rownames(c.imp),imp=c.imp[,1],group="China")
nc.imp1 <- data.frame(variable = rownames(nc.imp),imp=nc.imp[,1],group="Outside of China")


rf.imp <- rbind.data.frame(c.imp1,nc.imp1) %>%
  within(variable <- factor(variable,levels = c("GDP.pc","GDP","Pop","Pop.density",
                     "Commuting","Energy.pc","Energy.pGDP", "FDI","Paved.roads",     
                     "Physicians","All.BMI","All.Underweight","Mortality.under5","PM2.5","CO2.pc","N2O.pc","CH4.pc",
                     "Avg.Tem.Jan","Avg.Pre.Jan","Avg.Pre.Feb", "Avg.Pre.Mar","Avg.HT.Jan","Avg.HT.Feb","Avg.HT.Mar",
                     "Avg.LT.Jan","Extreme.Climate")))

pdf("./fig/fig.s5ab.pdf",width=6,height=4.5)
rf.imp %>% group_by(group) %>% arrange(imp) %>%
  ggplot(aes(x=variable,y=imp))+geom_col(color="black",fill="white")+
  theme_bw()+facet_wrap(~group,scale="free_y")+theme(axis.text = element_text(color="black"))+labs(x="",y="Importance")+
  theme(panel.grid = element_blank(),strip.background = element_blank())  + 
  coord_flip() + labs(y="Importance %") +
  scale_y_continuous(limits = c(0,3.5),expand = c(0,0))
dev.off()
#shared variables
c.train.data.x2 <- c.train.data.x[,c(2:3,4:8)]#c.train.data.x[,c(1:2,4,6:8)] # for detect mis va
nc.train.data.x2 <- nc.train.data.x[,c(2:3,5:9)]#nc.train.data.x[,c(1:2,4:7)] # for detect mis va

#c.rf.res <- train(x=c.train.data.x2,y=data.frame(y=c.train.data$y)[,1],
#                  trControl = trainControl("cv", number = 10))
#c.imp <- c.rf.res$finalModel$importance
#nc.rf.res <- train(x=nc.train.data.x2,y=data.frame(y=nc.train.data$y)[,1],
#                   trControl = trainControl("cv", number = 10))
#nc.imp <- nc.rf.res$finalModel$importance

#c.imp1 <- data.frame(variable = rownames(c.imp),imp=c.imp[,1],group="China")
#nc.imp1 <- data.frame(variable = rownames(nc.imp),imp=nc.imp[,1],group="Outside of China")


#rf.imp <- rbind.data.frame(c.imp1,nc.imp1) %>%
#within(variable <- factor(variable,levels = c( "GDP.pc","GDP","Pop",
#      "Pop.density","Commuting","Avg.Tem.Jan","Avg.HT.Jan","Avg.HT.Mar","Avg.LT.Jan",
#      "Avg.Pre.Jan","Avg.Pre.Feb","Avg.Pre.Mar"
#)))
#rf.imp$variable2 <- c("Avg.Pre","Avg.HT","GDP.pc","GDP","Pop","Pop.density",
#                   "Commuting","Avg.Pre","Avg.HT","GDP.pc","GDP","Pop","Pop.density",
#                   "Commuting")
#rf.imp <- rf.imp %>%
#  within(variable2 <- factor(variable2,levels = c( "Avg.Pre","Avg.HT","GDP.pc","GDP","Pop","Pop.density",
#                                                 "Commuting")))
save(rf.imp,file="./data/rf.imp.share.Rdata")
load("./data/rf.imp.share.Rdata")
pdf("./fig/fig.2ab.pdf",width=6,height=4.5)
rf.imp %>%
  ggplot(aes(x=variable2,y=imp))+geom_col(aes(fill=group),color="black",position ="dodge")+
  theme_bw()+theme(axis.text = element_text(color="black"))+labs(x="",y="Importance")+
  theme(panel.grid = element_blank(),strip.background = element_blank())  + 
  coord_flip() + labs(y="Importance %")+guides(fill=F) + scale_y_continuous(expand = c(0,0),limits = c(0,5))+
  scale_fill_manual(values=c("black","White"))
dev.off()

#### rdaenvpart ####
#### diff ####
c.parts <- varpart(c.train.data$y,c.train.data.x[,c(4:8)],c.train.data.x[,c(1:3)])
pdf("./fig/fig.S6a.pdf")
plot(c.parts)
dev.off()
pdf("./fig/fig.S5c.pdf")
c.rda.indi <- rdaenvpart(c.train.data$y,c.train.data.x)
dev.off()

nc.train.data.x3.names <- c(colnames(nc.train.data.x2),
 names(sort(-nc.train.data$imp[colnames(nc.train.data.x),1]))[!(names(sort(-nc.train.data$imp[colnames(nc.train.data.x),1])) %in% 
      colnames(nc.train.data.x2))][1:2])
nc.train.data.x3 <- nc.train.data.x[,nc.train.data.x3.names]
nc.parts <- varpart(nc.train.data$y,nc.train.data.x3[,c(3:8)],nc.train.data.x[,c(1:2)])
pdf("./fig/fig.S6b.pdf")
plot(nc.parts)
dev.off()
pdf("./fig/fig.S5d.pdf")
nc.rda.indi <- rdaenvpart(nc.train.data$y,nc.train.data.x3)
dev.off()
rf.imp$Explanation <- 0
rf.imp$Explanation[1:9] <- c.rda.indi$IJ.R2[,1] * 100
rf.imp$Explanation[c(11,12,14,15,16,17,18,25,22)] <- nc.rda.indi$IJ.R2[,1] * 100
colnames(rf.imp)[c(1,2,4)] <- c("Varis","Importance %","Explanation %")
pdf("./fig/fig.s6.pdf",width=6,height=5.5)
rf.imp %>% melt.data.frame(measure.vars = c("Importance %","Explanation %")) %>%
  ggplot(aes(x=Varis,y=value))+geom_col(aes(fill=group),color="black",position ="dodge")+
  theme_bw()+theme(axis.text = element_text(color="black"))+
  theme(panel.grid = element_blank(),strip.background = element_blank())  + 
  coord_flip()  +guides(fill=F) + scale_y_continuous(expand = c(0,0,0.1,0))+
  scale_fill_manual(values=c("black","White")) + facet_wrap(~variable,scale="free_x") +
  labs(x="",y="")
dev.off()


#### same ####
c.parts <- varpart(c.train.data$y,c.train.data.x2[,c(3:6)],c.train.data.x2[,c(1:2)])
pdf("./fig/fig.1c.pdf")
c.rda.indi <- rdaenvpart(c.train.data$y,c.train.data.x2)
dev.off()
pdf("./fig/fig.2c.pdf")
plot(c.parts)
dev.off()

nc.parts <- varpart(nc.train.data$y,nc.train.data.x2[,c(3:6)],nc.train.data.x2[,c(1:2)])
pdf("./fig/fig.1d.pdf")
nc.rda.indi <- rdaenvpart(nc.train.data$y,nc.train.data.x2)
dev.off()
pdf("./fig/fig.2d.pdf")
plot(nc.parts)
dev.off()

rf.imp$Explanation <- 0
rf.imp$Explanation[1:7] <- c.rda.indi$IJ.R2[,1] * 100
rf.imp$Explanation[8:14] <- nc.rda.indi$IJ.R2[,1] * 100
colnames(rf.imp)[c(1,2,5)] <- c("Varis","Importance %","Explanation %")
pdf("./fig/fig.2.pdf",width=6,height=3.5)
rf.imp %>% melt.data.frame(measure.vars = c("Importance %","Explanation %")) %>%
  ggplot(aes(x=variable2,y=value))+geom_col(aes(fill=group),color="black",position ="dodge")+
  theme_bw()+theme(axis.text = element_text(color="black"))+
  theme(panel.grid = element_blank(),strip.background = element_blank())  + 
  coord_flip()  +guides(fill=F) + scale_y_continuous(expand = c(0,0,0.1,0))+
  scale_fill_manual(values=c("black","White")) + facet_wrap(~variable,scale="free_x") +
  labs(x="",y="")
dev.off()
#### SEM.C2 ####
c.sem.data <- cbind.data.frame(growth_rate=c.train.data$y[,1],
                               c.train.data.x2)
nc.sem.data <- cbind.data.frame(growth_rate=nc.train.data$y[,1],
                                nc.train.data.x2)

#===
model.c.cor <- as.data.frame(cor(c.sem.data))
model.c <-'
# latent variable definitions
Nature =~ Avg.HT.Jan  + Avg.Pre.Jan 
Human =~GDP.pc + GDP+Pop.density + Commuting + Pop
# regressions
growth_rate ~ Nature + Human 
# residual correlations
Human ~~ 1*Human
Nature ~~ Nature
#=======================
GDP ~~   Pop
Pop.density ~~ Commuting
'

sem.c.fit <- sem(model=model.c,data =c.sem.data,check.gradient = FALSE)
fitMeasures(sem.c.fit,c("chisq","df","pvalue","gfi",
                        "cfi","rmr","srmr","rmsea"))
#chisq     df pvalue    gfi    cfi    rmr   srmr  rmsea 
#23.502 17.000  0.134  0.817  0.901  0.158  0.166  0.135
summary(sem.c.fit,fit.measures=T,standardized = TRUE)
pdf("./fig/fig.2a.pdf")
semPaths(sem.c.fit, what='std', nCharNodes=6, sizeMan=6,
         edge.label.cex=1, curvePivot = TRUE, fade=FALSE)
dev.off()

#===========
model.nc <-'
# latent variable definitions
Nature =~ Avg.HT.Mar + Avg.Pre.Mar
Human =~ GDP.pc+ GDP + Pop.density + Commuting  + Pop
# regressions
growth_rate ~ Nature + Human
# residual correlations
Human ~~ Human
Nature ~~ 1*Nature
#=======================
GDP.pc ~~ GDP 
GDP ~~  Commuting  + Pop
'
sem.nc.fit <- sem(model.nc,data =nc.sem.data,check.gradient = FALSE)
fitMeasures(sem.nc.fit,c("chisq","df","pvalue","gfi",
                        "cfi","rmr","srmr","rmsea"))
#chisq     df pvalue    gfi    cfi    rmr   srmr  rmsea 
#22.647 16.000  0.123  0.848  0.942  0.112  0.115  0.126 
summary(sem.nc.fit,fit.measures=T,standardized = TRUE)
pdf("./fig/fig.2b.pdf")
semPaths(sem.nc.fit, what='std', nCharNodes=6, sizeMan=6,
         edge.label.cex=1, curvePivot = TRUE, fade=FALSE)
dev.off()
#### GLM ####
##same
c.glm.fit <- glm(growth_rate~.,data=c.sem.data)
c.glm.fit <- step(c.glm.fit)
glm.same.res.c <- summary(c.glm.fit)
write.csv(glm.same.res.c$coefficients,"./table/glm.same.res.c.csv")
nc.glm.fit <- glm(growth_rate~.,data=nc.sem.data)
nc.glm.fit <- step(nc.glm.fit)
glm.same.res.nc <- summary(nc.glm.fit)
write.csv(glm.same.res.nc$coefficients,"./table/glm.same.res.nc.csv")
##Diff
c.glm.fit <- glm(growth_rate~.,data=cbind.data.frame(growth_rate=c.train.data$y[,1],c.train.data.x))
c.glm.fit <- step(c.glm.fit)
glm.diff.c <- summary(c.glm.fit)
write.csv(glm.diff.c$coefficients,"./table/glm.diff.res.c.csv")
nc.glm.fit <- glm(growth_rate~.,data=cbind.data.frame(growth_rate=nc.train.data$y[,1],nc.train.data.x))
nc.glm.fit <- step(nc.glm.fit)
glm.diff.nc <- summary(nc.glm.fit)
write.csv(glm.diff.nc$coefficients,"./table/glm.diff.res.nc.csv")

####
library(scales)
library(ggpubr)
#### world development ####
total_gdp <- read.xlsx("./data/world.xlsx",sheetIndex = 1,encoding = "UTF-8",stringsAsFactors=F)
total_pop <- read.xlsx("./data/world.xlsx",sheetIndex = 2,encoding = "UTF-8",stringsAsFactors=F)
total_epi <- read.xlsx("./data/world.xlsx",sheetIndex = 3,encoding = "UTF-8",stringsAsFactors=F)
epi.period <- list.rbind(strsplit(total_epi$Time.period,split="-"))
epi.period[5,2] <- "1979"
epi.period[c(15,19,20),2] <- "2020"

start.year <- as.numeric(str_extract(epi.period[,1],"[\\d]+"))
end.year <- as.numeric(str_extract(epi.period[,2],"[\\d]+"))

epi.s <- table(cut(start.year, breaks = seq(0,2100,100),labels=seq(50,2050,100)))
total_gdp$ye <- cut(total_gdp$Year, breaks = seq(0,2100,100),labels=seq(50,2050,100))
total_pop$ye <- cut(total_pop$Year, breaks = seq(0,2100,100),labels=seq(50,2050,100))
total_gdp.ye <- total_gdp %>% group_by(ye) %>% do(data.frame(gdp=mean(.$GDP.current)))
total_pop.ye <- total_pop %>% group_by(ye) %>% do(data.frame(pop=mean(.$World.Population)))
total_pop.ye$pop.hm <-  total_pop.ye$pop/100000000
total_gdp.ye <- total_gdp %>% group_by(ye) %>% do(data.frame(v=mean(.$GDP.current)/1000))
total_pop.ye <- total_pop %>% group_by(ye) %>% do(data.frame(v=mean(.$World.Population)/100000000))

df.epi = data.frame(ye=as.numeric(names(epi.s)),v=as.numeric(epi.s),variable="Epidemic")
cor.df <- merge.data.frame(total_pop.ye,df.epi,by="ye",suffixes = c(".pop",".epi"))
cor.test(log(cor.df$v.pop),cor.df$v.epi)
colnames(df.epi)[1:2] <- c("Year","value")
df.pop = data.frame(Year=total_pop$Year,value=total_pop$World.Population,variable="Pop")
df.gdp = data.frame(Year=total_gdp$Year,value=total_gdp$GDP.current/1000,variable="GDP")
total_epi$Death.toll[20] <- "170554"

df.epi2 = data.frame(Year=start.year,
                     Year2=end.year,
                     Name=total_epi$Name,
                     value=as.numeric(total_epi$Death.toll),
                     variable="Death")

df.plot = rbind.data.frame(df.epi,df.pop)
df.plot = rbind.data.frame(df.plot,df.gdp)

H <- df.epi %>% ggplot(aes(x=Year,y=value))+geom_col(color="black",fill="white") +theme_classic()+ scale_y_continuous(limits = c(0,5),breaks = c(0,5))+labs(x="",y="")
PL <- df.pop %>% filter(Year >0) %>% ggplot(aes(x=Year,y=value))+geom_line()+
  theme_bw()+theme(panel.grid = element_blank()) + 
  geom_point(data=df.epi2,aes(x=Year,y=value/200000000*7794798739,size=value))+guides(size=F)+
  scale_y_continuous(sec.axis=sec_axis(~rescale(.,c(0,2.05e8)),name="Death toll"))+
  labs(y="Population")+theme(axis.text = element_text(color="black"))
ggarrange(H,PL,ncol = 1,nrow =4,heights = c(1,2,2,2))
Fig.s1 <- arrangeGrob(H,PL,ncol=2,nrow = 4,layout_matrix = cbind(c(1,2,2,2),c(1,2,2,2)))
Fig.s1 <- as_ggplot(Fig.s1)+draw_plot_label(label=LETTERS[1:2],x=c(0,0),y=c(0,1),size=9)

pdf("./fig/fig.s1.pdf",width=6,height=4)
print(Fig.s1)
dev.off()
####
library(gtable)
library(grid)
double_y_axis <- function(p1, p2){
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  # draw it
  grid.draw(g)
  return(g)
}

p1 <- df.pop %>% filter(Year >0) %>% ggplot(aes(x=Year,y=value))+geom_line()+
  theme_bw()+theme(panel.grid = element_blank())
p2 <- ggplot(data=df.epi2,aes(x=Year,y=value,size=value))+geom_point()+
  theme_bw()+theme(panel.grid = element_blank())+guides(size=F)


#=====Fig======#
#fig.1b
load("./data/com_case.Rdata")
Fig.1b <- com_cases %>% melt.data.frame(measure.vars = c("China","Other countries")) %>%
  filter(Date <= as.Date("2020-03-26")) %>%
  ggplot(aes(x=Date,y=value,colour=variable)) + geom_line(size=1)+
  labs(y="Cumulative cases of COVID-19")+
  scale_x_date(breaks = as.Date(c("2020-02-01","2020-02-15","2020-03-01","2020-03-15")),
               labels = c("Feb 01","Feb 15","Mar 01","Mar 15"))+
  theme_bw()+guides(colour=guide_legend(title=""))+
  theme(panel.grid = element_blank(),
        axis.text = element_text(color="black"),
        legend.position = c(0.2,0.9),
        legend.background = element_blank())+
  scale_colour_manual(values = c("#003399","#F00000"))
pdf("./fig/fig_1b.pdf",width=6,height=4)
print(Fig.1b)
dev.off()

fig.1c.plot <- rbind.data.frame(
  data.frame(
  values=c.train.sf$growth_rate,
  variable="China"),
  data.frame(
    values=nc.train.sf$growth_rate,
    variable="Other countries")
)
#fig.1c
Fig.1c <- fig.1c.plot %>% ggplot(aes(x=variable,y=values)) + 
  geom_violin(fill="lightblue",alpha=0.3)+
  geom_boxplot(fill="white", width= .2) + theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(color="black")) + 
  labs(x="",y="COVID-19 growth rate")

pdf("./fig/fig_1c.pdf",width=6,height=4)
print(Fig.1c)
dev.off()
#fig.map
library(tidyverse)
library(ggplot2)
library(readr)
library(maps)
library(viridis)
world <- map_data("world")
datacov <- read.csv("./data/case_global.csv",stringsAsFactors = F) 
datacov$Country.Region[datacov$Country.Region == "US"] <- "USA"
world2 <- merge.data.frame(world,datacov,by.x="region",
                           by.y="Country.Region",all.x = T)
mybreaks <- c(1, 20, 100, 1000, 50000)

ggplot(data = world2, aes(x=long, y = lat)) +
  geom_polygon(aes(group = group,fill=`X3.26.20`), alpha=0.3)  +
  scale_fill_gradient(low="white",high="red")+coord_map("polyconic")+
  theme(panel.grid = element_blank(),
        panel.background=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.2,0.3))
