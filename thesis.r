
rm(list=ls())
library(sf)
library(readxl)
library(writexl)
library(raster) # not used
library(dplyr)  # work with data (included in tidyverse)
library(spData) # obtain data on geography
library(spDataLarge)   # load larger geographic data
library(ggspatial) # Improvement by mixing with ggplot
library(ggplot2)  # make sophisticated maps other than just plot 
library(tidyverse) 
install.packages("vtable")
library(vtable)
theme_set(theme_bw())

# Map practice (Explanatory variable for FDI?)
data(world)
ggplot(data=world)+geom_sf()

# Scratch
#E <- read_excel("/Users/taemin/Downloads/THESIS/Simple/EPI.xlsx")

#S <- read_excel("/Users/taemin/Downloads/THESIS/Simple/sovereignesgdata.xlsx", sheet="social")
#S <- S[c("iso3","Average")]

#G <- read_excel("/Users/taemin/Downloads/THESIS/Simple/WGI.xlsx", sheet="Data")
#G <- G[c("Country Name", "Country Code", "Series Name","Average")]
#G <- G %>% rename("Country_Name"="Country Name", "iso3"="Country Code", "Series_Name"="Series Name")
#G_modified <- G %>% group_by(Country_Name, iso3) %>%
#  summarise(avg = mean(Average), .groups="drop")
#G_modified <- drop_na(G_modified)
#G <- G_modified
#rm(G_modified)



# Dependent Variable
FDI <- read_excel("/Users/taemin/Downloads/THESIS/Simple/FDIinflow.xlsx", sheet="Data")
FDI <- FDI[c("Country Name","Country Code","FDI")]
#FDI <- FDI %>% rename("Country_Name"="Country Name", "iso3"="Country Code")
FDI <- FDI %>% rename("name_long"="Country Name", "iso3"="Country Code")
min(FDI$FDI)
max(FDI$FDI)

FDI
FDI_drawer$FDI <- FDI_drawer$FDI / 1000000000 #billion
FDI_drawer <- merge(x=world,y=FDI,by="name_long")

FDI
min(FDI_drawer$FDI)
max(FDI_drawer$FDI)

library(tmap)
binds <- c(-100000000000,1000000000,3000000000,9000000000,11000000000,200000000000)
tm_shape(FDI_drawer) + tm_fill(col="FDI", breaks=binds, title="FDI (US$)", midpoint=NA) + tm_borders()

FDI <- FDI %>% rename("Country_Name"="name_long")


# Indep variable: ESG
# E (CO2, Renewable, Oil Exporters)
CO2 <- read_excel("/Users/taemin/Downloads/THESIS/Complex/CO2.xlsx", sheet="Data")
CO2 <- CO2[c("Country Name","Country Code","co2")]
CO2 <- CO2 %>% rename("Country_Name"="Country Name","iso3"="Country Code")
CO2 <- drop_na(CO2)

Renewable <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Renewable.xlsx", sheet="Data")
Renewable <- Renewable[c("Country Name","Country Code","renewable")]
Renewable <- Renewable %>% rename("Country_Name"="Country Name","iso3"="Country Code")
Renewable <- drop_na(Renewable)

Oilcountry <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Oilexporter.xlsx", sheet="Oilexporter")
Oilcountry <- Oilcountry[c("Country Name","Country Code","oilproducer")]
Oilcountry <- Oilcountry %>% rename("Country_Name"="Country Name","iso3"="Country Code")



# S (Education, Diversity, GINI Index)


Educ <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Education.xlsx", sheet="Data")
Educ <- Educ[c("Country_Name","educ")]

Diversity <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Diversity.xlsx", sheet="Data")

Inequality <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Inequality.xlsx", sheet="Data")
Inequality_m <- Inequality %>% group_by(Country_Name) %>% summarise(inequality=mean(inequality), .groups="drop")
Inequality_m <- drop_na(Inequality_m)
Inequality <- Inequality_m
rm(Inequality_m)


# G (Voice and Acc, Rule of Law)

Voice_accountability <- read_excel("/Users/taemin/Downloads/THESIS/Complex/sovereignesgdata.xlsx", sheet="Voice_accountability")
Voice_accountability <- Voice_accountability[c("iso3", "voice_accountability")]
Voice_accountability <- Voice_accountability %>% rename("voice_acc"="voice_accountability")
Voice_accountability <- drop_na(Voice_accountability)

Ruleoflaw <- read_excel("/Users/taemin/Downloads/THESIS/Complex/sovereignesgdata.xlsx", sheet="Ruleoflaw")
Ruleoflaw <- Ruleoflaw[c("iso3", "ruleoflaw")]
Ruleoflaw <- drop_na(Ruleoflaw)


# Political (Democ, govt eff, govt stab)

Democ <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Democ.xlsx", sheet="Democ")
Democ <- Democ[c("Country_Name", "democ")]

Gov_effec <- read_excel("/Users/taemin/Downloads/THESIS/Complex/sovereignesgdata.xlsx", sheet="Gov_effec")
Gov_effec <- Gov_effec[c("iso3","gov_effec")]
Gov_effec <- drop_na(Gov_effec)

Gov_stab <- read_excel("/Users/taemin/Downloads/THESIS/Complex/sovereignesgdata.xlsx", sheet="Gov_stab")
Gov_stab <- Gov_stab[c("iso3","gov_stab")]
Gov_stab <- drop_na(Gov_stab)


# Economic ( trade openness, GDP, GDP growth, wage, premium, corp tax, infra)

Tradeopen <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Tradeopen.xlsx", sheet="Tradeopen")
Tradeopen <- Tradeopen %>% rename("Country_Name"="Entity", "iso3"="Code")
Tradeopen_modified <- Tradeopen %>% group_by(Country_Name,iso3) %>% summarise(tradeopen=mean(tradeopen),.groups="drop")
Tradeopen <- Tradeopen_modified
rm(Tradeopen_modified)

GDP <- read_excel("/Users/taemin/Downloads/THESIS/Complex/GDP.xlsx", sheet="Data")
GDP <- GDP[c("Country_Name","iso3","GDP")]
GDP$GDP <- GDP$GDP/100000000
GDP


GDPgrowth <- read_excel("/Users/taemin/Downloads/THESIS/Complex/GDPgrowth.xlsx", sheet="Data")
GDPgrowth <- GDPgrowth[c("Country_Name","iso3","GDPgrowth")]
GDPgrowth <- drop_na(GDPgrowth)

Income_per_capita <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Income_per_capita.xls", sheet="Data")
Income_per_capita <- Income_per_capita[c("Country_Name","iso3","income_per_capita")]
Income_per_capita <- drop_na(Income_per_capita)


Country_risk_premium <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Country_risk_premium.xlsx", sheet="Data")

Corporate_tax <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Corporate_tax.xlsx", sheet="Sheet1")

Infra <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Infra.xlsx", sheet="Sheet1")
Infra <- Infra[c("Country_Name","infra")]



# Cultural (openness to foreigners)

Foreigner <- read_excel("/Users/taemin/Downloads/THESIS/Complex/Foreigner.xlsx", sheet="Sheet1")


# MERGE
M <- merge(FDI,CO2, by=c("Country_Name","iso3"))
M <- merge(M,Renewable, by=c("Country_Name","iso3"))
M <- merge(M, Oilcountry, by=c("Country_Name","iso3"))
M <- merge(M, Democ, by=c("Country_Name"))
M <- merge(M, Diversity, by=c("Country_Name","iso3"))
M <- merge(M, Voice_accountability, by=c("iso3"))
M <- merge(M, Ruleoflaw, by=c("iso3"))
M <- merge(M, Gov_effec, by=c("iso3"))
M <- merge(M, Gov_stab, by=c("iso3"))
M <- merge(M, Tradeopen, by=c("iso3"))
M <- merge(M, GDP, by=c("iso3"))
M <- merge(M, GDPgrowth, by=c("Country_Name","iso3"))
M <- merge(M, Corporate_tax, by=c("iso3"))
M <- M[c("Country_Name","iso3","FDI","co2","democ","renewable","oilproducer","diversity","voice_acc","ruleoflaw","gov_effec","gov_stab","tradeopen","GDP","GDPgrowth","corporate_tax")]
M <- merge(M, Educ, by=c("Country_Name"))
M <- merge(M, Inequality, by=c("Country_Name"))
# fillna
M <- merge(M, Income_per_capita, by=c("Country_Name","iso3"))
M <- merge(M, Foreigner, by=c("Country_Name"))
M <- merge(M, Country_risk_premium, by=c("Country_Name"))
M <- merge(M, Infra, by=c("Country_Name"))



M2 <- M

col_order = c("Country_Name","FDI","co2","renewable","oilproducer","educ","diversity","inequality","voice_acc",
              "ruleoflaw","democ","gov_effec","gov_stab","tradeopen","GDP","GDPgrowth","income_per_capita",
              "country_risk_premium","corporate_tax","infra","foreigner")

M2 <- M[,col_order]

write_xlsx(x=M,path="/Users/taemin/Downloads/THESIS/Complex/M.xlsx",col_names=TRUE)



# Descriptive Statistics
st(M2)

hist(M2$co2)
hist(M2$renewable)
hist(M2$oilproducer)
hist(M2$educ)
hist(M2$diversity)
hist(M2$inequality)
hist(M2$voice_acc)
hist(M2$ruleoflaw)
hist(M2$democ)

hist(M2$gov_effec)
hist(M2$gov_stab)
hist(M2$tradeopen)
hist(M2$GDP)
hist(M2$GDPgrowth)
hist(M2$income_per_capita)
hist(M2$country_risk_premium)
hist(M2$corporate_tax)
ggplot(M2, aes(x=infra)) + geom_bar() + ggtitle("Infrastructure Level")
hist(M2$foreigner)



# Regression


simple_e_reg <- lm(formula = FDI ~ co2 + renewable + oilproducer, data=M2)
simple_s_reg <- lm(formula = FDI ~ educ + diversity + inequality, data=M2)
simple_g_reg <- lm(formula = FDI ~ voice_acc + ruleoflaw + gov_effec + gov_stab, data=M2)
  
  

reg1 <- lm(formula = FDI ~ co2 + renewable + oilproducer + educ + diversity + 
     inequality + voice_acc + ruleoflaw + democ + gov_effec + gov_stab + tradeopen +
       GDP + GDPgrowth + income_per_capita + country_risk_premium + corporate_tax + infra + foreigner, data=M2)

reg2 <- lm(formula = FDI ~ co2 + renewable + oilproducer + 
             democ + tradeopen + GDPgrowth + income_per_capita + country_risk_premium + corporate_tax + infra + foreigner, data=M2)

reg3 <- lm(formula = FDI ~ educ + diversity + inequality + 
             democ + tradeopen + GDPgrowth + income_per_capita + country_risk_premium + corporate_tax + infra + foreigner, data=M2)

reg4 <- lm(formula = FDI ~  gov_stab + ruleoflaw +
             democ + tradeopen + GDPgrowth + income_per_capita + country_risk_premium + corporate_tax + infra + foreigner, data=M2)


reg5 <- lm(formula = FDI ~ co2 + renewable + oilproducer + educ + diversity + inequality + gov_stab + ruleoflaw + democ + tradeopen + GDPgrowth + income_per_capita + country_risk_premium + corporate_tax + infra + foreigner, data=M2)


install.packages("jtools")
install.packages("huxtable")
library(jtools)
library(huxtable)

export_summs(simple_e_reg,simple_s_reg,simple_g_reg, scale=TRUE)

summary(reg1)
install.packages("car")
library(car)
#calculate the VIF for each independent variable in the model
vif(reg1)
vif(reg4)

summary(reg2) # E 
summary(reg3) # S
summary(reg4) # G

export_summs(reg2,reg3,reg4,scale=FALSE,model.names=c("E","S","G"))
plot_summs(reg2,reg3,reg4,scale=TRUE,plot.distributions = TRUE, model.names=c("E","S","G"))






