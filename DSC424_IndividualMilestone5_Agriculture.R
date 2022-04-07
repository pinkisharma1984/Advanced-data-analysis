library(corrplot)
library(tidyverse)
library(plyr)
library(dplyr)
library(psych)     # Has a much better scatterplot matrix function
library(Hmisc)
library(MASS)
library(rfUtilities)
### load the data
setwd("C:/Users/shivk/Desktop/DSC424/DSC424_Project")

fao = read.csv("proj_fao.csv")
head(fao)

# selecting variable name
df <- fao %>%
  select(Country    = Country
         ,Continent = Continent
         ,Year      = Year
         ,Proveg   = ProductionVegetables   
         ,Importveg = ImportQuantityVegetables
         ,Exportveg = ExportQuantityVegetables
         ,Domveg = DomesticSupplyQuantityVegetables
         ,Profruit = ProductionFruit
         ,Importfruit = ImportQuantityFruit
         ,Exportfruit = ExportQuantityFruit
         ,Domfruit = DomesticSupplyQuantityFruit
         ,Promeat = ProductionMeat
         ,Importmeat = ImportQuantityMeat
         ,Exportmeat = ExportQuantityMeat
         ,Dommeat = DomesticSupplyQuantityMeat
         ,Temp =  TempChange
         ,PrUN = PrevalenceOfUndernourishment
         ,PSFI = PrevalenceOfSevereFoodInsecurity
         ,obesity = PrevalenceOfObesityAdultPopulation
         ,CPFI = ConsumerPricesFoodIndices
         ,AgriLand = LandUsedForAgriculture
         ,GDP = GDPperCapita
         ,PSI = PoliticalStabilityIndex
         ,ASDWD = AccessToBasicDrinkingWaterServices
         ,ASaniS= AccessToBasicSanitationServices
         ,Totalpop = TotalPopulation
         ,Ruralpop = RuralPopulation
         ,Urbanpop = UrbanPopulation)

str(df)
# removing missing values from the dataset
df_fao <- df %>% 
  na_if("") %>%
  na.omit

# creating a level for "Political stability"
# creating a new column as ordinal variable
df_fao$PSI_level <- with(df_fao, ifelse(df_fao$PSI > 1, "VeryStable",
                                              ifelse(df_fao$PSI > 0.1, "Stable",
                                                     ifelse(df_fao$PSI > 0, "Neutral",
                                                          ifelse(df_fao$PSI >= -1, "Unstable", "VeryUnstable")))))

df_fao$PSI_level <- factor(df_fao$PSI_level, levels = c("VeryStable", "Stable", "Neutral", "Unstable", "VeryUnstable"))
# combining similar columns and create a new columns
df_fao$TotalImport <- rowSums(cbind(df_fao$Importveg, df_fao$Importfruit, df_fao$Importmeat), na.rm = TRUE)
df_fao$TotalExport <- rowSums(cbind(df_fao$Exportveg, df_fao$Exportfruit, df_fao$Exportmeat), na.rm = TRUE)
df_fao$TotalPro <- rowSums(cbind(df_fao$Proveg, df_fao$Profruit, df_fao$Promeat), na.rm = TRUE)
df_fao$TotalDomSupply <- rowSums(cbind(df_fao$Domveg, df_fao$Domfruit, df_fao$Dommeat), na.rm = TRUE)


#df_fao$PSI_level <- as.numeric(df_fao$PSI_level)
#str(df_fao)

# only selecting certain columns
df_fao <- df_fao[, c(-1,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16,-20,-23,-27,-28)]
write.csv(df_fao, "proj_fao_6.csv", row.names = F)


# load the new data
df_fao = read.csv("proj_fao_6.csv")
str(df_fao)

#### removing missing values from the data set
df_fao <- df_fao %>% 
  na_if("") %>%
  na.omit
# removing zero from dataset
df_fao[df_fao == 0] <- NA
df_fao <- na.omit(df_fao)
df_fao$PSI_level <- factor(df_fao$PSI_level, levels = c("VeryStable", "Stable", "Neutral", "Unstable", "VeryUnstable"))

library(ggplot2)
library(lessR)
#install.packages("lessR")

cat_var <- (df_fao$PSI_level)
cat <- table(cat_var)
pie(cat, col= hcl.colors(length(cat), "BluYl"))

# plot for continent
continent<- factor(df_fao$Continent)
cat2 <- data.frame(continent)
#cat2 <- table(continent)
cols <- hcl.colors(length(levels(continent)), "BluYl")
PieChart(continent, data= cat2, hole=0, fill= cols, labels_cex = 0.9)
ggplot(data= df_fao, aes(x=Continent, fill= PSI_level)) + geom_bar()# stacked bar graph
ggplot(data= df_fao, aes(x=Continent, fill= PSI_level)) + geom_bar(position = "dodge")

# creating dummy variable of "continent" column
df_fao$Con_Asia <- ifelse(df_fao$Continent == "Asia", 1,0)
df_fao$Con_Europe <- ifelse(df_fao$Continent == "Europe", 1,0)
df_fao$Con_nAmerica <- ifelse(df_fao$Continent == "North America", 1,0)
df_fao$Con_sAmerica <- ifelse(df_fao$Continent == "South America", 1,0)
df_fao$Con_Oceania <- ifelse(df_fao$Continent == "Oceania", 1,0)
df_fao$Con_Africa <- ifelse(df_fao$Continent == "Africa", 1,0)

# dropping the continent column
df_fao$Continent <- NULL
df_fao$AgriLand <- NULL
str(df_fao)
library(corrplot)
corrplot(cor(df_fao[,c(-9)]))
corrplot(cor(df_fao[,c(-8)]), order = "AOE")
corrplot(cor(df_fao))
# scatter plot 
plot(df_fao[,c(-8)], pch=16, col=factor(df_fao$PSI_level))

hist.data.frame(df_fao)

## transforming the variable
df_fao$GDP <- log(df_fao$GDP)
df_fao$Totalpop <- log(df_fao$Totalpop)
df_fao$TotalImport <- log(df_fao$TotalImport)
df_fao$TotalExport<- log(df_fao$TotalExport)
df_fao$TotalPro <- log(df_fao$TotalPro)
df_fao$TotalDomSupply <- log(df_fao$TotalDomSupply)
df_fao$obesity <- sqrt(df_fao$obesity)
df_fao$ASaniS <-  log(log10(max(df_fao$ASaniS+1) - df_fao$ASaniS))
df_fao$ASDWD <- log(log10(max(df_fao$ASDWD+1) - df_fao$ASDWD))
df_fao$ASaniS[df_fao$ASaniS == "-Inf"] <- 1e-9   
df_fao$ASDWD[df_fao$ASDWD == "-Inf"] <- 1e-9  
df_fao$PSFI <- log(df_fao$PSFI)
df_fao$PrUN <- log(df_fao$PrUN)

# creating new dataframe after transformation
write.csv(df_fao, "proj_fao_T.csv", row.names = F)


#########################
###intial analysis before doing LDA:
######################## 

fit1 = lm(PrUN ~ ., data=df_train)
summary(fit1)
vif(fit1)

library(psych)     # Has a much better scatterplot matrix function
# A nice correlation matrix visualization
library(car)       # Misc statistical methods
library(QuantPsyc) # Misc statistical methods
library(leaps)     # Gives forward, backward and stepwise
library(lm.beta)   # Gives us standardized coefficients
PruNSubsets = regsubsets(PrUN ~ ., data=df_train, nbest=1)
plot(PruNSubsets, scale="adjr2")

PruNSubsets = regsubsets(PrUN ~ ., data=df_train, nbest=1, nvmax=23)
plot(PruNSubsets, scale="adjr2")

bestR2Fit = lm(PrUN ~ Importveg + Exportveg + Domveg + Dommeat + PSFI + obesity + AgriLand + GDP + ASaniS + Ruralpop + Urbanpop, data=df_train)
summary(bestR2Fit)
lm.beta(bestR2Fit)
