# This code was written by Arbian Halilaj for XXX at the
# University of St. Gallen.
# Supervisor: XXX
# For questions contact arbian.halilaj@student.unisg.ch
# Last tested: 23/06/2020

#####################################################################
# Setup and data preparation

library(xtable)
library(naniar) # replace_with_na
library(tidyverse)
library(car) #Multicollinearity test
library(reshape2) #Correlation matrix
library(stargazer) #Output
library(ggpubr)
library(ggfortify) #autoplot diagnostics
library(lmtest) #Breusch-Pagan Test (Heteroskedasticity)
library(mfx) #marginal effects

rm(list = ls()) # memory cleaning

# Preparing data import from stata 
# Loading foreign library 
library(foreign)

# Importing data from stata file
data <- read.dta("/Users/arbiax/Desktop/BA_firsttry/trashba/Albania2019.dta")

# Select variables of interest
selected <- c("x","y","z")

# Subset data
data.subset <- data[selected]
data.subset <- as.data.frame(data.subset)

#####################################################################
# Rename variables
df<- data.subset %>% 
  rename(Hallo = x,
         Ich = y,
         Bims = z,
  )

# Recode variables
## Dependent variables
df <- df%>%
  mutate(variable=case_when(
    variable=="Yes" ~ 1,
    variable=="No" ~ 0
  ))

df$SalesGrowth <- 1/3*((df$Sales.1-df$Sales.0)/((df$Sales.1+df$Sales.0)/2))

df$Performance <- (df$Sales.1-df$Costofsales)/df$Employees.1
df$Performance <- ifelse (df$Performance < 0, NA, df$Performance)


x <- 1
df$Index <- as.numeric(df$x %in% x | df$y %in% x | df$z %in% x)

df <- df%>%
  mutate(TaxAdmin=case_when(
    TaxAdmin=="No obstacle" ~ 0,
    TaxAdmin=="Minor obstacle" ~ 1,
    TaxAdmin=="Moderate obstacle" ~2,
    TaxAdmin=="Major obstacle" ~ 3,
    TaxAdmin=="Very severe obstacle" ~ 4
  ))
df$TaxAdmin <- as.numeric(df$TaxAdmin)

df$PolicyObstacle <- with(df, ifelse(is.na(TaxAdmin), NA, ifelse(is.na(CustomTrade), NA, ifelse(is.na(BusinessPermit), NA, ifelse(is.na(LaborReg), NA, (TaxAdmin+CustomTrade+BusinessPermit+LaborReg)/4)))))

## Control variables
df$Age <- 2021-df$Year.0
df$lnAge <- log(df$Age)

df <- df%>%
  mutate(Sector=case_when(
    Sector=="Manufacturing" ~ 1,
    Sector=="Retail services" ~ 0,
    Sector=="Other services" ~ 0
  )) 

df$Size <- as.numeric(df$Size)
df$Small <- ifelse(df$Size <= 2, 1, ifelse((df$Size <= 3) & (df$Size <= 4), 0, 0))
df$Medium <- ifelse(df$Size <= 2, 0, ifelse((df$Size <= 3) & (df$Size <= 4), 1, 0))
df$Large <- ifelse(df$Size <= 2, 0, ifelse((df$Size <= 3) & (df$Size <= 4), 0, 1))


df$Export <- as.numeric(df$Export)
df <- df %>% replace_with_na(replace = list(Export = c(-9,-7)))

df$Experience <- as.numeric(df$Experience)
df <- df %>% replace_with_na(replace = list(Experience = -9))

df$lnExperience <- log(df$Experience)
df <- df %>% replace_with_na(replace = list(lnExperience = "NaN"))

#####################################################################
#####################################################################
# REGRESSION MODELS
###################################################################
# Descriptive Stats
descript <- c("Hallo", "Ich", "Bims")

df_descript <- df[descript]
df_descript <- df_descript[apply(df_descript, 1, function(x) !any(is.na(x))),] #Remove nan

## Summary
summary(df_descript)
stargazer(df_descript)

##Correlation matrix
Modcor1 <- c("SalesGrowth", "Bribes", "RD",
             "PolicyObstacle", "Sector", "Small", "Medium", "Large", "lnAge", "lnExperience", 
             "Foreign", "Export", "TrainingEmployees", "InformalCompetition")

Modcor1 <- df[Modcor1]
Modcor1 <- Modcor1[apply(Modcor1, 1, function(x) !any(is.na(x))),]

Modcor2 <- c("InnovationIndex", "Bribes", "Inspection.Bribe", "BribeIndex", 
             "PolicyObstacle", "InformalCompetition", "RD", "TechLicense", 
             "QualityCertificate", "Sector", "Small", "Medium", "Large", 
             "lnAge", "lnExperience", "Foreign", "Export", "TrainingEmployees")

sds <- df_model1
sds[] <- lapply(sds,as.numeric)


cormatweek <- round(cor(sds, method = "spearman"),2)


### Get upper triangle of the correlation matrix


get_upper_tri_week <- function(cormatweek){
  cormatweek[lower.tri(cormatweek)]<- NA
  return(cormatweek)
}


upper_tri_week <- get_upper_tri_week(cormatweek)
upper_tri_week
melted_cormat_week <- melt(upper_tri_week, na.rm = TRUE)



ggheatmap <- ggplot(data = melted_cormat_week, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+
  theme(axis.text.y = element_text(vjust = 1, 
                                   size = 8, hjust = 1))
### add numbers
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.75),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

###################################################################
# MODEL 1: y=SalesGrowth, x=Bribes
model1 <- c("Hallo", "Ich", "Bims")

df_model1 <- df[model1]
df_model1 <- df_model1[apply(df_model1, 1, function(x) !any(is.na(x))),]
str(df_model1)

##Outliers
hist(df_model1$Hallo)
ggboxplot(df_model1, y = "Hallo", width = 0.2)


###Hallo
outliers <- boxplot(df_model1$Hallo, plot=FALSE)$out
df_model1 <- df_model1[-which(df_model1$Hallo %in% outliers),]
ggboxplot(df_model1, y = "Hallo", width = 0.2)
df_model1 <- subset(df_model1,!(df_model1$Hallo > quantile(df_model1$Hallo, probs=c(.15, .85))[2] | df_model1$Hallo < quantile(df_model1$Hallo, probs=c(.1, .9))[1]) )

#####################################################################
##Regression
#####################################################################

OLS <- lm(y ~ x, data=df_model1)

summary(OLS)
stargazer(OLS, title="Results", align=TRUE, no.space=TRUE)

#####################################################################

Logit <- glm(y ~ x, data=df_model4, family = binomial(link = "logit"))

summary(Logit)
stargazer(Logit, title="Results", align=TRUE, no.space=TRUE)

###confinterval
logistic <- Model4.1.1
confint(logistic)
plot(logistic)

#### odds
exp(logistic$coefficients)

#### Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2

#### McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

#### chi-square value = 2*(LL(Proposed) - LL(Null))
#### p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((logistic$null.deviance - logistic$deviance), df=1)

#####################################################################
##Tests
#####################################################################
###Multicollinearity
cor(OLS$Hallo, OLS$SalesGrowth, use = "complete.obs")
cor <- cor(OLS)
correlation.matrix <- cor(OLS[, c(1, 2, 5, 6, 7, 8, 9, 11, 12, 13, 14, 18)], use = "complete.obs")
stargazer(correlation.matrix, title="Correlation Matrix")


vif_OLS <- vif(OLS)
vif_OLS <- round(vif_OLS, digits=2)
vif_OLS

###Diagnostics
par(mfrow=c(2,2))
plot(OLS) #or autoplot(model)

plot(OLS, 1) #Linearity
plot(OLS$Hallo, OLS$Ich)

plot(OLS, 2) #Normality
plot(OLS, 3) #Homoskedasticity
plot(OLS, 5) #Outliers

cooksd <- cooks.distance(OLS)
OLS %>%
  top_n(3, wt = cooksd)

###Heteroskedasticity
bptest(OLS)

###Normality
shapiro.test(resid(OLS))

###Autocorrelation
Box.test(resid(OLS), lag = 10, type = "Ljung")

#####################################################################
#####################################################################
# Instrumental Variable Approach
#####################################################################

library(AER)
library(ivpack)

ModelIV <- c()

ModelIV <- df[ModelIV]
ModelIV <- ModelIV[apply(ModelIV, 1, function(x) !any(is.na(x))),]

IV = ivreg(y ~ x1 + a1 + a2+ a3 | a1 + a2+ a3 + z1, 
            data=ModelIV)

IV = ivreg(SalesGrowth ~ x1 | z1, data=ModelIV)

summary(IV, vcov = sandwich, diagnostics = TRUE)

#####################################################################
#####################################################################
# Visualize Interaction Effects
#####################################################################

library(sjPlot)
library(sjmisc)
theme_set(theme_sjplot())

plot_model(Model1.2.4, type = "pred", terms = c("Bribes", "PolicyObstacle"))

tips %>% 
  ggplot() +
  aes(x = Bribes, color = PolicyObstacle, group = PolicyObstacle, y = SalesGrowth, data=df_model1) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")

#####################################################################

library(effects)

#Run the interaction 
Inter.HandPick <- effect('Inspection.Bribe*PolicyObstacle', Model1.3.4,
                         xlevels=list(PolicyObstacle = c(0, 1, 2, 3, 4),
                                      Inspection.Bribe = c(0, 1)),
                         se=TRUE, confidence.level=.95, typical=mean)

#Put data in data frame 
Inter.HandPick <- as.data.frame(Inter.HandPick)

#Check out what the "head" (first 6 rows) of your data looks like
head(Inter.HandPick)

#Create a factor of the IQ variable used in the interaction                   
Inter.HandPick$PolicyObstacle <- factor(Inter.HandPick$PolicyObstacle,
                                        levels=c(0, 1, 2, 3, 4),
                                        labels=c("No", "Minor", "Moderate", "Major", "Very severe"))

#Create a factor of the Work Ethic variable used in the interaction 
Inter.HandPick$Inspection.Bribe <- factor(Inter.HandPick$Inspection.Bribe,
                                          levels=c(0, 1),
                                          labels=c("No", "Yes"))

Plot.HandPick<-ggplot(data=Inter.HandPick, aes(x=Inspection.Bribe, y=fit, group=PolicyObstacle))+
  geom_line(size=1, aes(color=PolicyObstacle))+
  #scale_color_manual(values=wes_palette(n=5, name="Darjeeling2"))+
  scale_color_brewer(palette="RdYlGn", direction = -1)+
  ylim(-0.2,0.2)+
  ylab("SalesGrowth")+
  xlab("Inspection.Bribe")+
  ggtitle("Interaction Effect (Model 1.3.4)")+
  theme_stata(base_size = 10.7)


Plot.HandPick 
#################################
#Run the interaction 
Inter.HandPick2 <- effect('Bribes*InformalCompetition', Model1.1.6,
                          xlevels=list(InformalCompetition = c(1, 0),
                                       Bribes = c(0, 0.2)),
                          se=TRUE, confidence.level=.95, typical=mean)

#Put data in data frame 
Inter.HandPick2 <- as.data.frame(Inter.HandPick2)

#Check out what the "head" (first 6 rows) of your data looks like
head(Inter.HandPick2)

#Create a factor of the IQ variable used in the interaction                   
Inter.HandPick2$InformalCompetition <- factor(Inter.HandPick2$InformalCompetition,
                                              levels=c(1, 0),
                                              labels=c("Yes", "No"))

#Create a factor of the Work Ethic variable used in the interaction 
Inter.HandPick2$Bribes <- factor(Inter.HandPick2$Bribes,
                                 levels=c(0, 0.2),
                                 labels=c("No Bribes", "High Bribes"))

Plot.HandPick2<-ggplot(data=Inter.HandPick2, aes(x=Bribes, y=fit, group=InformalCompetition))+
  geom_line(size=1, aes(color=InformalCompetition))+
  scale_color_manual(name = "Informal Competition", values=c("red", "darkgreen"), labels = c("Yes", "No"))+
  ylim(0,1)+
  ylab("Sales Growth")+
  xlab("Bribes")+
  ggtitle("Interaction Effect (Model 1.1.6)")+
  theme_stata(base_size = 10.7)

Plot.HandPick2
