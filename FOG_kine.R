library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
library(DescTools)
library(randomForest)
library(gbm)
options(scipen = 999)
library(ISLR2)
library(leaps)
library(glmnet)

# Import data, define worst side -------------------------------------------------------------

FOG_kine <- fread("FOG_kine.txt")

names(FOG_kine)
FOG_kine <- gather(FOG_kine, Variable, Value, Cadence:COM_RMS_ML, factor_key=TRUE)

FOG_kine <- FOG_kine %>% mutate(To_delete = ifelse(grepl("Right", Variable) & worst_side == "Left", 1 ,
                                       ifelse(grepl("Left", Variable) & worst_side == "Right", 1, 0)))

FOG_kine <- FOG_kine %>% filter(To_delete == 0) %>% select(-To_delete)

FOG_kine <- FOG_kine %>% spread(key=Variable, value=Value)

FOG_kine <- FOG_kine %>% select(-GDI_Percent)

#  -------------------------------------------------------------
# Select Only Worst Side   -------------------------------------------------------------

FOG_kine <- FOG_kine %>% mutate(GDI_Percent_ws = ifelse( is.na(GDI_Percent_Left), GDI_Percent_Right, GDI_Percent_Left)) %>% 
  select(-c(GDI_Percent_Left, GDI_Percent_Right))

FOG_kine <- FOG_kine %>% mutate(Step_Time_ws = ifelse( is.na(Step_Time_Left), Step_Time_Right, Step_Time_Left)) %>% 
  select(-c(Step_Time_Left, Step_Time_Right))

FOG_kine <- FOG_kine %>% mutate(Step_Time_SD_ws = ifelse( is.na(Step_Time_Left_SD), Step_Time_Right_SD, Step_Time_Left_SD)) %>% 
  select(-c(Step_Time_Left_SD, Step_Time_Right_SD))

FOG_kine <- FOG_kine %>% mutate(Step_Time_Variability_ws = ifelse( is.na(Step_Time_Left_Variability), Step_Time_Right_Variability, Step_Time_Left_Variability)) %>% 
  select(-c(Step_Time_Left_Variability, Step_Time_Right_Variability))


FOG_kine <- FOG_kine %>% mutate(Swing_Time_ws = ifelse( is.na(Swing_Time_Left), Swing_Time_Right, Swing_Time_Left)) %>% 
  select(-c(Swing_Time_Left, Swing_Time_Right))

FOG_kine <- FOG_kine %>% mutate(Swing_Time_SD_ws = ifelse( is.na(Swing_Time_Left_SD), Swing_Time_Right_SD, Swing_Time_Left_SD)) %>% 
  select(-c(Swing_Time_Left_SD, Swing_Time_Right_SD))

FOG_kine <- FOG_kine %>% mutate(Swing_Time_Variability_ws = ifelse( is.na(Swing_Time_Left_Variability), Swing_Time_Right_Variability, Swing_Time_Left_Variability)) %>% 
  select(-c(Swing_Time_Left_Variability, Swing_Time_Right_Variability))


FOG_kine <- FOG_kine %>% mutate(Double_Support_Percent_ws = ifelse( is.na(Double_Support_Left_Percent), Double_Support_Right_Percent, Double_Support_Left_Percent)) %>% 
  select(-c(Double_Support_Left_Percent, Double_Support_Right_Percent))

FOG_kine <- FOG_kine %>% mutate(Double_Support_SD_ws = ifelse( is.na(Double_Support_Left_SD), Double_Support_Right_SD, Double_Support_Left_SD)) %>% 
  select(-c(Double_Support_Left_SD, Double_Support_Right_SD))

FOG_kine <- FOG_kine %>% mutate(Double_Support_Percent_Variability_ws = ifelse( is.na(Double_Support_Left_Percent_Variability), Double_Support_Right_Percent_Variability, Double_Support_Left_Percent_Variability)) %>% 
  select(-c(Double_Support_Left_Percent_Variability, Double_Support_Right_Percent_Variability))


FOG_kine <- FOG_kine %>% mutate(Stance_Time_Percent_ws = ifelse( is.na(Stance_Time_Left_Percent), Stance_Time_Right_Percent, Stance_Time_Left_Percent)) %>% 
  select(-c(Stance_Time_Left_Percent, Stance_Time_Right_Percent))

FOG_kine <- FOG_kine %>% mutate(Stance_Time_SD_ws = ifelse( is.na(Stance_Time_Left_SD), Stance_Time_Right_SD, Stance_Time_Left_SD)) %>% 
  select(-c(Stance_Time_Left_SD, Stance_Time_Right_SD))

FOG_kine <- FOG_kine %>% mutate(Stance_Time_Percent_Variability_ws = ifelse( is.na(Stance_Time_Left_Percent_Variability), Stance_Time_Right_Percent_Variability, Stance_Time_Left_Percent_Variability)) %>% 
  select(-c(Stance_Time_Left_Percent_Variability, Stance_Time_Right_Percent_Variability))


FOG_kine <- FOG_kine %>% mutate(Step_Length_ws = ifelse( is.na(Step_Length_Left), Step_Length_Right, Step_Length_Left)) %>% 
  select(-c(Step_Length_Left, Step_Length_Right))

FOG_kine <- FOG_kine %>% mutate(Step_Length_SD_ws = ifelse( is.na(Step_Length_Left_SD), Step_Length_Right_SD, Step_Length_Left_SD)) %>% 
  select(-c(Step_Length_Left_SD, Step_Length_Right_SD))

FOG_kine <- FOG_kine %>% mutate(Step_Length_Variability_ws = ifelse( is.na(Step_Length_Left_Variability), Step_Length_Right_Variability, Step_Length_Left_Variability)) %>% 
  select(-c(Step_Length_Left_Variability, Step_Length_Right_Variability))

fwrite(FOG_kine, "FOG_kine_ws.txt", sep="\t")

FOG_kine$condition <- as.factor(FOG_kine$condition)


FOG_kine <- FOG_kine %>% select(!contains("_SD"))
#  -------------------------------------------------------------

names(FOG_kine)

# FoG_Percent_StraightLine ---------------------------------


FOG_kine %>% 
  select(condition, FoG_Percent_StraightLine) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(FoG_Percent_StraightLine, na.rm=T))

wilcox.test(FoG_Percent_StraightLine ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",])


FOG_kine <- FOG_kine %>% mutate(FoG_Percent_StraightLine = ifelse( (is.na(FoG_Percent_StraightLine)&condition=="MedOFFStimOFF"), 100, 
                                 ifelse( ( is.na(FoG_Percent_StraightLine)&condition=="MedOFFStimON"), 92.2, 
                                        ifelse( (is.na(FoG_Percent_StraightLine)&condition=="MedONStimOFF"), 77.4,
                                         ifelse( (is.na(FoG_Percent_StraightLine)&condition=="ON130Hz"), 66.7, 
                                                 ifelse( (is.na(FoG_Percent_StraightLine)&condition=="ON60Hz"),87.8,  FoG_Percent_StraightLine)))))) 

wilcox.test(FoG_Percent_StraightLine ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON60Hz",], paired=T)


FOG_kine %>% 
  select(condition, FoG_Percent_StraightLine) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(FoG_Percent_StraightLine, na.rm=T))

FOG_kine$FoG_Percent_StraightLine <- FOG_kine$FoG_Percent_StraightLine + rnorm(FOG_kine$FoG_Percent_StraightLine, mean = 0, sd = 0.001)

friedman.test(y=FOG_kine$FoG_Percent_StraightLine, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$FoG_Percent_StraightLine, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$FoG_Percent_StraightLine, FOG_kine$condition, method ="BH")



# Speed ---------------------------------

FOG_kine %>% 
  select(condition, Speed) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Speed, na.rm=T))

wilcox.test(Speed ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",])


FOG_kine <- FOG_kine %>% mutate(Speed = ifelse( (is.na(Speed)&condition=="MedOFFStimOFF"), 0.283, 
                                 ifelse( ( is.na(Speed)&condition=="MedOFFStimON"), 0.211, 
                                        ifelse( (is.na(Speed)&condition=="MedONStimOFF"), 0.242,
                                         ifelse( (is.na(Speed)&condition=="ON130Hz"), 0.333, 
                                                 ifelse( (is.na(Speed)&condition=="ON60Hz"),0.351,  Speed)))))) 

wilcox.test(Speed ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine %>% 
  select(condition, Speed) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Speed, na.rm=T))

FOG_kine$Speed <- FOG_kine$Speed + rnorm(FOG_kine$Speed, mean = 0, sd = 0.001)


friedman.test(y=FOG_kine$Speed, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Speed, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Speed, FOG_kine$condition, method ="BH")


# Step Length ---------------------------------

FOG_kine %>% 
  select(condition, Step_Length_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=min(Step_Length_ws, na.rm=T))

wilcox.test(Step_Length_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",])


FOG_kine <- FOG_kine %>% mutate(Step_Length_ws = ifelse( (is.na(Step_Length_ws)&condition=="MedOFFStimOFF"), 2.48, 
                                 ifelse( ( is.na(Step_Length_ws)&condition=="MedOFFStimON"), 7.89, 
                                        ifelse( (is.na(Step_Length_ws)&condition=="MedONStimOFF"), 5.34,
                                         ifelse( (is.na(Step_Length_ws)&condition=="ON130Hz"), 7.03, 
                                                 ifelse( (is.na(Step_Length_ws)&condition=="ON60Hz"),7.26,  Step_Length_ws)))))) 

wilcox.test(Step_Length_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Step_Length_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON60Hz",], paired=T)



FOG_kine %>% 
  select(condition, Step_Length_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Step_Length_ws, na.rm=T))

FOG_kine$Step_Length_ws <- FOG_kine$Step_Length_ws + rnorm(FOG_kine$Step_Length_ws, mean = 0, sd = 0.001)

friedman.test(y=FOG_kine$Step_Length_ws, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Step_Length_ws, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Step_Length_ws, FOG_kine$condition, method ="BH")


# Cadence ---------------------------------

FOG_kine %>% 
  select(condition, Cadence) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=min(Cadence, na.rm=T))

wilcox.test(Cadence ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Cadence = ifelse( (is.na(Cadence)&condition=="MedOFFStimOFF"), 40.7, 
                                 ifelse( ( is.na(Cadence)&condition=="MedOFFStimON"), 61.2, 
                                        ifelse( (is.na(Cadence)&condition=="MedONStimOFF"), 28.6,
                                         ifelse( (is.na(Cadence)&condition=="ON130Hz"), 56.7, 
                                                 ifelse( (is.na(Cadence)&condition=="ON60Hz"),76.4,  Cadence)))))) 

wilcox.test(Cadence ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Cadence ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Cadence) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Cadence, na.rm=T))

FOG_kine$Cadence <- FOG_kine$Cadence + rnorm(FOG_kine$Cadence, mean = 0, sd = 0.0001)

friedman.test(y=FOG_kine$Cadence, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Cadence, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Cadence, FOG_kine$condition, method ="BH")


# Step Time ----------------------------

FOG_kine %>% 
  select(condition, Step_Time_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Step_Time_ws, na.rm=T))

wilcox.test(Step_Time_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Step_Time_ws = ifelse( (is.na(Step_Time_ws)&condition=="MedOFFStimOFF"), 1.59 , 
                                 ifelse( ( is.na(Step_Time_ws)&condition=="MedOFFStimON"), 0.975, 
                                        ifelse( (is.na(Step_Time_ws)&condition=="MedONStimOFF"), 2.09 ,
                                         ifelse( (is.na(Step_Time_ws)&condition=="ON130Hz"), 1.31 , 
                                                 ifelse( (is.na(Step_Time_ws)&condition=="ON60Hz"),0.75 ,  Step_Time_ws)))))) 

wilcox.test(Step_Time_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Step_Time_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Step_Time_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Step_Time_ws, na.rm=T))

FOG_kine$Step_Time_ws <- FOG_kine$Step_Time_ws + rnorm(FOG_kine$Step_Time_ws, mean = 0, sd = 0.0001)

friedman.test(y=FOG_kine$Step_Time_ws, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Step_Time_ws, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Step_Time_ws, FOG_kine$condition, method ="BH")



# Swing Time ----------------------------

FOG_kine %>% 
  select(condition, Swing_Time_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Swing_Time_ws, na.rm=T))

wilcox.test(Swing_Time_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Swing_Time_ws = ifelse( (is.na(Swing_Time_ws)&condition=="MedOFFStimOFF"), 64.7 , 
                                 ifelse( ( is.na(Swing_Time_ws)&condition=="MedOFFStimON"), 37.8, 
                                        ifelse( (is.na(Swing_Time_ws)&condition=="MedONStimOFF"), 55.4 ,
                                         ifelse( (is.na(Swing_Time_ws)&condition=="ON130Hz"), 58.7 , 
                                                 ifelse( (is.na(Swing_Time_ws)&condition=="ON60Hz"),66.9 ,  Swing_Time_ws)))))) 

wilcox.test(Swing_Time_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Swing_Time_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Swing_Time_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Swing_Time_ws, na.rm=T))

FOG_kine$Swing_Time_ws <- FOG_kine$Swing_Time_ws + rnorm(FOG_kine$Swing_Time_ws, mean = 0, sd = 0.0001)

friedman.test(y=FOG_kine$Swing_Time_ws, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Swing_Time_ws, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Swing_Time_ws, FOG_kine$condition, method ="BH")



# Entropy_AP ----------------------------

FOG_kine %>% 
  select(condition, Entropy_AP) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Entropy_AP, na.rm=T))

wilcox.test(Entropy_AP ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Entropy_AP = ifelse( (is.na(Entropy_AP)&condition=="MedOFFStimOFF"), 1.68 , 
                                 ifelse( ( is.na(Entropy_AP)&condition=="MedOFFStimON"), 1.87, 
                                        ifelse( (is.na(Entropy_AP)&condition=="MedONStimOFF"), 2.02 ,
                                         ifelse( (is.na(Entropy_AP)&condition=="ON130Hz"), 2.08 , 
                                                 ifelse( (is.na(Entropy_AP)&condition=="ON60Hz"),2.16 ,  Entropy_AP)))))) 

wilcox.test(Entropy_AP ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Entropy_AP ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Entropy_AP) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Entropy_AP, na.rm=T))

FOG_kine$Entropy_AP <- FOG_kine$Entropy_AP + rnorm(FOG_kine$Entropy_AP, mean = 0, sd = 0.0001)

friedman.test(y=FOG_kine$Entropy_AP, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Entropy_AP, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Entropy_AP, FOG_kine$condition, method ="BH")




# Entropy_Vert ----------------------------

FOG_kine %>% 
  select(condition, Entropy_Vert) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Entropy_Vert, na.rm=T))

wilcox.test(Entropy_Vert ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Entropy_Vert = ifelse( (is.na(Entropy_Vert)&condition=="MedOFFStimOFF"), 2.14 , 
                                 ifelse( ( is.na(Entropy_Vert)&condition=="MedOFFStimON"), 2.17, 
                                        ifelse( (is.na(Entropy_Vert)&condition=="MedONStimOFF"), 2.15 ,
                                         ifelse( (is.na(Entropy_Vert)&condition=="ON130Hz"), 2.12 , 
                                                 ifelse( (is.na(Entropy_Vert)&condition=="ON60Hz"),2.17 ,  Entropy_Vert)))))) 

wilcox.test(Entropy_Vert ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Entropy_Vert ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Entropy_Vert) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Entropy_Vert, na.rm=T))

FOG_kine$Entropy_Vert <- FOG_kine$Entropy_Vert + rnorm(FOG_kine$Entropy_Vert, mean = 0, sd = 0.0001)

friedman.test(y=FOG_kine$Entropy_Vert, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Entropy_Vert, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Entropy_Vert, FOG_kine$condition, method ="BH")

# Entropy_ML ----------------------------

FOG_kine %>% 
  select(condition, Entropy_ML) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Entropy_ML, na.rm=T))

wilcox.test(Entropy_Vert ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Entropy_ML = ifelse( (is.na(Entropy_ML)&condition=="MedOFFStimOFF"), 0.700 , 
                                 ifelse( ( is.na(Entropy_ML)&condition=="MedOFFStimON"), 1.39 , 
                                        ifelse( (is.na(Entropy_ML)&condition=="MedONStimOFF"), 1.23  ,
                                         ifelse( (is.na(Entropy_ML)&condition=="ON130Hz"), 1.22  , 
                                                 ifelse( (is.na(Entropy_ML)&condition=="ON60Hz"),1.13  ,  Entropy_ML)))))) 

wilcox.test(Entropy_ML ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Entropy_ML ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Entropy_ML) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Entropy_ML, na.rm=T))

FOG_kine$Entropy_ML <- FOG_kine$Entropy_ML + rnorm(FOG_kine$Entropy_ML, mean = 0, sd = 0.0001)

friedman.test(y=FOG_kine$Entropy_ML, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Entropy_ML, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Entropy_ML, FOG_kine$condition, method ="BH")

# HR_AP ----------------------------

FOG_kine %>% 
  select(condition, HR_AP) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(HR_AP, na.rm=T))

wilcox.test(HR_AP ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(HR_AP = ifelse( (is.na(HR_AP)&condition=="MedOFFStimOFF"), 0.260 , 
                                 ifelse( ( is.na(HR_AP)&condition=="MedOFFStimON"), 0.287  , 
                                        ifelse( (is.na(HR_AP)&condition=="MedONStimOFF"), 0.347   ,
                                         ifelse( (is.na(HR_AP)&condition=="ON130Hz"), 0.284   , 
                                                 ifelse( (is.na(HR_AP)&condition=="ON60Hz"),0.143   ,  HR_AP)))))) 

wilcox.test(HR_AP ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(HR_AP ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, HR_AP) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(HR_AP, na.rm=T))

FOG_kine$HR_AP <- FOG_kine$HR_AP + rnorm(FOG_kine$HR_AP, mean = 0, sd = 0.0001)

friedman.test(y=FOG_kine$HR_AP, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$HR_AP, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$HR_AP, FOG_kine$condition, method ="BH")
# HR_Vert ----------------------------

FOG_kine %>% 
  select(condition, HR_Vert) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=min(HR_Vert, na.rm=T))

wilcox.test(HR_Vert ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(HR_Vert = ifelse( (is.na(HR_Vert)&condition=="MedOFFStimOFF"), 0.147 , 
                                 ifelse( ( is.na(HR_Vert)&condition=="MedOFFStimON"), 0.190  , 
                                        ifelse( (is.na(HR_Vert)&condition=="MedONStimOFF"), 0.263   ,
                                         ifelse( (is.na(HR_Vert)&condition=="ON130Hz"), 0.230   , 
                                                 ifelse( (is.na(HR_Vert)&condition=="ON60Hz"),0.248   ,  HR_Vert)))))) 

wilcox.test(HR_Vert ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(HR_Vert ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, HR_Vert) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(HR_Vert, na.rm=T))

FOG_kine$HR_Vert <- FOG_kine$HR_Vert + rnorm(FOG_kine$HR_Vert, mean = 0, sd = 0.0001)

friedman.test(y=FOG_kine$HR_Vert, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$HR_Vert, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$HR_Vert, FOG_kine$condition, method ="BH")




# HR_ML ----------------------------

FOG_kine %>% 
  select(condition, HR_ML) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=min(HR_ML, na.rm=T))

wilcox.test(HR_ML ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(HR_ML = ifelse( (is.na(HR_ML)&condition=="MedOFFStimOFF"), 0.460 , 
                                 ifelse( ( is.na(HR_ML)&condition=="MedOFFStimON"), 0.932  , 
                                        ifelse( (is.na(HR_ML)&condition=="MedONStimOFF"), 0.645   ,
                                         ifelse( (is.na(HR_ML)&condition=="ON130Hz"), 0.843   , 
                                                 ifelse( (is.na(HR_ML)&condition=="ON60Hz"),0.680   ,  HR_ML)))))) 

wilcox.test(HR_ML ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(HR_ML ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, HR_ML) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(HR_ML, na.rm=T))

FOG_kine$HR_ML <- FOG_kine$HR_ML + rnorm(FOG_kine$HR_ML, mean = 0, sd = 0.0001)

friedman.test(y=FOG_kine$HR_ML, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$HR_ML, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$HR_ML, FOG_kine$condition, method ="BH")





# COM_RMS_AP ----------------------------

FOG_kine %>% 
  select(condition, COM_RMS_AP) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=min(COM_RMS_AP, na.rm=T))

wilcox.test(COM_RMS_AP ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(COM_RMS_AP = ifelse( (is.na(COM_RMS_AP)&condition=="MedOFFStimOFF"), 0.00606 , 
                                 ifelse( ( is.na(COM_RMS_AP)&condition=="MedOFFStimON"), 0.00711  , 
                                        ifelse( (is.na(COM_RMS_AP)&condition=="MedONStimOFF"), 0.0116    ,
                                         ifelse( (is.na(COM_RMS_AP)&condition=="ON130Hz"), 0.00859   , 
                                                 ifelse( (is.na(COM_RMS_AP)&condition=="ON60Hz"),0.00597   ,  COM_RMS_AP)))))) 

wilcox.test(COM_RMS_AP ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(COM_RMS_AP ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, COM_RMS_AP) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(COM_RMS_AP, na.rm=T))

FOG_kine$COM_RMS_AP <- FOG_kine$COM_RMS_AP + rnorm(FOG_kine$COM_RMS_AP, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$COM_RMS_AP, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$COM_RMS_AP, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$COM_RMS_AP, FOG_kine$condition, method ="BH")


# COM_RMS_Vert ----------------------------

FOG_kine %>% 
  select(condition, COM_RMS_Vert) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=min(COM_RMS_Vert, na.rm=T))

wilcox.test(COM_RMS_Vert ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(COM_RMS_Vert = ifelse( (is.na(COM_RMS_Vert)&condition=="MedOFFStimOFF"), 0.00994 , 
                                 ifelse( ( is.na(COM_RMS_Vert)&condition=="MedOFFStimON"), 0.00863  , 
                                        ifelse( (is.na(COM_RMS_Vert)&condition=="MedONStimOFF"), 0.00719    ,
                                         ifelse( (is.na(COM_RMS_Vert)&condition=="ON130Hz"), 0.0105    , 
                                                 ifelse( (is.na(COM_RMS_Vert)&condition=="ON60Hz"),0.00305   ,  COM_RMS_Vert)))))) 

wilcox.test(COM_RMS_Vert ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(COM_RMS_Vert ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, COM_RMS_Vert) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(COM_RMS_Vert, na.rm=T))

FOG_kine$COM_RMS_Vert <- FOG_kine$COM_RMS_Vert + rnorm(FOG_kine$COM_RMS_Vert, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$COM_RMS_Vert, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$COM_RMS_Vert, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$COM_RMS_Vert, FOG_kine$condition, method ="BH")

# COM_RMS_ML ----------------------------

FOG_kine %>% 
  select(condition, COM_RMS_ML) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=min(COM_RMS_ML, na.rm=T))

wilcox.test(COM_RMS_ML ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(COM_RMS_ML = ifelse( (is.na(COM_RMS_ML)&condition=="MedOFFStimOFF"), 0.00308 , 
                                 ifelse( ( is.na(COM_RMS_ML)&condition=="MedOFFStimON"), 0.00377  , 
                                        ifelse( (is.na(COM_RMS_ML)&condition=="MedONStimOFF"), 0.00655    ,
                                         ifelse( (is.na(COM_RMS_ML)&condition=="ON130Hz"), 0.00569    , 
                                                 ifelse( (is.na(COM_RMS_ML)&condition=="ON60Hz"),0.00587   ,  COM_RMS_ML)))))) 

wilcox.test(COM_RMS_ML ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(COM_RMS_ML ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, COM_RMS_ML) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(COM_RMS_ML, na.rm=T))

FOG_kine$COM_RMS_ML <- FOG_kine$COM_RMS_ML + rnorm(FOG_kine$COM_RMS_ML, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$COM_RMS_ML, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$COM_RMS_ML, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$COM_RMS_ML, FOG_kine$condition, method ="BH")

# GDI_Percent_ws ----------------------------

FOG_kine %>% 
  select(condition, GDI_Percent_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(GDI_Percent_ws, na.rm=T))

wilcox.test(GDI_Percent_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(GDI_Percent_ws = ifelse( (is.na(GDI_Percent_ws)&condition=="MedOFFStimOFF"), 93.4 , 
                                 ifelse( ( is.na(GDI_Percent_ws)&condition=="MedOFFStimON"), 104  , 
                                        ifelse( (is.na(GDI_Percent_ws)&condition=="MedONStimOFF"), 99.2    ,
                                         ifelse( (is.na(GDI_Percent_ws)&condition=="ON130Hz"), 98.9    , 
                                                 ifelse( (is.na(GDI_Percent_ws)&condition=="ON60Hz"),102   ,  GDI_Percent_ws)))))) 

wilcox.test(GDI_Percent_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(GDI_Percent_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, GDI_Percent_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(GDI_Percent_ws, na.rm=T))

FOG_kine$GDI_Percent_ws <- FOG_kine$GDI_Percent_ws + rnorm(FOG_kine$GDI_Percent_ws, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$GDI_Percent_ws, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$GDI_Percent_ws, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$GDI_Percent_ws, FOG_kine$condition, method ="BH")

# Cycle_Time ----------------------------

FOG_kine %>% 
  select(condition, Cycle_Time) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Cycle_Time, na.rm=T))

wilcox.test(Cycle_Time ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Cycle_Time = ifelse( (is.na(Cycle_Time)&condition=="MedOFFStimOFF"), 5.09 , 
                                 ifelse( ( is.na(Cycle_Time)&condition=="MedOFFStimON"), 2.06  , 
                                        ifelse( (is.na(Cycle_Time)&condition=="MedONStimOFF"), 3.91    ,
                                         ifelse( (is.na(Cycle_Time)&condition=="ON130Hz"), 2.66    , 
                                                 ifelse( (is.na(Cycle_Time)&condition=="ON60Hz"),2.15   ,  Cycle_Time)))))) 

wilcox.test(Cycle_Time ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Cycle_Time ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Cycle_Time) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Cycle_Time, na.rm=T))

FOG_kine$Cycle_Time <- FOG_kine$Cycle_Time + rnorm(FOG_kine$Cycle_Time, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Cycle_Time, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Cycle_Time, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Cycle_Time, FOG_kine$condition, method ="BH")

# Step_Width ----------------------------

FOG_kine %>% 
  select(condition, Step_Width) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=min(Step_Width, na.rm=T))

wilcox.test(Step_Width ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Step_Width = ifelse( (is.na(Step_Width)&condition=="MedOFFStimOFF"), 7.90 , 
                                 ifelse( ( is.na(Step_Width)&condition=="MedOFFStimON"), 10.8   , 
                                        ifelse( (is.na(Step_Width)&condition=="MedONStimOFF"), 9.78    ,
                                         ifelse( (is.na(Step_Width)&condition=="ON130Hz"), 13.5     , 
                                                 ifelse( (is.na(Step_Width)&condition=="ON60Hz"),6.44   ,  Step_Width)))))) 

wilcox.test(Step_Width ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Step_Width ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Step_Width) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Step_Width, na.rm=T))

FOG_kine$Step_Width <- FOG_kine$Step_Width + rnorm(FOG_kine$Step_Width, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Step_Width, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Step_Width, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Step_Width, FOG_kine$condition, method ="BH")



# Stance_Time_Percent_ws ----------------------------

FOG_kine %>% 
  select(condition, Stance_Time_Percent_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=min(Stance_Time_Percent_ws, na.rm=T))

wilcox.test(Stance_Time_Percent_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Stance_Time_Percent_ws = ifelse( (is.na(Stance_Time_Percent_ws)&condition=="MedOFFStimOFF"), 35.3 , 
                                 ifelse( ( is.na(Stance_Time_Percent_ws)&condition=="MedOFFStimON"), 62.2   , 
                                        ifelse( (is.na(Stance_Time_Percent_ws)&condition=="MedONStimOFF"), 44.6    ,
                                         ifelse( (is.na(Stance_Time_Percent_ws)&condition=="ON130Hz"), 41.3     , 
                                                 ifelse( (is.na(Stance_Time_Percent_ws)&condition=="ON60Hz"),33.1   ,  Stance_Time_Percent_ws)))))) 

wilcox.test(Stance_Time_Percent_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Stance_Time_Percent_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Stance_Time_Percent_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Stance_Time_Percent_ws, na.rm=T))

FOG_kine$Stance_Time_Percent_ws <- FOG_kine$Stance_Time_Percent_ws + rnorm(FOG_kine$Stance_Time_Percent_ws, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Stance_Time_Percent_ws, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Stance_Time_Percent_ws, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Stance_Time_Percent_ws, FOG_kine$condition, method ="BH")
# Double_Support_Percent_ws ----------------------------

FOG_kine %>% 
  select(condition, Double_Support_Percent_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=min(Double_Support_Percent_ws, na.rm=T))

wilcox.test(Double_Support_Percent_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Double_Support_Percent_ws = ifelse( (is.na(Double_Support_Percent_ws)&condition=="MedOFFStimOFF"), 37.1 , 
                                 ifelse( ( is.na(Double_Support_Percent_ws)&condition=="MedOFFStimON"), 48.2   , 
                                        ifelse( (is.na(Double_Support_Percent_ws)&condition=="MedONStimOFF"), 47.9    ,
                                         ifelse( (is.na(Double_Support_Percent_ws)&condition=="ON130Hz"), 43.6     , 
                                                 ifelse( (is.na(Double_Support_Percent_ws)&condition=="ON60Hz"),58.4   ,  Double_Support_Percent_ws)))))) 

wilcox.test(Double_Support_Percent_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Double_Support_Percent_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Double_Support_Percent_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Double_Support_Percent_ws, na.rm=T))

FOG_kine$Double_Support_Percent_ws <- FOG_kine$Double_Support_Percent_ws + rnorm(FOG_kine$Double_Support_Percent_ws, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Double_Support_Percent_ws, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Double_Support_Percent_ws, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Double_Support_Percent_ws, FOG_kine$condition, method ="BH")

# Step_Time_Asymmetry ----------------------------

FOG_kine %>% 
  select(condition, Step_Time_Asymmetry) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Step_Time_Asymmetry, na.rm=T))

wilcox.test(Step_Time_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Step_Time_Asymmetry = ifelse( (is.na(Step_Time_Asymmetry)&condition=="MedOFFStimOFF"), 132 , 
                                 ifelse( ( is.na(Step_Time_Asymmetry)&condition=="MedOFFStimON"), 87.4   , 
                                        ifelse( (is.na(Step_Time_Asymmetry)&condition=="MedONStimOFF"), 78.3    ,
                                         ifelse( (is.na(Step_Time_Asymmetry)&condition=="ON130Hz"), 73.0     , 
                                                 ifelse( (is.na(Step_Time_Asymmetry)&condition=="ON60Hz"),74.6   ,  Step_Time_Asymmetry)))))) 

wilcox.test(Step_Time_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Step_Time_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Step_Time_Asymmetry) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Step_Time_Asymmetry, na.rm=T))

FOG_kine$Step_Time_Asymmetry <- FOG_kine$Step_Time_Asymmetry + rnorm(FOG_kine$Step_Time_Asymmetry, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Step_Time_Asymmetry, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Step_Time_Asymmetry, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Step_Time_Asymmetry, FOG_kine$condition, method ="BH")


# Step_Length_Asymmetry ----------------------------

FOG_kine %>% 
  select(condition, Step_Length_Asymmetry) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Step_Length_Asymmetry, na.rm=T))

wilcox.test(Step_Length_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Step_Length_Asymmetry = ifelse( (is.na(Step_Length_Asymmetry)&condition=="MedOFFStimOFF"), 74.7 , 
                                 ifelse( ( is.na(Step_Length_Asymmetry)&condition=="MedOFFStimON"), 75.5   , 
                                        ifelse( (is.na(Step_Length_Asymmetry)&condition=="MedONStimOFF"), 107    ,
                                         ifelse( (is.na(Step_Length_Asymmetry)&condition=="ON130Hz"), 120     , 
                                                 ifelse( (is.na(Step_Length_Asymmetry)&condition=="ON60Hz"),111   ,  Step_Length_Asymmetry)))))) 

wilcox.test(Step_Length_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Step_Length_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Step_Length_Asymmetry) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Step_Length_Asymmetry, na.rm=T))

FOG_kine$Step_Length_Asymmetry <- FOG_kine$Step_Length_Asymmetry + rnorm(FOG_kine$Step_Length_Asymmetry, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Step_Length_Asymmetry, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Step_Length_Asymmetry, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Step_Length_Asymmetry, FOG_kine$condition, method ="BH")

# Swing_Time_Asymmetry ----------------------------

FOG_kine %>% 
  select(condition, Swing_Time_Asymmetry) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Swing_Time_Asymmetry, na.rm=T))

wilcox.test(Swing_Time_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Swing_Time_Asymmetry = ifelse( (is.na(Swing_Time_Asymmetry)&condition=="MedOFFStimOFF"), 38.8 , 
                                 ifelse( ( is.na(Swing_Time_Asymmetry)&condition=="MedOFFStimON"), 35.7   , 
                                        ifelse( (is.na(Swing_Time_Asymmetry)&condition=="MedONStimOFF"), 47.0    ,
                                         ifelse( (is.na(Swing_Time_Asymmetry)&condition=="ON130Hz"), 60.9     , 
                                                 ifelse( (is.na(Swing_Time_Asymmetry)&condition=="ON60Hz"),79.5   ,  Swing_Time_Asymmetry)))))) 

wilcox.test(Swing_Time_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Swing_Time_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Swing_Time_Asymmetry) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Swing_Time_Asymmetry, na.rm=T))

FOG_kine$Swing_Time_Asymmetry <- FOG_kine$Swing_Time_Asymmetry + rnorm(FOG_kine$Swing_Time_Asymmetry, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Swing_Time_Asymmetry, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Swing_Time_Asymmetry, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Swing_Time_Asymmetry, FOG_kine$condition, method ="BH")

# Double_Support_Percent_Asymmetry ----------------------------

FOG_kine %>% 
  select(condition, Double_Support_Percent_Asymmetry) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=min(Double_Support_Percent_Asymmetry, na.rm=T))

wilcox.test(Double_Support_Percent_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Double_Support_Percent_Asymmetry = ifelse( (is.na(Double_Support_Percent_Asymmetry)&condition=="MedOFFStimOFF"), 1.34  , 
                                 ifelse( ( is.na(Double_Support_Percent_Asymmetry)&condition=="MedOFFStimON"), 0.420   , 
                                        ifelse( (is.na(Double_Support_Percent_Asymmetry)&condition=="MedONStimOFF"), 0.528    ,
                                         ifelse( (is.na(Double_Support_Percent_Asymmetry)&condition=="ON130Hz"), 2.00      , 
                                                 ifelse( (is.na(Double_Support_Percent_Asymmetry)&condition=="ON60Hz"),12.1     ,  Double_Support_Percent_Asymmetry)))))) 

wilcox.test(Double_Support_Percent_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Double_Support_Percent_Asymmetry ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Double_Support_Percent_Asymmetry) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Double_Support_Percent_Asymmetry, na.rm=T))

FOG_kine$Double_Support_Percent_Asymmetry <- FOG_kine$Double_Support_Percent_Asymmetry + rnorm(FOG_kine$Double_Support_Percent_Asymmetry, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Double_Support_Percent_Asymmetry, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Double_Support_Percent_Asymmetry, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Double_Support_Percent_Asymmetry, FOG_kine$condition, method ="BH")
# Speed_Variability ----------------------------

FOG_kine %>% 
  select(condition, Speed_Variability) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Speed_Variability, na.rm=T))

wilcox.test(Speed_Variability ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Speed_Variability = ifelse( (is.na(Speed_Variability)&condition=="MedOFFStimOFF"), 322  , 
                                 ifelse( ( is.na(Speed_Variability)&condition=="MedOFFStimON"), 317   , 
                                        ifelse( (is.na(Speed_Variability)&condition=="MedONStimOFF"), 283    ,
                                         ifelse( (is.na(Speed_Variability)&condition=="ON130Hz"), 237      , 
                                                 ifelse( (is.na(Speed_Variability)&condition=="ON60Hz"),367     ,  Speed_Variability)))))) 

wilcox.test(Speed_Variability ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Speed_Variability ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Speed_Variability) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Speed_Variability, na.rm=T))

FOG_kine$Speed_Variability <- FOG_kine$Speed_Variability + rnorm(FOG_kine$Speed_Variability, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Speed_Variability, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Speed_Variability, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Speed_Variability, FOG_kine$condition, method ="BH")

# Step_Width_Variability ----------------------------

FOG_kine %>% 
  select(condition, Step_Width_Variability) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Step_Width_Variability, na.rm=T))

wilcox.test(Step_Width_Variability ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Step_Width_Variability = ifelse( (is.na(Step_Width_Variability)&condition=="MedOFFStimOFF"), 73.9  , 
                                 ifelse( ( is.na(Step_Width_Variability)&condition=="MedOFFStimON"), 52.7   , 
                                        ifelse( (is.na(Step_Width_Variability)&condition=="MedONStimOFF"), 99.6    ,
                                         ifelse( (is.na(Step_Width_Variability)&condition=="ON130Hz"), 63.9      , 
                                                 ifelse( (is.na(Step_Width_Variability)&condition=="ON60Hz"),63.3     ,  Step_Width_Variability)))))) 

wilcox.test(Step_Width_Variability ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Step_Width_Variability ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Step_Width_Variability) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Step_Width_Variability, na.rm=T))

FOG_kine$Step_Width_Variability <- FOG_kine$Step_Width_Variability + rnorm(FOG_kine$Step_Width_Variability, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Step_Width_Variability, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Step_Width_Variability, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Step_Width_Variability, FOG_kine$condition, method ="BH")
# Cycle_Time_Variability ----------------------------

FOG_kine %>% 
  select(condition, Cycle_Time_Variability) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Cycle_Time_Variability, na.rm=T))

wilcox.test(Cycle_Time_Variability ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Cycle_Time_Variability = ifelse( (is.na(Cycle_Time_Variability)&condition=="MedOFFStimOFF"), 169  , 
                                 ifelse( ( is.na(Cycle_Time_Variability)&condition=="MedOFFStimON"), 80.7   , 
                                        ifelse( (is.na(Cycle_Time_Variability)&condition=="MedONStimOFF"), 105    ,
                                         ifelse( (is.na(Cycle_Time_Variability)&condition=="ON130Hz"), 108      , 
                                                 ifelse( (is.na(Cycle_Time_Variability)&condition=="ON60Hz"),198     ,  Cycle_Time_Variability)))))) 

wilcox.test(Cycle_Time_Variability ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Cycle_Time_Variability ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Cycle_Time_Variability) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Cycle_Time_Variability, na.rm=T))

FOG_kine$Cycle_Time_Variability <- FOG_kine$Cycle_Time_Variability + rnorm(FOG_kine$Cycle_Time_Variability, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Cycle_Time_Variability, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Cycle_Time_Variability, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Cycle_Time_Variability, FOG_kine$condition, method ="BH")

# Step_Time_Variability_ws ----------------------------

FOG_kine %>% 
  select(condition, Step_Time_Variability_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Step_Time_Variability_ws, na.rm=T))

wilcox.test(Step_Time_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Step_Time_Variability_ws = ifelse( (is.na(Step_Time_Variability_ws)&condition=="MedOFFStimOFF"), 135  , 
                                 ifelse( ( is.na(Step_Time_Variability_ws)&condition=="MedOFFStimON"), 106   , 
                                        ifelse( (is.na(Step_Time_Variability_ws)&condition=="MedONStimOFF"), 137    ,
                                         ifelse( (is.na(Step_Time_Variability_ws)&condition=="ON130Hz"), 130      , 
                                                 ifelse( (is.na(Step_Time_Variability_ws)&condition=="ON60Hz"),82.2     ,  Step_Time_Variability_ws)))))) 

wilcox.test(Step_Time_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Step_Time_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Step_Time_Variability_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Step_Time_Variability_ws, na.rm=T))

FOG_kine$Step_Time_Variability_ws <- FOG_kine$Step_Time_Variability_ws + rnorm(FOG_kine$Step_Time_Variability_ws, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Step_Time_Variability_ws, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Step_Time_Variability_ws, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Step_Time_Variability_ws, FOG_kine$condition, method ="BH")
# Swing_Time_Variability_ws ----------------------------

FOG_kine %>% 
  select(condition, Swing_Time_Variability_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Swing_Time_Variability_ws, na.rm=T))

wilcox.test(Step_Time_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Swing_Time_Variability_ws = ifelse( (is.na(Swing_Time_Variability_ws)&condition=="MedOFFStimOFF"), 59.9  , 
                                 ifelse( ( is.na(Swing_Time_Variability_ws)&condition=="MedOFFStimON"), 85.2   , 
                                        ifelse( (is.na(Swing_Time_Variability_ws)&condition=="MedONStimOFF"), 94.8    ,
                                         ifelse( (is.na(Swing_Time_Variability_ws)&condition=="ON130Hz"), 67.8      , 
                                                 ifelse( (is.na(Swing_Time_Variability_ws)&condition=="ON60Hz"),53.3     ,  Swing_Time_Variability_ws)))))) 

wilcox.test(Swing_Time_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Swing_Time_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Swing_Time_Variability_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Swing_Time_Variability_ws, na.rm=T))

FOG_kine$Swing_Time_Variability_ws <- FOG_kine$Swing_Time_Variability_ws + rnorm(FOG_kine$Swing_Time_Variability_ws, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Swing_Time_Variability_ws, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Swing_Time_Variability_ws, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Swing_Time_Variability_ws, FOG_kine$condition, method ="BH")
# Double_Support_Percent_Variability_ws ----------------------------

FOG_kine %>% 
  select(condition, Double_Support_Percent_Variability_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Double_Support_Percent_Variability_ws, na.rm=T))

wilcox.test(Double_Support_Percent_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Double_Support_Percent_Variability_ws = ifelse( (is.na(Double_Support_Percent_Variability_ws)&condition=="MedOFFStimOFF"), 76.1  , 
                                 ifelse( ( is.na(Double_Support_Percent_Variability_ws)&condition=="MedOFFStimON"), 75.5   , 
                                        ifelse( (is.na(Double_Support_Percent_Variability_ws)&condition=="MedONStimOFF"), 85.5    ,
                                         ifelse( (is.na(Double_Support_Percent_Variability_ws)&condition=="ON130Hz"), 101      , 
                                                 ifelse( (is.na(Double_Support_Percent_Variability_ws)&condition=="ON60Hz"),73.7     ,  Double_Support_Percent_Variability_ws)))))) 

wilcox.test(Double_Support_Percent_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Double_Support_Percent_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Double_Support_Percent_Variability_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Double_Support_Percent_Variability_ws, na.rm=T))

FOG_kine$Double_Support_Percent_Variability_ws <- FOG_kine$Double_Support_Percent_Variability_ws + rnorm(FOG_kine$Double_Support_Percent_Variability_ws, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Double_Support_Percent_Variability_ws, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Double_Support_Percent_Variability_ws, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Double_Support_Percent_Variability_ws, FOG_kine$condition, method ="BH")
# Stance_Time_Percent_Variability_ws ----------------------------

FOG_kine %>% 
  select(condition, Stance_Time_Percent_Variability_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=max(Stance_Time_Percent_Variability_ws, na.rm=T))

wilcox.test(Stance_Time_Percent_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Stance_Time_Percent_Variability_ws = ifelse( (is.na(Stance_Time_Percent_Variability_ws)&condition=="MedOFFStimOFF"), 64.5  , 
                                 ifelse( ( is.na(Stance_Time_Percent_Variability_ws)&condition=="MedOFFStimON"), 36.3   , 
                                        ifelse( (is.na(Stance_Time_Percent_Variability_ws)&condition=="MedONStimOFF"), 53.0    ,
                                         ifelse( (is.na(Stance_Time_Percent_Variability_ws)&condition=="ON130Hz"), 68.2      , 
                                                 ifelse( (is.na(Stance_Time_Percent_Variability_ws)&condition=="ON60Hz"),49.7     ,  Stance_Time_Percent_Variability_ws)))))) 

wilcox.test(Stance_Time_Percent_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Stance_Time_Percent_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Stance_Time_Percent_Variability_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Stance_Time_Percent_Variability_ws, na.rm=T))

FOG_kine$Stance_Time_Percent_Variability_ws <- FOG_kine$Stance_Time_Percent_Variability_ws + rnorm(FOG_kine$Stance_Time_Percent_Variability_ws, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Double_Support_Percent_Variability_ws, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Stance_Time_Percent_Variability_ws, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Stance_Time_Percent_Variability_ws, FOG_kine$condition, method ="BH")
# Step_Length_Variability_ws ----------------------------

FOG_kine %>% 
  select(condition, Step_Length_Variability_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Step_Length_Variability_ws, na.rm=T))

wilcox.test(Step_Length_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",])


FOG_kine <- FOG_kine %>% mutate(Step_Length_Variability_ws = ifelse( (is.na(Step_Length_Variability_ws)&condition=="MedOFFStimOFF"), 47.9  , 
                                 ifelse( ( is.na(Step_Length_Variability_ws)&condition=="MedOFFStimON"), 31.5   , 
                                        ifelse( (is.na(Step_Length_Variability_ws)&condition=="MedONStimOFF"), 45.7    ,
                                         ifelse( (is.na(Step_Length_Variability_ws)&condition=="ON130Hz"), 42.7      , 
                                                 ifelse( (is.na(Step_Length_Variability_ws)&condition=="ON60Hz"),46.7     ,  Step_Length_Variability_ws)))))) 

wilcox.test(Step_Length_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="MedOFFStimON",], paired=T)
wilcox.test(Step_Length_Variability_ws ~ condition, data=FOG_kine[FOG_kine$condition=="MedOFFStimOFF"|FOG_kine$condition=="ON130Hz",], paired=T)



FOG_kine %>% 
  select(condition, Step_Length_Variability_ws) %>%
  arrange(condition) %>%
 group_by(condition) %>% summarise(n=mean(Step_Length_Variability_ws, na.rm=T))

FOG_kine$Step_Length_Variability_ws <- FOG_kine$Step_Length_Variability_ws + rnorm(FOG_kine$Step_Length_Variability_ws, mean = 0, sd = 0.000001)

friedman.test(y=FOG_kine$Step_Length_Variability_ws, groups=FOG_kine$condition, blocks=FOG_kine$patient_name)

pairwise.wilcox.test(FOG_kine$Step_Length_Variability_ws, FOG_kine$condition, p.adj = "BH", paired=T)
ConoverTest(FOG_kine$Step_Length_Variability_ws, FOG_kine$condition, method ="BH")
# -----------------------------------
# Save complete file ----------
fwrite(FOG_kine, "FOG_kine_ws.txt", sep="\t")

# Means / Medians / Etc -----------------------------------------
FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")
FOG_kine <- FOG_kine %>% select(-worst_side)
FOG_kine$condition <- as.factor(FOG_kine$condition)
FOG_kine <- FOG_kine %>% mutate(FoG_Percent_StraightLine=ifelse(FoG_Percent_StraightLine<0,0,FoG_Percent_StraightLine))
fwrite(FOG_kine, "FOG_kine_ws.txt", sep="\t")

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")

sum(FOG_kine$FoG_Percent_StraightLine<0)

data.frame(names(FOG_kine))

deter_data_clinical <- fread("deter_data_clinical.csv")

unique(deter_data_clinical$condition)

pvalues <- data.frame()


for(i in names(FOG_kine)[3:34]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  FOG_kine_temp <- FOG_kine %>% filter(condition == "MedOFFStimOFF" | condition == "ON60Hz") 
  WCT <- wilcox.test(get(i)~condition, data = FOG_kine_temp, paired=T)
  pvalues <- pvalues %>% bind_rows(data.frame(WCT$p.value)) 
  
}




means <- data.frame()
means <- round(means, 3)

for(i in names(FOG_kine)[3:34]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  tempmean <- data.frame(FOG_kine %>%  group_by(condition) %>% summarise(n=mean(get(i), na.rm = T)) %>%
    spread(key=condition, value=n))
    means <- means %>% bind_rows(tempmean) 

}


medians <- data.frame()
medians <- round(medians, 3)

for(i in names(FOG_kine)[3:34]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  tempmedians <- data.frame(FOG_kine %>%  group_by(condition) %>% summarise(n=median(get(i), na.rm = T)) %>%
    spread(key=condition, value=n))
    medians <- medians %>% bind_rows(tempmedians) 

}

SDS <- data.frame()
SDS <- round(SDS, 3)

for(i in names(FOG_kine)[3:34]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  tempsds <- data.frame(FOG_kine %>%  group_by(condition) %>% summarise(n=sd(get(i), na.rm = T)) %>%
    spread(key=condition, value=n))
    SDS <- SDS %>% bind_rows(tempsds) 

}


IQR <- data.frame()
IQR <- round(IQR, 3)

for(i in names(FOG_kine)[3:34]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  tempiqr <- data.frame(
    FOG_kine %>%  group_by(condition) %>%
                     summarise(n=round(quantile(get(i), na.rm = T, probs = seq(0.25,0.75,0.5)), 3)) %>%
      mutate(flag=row_number()) %>% ungroup() %>% spread(key=flag, value=n) %>%
      mutate(IQR=paste0("[",`1`, "-", `2`,"]")) %>% select(condition, IQR) %>%
      transpose()
  )
  IQR <- IQR %>% bind_rows(tempiqr) 

}

row_odd <- seq_len(nrow(IQR)) %% 2    
IQR[row_odd == 0, ] 

# -----------------------------------
# Random forests --------------------------------
 
# MedOFFStimOFF   ->     MedOFFStimON

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")
unique(FOG_kine$condition)

temp <- FOG_kine %>% filter(condition=="MedOFFStimOFF"|condition=="MedOFFStimON")
temp <- temp %>% mutate(condition=ifelse(condition=="MedOFFStimOFF",0,1)) %>% mutate(condition=as.factor(condition))

modelAll_1_randomForest <- randomForest(condition ~ . , data = temp[,-1])

data.frame(modelAll_1_randomForest$importance) %>% arrange(-MeanDecreaseGini)

data.frame(
  temp %>% select(condition) %>% 
    bind_cols(predict(modelAll_1_randomForest, temp[,-1], type = 'prob')) 
) %>%
  gather( PredCondiction, Score, X0:X1, factor_key=TRUE) %>%
  rename("Group/State Prediction"="PredCondiction") %>%
  mutate(condition=ifelse(condition==0,"MedOFF-StimOFF", "MedOFF-StimON 130Hz")) %>%
  mutate(`Group/State Prediction`=ifelse(`Group/State Prediction`=="X0","Predicted MedOFF-StimOFF", "Predicted MedOFF-StimON 130Hz")) %>%
  ggplot(aes(Score, colour=`Group/State Prediction`, fill=`Group/State Prediction`)) +
  geom_density(alpha=0.5) +
  facet_wrap(~condition) +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()  +
  xlab("\n Propensity Score (->  Med OFF-Stim ON 130Hz)")



# MedOFFStimOFF   ->     ON130Hz

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")

temp <- FOG_kine %>% filter(condition=="MedOFFStimOFF"|condition=="ON130Hz")
temp <- temp %>% mutate(condition=ifelse(condition=="MedOFFStimOFF",0,1)) %>% mutate(condition=as.factor(condition))

modelAll_1_randomForest <- randomForest(condition ~ . , data = temp[,-1])

data.frame(modelAll_1_randomForest$importance) %>% arrange(-MeanDecreaseGini)

data.frame(
  temp %>% select(condition) %>% 
    bind_cols(predict(modelAll_1_randomForest, temp[,-1], type = 'prob')) 
) %>%
  gather( PredCondiction, Score, X0:X1, factor_key=TRUE) %>%
  rename("Group/State Prediction"="PredCondiction") %>%
  mutate(condition=ifelse(condition==0,"MedOFF-StimOFF", "MedON-StimON 130Hz")) %>%
  mutate(`Group/State Prediction`=ifelse(`Group/State Prediction`=="X0","Predicted MedOFF-StimOFF", "Predicted MedON-StimON 130Hz")) %>%
  ggplot(aes(Score, colour=`Group/State Prediction`, fill=`Group/State Prediction`)) +
  geom_density(alpha=0.5) +
  facet_wrap(~condition) +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()  +
  xlab("\n Propensity Score (->  Med ON-Stim ON 130Hz)")



# ON130Hz   ->     ON60Hz

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")

temp <- FOG_kine %>% filter(condition=="ON130Hz"|condition=="ON60Hz")
temp <- temp %>% mutate(condition=ifelse(condition=="ON130Hz",0,1)) %>% mutate(condition=as.factor(condition))

modelAll_1_randomForest <- randomForest(condition ~ . , data = temp[,-1])

data.frame(modelAll_1_randomForest$importance) %>% arrange(-MeanDecreaseGini)

data.frame(
  temp %>% select(condition) %>% 
    bind_cols(predict(modelAll_1_randomForest, temp[,-1], type = 'prob')) 
) %>%
  gather( PredCondiction, Score, X0:X1, factor_key=TRUE) %>%
  rename("Group/State Prediction"="PredCondiction") %>%
  mutate(condition=ifelse(condition==0,"MedON-StimON 130Hz", "MedON-StimON 60Hz")) %>%
  mutate(`Group/State Prediction`=ifelse(`Group/State Prediction`=="X0","Predicted MedON-StimON 130Hz", "Predicted MedON-StimON 60Hz")) %>%
  ggplot(aes(Score, colour=`Group/State Prediction`, fill=`Group/State Prediction`)) +
  geom_density(alpha=0.5) +
  facet_wrap(~condition) +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()  +
  xlab("\n Propensity Score (->  Med ON-Stim ON 60Hz)")






# ---------------------------------------
# Explainer range ------------------------------------

explainer_ranger <- explain(modelAll_1_randomForest,
                            data = temp[,-1],
                            y =  temp$condition,
                            label = "model_RandomForest")


new_observation <- temp[20]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)

# ---------------------------------------
# PCA ---------------------------

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")

temp <- FOG_kine %>% filter(condition=="MedOFFStimOFF"|condition=="MedOFFStimON")

prcompFOG_kine <- prcomp(temp[,3:34], center = TRUE, scale = TRUE)
summary(prcompFOG_kine)

prcompFOG_kine$rotation[,1:7]

my.var = varimax(prcompFOG_kine$rotation)

myvarshort <-my.var$loadings[,1:7]
myvarshort <- as.data.frame(myvarshort)

screeplot(prcompFOG_kine, type = "l", npcs = 12, main = "Screeplot of the first 12 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)


cumpro2 <- cumsum(prcompFOG_kine$sdev^2 / sum(prcompFOG_kine$sdev^2))
plot(cumpro2[0:12], xlab = "PC #", ylab = "Cumulative proportion of explained variance", main = "Cumulative variance plot")
abline(v = 12, col="blue", lty=5)
abline(h = 0.85510 , col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC7"),
       col=c("blue"), lty=5, cex=0.6)


prcompFOG_kine

PC <- predict(prcompFOG_kine, temp[,3:34])

tr <- cbind(PC, label=temp[,2])
tr$label<-as.factor(tr$label)

tr %>%
  ggplot(aes(PC1, PC2, colour=label)) +
  geom_point()


# ------------------------------
# Random forest regression for Fog -------------------

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")

temp <- FOG_kine %>% select(-c(patient_name , condition))

modelAll_1_randomForest <- randomForest(FoG_Percent_StraightLine ~ . , data = temp)
summary(modelAll_1_randomForest)

data.frame(modelAll_1_randomForest$importance) %>% arrange(-IncNodePurity)

  
  
data.frame(
  temp %>% select(FoG_Percent_StraightLine) %>% 
    bind_cols(predict(modelAll_1_randomForest, temp)) 
)   %>%
  gather( Group, FOGScore, FoG_Percent_StraightLine:`...2`, factor_key=TRUE) %>%
  mutate(Group=ifelse(Group=="FoG_Percent_StraightLine", "Observed % FOG", "Predicted % FOG")) %>%
  ggplot(aes(FOGScore, colour=Group, fill=Group)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()  +
  xlab("\n % FOG") + ylab("Patient kernel density \n")



explainer_ranger <- explain(modelAll_1_randomForest,
                            data = temp,
                            y =  temp$FoG_Percent_StraightLine,
                            label = "model_RandomForest")


new_observation <- temp[78]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)


new_observation <- temp[84]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)







FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")
unique(FOG_kine$condition)

tempOFFOFF <- FOG_kine %>% filter(condition=="MedOFFStimOFF") %>% select(-c(patient_name,condition))
tempOFFON <- FOG_kine %>% filter(condition=="ON130Hz") %>% select(-c(patient_name,condition))

temp <-  tempOFFON- tempOFFOFF


modelAll_1_randomForest <- randomForest(FoG_Percent_StraightLine ~ . , data = temp)
summary(modelAll_1_randomForest)

data.frame(modelAll_1_randomForest$importance) %>% arrange(-IncNodePurity)

  
  
data.frame(
  temp %>% select(FoG_Percent_StraightLine) %>% 
    bind_cols(predict(modelAll_1_randomForest, temp)) 
)   %>%
  gather( Group, FOGScore, FoG_Percent_StraightLine:`...2`, factor_key=TRUE) %>%
  mutate(Group=ifelse(Group=="FoG_Percent_StraightLine", "Observed Change in % FOG", "Predicted Change in % FOG")) %>%
  ggplot(aes(FOGScore, colour=Group, fill=Group)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()  +
  xlab("\n Change in % FOG") + ylab("Patient kernel density \n")



explainer_ranger <- explain(modelAll_1_randomForest,
                            data = temp,
                            y =  temp$FoG_Percent_StraightLine,
                            label = "model_RandomForest")


new_observation <- temp[1]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)


new_observation <- temp[14]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)






# MedOFFStimOFF vs MedOFFStimON

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")
unique(FOG_kine$condition)

tempOFFOFF <- FOG_kine %>% filter(condition=="MedOFFStimOFF") %>% select(-c(patient_name,condition))
tempOFFON <- FOG_kine %>% filter(condition=="MedOFFStimON") %>% select(-c(patient_name,condition))

temp <-  tempOFFON- tempOFFOFF


modelAll_1_randomForest <- randomForest(FoG_Percent_StraightLine ~ . , data = temp)
summary(modelAll_1_randomForest)

data.frame(modelAll_1_randomForest$importance) %>% arrange(-IncNodePurity)

  
  
data.frame(
  temp %>% select(FoG_Percent_StraightLine) %>% 
    bind_cols(predict(modelAll_1_randomForest, temp)) 
)   %>%
  gather( Group, FOGScore, FoG_Percent_StraightLine:`...2`, factor_key=TRUE) %>%
  mutate(Group=ifelse(Group=="FoG_Percent_StraightLine", "Observed Change in % FOG", "Predicted Change in % FOG")) %>%
  ggplot(aes(FOGScore, colour=Group, fill=Group)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()  +
  xlab("\n Change in % FOG OFF / OFF to OFF / ON") + ylab("Patient kernel density \n")



explainer_ranger <- explain(modelAll_1_randomForest,
                            data = temp,
                            y =  temp$FoG_Percent_StraightLine,
                            label = "model_RandomForest")


new_observation <- temp[1]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)


new_observation <- temp[14]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)




# MedOFFStimOFF vs MedONStimOFF

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")
unique(FOG_kine$condition)

tempOFFOFF <- FOG_kine %>% filter(condition=="MedOFFStimOFF") %>% select(-c(patient_name,condition))
tempONOFF <- FOG_kine %>% filter(condition=="MedONStimOFF") %>% select(-c(patient_name,condition))

temp <-  tempONOFF- tempOFFOFF


modelAll_1_randomForest <- randomForest(FoG_Percent_StraightLine ~ . , data = temp)
summary(modelAll_1_randomForest)

data.frame(modelAll_1_randomForest$importance) %>% arrange(-IncNodePurity)

  
  
data.frame(
  temp %>% select(FoG_Percent_StraightLine) %>% 
    bind_cols(predict(modelAll_1_randomForest, temp)) 
)   %>%
  gather( Group, FOGScore, FoG_Percent_StraightLine:`...2`, factor_key=TRUE) %>%
  mutate(Group=ifelse(Group=="FoG_Percent_StraightLine", "Observed Change in % FOG", "Predicted Change in % FOG")) %>%
  ggplot(aes(FOGScore, colour=Group, fill=Group)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()  +
  xlab("\n Change in % FOG OFF / OFF to ON / OFF") + ylab("Patient kernel density \n")



explainer_ranger <- explain(modelAll_1_randomForest,
                            data = temp,
                            y =  temp$FoG_Percent_StraightLine,
                            label = "model_RandomForest")


new_observation <- temp[1]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)


new_observation <- temp[14]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)




# # ON60Hz vs ON130Hz

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")
unique(FOG_kine$condition)

tempON130Hz <- FOG_kine %>% filter(condition=="ON130Hz") %>% select(-c(patient_name,condition))
tempON60Hz <- FOG_kine %>% filter(condition=="ON60Hz") %>% select(-c(patient_name,condition))

temp <-  tempON60Hz- tempON130Hz


modelAll_1_randomForest <- randomForest(FoG_Percent_StraightLine ~ . , data = temp)
summary(modelAll_1_randomForest)

data.frame(modelAll_1_randomForest$importance) %>% arrange(-IncNodePurity)

  
  
data.frame(
  temp %>% select(FoG_Percent_StraightLine) %>% 
    bind_cols(predict(modelAll_1_randomForest, temp)) 
)   %>%
  gather( Group, FOGScore, FoG_Percent_StraightLine:`...2`, factor_key=TRUE) %>%
  mutate(Group=ifelse(Group=="FoG_Percent_StraightLine", "Observed Change in % FOG", "Predicted Change in % FOG")) %>%
  ggplot(aes(FOGScore, colour=Group, fill=Group)) +
  geom_density(alpha=0.5) +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()  +
  xlab("\n Change in % FOG 130Hz to 60Hz") + ylab("Patient kernel density \n")



explainer_ranger <- explain(modelAll_1_randomForest,
                            data = temp,
                            y =  temp$FoG_Percent_StraightLine,
                            label = "model_RandomForest")


new_observation <- temp[1]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)


new_observation <- temp[14]
bd_ranger <- predict_parts_break_down(explainer_ranger, new_observation = new_observation)
head(bd_ranger)
plot(bd_ranger)


# -----------------------


# Regression ----------


# MedOFFStimOFF vs MedOFFStimON

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")

temp <- FOG_kine

temp <- temp %>% select(-c(condition, patient_name))

temp <- temp %>% select(FoG_Percent_StraightLine) %>%
  bind_cols(temp %>% select(-FoG_Percent_StraightLine))

regit_full <- regsubsets(FoG_Percent_StraightLine ~ . , data=temp, nvmax=19)

summary(regit_full)

reg_summary <- summary(regit_full)

names(reg_summary)

reg_summary$rss

# par(mfrow = c(1, 1))

plot(reg_summary$rss , xlab = " Number of Variables ", ylab = " RSS ", type = "l")

plot(reg_summary$adjr2 , xlab = " Number of Variables ", ylab = " Adjusted RSq ", type = "l")

plot(reg_summary$bic , xlab = " Number of Variables ", ylab = "BIC ", type = "l")

plot(reg_summary$cp, xlab = " Number of Variables ", ylab = "Cp", type = "l")

which.min(reg_summary$cp)

points(9, reg_summary$cp[9], col = " red ", cex = 2, pch = 20)

which.min(reg_summary$bic)

plot(reg_summary$bic , xlab = " Number of Variables ", ylab = " BIC ", type = "l")

plot(regit_full , scale = "r2")
plot(regit_full , scale = "adjr2")
plot(regit_full , scale = "Cp")
plot(regit_full , scale = "bic")

coef(regit_full , 7)




set.seed(1)
train <- sample(c(TRUE , FALSE), nrow (temp), replace = TRUE)
test <- (!train)

regit_full <- regsubsets(FoG_Percent_StraightLine ~ . , data=temp[train,], nvmax=19)
test.mat <- model.matrix(FoG_Percent_StraightLine ~ . , data=temp[test,])

val.errors <- rep (NA, 19)

for (i in 1:19){
  coefi <- coef(regit_full , id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((temp$FoG_Percent_StraightLine[test] - pred)^2)
}


which.min(val.errors)


predict.regsubsets <- function (object , newdata , id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form , newdata)
  coefi <- coef(object , id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

regfit.best <- regsubsets(FoG_Percent_StraightLine ~., data = temp , nvmax = 19)

coef(regfit.best , 9)


k <- 10
n <- nrow(temp)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL , paste(1:19)))


for (j in 1:k) {
  best.fit <- regsubsets(FoG_Percent_StraightLine ~ ., data = temp[folds != j, ], nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit , temp[folds == j, ], id = i)
    cv.errors[j, i] <- mean((temp$FoG_Percent_StraightLine[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors , 2, mean)

sqrt(mean.cv.errors)

plot(mean.cv.errors , type = "b")

data.frame(mean.cv.errors) %>% mutate(N=row_number()) %>%
  ggplot(aes(N, mean.cv.errors)) +
  geom_point(size=3, alpha=1, shape=4) +
  geom_line(size=2, alpha=0.3, colour="deepskyblue4") +
  theme_minimal() +
  xlab("\n Number of Predictors") + ylab("10-fold cross-validation error \n")
  
reg.best <- regsubsets(FoG_Percent_StraightLine ~ ., data = temp , nvmax = 19)
coef(reg.best , 7)


#                           (Intercept) 
#                            53.7481254 
#                   Step_Time_Asymmetry 
#                             0.2459206 
#                Step_Width_Variability 
#                            -0.4152489 
#                            Entropy_ML 
#                           -27.7512177 
#              Step_Time_Variability_ws 
#                             0.3085122 
#             Double_Support_Percent_ws 
#                             0.8099760 
# Double_Support_Percent_Variability_ws 
#                            -0.2943406 
#            Step_Length_Variability_ws 
#                             0.3438915 
                            
                            


summary_best <- summary(reg.best)

summary_best

plot(summary_best$adjr2 , xlab = " Number of Variables ", ylab = " Adjusted RSq ", type = "l")

plot(summary_best$bic , xlab = " Number of Variables ", ylab = "BIC ", type = "l")

data.frame(summary_best$bic) %>%
   mutate(N=row_number()) %>%
  ggplot(aes(N, summary_best.bic)) +
  geom_point(size=3, alpha=1, shape=4) +
  geom_line(size=2, alpha=0.3, colour="firebrick") +
  theme_minimal() +
  xlab("\n Number of Predictors") + ylab("Bayesian information criterion (BIC) \n")
  




plot(summary_best$cp, xlab = " Number of Variables ", ylab = "Cp", type = "l")


regsubsets_best_7 <- fread("regsubsets_best_7.csv")
regsubsets_best_7[is.na(regsubsets_best_7)] <- 0
regsubsets_best_7 <- regsubsets_best_7 %>% select(-GDI_Percent_ws)

regsubsets_best_7 %>% gather(Var, Pres, Cadence:Step_Length_Variability_ws) %>%
  mutate(Pres=ifelse(Pres==1, "Yes", "No")) %>%
  rename("Predictor_Included"="Pres") %>%
  mutate(Predictor_Included=as.factor(Predictor_Included)) %>%
  ggplot(aes(x=N_vars , y=Var, fill = Predictor_Included)) + 
  geom_tile(color = "white", size = 0.1) + 
  scale_fill_manual( values= c("snow", "deepskyblue4") ) +
  #scale_x_discrete(expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  coord_equal() + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(regsubsets_best_7$N_vars),max(regsubsets_best_7$N_vars),by=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("\n Number of Predictors") +ylab("Predictor Included (yes/no) \n")


# ------------------

# RIDGES / LASSO ----------------

x <- model.matrix(FoG_Percent_StraightLine ~ ., temp)[, -1]
y <- temp$FoG_Percent_StraightLine

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]

set.seed(1)
train <- sample(1: nrow (x), nrow (x) / 2)
test <- (-train)
y.test <- y[test]


ridge.mod <- glmnet(x[train , ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod , s = 4, newx = x[test , ])
mean((ridge.pred - y.test)^2)

mean((mean(y[train]) - y.test)^2)


ridge.pred <- predict(ridge.mod , s = 0, newx = x[test , ], exact = T, x = x[train , ], y = y[train])
mean((ridge.pred - y.test)^2)

lm( y ~ x, subset = train)
predict(ridge.mod , s = 0, exact = T, type = "coefficients", x = x[train , ], y = y[train])[1:20, ]

set.seed(1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam


ridge.pred <- predict(ridge.mod , s = bestlam , newx = x[test , ])

mean((ridge.pred - y.test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out , type = "coefficients", s = bestlam)[1:20, ]









lasso.mod <- glmnet(x[train , ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)


set.seed(1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod , s = bestlam , newx = x[test , ])
mean((lasso.pred - y.test)^2)

     
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out , type = "coefficients", s = bestlam)[, ]

lasso.coef[lasso.coef>0]

# ------------
# Correlations-------------------
# MedOFFStimOFF vs MedOFFStimON

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")

temp <- FOG_kine
unique(temp$condition)

tempOFFOFF <- FOG_kine %>% filter(condition=="MedOFFStimOFF") %>% select(-c(patient_name,condition))
tempOFFON <- FOG_kine %>% filter(condition=="MedOFFStimON") %>% select(-c(patient_name,condition))
tempONOFF <- FOG_kine %>% filter(condition=="MedONStimOFF") %>% select(-c(patient_name,condition))
 
temp <-  tempOFFON- tempOFFOFF

temp <- temp %>% select(FoG_Percent_StraightLine) %>%
  bind_cols(temp %>% select(-FoG_Percent_StraightLine))

# temp <- temp %>% select(-c(patient_name,condition))


list <- list()
list2 <- list()


for (i in names(temp)[2:32]) {
  list <- append(list, cor(temp$FoG_Percent_StraightLine, temp[,get(i)]))
  list2 <- append(list2, cor.test(temp$FoG_Percent_StraightLine, temp[,get(i)])$p.value)
}

list <- unlist(list)
list2 <- unlist(list2)

data.frame(names(temp)[2:32]) %>% bind_cols(data.frame(list)) %>% bind_cols(data.frame(list2)) %>%
  arrange(-abs(list))



# Change from OFF OFF to OFF ON

                       names.temp..2.32.        list        list2
1                      Speed_Variability  0.76494874 0.0003471805
2                         Step_Length_ws -0.74598922 0.0005848553
3                             Step_Width -0.74052791 0.0006741493
4              Double_Support_Percent_ws  0.73008279 0.0008765073
5               Step_Time_Variability_ws  0.71015654 0.0014021980
6  Double_Support_Percent_Variability_ws  0.70796279 0.0014732556
7                                  HR_ML -0.68070477 0.0026319163
8             Step_Length_Variability_ws  0.62310641 0.0075372172
9              Swing_Time_Variability_ws  0.57713290 0.0152784400
10    Stance_Time_Percent_Variability_ws  0.55480019 0.0208043355
11                Step_Width_Variability  0.52489698 0.0305089254
12                            Entropy_ML -0.50969331 0.0366156974
13                            COM_RMS_ML -0.50634896 0.0380757680
14                                 HR_AP -0.46072424 0.0627163300
15      Double_Support_Percent_Asymmetry -0.44989494 0.0699874574
16                   Step_Time_Asymmetry  0.44737834 0.0717622710
17                Stance_Time_Percent_ws -0.43801060 0.0786594423
18                         Swing_Time_ws  0.43800917 0.0786605291
19                  Swing_Time_Asymmetry  0.42631992 0.0879288314
20                 Step_Length_Asymmetry  0.37820493 0.1344306954
21                Cycle_Time_Variability  0.36578974 0.1487619619
22                            Entropy_AP  0.33669963 0.1863387383
23                               Cadence -0.25367265 0.3258794537
24                            Cycle_Time  0.22000776 0.3961658196
25                          Step_Time_ws  0.21370792 0.4101649429
26                            COM_RMS_AP  0.20573690 0.4282484982
27                          Entropy_Vert  0.18808674 0.4697270988
28                          COM_RMS_Vert -0.13423438 0.6075035507
29                        GDI_Percent_ws  0.10256365 0.6952756498
30                                 Speed  0.02964764 0.9100675772
31                               HR_Vert -0.01702004 0.9483059035


# Change from OFF OFF to ON OFF

                       names.temp..2.32.        list         list2
1                             Cycle_Time  0.82548947 0.00004494322
2               Step_Time_Variability_ws  0.82040264 0.00005488198
3                      Speed_Variability  0.80954156 0.00008242647
4     Stance_Time_Percent_Variability_ws  0.80917145 0.00008353931
5                 Stance_Time_Percent_ws -0.80614110 0.00009313615
6                          Swing_Time_ws  0.80613661 0.00009315103
7                         Step_Length_ws -0.78812623 0.00017144530
8                 Cycle_Time_Variability  0.78587804 0.00018426538
9                             Entropy_ML -0.74217054 0.00064617826
10                   Step_Time_Asymmetry  0.70936622 0.00142746449
11                          Step_Time_ws  0.69558618 0.00193186707
12                            Step_Width -0.67308427 0.00306340700
13 Double_Support_Percent_Variability_ws  0.66839213 0.00335657863
14             Swing_Time_Variability_ws  0.65562070 0.00427180289
15                               Cadence -0.60404084 0.01023083787
16             Double_Support_Percent_ws  0.57211552 0.01640548581
17                            COM_RMS_ML -0.56079064 0.01918949361
18                          COM_RMS_Vert -0.53163453 0.02806758382
19                Step_Width_Variability  0.45701427 0.06514127636
20            Step_Length_Variability_ws  0.45039202 0.06964074190
21                                 HR_AP -0.44968394 0.07013501370
22                 Step_Length_Asymmetry  0.44217862 0.07553352464
23                               HR_Vert -0.38612895 0.12580052709
24                  Swing_Time_Asymmetry  0.31784180 0.21378438192
25                                 HR_ML -0.30568607 0.23279061508
26                            COM_RMS_AP -0.29769263 0.24585540712
27      Double_Support_Percent_Asymmetry -0.24436663 0.34453452366
28                          Entropy_Vert -0.20306490 0.43440188231
29                                 Speed -0.14791295 0.57102723953
30                            Entropy_AP  0.09693176 0.71131447904
31                        GDI_Percent_ws  0.04868577 0.85279333759

# --------------------

# random stuff

deter_data_clinical <- fread("deter_data_clinical.csv")

unique(deter_data_clinical$condition)

wilcox.test(DetUPDRS_III ~condition, data = deter_data_clinical[condition=="Med_OFF_Stim_OFF"|,], paired=T)

deter_data_clinical <- deter_data_clinical[condition=="Med_ON_Stim_130Hz"|condition=="Med_ON_Stim_60Hz",]

deter_data_clinical %>%
  select(DetAXIAL_score          , condition) %>%
  filter(condition=="Med_ON_Stim_130Hz") %>% select(1) %>% rename("Med_ON_Stim_130Hz"="DetAXIAL_score") %>%
  bind_cols(deter_data_clinical %>%
  select(DetAXIAL_score          , condition) %>%
  filter(condition=="Med_ON_Stim_60Hz") %>% select(1) %>% rename("Med_ON_Stim_60Hz"="DetAXIAL_score")) %>%
  ggplot((aes(Med_ON_Stim_60Hz, Med_ON_Stim_130Hz))) +
  geom_jitter(alpha=0.8, size=4 , colour="deepskyblue4") +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()  +
  xlab("\n Axial Score \n Med ON-Stim ON 60Hz") +
  ylab("Axial Score \n Med ON-Stim ON 130Hz \n") +
  xlim(0,20) +ylim(0,20) +
  geom_abline(intercept =0 , slope = 1, size=1, colour="firebrick")

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")

unique(FOG_kine$condition)

FOG_kine <- FOG_kine[condition=="ON130Hz"|condition=="ON60Hz",]


FOG_kine %>%
  select(FoG_Percent_StraightLine , condition) %>%
  filter(condition=="ON130Hz") %>% select(1) %>% rename("ON130Hz"="FoG_Percent_StraightLine") %>%
  bind_cols(FOG_kine %>%
  select(FoG_Percent_StraightLine , condition) %>%
  filter(condition=="ON60Hz") %>% select(1) %>% rename("ON60Hz"="FoG_Percent_StraightLine")) %>%
  ggplot((aes(ON60Hz, ON130Hz))) +
  geom_jitter(alpha=0.8, size=4 , colour="deepskyblue4") +
  theme_minimal() +
  ggsci::scale_color_nejm() +
  ggsci::scale_fill_nejm()  +
  xlab("\n FOG % Time \n Med ON-Stim ON 60Hz") +
  ylab("FOG % Time \n Med ON-Stim ON 130Hz \n") +
  xlim(0,90) +ylim(0,90) +
  geom_abline(intercept =0 , slope = 1, size=1, colour="firebrick")


FOG_Stim_conditions <- fread("FOG_Stim_conditions.csv", sep=";")
unique(FOG_Stim_conditions$condition)
FOG_Stim_conditions <- FOG_Stim_conditions[condition=="Med_ON_preOP"|condition=="MED_ON_STIM_ON", c("UPDRS_III","condition")]
FOG_Stim_conditions <- FOG_Stim_conditions %>% drop_na() %>% filter(UPDRS_III!=21)


wilcox.test(UPDRS_III~condition, data = FOG_Stim_conditions[condition=="Med_OFF_preop"|condition=="MED_OFF_STIM_OFF",], paired=T, correct=T)
wilcox.test(UPDRS_III~condition, data = FOG_Stim_conditions[condition=="Med_ON_preOP"|condition=="MED_ON_STIM_ON",], paired=T, correct=T)

# Correlation between FOG % time straight-line vs SWS time or FOG # events ---------

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")
FOG_kine <- FOG_kine %>% select(patient_name, condition, FoG_Percent_StraightLine)
unique(FOG_kine$condition)
FOG_kine <- FOG_kine %>% mutate(condition=ifelse(condition=="MedOFFStimOFF", "MED_OFF_STIM_OFF",
                                                 ifelse(condition=="MedOFFStimON", "MED_OFF_STIM_ON",
                                                        ifelse(condition=="MedONStimOFF", "MED_ON_STIM_OFF",
                                                               ifelse(condition=="ON130Hz", "MED_ON_STIM_ON", "MED_ON_STIM_ON_60Hz")))))
  
  
FOG_Stim_conditions <- fread("FOG_Stim_conditions.csv", sep=";")
FOG_Stim_conditions <- FOG_Stim_conditions %>% select(patient, condition, SWS_time_s, SWS_N_FOG_Events)
FOG_Stim_conditions$patient <- parse_number(FOG_Stim_conditions$patient)
names(FOG_Stim_conditions)[1] <- "patient_name"
unique(FOG_Stim_conditions$condition)


FOG_Stim_conditions <- FOG_kine %>% inner_join(FOG_Stim_conditions)
FOG_Stim_conditions <- FOG_Stim_conditions %>% drop_na()

cor.test(FOG_Stim_conditions$FoG_Percent_StraightLine, FOG_Stim_conditions$SWS_time_s)


# 	Pearson's product-moment correlation
# 
# data:  FOG_Stim_conditions$FoG_Percent_StraightLine and FOG_Stim_conditions$SWS_time_s
# t = 13.122, df = 77, p-value < 0.00000000000000022
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.7475384 0.8889711
# sample estimates:
#       cor 
# 0.8312523 



cor.test(FOG_Stim_conditions$FoG_Percent_StraightLine, FOG_Stim_conditions$SWS_N_FOG_Events)

# 	Pearson's product-moment correlation
# 
# data:  FOG_Stim_conditions$FoG_Percent_StraightLine and FOG_Stim_conditions$SWS_N_FOG_Events
# t = 11.036, df = 77, p-value < 0.00000000000000022
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6791546 0.8557344
# sample estimates:
#       cor 
# 0.7827247 
# --------------------------
# Plots 60Hz vs 130Hz ------------------------------

FOG_kine <- fread("FOG_kine_ws.txt", sep="\t")
FOG_kine <- FOG_kine %>% select(patient_name, FoG_Percent_StraightLine, condition)
unique(FOG_kine$condition)
FOG_kine <- FOG_kine %>% filter(condition=="ON130Hz"|condition=="ON60Hz")

#FOG_kine <- FOG_kine %>% spread(key=condition, value=FoG_Percent_StraightLine)
FOG_kine <- FOG_kine %>% arrange(condition) %>% select(patient_name)



FOG_kine %>% ggplot(aes(ON60Hz, ON130Hz)) +
  geom_point(colour="firebrick", size=3, alpha=0.9) +
  theme_minimal() +
  xlab("\n FOG % time straight-line basis \n (ON-ON 60 Hz)") + ylab("FOG % time straight-line basis \n ON-ON 130 Hz") +
  xlim(0,100) + ylim(0,100) +
  geom_abline(intercept = 0)


deter_data_clinical <- fread("deter_data_clinical.csv", sep=";")

DetSWS_N_FOG_Events  <- deter_data_clinical %>% 
  filter(condition=="Med_ON_Stim_130Hz"|condition=="Med_ON_Stim_60Hz") %>% 
  select(condition, DetSWS_N_FOG_Events )

FOG_kine %>% bind_cols(DetSWS_N_FOG_Events) %>% spread(key=condition, value=DetSWS_N_FOG_Events) %>%
  select(-patient_name) %>%
  ggplot(aes(Med_ON_Stim_60Hz, Med_ON_Stim_130Hz )) +
    geom_point(colour="firebrick", size=3, alpha=0.9) +
  theme_minimal() +
  xlab("\n # FOG episodes \n (ON-ON 60 Hz)") + ylab(" # FOG episodes \n ON-ON 130 Hz") +
  xlim(0,32) + ylim(0,32) +
  geom_abline(intercept = 0)

DetSWS_time_s  <- deter_data_clinical %>% 
  filter(condition=="Med_ON_Stim_130Hz"|condition=="Med_ON_Stim_60Hz") %>% 
  select(condition, DetSWS_time_s )

FOG_kine %>% bind_cols(DetSWS_time_s) %>% spread(key=condition, value=DetSWS_time_s) %>%
  select(-patient_name) %>%
  ggplot(aes(Med_ON_Stim_60Hz, Med_ON_Stim_130Hz )) +
    geom_point(colour="firebrick", size=3, alpha=0.9) +
  theme_minimal() +
  xlab("\n SWS time (s) \n (ON-ON 60 Hz)") + ylab(" SWS time (s) \n ON-ON 130 Hz") +
  xlim(0,300) + ylim(0,300) +
  geom_abline(intercept = 0)


DetAXIAL_score  <- deter_data_clinical %>% 
  filter(condition=="Med_ON_Stim_130Hz"|condition=="Med_ON_Stim_60Hz") %>% 
  select(condition, DetAXIAL_score )

FOG_kine %>% bind_cols(DetAXIAL_score) %>% spread(key=condition, value=DetAXIAL_score) %>%
  select(-patient_name) %>%
  ggplot(aes(Med_ON_Stim_60Hz, Med_ON_Stim_130Hz )) +
    geom_point(colour="firebrick", size=3, alpha=0.9) +
  theme_minimal() +
  xlab("\n Axial Score \n (ON-ON 60 Hz)") + ylab(" Axial Score \n ON-ON 130 Hz") +
  xlim(0,20) + ylim(0,20) +
  geom_abline(intercept = 0)

# ------------------