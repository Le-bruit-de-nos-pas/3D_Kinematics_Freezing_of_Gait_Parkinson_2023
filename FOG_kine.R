library(tidyverse)
library(data.table)
library(hacksaw)
library(splitstackshape)
library(spatstat)
library(lubridate)
options(scipen = 999)

# Import data, define worst side

FOG_kine <- fread("FOG_kine.txt")

FOG_kine <- gather(FOG_kine, Variable, Value, Cadence:COM_RMS_ML, factor_key=TRUE)

FOG_kine <- FOG_kine %>% mutate(To_delete = ifelse(grepl("Right", Variable) & worst_side == "Left", 1 ,
                                       ifelse(grepl("Left", Variable) & worst_side == "Right", 1, 0)))

FOG_kine <- FOG_kine %>% filter(To_delete == 0) %>% select(-To_delete)

FOG_kine <- FOG_kine %>% spread(key=Variable, value=Value)


# Select Only Worst Side 

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
  select(-c(Stance_Time_Left_Percent, Stance_Time_Left_Percent))

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


# FoG_Percent_StraightLine

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



# Speed

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


# Step Length

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


# Cadence

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



















for(i in names(FOG_kine)[4:45]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  print(FOG_kine %>% filter(condition == "MedOFFStimOFF" | condition == "ON130Hz")  %>%
    group_by(condition) %>% summarise(n=median(get(i), na.rm = T)))
}


for(i in names(FOG_kine)[4:45]){
  print(i)
  cat(" - - - - - - - - - - - - - - - - - - - - - - - - -")
  FOG_kine_temp <- FOG_kine %>% filter(condition == "MedOFFStimOFF" | condition == "ON130Hz") 
  print(wilcox.test(get(i)~condition, data = FOG_kine_temp))
}

