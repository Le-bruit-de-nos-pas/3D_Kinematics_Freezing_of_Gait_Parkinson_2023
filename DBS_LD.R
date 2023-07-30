library(tidyverse)
library(data.table)
library(readxl)
library(DescTools)
library(randomForest)
options(scipen = 999)

# Impute missing values, checks and balances ---------------------------

kinematics_DBS_LD <- read_xlsx(path="kinematics_DBS_LD.xlsx", skip=0, trim_ws = TRUE)

kinematics_DBS_LD <- kinematics_DBS_LD[,c(1,33,3:32)]

names(kinematics_DBS_LD)
unique(kinematics_DBS_LD$State)

kinematics_DT <- as.data.table(kinematics_DBS_LD)

# Speed 

Worst_OFFOFF <- kinematics_DT[, .(n = min(`Speed_(m/s)`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Speed_(m/s)`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Speed_(m/s)`]

kinematics_DT[, `Speed_(m/s)` := fifelse(is.na(`Speed_(m/s)`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Speed_(m/s)`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Speed_(m/s)`))]

wilcox.test(`Speed_(m/s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Speed_(m/s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Speed_(m/s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Speed_(m/s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Speed_(m/s)`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Speed_(m/s)` <- kinematics_DT$`Speed_(m/s)` + rnorm(kinematics_DT$`Speed_(m/s)`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Speed_(m/s)`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Speed_(m/s)`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Speed_(m/s)`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)


# Cadence 

Worst_OFFOFF <- kinematics_DT[, .(n = min(`Cadence_(steps/min)`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Cadence_(steps/min)`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Cadence_(steps/min)`]

kinematics_DT[, `Cadence_(steps/min)` := fifelse(is.na(`Cadence_(steps/min)`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Cadence_(steps/min)`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Cadence_(steps/min)`))]

wilcox.test(`Cadence_(steps/min)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Cadence_(steps/min)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Cadence_(steps/min)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Cadence_(steps/min)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Cadence_(steps/min)`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Cadence_(steps/min)` <- kinematics_DT$`Cadence_(steps/min)` + rnorm(kinematics_DT$`Cadence_(steps/min)`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Cadence_(steps/min)`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Cadence_(steps/min)`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Cadence_(steps/min)`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)


# Step_Time_(s) 

Worst_OFFOFF <- kinematics_DT[, .(n = max(`Step_Time_(s)`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Step_Time_(s)`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Step_Time_(s)`]

kinematics_DT[, `Step_Time_(s)` := fifelse(is.na(`Step_Time_(s)`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Step_Time_(s)`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Step_Time_(s)`))]

wilcox.test(`Step_Time_(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Step_Time_(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Step_Time_(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Step_Time_(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Step_Time_(s)`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Step_Time_(s)` <- kinematics_DT$`Step_Time_(s)` + rnorm(kinematics_DT$`Step_Time_(s)`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Step_Time_(s)`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Step_Time_(s)`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Step_Time_(s)`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)


#Step_Length_t_(m)

Worst_OFFOFF <- kinematics_DT[, .(n = min(`Step_Length_t_(m)`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Step_Length_t_(m)`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Step_Length_t_(m)`]

kinematics_DT[, `Step_Length_t_(m)` := fifelse(is.na(`Step_Length_t_(m)`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Step_Length_t_(m)`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Step_Length_t_(m)`))]

wilcox.test(`Step_Length_t_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Step_Length_t_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Step_Length_t_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Step_Length_t_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Step_Length_t_(m)`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Step_Length_t_(m)` <- kinematics_DT$`Step_Length_t_(m)` + rnorm(kinematics_DT$`Step_Length_t_(m)`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Step_Length_t_(m)`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Step_Length_t_(m)`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Step_Length_t_(m)`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)



#Stride_Time_(s)

Worst_OFFOFF <- kinematics_DT[, .(n = max(`Stride_Time_(s)`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Stride_Time_(s)`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Stride_Time_(s)`]

kinematics_DT[, `Stride_Time_(s)` := fifelse(is.na(`Stride_Time_(s)`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Stride_Time_(s)`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Stride_Time_(s)`))]

wilcox.test(`Stride_Time_(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Stride_Time_(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Stride_Time_(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Stride_Time_(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Stride_Time_(s)`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Stride_Time_(s)` <- kinematics_DT$`Stride_Time_(s)` + rnorm(kinematics_DT$`Stride_Time_(s)`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Stride_Time_(s)`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Stride_Time_(s)`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Stride_Time_(s)`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)





#Stride_Length_(m)

Worst_OFFOFF <- kinematics_DT[, .(n = min(`Stride_Length_(m)`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Stride_Length_(m)`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Stride_Length_(m)`]

kinematics_DT[, `Stride_Length_(m)` := fifelse(is.na(`Stride_Length_(m)`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Stride_Length_(m)`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Stride_Length_(m)`))]

wilcox.test(`Stride_Length_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Stride_Length_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Stride_Length_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Stride_Length_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Stride_Length_(m)`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Stride_Length_(m)` <- kinematics_DT$`Stride_Length_(m)` + rnorm(kinematics_DT$`Stride_Length_(m)`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Stride_Length_(m)`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Stride_Length_(m)`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Stride_Length_(m)`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)




#Step_Width_(m)

Worst_OFFOFF <- kinematics_DT[, .(n = min(`Step_Width_(m)`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Step_Width_(m)`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Step_Width_(m)`]

kinematics_DT[, `Step_Width_(m)` := fifelse(is.na(`Step_Width_(m)`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Step_Width_(m)`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Step_Width_(m)`))]

wilcox.test(`Step_Width_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Step_Width_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Step_Width_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Step_Width_(m)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Step_Width_(m)`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Step_Width_(m)` <- kinematics_DT$`Step_Width_(m)` + rnorm(kinematics_DT$`Step_Width_(m)`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Step_Width_(m)`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Step_Width_(m)`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Step_Width_(m)`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)




#Stance_Time__(s)

Worst_OFFOFF <- kinematics_DT[, .(n = max(`Stance_Time__(s)`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Stance_Time__(s)`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Stance_Time__(s)`]

kinematics_DT[, `Stance_Time__(s)` := fifelse(is.na(`Stance_Time__(s)`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Stance_Time__(s)`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Stance_Time__(s)`))]

wilcox.test(`Stance_Time__(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Stance_Time__(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Stance_Time__(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Stance_Time__(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Stance_Time__(s)`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Stance_Time__(s)` <- kinematics_DT$`Stance_Time__(s)` + rnorm(kinematics_DT$`Stance_Time__(s)`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Stance_Time__(s)`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Stance_Time__(s)`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Stance_Time__(s)`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)





#Swing_Time__(s)

Worst_OFFOFF <- kinematics_DT[, .(n = min(`Swing_Time__(s)`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Swing_Time__(s)`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Swing_Time__(s)`]

kinematics_DT[, `Swing_Time__(s)` := fifelse(is.na(`Swing_Time__(s)`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Swing_Time__(s)`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Swing_Time__(s)`))]

wilcox.test(`Swing_Time__(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Swing_Time__(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Swing_Time__(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Swing_Time__(s)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Swing_Time__(s)`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Swing_Time__(s)` <- kinematics_DT$`Swing_Time__(s)` + rnorm(kinematics_DT$`Swing_Time__(s)`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Swing_Time__(s)`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Swing_Time__(s)`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Swing_Time__(s)`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)








#Double_Support_(%_cycle)

Worst_OFFOFF <- kinematics_DT[, .(n = max(`Double_Support_(%_cycle)`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Double_Support_(%_cycle)`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Double_Support_(%_cycle)`]

kinematics_DT[, `Double_Support_(%_cycle)` := fifelse(is.na(`Double_Support_(%_cycle)`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Double_Support_(%_cycle)`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Double_Support_(%_cycle)`))]

wilcox.test(`Double_Support_(%_cycle)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Double_Support_(%_cycle)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Double_Support_(%_cycle)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Double_Support_(%_cycle)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Double_Support_(%_cycle)`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Double_Support_(%_cycle)` <- kinematics_DT$`Double_Support_(%_cycle)` + rnorm(kinematics_DT$`Double_Support_(%_cycle)`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Double_Support_(%_cycle)`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Double_Support_(%_cycle)`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Double_Support_(%_cycle)`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)




#Single_Support_(%_cycle)

Worst_OFFOFF <- kinematics_DT[, .(n = min(`Single_Support_(%_cycle)`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Single_Support_(%_cycle)`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Single_Support_(%_cycle)`]

kinematics_DT[, `Single_Support_(%_cycle)` := fifelse(is.na(`Single_Support_(%_cycle)`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Single_Support_(%_cycle)`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Single_Support_(%_cycle)`))]

wilcox.test(`Single_Support_(%_cycle)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Single_Support_(%_cycle)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Single_Support_(%_cycle)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Single_Support_(%_cycle)` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Single_Support_(%_cycle)`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Single_Support_(%_cycle)` <- kinematics_DT$`Single_Support_(%_cycle)` + rnorm(kinematics_DT$`Single_Support_(%_cycle)`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Single_Support_(%_cycle)`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Single_Support_(%_cycle)`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Single_Support_(%_cycle)`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)






#Speed_var

Worst_OFFOFF <- kinematics_DT[, .(n = max(`Speed_var`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Speed_var`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Speed_var`]

kinematics_DT[, `Speed_var` := fifelse(is.na(`Speed_var`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Speed_var`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Speed_var`))]

wilcox.test(`Speed_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Speed_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Speed_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Speed_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Speed_var`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Speed_var` <- kinematics_DT$`Speed_var` + rnorm(kinematics_DT$`Speed_var`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Speed_var`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Speed_var`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Speed_var`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)







#StepTime_var

Worst_OFFOFF <- kinematics_DT[, .(n = max(`StepTime_var`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`StepTime_var`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`StepTime_var`]

kinematics_DT[, `StepTime_var` := fifelse(is.na(`StepTime_var`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`StepTime_var`)&State=="MedOnStimON", as.numeric(Mean_ONON), `StepTime_var`))]

wilcox.test(`StepTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`StepTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StepTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StepTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`StepTime_var`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`StepTime_var` <- kinematics_DT$`StepTime_var` + rnorm(kinematics_DT$`StepTime_var`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`StepTime_var`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`StepTime_var`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`StepTime_var`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)





#StepLenght_var

Worst_OFFOFF <- kinematics_DT[, .(n = max(`StepLenght_var`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`StepLenght_var`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`StepLenght_var`]

kinematics_DT[, `StepLenght_var` := fifelse(is.na(`StepLenght_var`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`StepLenght_var`)&State=="MedOnStimON", as.numeric(Mean_ONON), `StepLenght_var`))]

wilcox.test(`StepLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`StepLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StepLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StepLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`StepLenght_var`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`StepLenght_var` <- kinematics_DT$`StepLenght_var` + rnorm(kinematics_DT$`StepLenght_var`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`StepLenght_var`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`StepLenght_var`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`StepLenght_var`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)





#Stepwidth_var

Worst_OFFOFF <- kinematics_DT[, .(n = max(`Stepwidth_var`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Stepwidth_var`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Stepwidth_var`]

kinematics_DT[, `Stepwidth_var` := fifelse(is.na(`Stepwidth_var`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Stepwidth_var`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Stepwidth_var`))]

wilcox.test(`Stepwidth_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Stepwidth_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Stepwidth_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Stepwidth_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Stepwidth_var`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Stepwidth_var` <- kinematics_DT$`Stepwidth_var` + rnorm(kinematics_DT$`Stepwidth_var`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Stepwidth_var`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Stepwidth_var`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Stepwidth_var`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)





#StrideTime_var

Worst_OFFOFF <- kinematics_DT[, .(n = max(`StrideTime_var`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`StrideTime_var`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`StrideTime_var`]

kinematics_DT[, `StrideTime_var` := fifelse(is.na(`StrideTime_var`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`StrideTime_var`)&State=="MedOnStimON", as.numeric(Mean_ONON), `StrideTime_var`))]

wilcox.test(`StrideTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`StrideTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StrideTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StrideTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`StrideTime_var`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`StrideTime_var` <- kinematics_DT$`StrideTime_var` + rnorm(kinematics_DT$`StrideTime_var`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`StrideTime_var`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`StrideTime_var`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`StrideTime_var`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)







#StrideLenght_var

Worst_OFFOFF <- kinematics_DT[, .(n = max(`StrideLenght_var`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`StrideLenght_var`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`StrideLenght_var`]

kinematics_DT[, `StrideLenght_var` := fifelse(is.na(`StrideLenght_var`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`StrideLenght_var`)&State=="MedOnStimON", as.numeric(Mean_ONON), `StrideLenght_var`))]

wilcox.test(`StrideLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`StrideLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StrideLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StrideLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`StrideLenght_var`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`StrideLenght_var` <- kinematics_DT$`StrideLenght_var` + rnorm(kinematics_DT$`StrideLenght_var`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`StrideLenght_var`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`StrideLenght_var`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`StrideLenght_var`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)






#Stepwidth_var

Worst_OFFOFF <- kinematics_DT[, .(n = max(`Stepwidth_var`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`Stepwidth_var`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`Stepwidth_var`]

kinematics_DT[, `Stepwidth_var` := fifelse(is.na(`Stepwidth_var`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`Stepwidth_var`)&State=="MedOnStimON", as.numeric(Mean_ONON), `Stepwidth_var`))]

wilcox.test(`Stepwidth_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`Stepwidth_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Stepwidth_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`Stepwidth_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`Stepwidth_var`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`Stepwidth_var` <- kinematics_DT$`Stepwidth_var` + rnorm(kinematics_DT$`Stepwidth_var`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`Stepwidth_var`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`Stepwidth_var`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`Stepwidth_var`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)





#StrideTime_var

Worst_OFFOFF <- kinematics_DT[, .(n = max(`StrideTime_var`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`StrideTime_var`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`StrideTime_var`]

kinematics_DT[, `StrideTime_var` := fifelse(is.na(`StrideTime_var`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`StrideTime_var`)&State=="MedOnStimON", as.numeric(Mean_ONON), `StrideTime_var`))]

wilcox.test(`StrideTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`StrideTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StrideTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StrideTime_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`StrideTime_var`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`StrideTime_var` <- kinematics_DT$`StrideTime_var` + rnorm(kinematics_DT$`StrideTime_var`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`StrideTime_var`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`StrideTime_var`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`StrideTime_var`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)





#StrideLenght_var

Worst_OFFOFF <- kinematics_DT[, .(n = max(`StrideLenght_var`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`StrideLenght_var`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`StrideLenght_var`]

kinematics_DT[, `StrideLenght_var` := fifelse(is.na(`StrideLenght_var`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`StrideLenght_var`)&State=="MedOnStimON", as.numeric(Mean_ONON), `StrideLenght_var`))]

wilcox.test(`StrideLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`StrideLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StrideLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StrideLenght_var` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`StrideLenght_var`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`StrideLenght_var` <- kinematics_DT$`StrideLenght_var` + rnorm(kinematics_DT$`StrideLenght_var`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`StrideLenght_var`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`StrideLenght_var`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`StrideLenght_var`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)







#StepLenght_asym

Worst_OFFOFF <- kinematics_DT[, .(n = max(`StepLenght_asym`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`StepLenght_asym`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`StepLenght_asym`]

kinematics_DT[, `StepLenght_asym` := fifelse(is.na(`StepLenght_asym`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`StepLenght_asym`)&State=="MedOnStimON", as.numeric(Mean_ONON), `StepLenght_asym`))]

wilcox.test(`StepLenght_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`StepLenght_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StepLenght_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StepLenght_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`StepLenght_asym`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`StepLenght_asym` <- kinematics_DT$`StepLenght_asym` + rnorm(kinematics_DT$`StepLenght_asym`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`StepLenght_asym`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`StepLenght_asym`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`StepLenght_asym`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)






#StepTime_asym

Worst_OFFOFF <- kinematics_DT[, .(n = max(`StepTime_asym`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`StepTime_asym`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`StepTime_asym`]

kinematics_DT[, `StepTime_asym` := fifelse(is.na(`StepTime_asym`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`StepTime_asym`)&State=="MedOnStimON", as.numeric(Mean_ONON), `StepTime_asym`))]

wilcox.test(`StepTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`StepTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StepTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StepTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`StepTime_asym`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`StepTime_asym` <- kinematics_DT$`StepTime_asym` + rnorm(kinematics_DT$`StepTime_asym`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`StepTime_asym`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`StepTime_asym`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`StepTime_asym`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)




#StanceTime_asym

Worst_OFFOFF <- kinematics_DT[, .(n = max(`StanceTime_asym`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`StanceTime_asym`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`StanceTime_asym`]

kinematics_DT[, `StanceTime_asym` := fifelse(is.na(`StanceTime_asym`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`StanceTime_asym`)&State=="MedOnStimON", as.numeric(Mean_ONON), `StanceTime_asym`))]

wilcox.test(`StanceTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`StanceTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StanceTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`StanceTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`StanceTime_asym`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`StanceTime_asym` <- kinematics_DT$`StanceTime_asym` + rnorm(kinematics_DT$`StanceTime_asym`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`StanceTime_asym`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`StanceTime_asym`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`StanceTime_asym`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)






#SwingTime_asym

Worst_OFFOFF <- kinematics_DT[, .(n = max(`SwingTime_asym`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`SwingTime_asym`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`SwingTime_asym`]

kinematics_DT[, `SwingTime_asym` := fifelse(is.na(`SwingTime_asym`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`SwingTime_asym`)&State=="MedOnStimON", as.numeric(Mean_ONON), `SwingTime_asym`))]

wilcox.test(`SwingTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`SwingTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`SwingTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`SwingTime_asym` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`SwingTime_asym`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`SwingTime_asym` <- kinematics_DT$`SwingTime_asym` + rnorm(kinematics_DT$`SwingTime_asym`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`SwingTime_asym`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`SwingTime_asym`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`SwingTime_asym`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)





#entropy1

Worst_OFFOFF <- kinematics_DT[, .(n = max(`entropy1`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`entropy1`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`entropy1`]

kinematics_DT[, `entropy1` := fifelse(is.na(`entropy1`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`entropy1`)&State=="MedOnStimON", as.numeric(Mean_ONON), `entropy1`))]

wilcox.test(`entropy1` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`entropy1` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`entropy1` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`entropy1` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`entropy1`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`entropy1` <- kinematics_DT$`entropy1` + rnorm(kinematics_DT$`entropy1`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`entropy1`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`entropy1`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`entropy1`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)




#entropy2

Worst_OFFOFF <- kinematics_DT[, .(n = max(`entropy2`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`entropy2`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`entropy2`]

kinematics_DT[, `entropy2` := fifelse(is.na(`entropy2`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`entropy2`)&State=="MedOnStimON", as.numeric(Mean_ONON), `entropy2`))]

wilcox.test(`entropy2` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`entropy2` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`entropy2` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`entropy2` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`entropy2`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`entropy2` <- kinematics_DT$`entropy2` + rnorm(kinematics_DT$`entropy2`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`entropy2`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`entropy2`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`entropy2`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)




#entropy3

Worst_OFFOFF <- kinematics_DT[, .(n = max(`entropy3`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`entropy3`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`entropy3`]

kinematics_DT[, `entropy3` := fifelse(is.na(`entropy3`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`entropy3`)&State=="MedOnStimON", as.numeric(Mean_ONON), `entropy3`))]

wilcox.test(`entropy3` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`entropy3` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`entropy3` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`entropy3` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`entropy3`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`entropy3` <- kinematics_DT$`entropy3` + rnorm(kinematics_DT$`entropy3`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`entropy3`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`entropy3`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`entropy3`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)





#hr_ap

Worst_OFFOFF <- kinematics_DT[, .(n = min(`hr_ap`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`hr_ap`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`hr_ap`]

kinematics_DT[, `hr_ap` := fifelse(is.na(`hr_ap`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`hr_ap`)&State=="MedOnStimON", as.numeric(Mean_ONON), `hr_ap`))]

wilcox.test(`hr_ap` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`hr_ap` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`hr_ap` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`hr_ap` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`hr_ap`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`hr_ap` <- kinematics_DT$`hr_ap` + rnorm(kinematics_DT$`hr_ap`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`hr_ap`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`hr_ap`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`hr_ap`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)






#hr_vert

Worst_OFFOFF <- kinematics_DT[, .(n = min(`hr_vert`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`hr_vert`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`hr_vert`]

kinematics_DT[, `hr_vert` := fifelse(is.na(`hr_vert`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`hr_vert`)&State=="MedOnStimON", as.numeric(Mean_ONON), `hr_vert`))]

wilcox.test(`hr_vert` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`hr_vert` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`hr_vert` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`hr_vert` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`hr_vert`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`hr_vert` <- kinematics_DT$`hr_vert` + rnorm(kinematics_DT$`hr_vert`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`hr_vert`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`hr_vert`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`hr_vert`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)






#hr_ml

Worst_OFFOFF <- kinematics_DT[, .(n = min(`hr_ml`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`hr_ml`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`hr_ml`]

kinematics_DT[, `hr_ml` := fifelse(is.na(`hr_ml`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`hr_ml`)&State=="MedOnStimON", as.numeric(Mean_ONON), `hr_ml`))]

wilcox.test(`hr_ml` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`hr_ml` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`hr_ml` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`hr_ml` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`hr_ml`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`hr_ml` <- kinematics_DT$`hr_ml` + rnorm(kinematics_DT$`hr_ml`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`hr_ml`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`hr_ml`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`hr_ml`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)






#com_rms_ap

Worst_OFFOFF <- kinematics_DT[, .(n = min(`com_rms_ap`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`com_rms_ap`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`com_rms_ap`]

kinematics_DT[, `com_rms_ap` := fifelse(is.na(`com_rms_ap`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`com_rms_ap`)&State=="MedOnStimON", as.numeric(Mean_ONON), `com_rms_ap`))]

wilcox.test(`com_rms_ap` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`com_rms_ap` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`com_rms_ap` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`com_rms_ap` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`com_rms_ap`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`com_rms_ap` <- kinematics_DT$`com_rms_ap` + rnorm(kinematics_DT$`com_rms_ap`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`com_rms_ap`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`com_rms_ap`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`com_rms_ap`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)







#com_rms_vert

Worst_OFFOFF <- kinematics_DT[, .(n = min(`com_rms_vert`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`com_rms_vert`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`com_rms_vert`]

kinematics_DT[, `com_rms_vert` := fifelse(is.na(`com_rms_vert`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`com_rms_vert`)&State=="MedOnStimON", as.numeric(Mean_ONON), `com_rms_vert`))]

wilcox.test(`com_rms_vert` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`com_rms_vert` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`com_rms_vert` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`com_rms_vert` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`com_rms_vert`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`com_rms_vert` <- kinematics_DT$`com_rms_vert` + rnorm(kinematics_DT$`com_rms_vert`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`com_rms_vert`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`com_rms_vert`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`com_rms_vert`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)




#com_rms_ml

Worst_OFFOFF <- kinematics_DT[, .(n = min(`com_rms_ml`, na.rm = TRUE)), by = State][order(State)][1,2]
Mean_ONON <- kinematics_DT[, .(n = mean(`com_rms_ml`, na.rm = TRUE)), by = State][order(State)][4,2]

kinematics_DT[,`com_rms_ml`]

kinematics_DT[, `com_rms_ml` := fifelse(is.na(`com_rms_ml`)&State=="MedOFFStimOFF", as.numeric(Worst_OFFOFF),
                                        fifelse(is.na(`com_rms_ml`)&State=="MedOnStimON", as.numeric(Mean_ONON), `com_rms_ml`))]

wilcox.test(`com_rms_ml` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOFFStimON",], paired=T)
wilcox.test(`com_rms_ml` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`com_rms_ml` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimON"|kinematics_DT$State=="MedONStimOFF",], paired=T)
wilcox.test(`com_rms_ml` ~ State, data=kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON",], , paired=T)

kinematics_DT[, .(n = mean(`com_rms_ml`, na.rm = TRUE)), by = State][order(State)]

kinematics_DT$`com_rms_ml` <- kinematics_DT$`com_rms_ml` + rnorm(kinematics_DT$`com_rms_ml`, mean = 0, sd = 0.00001)

friedman.test(y=kinematics_DT$`com_rms_ml`, groups=kinematics_DT$State, blocks=kinematics_DT$ID)

pairwise.wilcox.test(kinematics_DT$`com_rms_ml`, kinematics_DT$State, p.adj = "BH", paired=T)
ConoverTest(kinematics_DT$`com_rms_ml`, kinematics_DT$State, method ="BH")
names(kinematics_DBS_LD)



unique(kinematics_DT$State) # "MedOFFStimOFF" "MedOFFStimON"  "MedONStimOFF"  "MedOnStimON"  

mean_fun <- function(df, group) {
    df <- df[df$State==group]
    print(dim(df))
    column_means <- list(); name <- c()
    for (i in 3:ncol(df)) {
        if (class(df[[i]]) %in% c('numeric', 'integer')) {
            column_means[i] <- mean(df[[i]])
            name[i] <- i
        }
    }
    names(column_means) <- colnames(df)[name]
    return(unlist(column_means))
}

MedOFFStimOFF <- data.frame(mean_fun(kinematics_DT, "MedOFFStimOFF")) %>% mutate(Var=row.names(.))
MedOFFStimON <- data.frame(mean_fun(kinematics_DT, "MedOFFStimON")) %>% mutate(Var=row.names(.))
MedONStimOFF <- data.frame(mean_fun(kinematics_DT, "MedONStimOFF")) %>% mutate(Var=row.names(.))
MedOnStimON <- data.frame(mean_fun(kinematics_DT, "MedOnStimON")) %>% mutate(Var=row.names(.))

means <- MedOFFStimOFF %>% left_join(MedOFFStimON) %>% left_join(MedONStimOFF) %>% left_join(MedOnStimON) %>% select(2,1,3,4,5)
names(means) <- c("Var", "MedOFFStimOFF", "MedOFFStimON"  ,"MedONStimOFF" , "MedOnStimON")

sd_fun <- function(df, group) {
    df <- df[df$State==group]
    print(dim(df))
    column_sds <- list(); name <- c()
    for (i in 3:ncol(df)) {
        if (class(df[[i]]) %in% c('numeric', 'integer')) {
            column_sds[i] <- sd(df[[i]])
            name[i] <- i
        }
    }
    names(column_sds) <- colnames(df)[name]
    return(unlist(column_sds))
}

MedOFFStimOFF <- data.frame(sd_fun(kinematics_DT, "MedOFFStimOFF")) %>% mutate(Var=row.names(.))
MedOFFStimON <- data.frame(sd_fun(kinematics_DT, "MedOFFStimON")) %>% mutate(Var=row.names(.))
MedONStimOFF <- data.frame(sd_fun(kinematics_DT, "MedONStimOFF")) %>% mutate(Var=row.names(.))
MedOnStimON <- data.frame(sd_fun(kinematics_DT, "MedOnStimON")) %>% mutate(Var=row.names(.))

sds <- MedOFFStimOFF %>% left_join(MedOFFStimON) %>% left_join(MedONStimOFF) %>% left_join(MedOnStimON) %>% select(2,1,3,4,5)
names(sds) <- c("Var", "MedOFFStimOFF", "MedOFFStimON"  ,"MedONStimOFF" , "MedOnStimON")

means; sds
kinematics_DT %>% group_by(State) %>% summarise(n=mean(`Single_Support_(%_cycle)`))

dim(kinematics_DT)



for(i in names(kinematics_DT)[3:32]){
  fried <- friedman.test(y=kinematics_DT[[i]], groups=kinematics_DT$State, blocks=kinematics_DT$ID)
  print(fried$p.value)
  }


for(i in names(kinematics_DT)[3:32]){
  print(i)
  print(pairwise.wilcox.test(kinematics_DT[[i]], kinematics_DT$State, p.adj = "BH", paired=T))
  }


for(i in names(kinematics_DT)[3:32]){
  print(i)
  print( wilcox.test(get(i)~State, data = kinematics_DT[kinematics_DT$State=="MedOFFStimOFF"|kinematics_DT$State=="MedOnStimON"], paired=T))
}





temp <- kinematics_DT %>% filter(State=="MedOFFStimOFF") %>% bind_rows(kinematics_DT %>% filter(State=="MedOFFStimOFF")) %>%
  bind_rows(kinematics_DT %>% filter(State=="MedOFFStimON") %>% bind_rows(kinematics_DT %>% filter(State=="MedOFFStimON"))) %>%
  bind_rows(kinematics_DT %>% filter(State=="MedONStimOFF") %>% bind_rows(kinematics_DT %>% filter(State=="MedONStimOFF"))) %>%
  bind_rows(kinematics_DT %>% filter(State=="MedOnStimON") %>% bind_rows(kinematics_DT %>% filter(State=="MedOnStimON")))


for(i in names(temp)[3:32]){
  print(i)
  print( wilcox.test(get(i)~State, data = temp[temp$State=="MedOFFStimON"|temp$State=="MedONStimOFF"], paired=T))
}



fwrite(kinematics_DT, "kinematics_DT_complete.csv")


# --------------------------------
# Complete with 34 patients ----------------

kinematics_DT_complete_34pats <- fread("kinematics_DT_complete_34pats.csv")

unique(kinematics_DT_complete_34pats$State)
unique(kinematics_DT_complete_34pats$ID)

data.frame(names(kinematics_DT_complete_34pats))

for(i in names(kinematics_DT_complete_34pats)[3:23]){
  fried <- friedman.test(y=kinematics_DT_complete_34pats[[i]], groups=kinematics_DT_complete_34pats$State, blocks=kinematics_DT_complete_34pats$ID)
  print(fried$p.value)
}


mean_fun <- function(df, group) {
    df <- df[df$State==group]
    print(dim(df))
    column_means <- list(); name <- c()
    for (i in 3:ncol(df)) {
        if (class(df[[i]]) %in% c('numeric', 'integer')) {
            column_means[i] <- mean(df[[i]])
            name[i] <- i
        }
    }
    names(column_means) <- colnames(df)[name]
    return(unlist(column_means))
}

MedOFFStimOFF <- data.frame(mean_fun(kinematics_DT_complete_34pats, "MedOFFStimOFF")) %>% mutate(Var=row.names(.))
MedOFFStimON <- data.frame(mean_fun(kinematics_DT_complete_34pats, "MedOFFStimON")) %>% mutate(Var=row.names(.))
MedONStimOFF <- data.frame(mean_fun(kinematics_DT_complete_34pats, "MedONStimOFF")) %>% mutate(Var=row.names(.))
MedONStimON <- data.frame(mean_fun(kinematics_DT_complete_34pats, "MedONStimON")) %>% mutate(Var=row.names(.))

means <- MedOFFStimOFF %>% left_join(MedOFFStimON) %>% left_join(MedONStimOFF) %>% left_join(MedONStimON) %>% select(2,1,3,4,5)
names(means) <- c("Var", "MedOFFStimOFF", "MedOFFStimON"  ,"MedONStimOFF" , "MedONStimON")

sd_fun <- function(df, group) {
    df <- df[df$State==group]
    print(dim(df))
    column_sds <- list(); name <- c()
    for (i in 3:ncol(df)) {
        if (class(df[[i]]) %in% c('numeric', 'integer')) {
            column_sds[i] <- sd(df[[i]])
            name[i] <- i
        }
    }
    names(column_sds) <- colnames(df)[name]
    return(unlist(column_sds))
}

MedOFFStimOFF <- data.frame(sd_fun(kinematics_DT_complete_34pats, "MedOFFStimOFF")) %>% mutate(Var=row.names(.))
MedOFFStimON <- data.frame(sd_fun(kinematics_DT_complete_34pats, "MedOFFStimON")) %>% mutate(Var=row.names(.))
MedONStimOFF <- data.frame(sd_fun(kinematics_DT_complete_34pats, "MedONStimOFF")) %>% mutate(Var=row.names(.))
MedONStimON <- data.frame(sd_fun(kinematics_DT_complete_34pats, "MedONStimON")) %>% mutate(Var=row.names(.))

sds <- MedOFFStimOFF %>% left_join(MedOFFStimON) %>% left_join(MedONStimOFF) %>% left_join(MedONStimON) %>% select(2,1,3,4,5)
names(sds) <- c("Var", "MedOFFStimOFF", "MedOFFStimON"  ,"MedONStimOFF" , "MedONStimON")

means; sds




for(i in names(kinematics_DT_complete_34pats)[3:23]){
  print(i)
  print(pairwise.wilcox.test(kinematics_DT_complete_34pats[[i]], kinematics_DT_complete_34pats$State, p.adj = "BH", paired=T))
  }


for(i in names(kinematics_DT_complete_34pats)[3:23]){
  print(i)
  print( wilcox.test(get(i)~State, data = kinematics_DT_complete_34pats[kinematics_DT_complete_34pats$State=="MedOFFStimON"|kinematics_DT_complete_34pats$State=="MedONStimOFF"], paired=T))
}

# --------------------------------
# RandomForest PCA ----------------

kinematics_DT_complete_34pats <- fread("kinematics_DT_complete_34pats.csv")
unique(kinematics_DT_complete_34pats$State)


temp <- kinematics_DT_complete_34pats %>% filter(State=="MedOFFStimON"|State=="MedONStimOFF") %>% select(-ID) %>% mutate(State=as.factor(State))
names(temp) <- paste0("A",c(1:22))
lookup <- data.frame(names(kinematics_DT_complete_34pats[,c(3:23)])) %>% bind_cols(data.frame(names(temp[,c(2:22)])))


modelAll_1_randomForest <- randomForest(A1 ~ ., data = temp)
summary(modelAll_1_randomForest)
data.frame(modelAll_1_randomForest$importance) %>% arrange(-MeanDecreaseGini)


predict <- predict(modelAll_1_randomForest, temp, type = 'response')
ignore <- temp %>% bind_cols(data.frame(predict))

ignore %>% group_by(A1, predict) %>% count()

library(psych)
library(factoextra)

pc <- prcomp(temp[,-c(1,2)],
             center = TRUE,
            scale. = TRUE)

attributes(pc)
pc$rotation
print(pc)
summary(pc)

pc$x[,1:2] %>% bind_cols(temp %>% select(A1)) %>%
  ggplot(aes(PC1, PC2, colour=A1, value=A1)) +
  geom_point()

pc$rotation[,1:6]



# --------------------------------