library(tidyverse)
library(readxl) # part of tidyverse but have to load seperately
library(dplyr)
library(corrplot)
library(car)
#library(Hmisc)

options(scipen=999)

df1 <- read_csv("Driving_full.csv")

###First create composite measures for our main driving metrics before running inner_join

test <- df1 %>%
  mutate(ID = factor(ID),
         Age = factor(Age),
         group = factor(group),
         scenario = as.factor(scenario),
         Variable = factor(Variable),
         Cond = factor(Cond))

test$total <- c(test$c1r	+ test$c1l+	test$c2r+	test$c2l+	test$c3r	
                +test$c3l	+test$c4r	+test$c4l	+test$c5r+	test$c5l
                +	test$w1	+test$w2+	test$w3+	test$w4	+test$w5+	test$w6+
                  test$urb	+ test$rur	+ test$st1	+test$st1a	+test$st2	
                +test$st2a	+test$st3	+test$st3a	+test$h1	+test$h2
                +test$h3	+test$h4	+test$h5)/29

test$total2 <-  c(test$c1r	+ test$c1l+	test$c2r+	test$c2l+	test$c3r	
                  +test$c3l	+test$c4r	+test$c4l	+test$c5r+	test$c5l
                  +	test$w1	+test$w2+	test$w3+	test$w4	+test$w5+	test$w6+
                    test$urb	+ test$rur +test$h1	+test$h2
                  +test$h3	+test$h4	+test$h5)/23

test$totals <- c(test$c1r	+ test$c1l+	test$c2r+	test$c2l+	test$c3r	
                 +test$c3l	+test$c4r	+test$c4l	+test$c5r+	test$c5l
                 +	test$w1	+test$w2+	test$w3+	test$w4	+test$w5+	test$w6+
                   test$urb	+ test$rur)/18

test$curveswinding <- c(test$c1r	+ test$c1l+	test$c2r+	test$c2l+	test$c3r	
                        +test$c3l	+test$c4r	+test$c4l	+test$c5r+	test$c5l
                        +	test$w1	+test$w2+	test$w3+	test$w4	+test$w5+	test$w6)/16

## Convert driving data to df1
df1 <- test %>%
  select(one_of(c("ID", "Age", "Cond" ,"group","scenario", "Variable" ,
                  "total", "total2" ,"totals",  "curveswinding")))

#Make ID factor and then choose only variables of interest

df2 <- read_csv("Posturography2.csv")

df21 <- df2 %>%
  mutate(ID = factor(ID))%>%
  select(one_of(c('ID','trial', 'Path_Length')))
df21$trial = paste0('C', df21$trial )
df21 <- spread(df21, trial, Path_Length)



df2 <- df21 

##Convert dynamic visual acuity values into numeric for regression

df4 <- read_csv("Visual aquity for R.csv")

df4 <- df4 %>%
  mutate(ID = factor(ID),
         LVA = as.numeric(LVA),
         RVA = as.numeric(RVA),
         DVAH = as.numeric(DVAH),
         DVAV = as.numeric(DVAV))%>%
  select(-one_of(c('fID','Age', 'Cond')))

##Input FMS and SSQ data
df5 <- read_csv("SSQ_FMS_complete1.csv")
df5 <- df5 %>%
  mutate(ID = factor(ID))%>%
  mutate(years_drive = as.numeric(years_drive))
  
df5$Nausea <- c(df5$SSQ_GD+ df5$SSQ_IS + df5$SSQ_Sw + 
                  df5$SSQ_Nau +df5$SSQ_DC +df5$SSQ_SA + 
                  df5$SSQ_Brp)

df5$Oculomotor <- c(df5$SSQ_GD + df5$SSQ_Fg + df5$SSQ_HA +
                      df5$SSQ_ES + df5$SSQ_DF + df5$SSQ_DC+
                      df5$SSQ_BV)

df5$Disorientation <- c(df5$SSQ_DF + df5$SSQ_Nau + df5$SSQ_FoH +
                          df5$SSQ_BV + df5$SSQ_DEO + df5$SSQ_DEC +
                          df5$SSQ_Vrt)

df5$SSQ <- c(df5$Nausea + df5$Oculomotor +df5$Disorientation)*3.75

df5

## Merge data sets using inner_join
df <- inner_join(df1,df2, by = "ID")

df1 <- inner_join(df4, df5, by = "ID")

df <- inner_join(df, df1, by = "ID")


# Choose variables of interest for Multiple regressions



df$DVAcomp <- df$Better_eye- df$DVAH
df$DVAcomp <- abs(df$DVAcomp)


df1 <- df %>%
  select(one_of(c("ID","Age", "Cond" ,"group","scenario", "Variable" ,
                  "totals", 'SSQ', 'FMSpeak', "DVAcomp", "VIMS", 
                  "Cecr","RQ_total",'VIMS',"years_drive", 'Presence', "curveswinding" )))


df1wide <- spread(df1, Variable, totals)

df1wide <- df1wide %>%
  select(-one_of(c("Heading change","Max lat acc", "Mean lat acc", 
                   "SD lat acc", "SD Long acc", "Mean Long acc",
                   "Max long acc" ,  "Min Speed", "Max Speed")))


####Final way opf looking at it
df12 <- df1 %>%
  filter(scenario != "1")

df99 <- spread(df12, scenario, totals)

df99$means <- rowMeans(df99[ , 14:15], na.rm = TRUE)

df9 <- df99 %>%
  select(one_of(c("ID","Age","group", "Variable" ,
                  "means", 'SSQ', 'FMSpeak', "DVAcomp", 
                  "Cecr","RQ_total",'VIMS',"years_drive", 'Presence' )))

df2wide <- spread(df9, Variable, means)


df2wide <- df2wide %>%
  select(-one_of(c("Heading change","Max lat acc", "Mean lat acc", 
                   "SD lat acc", "SD Long acc", "Mean Long acc",
                   "Max long acc" ,  "Min Speed", "Max Speed", 
                   'age_years',"years_drive",  'age_years'))) 

df2wide <- df2wide%>%
  rename(
    Posture = Cecr,
    DVA = DVAcomp,
    Lane_Depart = 'Lane departures',
    Mean_Speed = 'Mean Speed',
    SD_Speed = 'SD Speed',
    Realism = "RQ_total",
    FMS = "FMSpeak",
    Steer_Rev = "Steer Rev")


dfwideoa <- df2wide %>%
  filter(Age == "oa")

dfwideya <- df2wide %>%
  filter(Age == "ya")

dffb <- df2wide %>%
  filter(group == "fb")

dftt <- df2wide %>%
  filter(group == "tt")


dffm <- df2wide %>%
  filter(group == "fm")

dfoafb <- df2wide %>%
  filter(Age == "oa")%>%
  filter(group == "fb")

dfoatt <- df2wide %>%
  filter(Age == "oa")%>%
  filter(group == "tt")

dfoafm <- df2wide %>%
  filter(Age == "oa")%>%
  filter(group == "fm")

dfyafb <- df2wide %>%
  filter(Age == "ya")%>%
  filter(group == "fb")

dfyatt <- df2wide %>%
  filter(Age == "ya")%>%
  filter(group == "tt")

dfyafm <- df2wide %>%
  filter(Age == "ya")%>%
  filter(group == "fm")


library(corrplot)


vsdp <- df2wide[,4:15]
hm <- cor(vsdp,use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")




####Now lets try for different groups_OA
ovsdp<- dfwideoa [,4:15]
hm <- cor(ovsdp,use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")



####Now lets try for different groups_YA
yvsdp<- dfwideya [,4:15]
hm <- cor(yvsdp,use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")



####Now lets try for different groups_fb
fbvsdp<- dffb [,4:15]
hm <- cor(fbvsdp,use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")


####Now lets try for different groups_tt
ttvsdp<- dftt [,4:15]
hm <- cor(ttvsdp,use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")




####Now lets try for different groups_fm
fmvsdp<- dffm [,4:15]
hm <- cor(fmvsdp,use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")





####Now lets try for different groups_fb x OA
fbvsdp<- dfoafb [,4:15]
hm <- cor(fbvsdp,use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")




####Now lets try for different groups_tt
ttvsdp<- dfoatt [,4:15]
hm <- cor(ttvsdp,use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")




####Now lets try for different groups_fm x OA

fmvsdp<- dfoafm [,4:15]
hm <- cor(fmvsdp, use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")



scatterplot(DVA ~ Mean_Speed, data = dfoafm)
plot(dfoafm$SD_Speed ~ dfoafm$Mean_Speed)


####Now lets try for different groups_fb x YA


fbvsdp<- dfyafb [,4:15]
hm <- cor(fbvsdp,use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")

####Now lets try for different groups_tt X YA


ttvsdp<- dfyatt [,4:15]
hm <- cor(ttvsdp,use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")


####Now lets try for different groups_fm x YA


fmvsdp<- dfyafm [,4:15]
hm <- cor(fmvsdp,use="complete.obs")
round(hm, 2)
p.hm <- cor.mtest(hm)
corrplot(hm, type = "upper", order = ,method = "color",tl.col = "black", tl.srt = 45,
         p.mat = p.hm$p, sig.level = .05, insig = "label_sig")



