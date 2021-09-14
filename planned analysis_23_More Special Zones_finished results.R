library(tidyverse)
library(readxl) # part of tidyverse but have to load seperately
library(dplyr)
library(afex)
library(emmeans)
library(car)
library(rstatix)
library(extrafont)
library(ggpubr)
library(effectsize)
library(ggplot2)
library(doBy)
library(jtools)
library(ggrepel)
library(onewaytests)

                 

df <- read_csv("Driving_full.csv")

library(extrafont)

loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()                

# convert bellow columns into factors for organiziation 
test <- df %>%
  mutate(ID = factor(ID),
         Age = factor(Age),
         group = factor(group),
         scenario = as.factor(scenario),
         Variable = factor(Variable),
         group = factor(group))

###test <- inner_join(test1, test2, by = "t1r")

##Calculate composite means for different scenario elements 
test$gentlecurves <- c(test$c1r	+test$c1l	+test$c2r+	test$c2l + test$c3r+	test$c3l)/6

test$gentlecurves1 <- c(test$c1r	+test$c1l	+test$c2r+	test$c2l + test$c3r+	test$c3l)

test$gentlecurves2 <- c(abs(test$c1r)	+abs(test$c1l)	+abs(test$c2r)+	abs(test$c2l) + abs(test$c3r)+	abs(test$c3l))/6

test$sharpcurves <- c(test$c4r	+test$c4l	+test$c5r+	test$c5l)/4

test$sharpcurves1 <- c(test$c4r	+test$c4l	+test$c5r+	test$c5l)

test$sharpcurves2 <- c(abs(test$c4r)	+abs(test$c4l)	+abs(test$c5r)+	abs(test$c5l))/4

test$winding <- abs(c(abs(test$w1)	+abs(test$w2)+	abs(test$w3) +	abs(test$w4)	+abs(test$w5)+	abs(test$w6))/6)

test$winding1 <- abs(c(abs(test$w1)	+abs(test$w2)+	abs(test$w3) +	abs(test$w4)	+abs(test$w5)+	abs(test$w6)))

test$stops <- c(test$st1 + test$st2 +test$st3)/3

test$starts <- c(test$st1a + test$st2a +test$st3a)/3

test$yield <- c(test$starts + test$stops)/2

test$turn1 <- c(abs(test$t1r) + abs(test$t2l) + abs(test$t3l) + abs(test$t4r))

test$turn <- c(test$t1r + test$t2l + test$t3l + test$t4r)/4

test$hills <- c(test$h1 + test$h2 + test$h3 + test$h4 + test$h5)/5

test$peaks <- c(test$h2 + test$h4)/5

test$curveswinding <- c(abs(test$c1r)	+ abs(test$c1l) +	abs(test$c2r) +	abs(test$c2l)+	abs(test$c3r)	
                        + abs(test$c3l)	+abs(test$c4r)	+ abs(test$c4l)	+ abs(test$c5r)+	abs(test$c5l)
                        +	abs(test$w1)	+ abs(test$w2)+	abs(test$w3) +	abs(test$w4)	+ abs(test$w5)+	abs(test$w6))/16
##Create smaller data frame with totals variable -makes it easier to convert from wide to long if needed
total <- test %>%
  select(one_of(c('ID', 'Age', 'scenario', "group", 
                  "Variable", "winding", "stops", 
                  "starts", "turn","turn1", "hills", "sharpcurves","sharpcurves1",
                  "yield",'winding1', "urb", "gentlecurves", "gentlecurves1", "peaks", "h1", "h3", "h5")))%>%
  filter(scenario != "1")

total$group <- factor(total$group, levels = c("fb","tt", "fm"))
levels(total$group) <- c("Fixed Base", "Turn Table", "Full Motion")
levels(total$Age) <- c("Older Adults", "Younger Adults")



  

  

#######All variables for Gentle Curves
Mspeed <- total %>%
  filter(Variable == "Mean Speed")
ms <- aov_4(urb  ~ Age*group, Error(ID), 
            data = Mspeed)
summary(ms)
ms
eta_squared(ms)
TukeyHSD(ms)
leveneTest(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ group|Age)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

boxplot(urb~group*Age,data=Mspeed)
outliers <- boxplot(urb~group*Age,data=Mspeed,plot=FALSE)$out
Mspeed1 <- Mspeed
Mspeed1 <- Mspeed1[-which(Mspeed1$urb %in% outliers),]
boxplot(urb~group*Age,data=Mspeed1)

Mspeed <- total %>%
  filter(Variable == "Mean Speed1")
ms <- aov(urb  ~ Age*group, 
          data = Mspeed1)
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

test_levene(ms)
test_sphericity(ms)

leveneTest(ms)

SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")

shapiro_test(SDoa$urb)

##SD Speed
SDspeed <- total %>%
  filter(Variable == "SD Speed") 
ss <- aov(urb~ Age*group, 
            data = SDspeed)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ Age|group)
emmeans(ss, pairwise ~ group|Age)
emmeans(ss, pairwise ~ group)
emmip(ss, ~ group|Age)

boxplot(urb~group*Age,data=SDspeed)
outliers <- boxplot(urb~group*Age,data=SDspeed,plot=FALSE)$out
SDspeed1 <- SDspeed
SDspeed1 <- SDspeed1[-which(SDspeed1$urb %in% outliers),]
boxplot(urb~group*Age,data=SDspeed1)

boxplot(urb~group*Age,data=SDspeed1)
outliers <- boxplot(urb~group*Age,data=SDspeed1,plot=FALSE)$out
SDspeed2 <- SDspeed1
SDspeed2 <- SDspeed2[-which(SDspeed2$urb %in% outliers),]
boxplot(urb~group*Age,data=SDspeed2)

ss <- aov(urb~ Age*group, 
          data = SDspeed2)
summary(ss)
TukeyHSD(ss)
eta_squared(ss)
leveneTest(ss)

SDoa <- SDspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")

shapiro_test(SDoa$urb)


ss1 <- oneway.test(urb ~ Age,data=SDspeed1, var.equal = FALSE)
ss1 <- oneway.test(urb ~ group,data=SDspeed1, var.equal = FALSE)
ss1

with(ss1,oneway(x=group,y=urb,posthoc="games-howell"))


ss$'Sphericity Corrections'
emmeans(ss, pairwise ~ group)
emmeans(ss, pairwise ~ Age)
emmeans(ss, pairwise ~ Age|group)
emmip(ss, ~ group|Age)
emmip(ss, ~ group)

SDspeed3 <- SDspeed2 %>%
  filter(Variable == "SD Speed") %>%
  select(one_of(c("ID", "Age", "group", "urb")))%>%
  group_by(group, Age)%>%
  summarise(
    n=n(),
    mean=mean(urb),
    sd=sd(urb)
  )%>%
  mutate(se=sd/sqrt(n-1))

shapiro.test(SDspeed3)


p <- ggplot(SDspeed2, aes(x=group, y=urb, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin(outlier.alpha = 0)+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("SD SPeed Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p
##SDLP
SDLP <- total %>%
  filter(Variable == "SDLP")
sdlp <- aov(urb ~ Age*group, data = SDLP)
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)

boxplot(urb~group*Age,data=SDLP)
outliers <- boxplot(urb~group*Age,data=SDLP,plot=FALSE)$out
SDLP1 <- SDLP
SDLP1 <- SDLP1[-which(SDLP1$urb %in% outliers),]
boxplot(urb~group*Age,data=SDLP1)

sdlp <- aov(urb ~ Age*group, data = SDLP1)
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmeans(sdlp,pairwise ~ Age)
emmeans(sdlp,pairwise ~ group|Age)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
 leveneTest(sdlp)

sr$'Sphericity Corrections'

SDLP2 <- SDLP1 %>%
  filter(Variable == "SDLP") %>%
  select(one_of(c("ID", "Age", "group", "urb")))%>%
  group_by(group, Age)%>%
  summarise(
    n=n(),
    mean=mean(urb),
    sd=sd(urb)
  )%>%
  mutate(se=sd/sqrt(n-1))

p <- ggplot(SDLP1, aes(x=group, y=urb, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_boxplot(outlier.alpha = 0)+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.75)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p
###Steering Reversals
SR <- total %>%
  filter(Variable == "Steer Rev")
sr <- aov(urb ~ Age*group, data = SR,anova_table = list(es = "pes"))
summary(sr)
eta_squared(sr)
emmeans(sr,pairwise ~ Age|group)
emmeans(sr,pairwise ~ group|Age)
emmeans(sr,pairwise ~ group)
emmip(sr, ~ group)
emmip(sr, Age ~ group)
emmip(sr, ~ scenario)
summary(sr)
test_levene(sr)
test_sphericity(sr)

boxplot(urb~group*Age,data=SR)
outliers <- boxplot(urb~group*Age,data=SR,plot=FALSE)$out
SR1 <- SR
SR1 <- SR1[-which(SR1$urb %in% outliers),]
boxplot(urb~group*Age,data=SR1)

boxplot(urb~group*Age,data=SR1)
outliers <- boxplot(urb~group*Age,data=SR1,plot=FALSE)$out
SR2 <- SR1
SR2 <- SR2[-which(SR2$urb %in% outliers),]
boxplot(urb~group*Age,data=SR2)

boxplot(urb~group*Age,data=SR2)
outliers <- boxplot(urb~group*Age,data=SR2,plot=FALSE)$out
SR3 <- SR2
SR3 <- SR3[-which(SR3$urb %in% outliers),]
boxplot(urb~group*Age,data=SR3)


sr <- aov(urb ~ Age*group, data = SR2,anova_table = list(es = "pes"))
summary(sr)
eta_squared(sr)
emmeans(sr,pairwise ~ Age|group)
emmeans(sr,pairwise ~ group|Age)
emmeans(sr,pairwise ~ group)
emmip(sr, ~ group)
emmip(sr, Age ~ group)
emmip(sr, ~ scenario)
summary(sr)
leveneTest(sr)
test_sphericity(sr)
TukeyHSD(sr)

SR3 <- SR2 %>%
  select(one_of(c("ID", "Age", "group", "gentlecurves")))%>%
  group_by( group)%>%
  filter(ID != "26")%>%
  filter(ID != "59")%>%
  summarise(
    n=n(),
    mean=mean(gentlecurves),
    sd=sd(gentlecurves)
  )%>%
  mutate(se=sd/sqrt(n-1))

p <- ggplot(SR2, aes(x=group, y=urb, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin(outlier.alpha = 0)+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Steering Reversals")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SDoa <- SR2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$gentlecurves)

shapiro_test(SDoa$gentlecurves)

kruskal.test(gentlecurves ~ group, SR2)
oneway.test(gentlecurves ~ group,data=SR2, var.equal = FALSE)

SR2 %>%
tapply(gentlecurves, c(group, Age), shapiro.test)

pairwise.wilcox.test(SR2$gentlecurves, SR2$group,
                     p.adjust.method = "BH")

leveneTest(sr)

summary(SR2)

###Lane Departures
LD <- total %>%
  filter(Variable == "Lane departures")
ld <- aov(urb ~ Age*group, data = LD,anova_table = list(es = "pes"))
summary(ld)
eta_squared(ld)
emmeans(ld,pairwise ~ group|Age)
emmeans(ld,pairwise ~ Age)
emmeans(ld,pairwise ~ group)
emmeans(ld,pairwise ~ Age|group)
emmip(ld, ~ group)
emmip(ld, Age ~ group)
emmip(ld, ~ scenario)
summary(ld)
test_levene(ld)
test_sphericity(ld)

boxplot(urb~group*Age,data=LD)
outliers <- boxplot(urb~group*Age,data=LD,plot=FALSE)$out
LD1 <- LD
LD1 <- LD1[-which(LD1$urb %in% outliers),]
boxplot(urb~group*Age,data=LD1)

LD <- total %>%
  filter(Variable == "Lane departures")
ld <- aov(urb ~ Age*group, data = LD1,anova_table = list(es = "pes"))
summary(ld)
eta_squared(ld)
emmeans(ld,pairwise ~ group|Age)
emmeans(ld,pairwise ~ scenario)
emmip(ld, ~ group)
emmip(ld, Age ~ group)
emmip(ld, ~ scenario)
summary(ld)
test_levene(ld)
test_sphericity(ld)

SR3 <- SR2 %>%
  select(one_of(c("ID", "Age", "group", "urb")))%>%
  group_by(Age, group)%>%
  filter(ID != "59")%>%
  summarise(
    n=n(),
    mean=mean(urb),
    sd=sd(urb)
  )%>%
  mutate(se=sd/sqrt(n-1))

p <- ggplot(SR2, aes(x=group, y=mean, fill=Age),colour="black") + 
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes( ymin=mean-se, ymax=mean+se),width=.2,
                position=position_dodge(.9)) + 
  xlab("Motion Group") + ylab("Lane Departures")+ 
  scale_fill_grey(start = 0.25, end = 0.75)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0,0)) +
  theme(text=element_text(family="Times", face="bold", size=12))+
  theme(legend.position = c(0.15, 0.85))
p

TukeyHSD(ld)






#################################################Gentle Curve

Mspeed <- total %>%
  filter(Variable == "Mean Speed")
ms <- aov(gentlecurves  ~ Age*group, 
          data = Mspeed,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

boxplot(gentlecurves~group*Age,data=Mspeed)
outliers <- boxplot(gentlecurves~group*Age,data=Mspeed,plot=FALSE)$out
Mspeed1 <- Mspeed
Mspeed1 <- Mspeed1[-which(Mspeed1$gentlecurves %in% outliers),]
boxplot(gentlecurves~group*Age,data=Mspeed1)

boxplot(gentlecurves~group*Age,data=Mspeed1)
outliers <- boxplot(gentlecurves~group*Age,data=Mspeed1,plot=FALSE)$out
Mspeed2 <- Mspeed1
Mspeed2 <- Mspeed2[-which(Mspeed2$gentlecurves %in% outliers),]
boxplot(gentlecurves~group*Age,data=Mspeed2)

boxplot(gentlecurves~group*Age,data=Mspeed2)
outliers <- boxplot(gentlecurves~group*Age,data=Mspeed2,plot=FALSE)$out
Mspeed3 <- Mspeed2
Mspeed3 <- Mspeed3[-which(Mspeed3$gentlecurves %in% outliers),]
boxplot(gentlecurves~group*Age,data=Mspeed3)


Mspeed <- total %>%
  filter(Variable == "Mean Speed1")
ms <- aov(gentlecurves  ~ Age*group, 
          data = Mspeed3,anova_table = list(es = "pes"))
summary(ms)
 leveneTest(ms)
TukeyHSD(b)
shapiro_test()
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)
TukeyHSD(ms)

leveneTest(ms)
test_sphericity(ms)
shapiro.test(Mspeed3$gentlecurves)

SDoa <- Mspeed2 %>%
  group_by(Age, group)

sa <- shapiro_test(SDoa$gentlecurves)

kruskal.test(gentlecurves ~ group, Mspeed3)
pairwise.wilcox.test(Mspeed3$gentlecurves, Mspeed3$group,
                     p.adjust.method = "BH")

Mspeed3 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

SDoa <- Mspeed3 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$gentlecurves)
SDoa <- Mspeed3 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$gentlecurves)
SDoa <- Mspeed3 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$gentlecurves)
SDoa <- Mspeed3 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$gentlecurves)
SDoa <- Mspeed3 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$gentlecurves)
SDoa <- Mspeed3 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$gentlecurves)

Mspeed4 <- Mspeed3 %>%
  select(one_of(c("ID", "Age", "group", "gentlecurves")))%>%
  filter(ID != "26")%>%
  group_by(Age, group)%>%
  summarise(
    n=n(),
    mean=mean(gentlecurves),
    sd=sd(gentlecurves)
  )%>%
  mutate(se=sd/sqrt(n-1))

p <- ggplot(Mspeed3, aes(x=group, y=gentlecurves, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin(outlier.alpha = 0)+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Mean Speed Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p


##SD Speed
SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(gentlecurves~ Age*group, 
          data = SDspeed)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ scenario)
emmeans(ss, pairwise ~ group|Age)
emmip(ss, ~ group|Age)

boxplot(gentlecurves~group*Age,data=SDspeed)
outliers <- boxplot(gentlecurves~group*Age,data=SDspeed,plot=FALSE)$out
SDspeed1 <- SDspeed
SDspeed1 <- SDspeed1[-which(SDspeed1$gentlecurves %in% outliers),]
boxplot(gentlecurves~group*Age,data=SDspeed1)

boxplot(gentlecurves~group*Age,data=SDspeed1)
outliers <- boxplot(gentlecurves~group*Age,data=SDspeed1,plot=FALSE)$out
SDspeed2 <- SDspeed1
SDspeed2 <- SDspeed2[-which(SDspeed2$gentlecurves %in% outliers),]
boxplot(gentlecurves~group*Age,data=SDspeed2)


SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(gentlecurves~ Age*group, 
          data = SDspeed1)
summary(ss)
eta_squared(ss)
leveneTest(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ group)
emmeans(ss, pairwise ~ group|Age)
emmeans(ss, pairwise ~Age)
emmeans(ss, pairwise ~ Age|group)
emmip(ss, ~ group|Age)
emmip(ss, ~ group)

##Sharpe Curves
SDLP <- total %>%
  filter(Variable == "SDLP")
sdlp <- aov(gentlecurves ~ Age*group, data = SDLP,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)

boxplot(gentlecurves~group*Age,data=SDLP)
outliers <- boxplot(gentlecurves~group*Age,data=SDLP,plot=FALSE)$out
SDLP1 <- SDLP
SDLP1 <- SDLP1[-which(SDLP1$gentlecurves %in% outliers),]
boxplot(gentlecurves~group*Age,data=SDLP1)

boxplot(gentlecurves~group*Age,data=SDLP1)
outliers <- boxplot(gentlecurves~group*Age,data=SDLP1,plot=FALSE)$out
SDLP2 <- SDLP1
SDLP2 <- SDLP2[-which(SDLP2$gentlecurves %in% outliers),]
boxplot(gentlecurves~group*Age,data=SDLP2)

boxplot(gentlecurves~group*Age,data=SDLP2)
outliers <- boxplot(gentlecurves~group*Age,data=SDLP2,plot=FALSE)$out
SDLP3 <- SDLP2
SDLP3 <- SDLP3[-which(SDLP3$gentlecurves %in% outliers),]
boxplot(gentlecurves~group*Age,data=SDLP3)


sdlp <- aov(gentlecurves ~ Age*group, data = SDLP3,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmeans(sdlp,pairwise ~ group|Age)
emmeans(sdlp,pairwise ~ Age)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
leveneTest(sdlp)

sa <- SDLP3 %>%
  group_by(group)%>%
  shapiro.test(sa$gentlecurves)

SDLP4 <- SDLP3 %>%
  select(one_of(c("ID", "Age", "group", "gentlecurves")))%>%
  group_by(group)%>%
  filter(ID != "26")%>%
  summarise(
    n=n(),
    mean=mean(gentlecurves),
    sd=sd(gentlecurves)
  )%>%
  mutate(se=sd/sqrt(n-1))

p <- ggplot(SDLP3, aes(x=group, y=gentlecurves, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin(outlier.alpha = 0)+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("SDLP (m)")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SDoa <- SDLP3 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$gentlecurves)
SDoa <- SDLP3 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$gentlecurves)
SDoa <- SDLP3 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$gentlecurves)
SDoa <- SDLP3 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$gentlecurves)
SDoa <- SDLP3 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$gentlecurves)
SDoa <- SDLP3 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$gentlecurves)


SDoa <- SDLP1 %>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$gentlecurves)
SDoa <- SDLP1 %>%
  filter(group == "Turn Table")
shapiro_test(SDoa$gentlecurves)
SDoa <- SDLP1 %>%
  filter(group == "Full Motion")
shapiro_test(SDoa$gentlecurves)

SDoa <- SDLP1 %>%
  filter(Age == "Younger Adults")
shapiro_test(SDoa$gentlecurves)

SDoa <- SDLP1 %>%
  filter(Age == "Older Adults")
shapiro_test(SDoa$gentlecurves)

pairwise.wilcox.test(SDLP1$urb, SDLP1$group,
                     p.adjust.method = "BH")



ss1 <- oneway.test(gentlecurves~ group,data=SDLP3, var.equal = FALSE)
ss1

with(ss1,oneway(x=group,y=urb,posthoc="games-howell"))


kruskal.test(gentlecurves ~ group, SDLP1)

SR2 %>%
  tapply(gentlecurves, c(group, Age), shapiro.test)

pairwise.wilcox.test(SDLP1$gentlecurves, SDLP1$group,
                     p.adjust.method = "BH")
TukeyHSD(sdlp)
###Steering Reversals
SR <- total %>%
  filter(Variable == "Steer Rev")
sr <- aov(gentlecurves1 ~ Age*group, data = SR,anova_table = list(es = "pes"))
summary(sr)
eta_squared(sr)
emmeans(sr,pairwise ~ Age|group)
emmeans(sr,pairwise ~ group|Age)
emmeans(sr,pairwise ~ group)
emmip(sr, ~ group)
emmip(sr, Age ~ group)
emmip(sr, ~ scenario)
summary(sr)
LeveneTest(sr)
test_sphericity(sr)

boxplot(gentlecurves~group*Age,data=SR)
outliers <- boxplot(gentlecurves~group*Age,data=SR,plot=FALSE)$out
SR1 <- SR
SR1 <- SR1[-which(SR1$gentlecurves %in% outliers),]
boxplot(gentlecurves~group*Age,data=SR1)

ss1 <- oneway.test(gentlecurves ~ group,data=SR1)
ss1
ss1 <- oneway.test(gentlecurves ~ Age,data=SR1)
ss1

sr <- aov(gentlecurves ~ Age*group, data = SR1,anova_table = list(es = "pes"))
summary(sr)
eta_squared(sr)
emmeans(sr,pairwise ~ Age|group)
emmeans(sr,pairwise ~ group|Age)
emmeans(sr,pairwise ~ group)
emmeans(sr,pairwise ~ Age)
emmip(sr, ~ group)
emmip(sr, Age ~ group)
emmip(sr, ~ scenario)
summary(sr)
 leveneTest(sr)


SR4 <- SR3 %>%
  select(one_of(c("ID", "Age", "group", "gentlecurves1")))%>%
  group_by(group)%>%
  filter(ID != '26')%>%
  summarise(
    n=n(),
    mean=mean(gentlecurves1),
    sd=sd(gentlecurves1)
  )%>%
  mutate(se=sd/sqrt(n-1))

p <- ggplot(SR1, aes(x=group, y=gentlecurves1, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin(outlier.alpha = 0)+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Steering Reversals")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SDoa <- SR1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$gentlecurves)

SDoa <- SR2 %>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR2 %>%
  filter(group == "Turn Table")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR2 %>%
  filter(group == "Full Motion")
shapiro_test(SDoa$gentlecurves)

SDoa <- SR2 %>%
  filter(Age == "Older Adults")
shapiro_test(SDoa$gentlecurves)
SDoa <- SR2 %>%
  filter(Age == "Younger Adults")
shapiro_test(SDoa$gentlecurves)


shapiro_test(SDoa$gentlecurves)

kruskal.test(gentlecurves ~ group, SR2)
oneway.test(gentlecurves ~ group,data=SR2, var.equal = FALSE)
oneway.test(gentlecurves ~ Age,data=SR2, var.equal = FALSE)

SR2 %>%
  tapply(gentlecurves, c(group, Age), shapiro.test)

pairwise.wilcox.test(SR2$gentlecurves, SR2$group,
                     p.adjust.method = "BH")

leveneTest(sr)

summary(SR2)


###Lane Departures
LD <- total %>%
  filter(Variable == "Lane departures")
ld <- aov(gentlecurves ~ Age*group, data = LD,anova_table = list(es = "pes"))
summary(ld)
eta_squared(ld)
emmeans(ld,pairwise ~ group|Age)
emmeans(ld,pairwise ~ group)
emmeans(ld,pairwise ~ Age|group)
emmip(ld, ~ group)
emmip(ld, Age ~ group)
emmip(ld, ~ scenario)
summary(ld)
test_levene(ld)
test_sphericity(ld)

boxplot(gentlecurves~group*Age,data=LD)
outliers <- boxplot(gentlecurves~group*Age,data=LD,plot=FALSE)$out
LD1 <- LD
LD1 <- LD1[-which(LD1$gentlecurves %in% outliers),]
boxplot(gentlecurves~group*Age,data=LD1)

boxplot(gentlecurves~group*Age,data=LD1)
outliers <- boxplot(gentlecurves~group*Age,data=LD1,plot=FALSE)$out
LD2 <- LD1
LD2 <- LD2[-which(LD2$gentlecurves %in% outliers),]
boxplot(gentlecurves~group*Age,data=LD2)

LD <- total %>%
  filter(Variable == "Lane departures")
ld <- aov(gentlecurves ~ Age*group, data = LD1,anova_table = list(es = "pes"))
summary(ld)
eta_squared(ld)
emmeans(ld,pairwise ~ group|Age)
emmeans(ld,pairwise ~ Age|group)
emmeans(ld,pairwise ~ scenario)
emmip(ld, ~ group)
emmip(ld, Age ~ group)
emmip(ld, ~ scenario)
summary(ld)
test_levene(ld)
test_sphericity(ld)




#################################################sharp Curve

Mspeed <- total %>%
  filter(Variable == "Mean Speed")
ms <- aov(sharpcurves  ~ Age*group, 
          data = Mspeed,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

boxplot(sharpcurves~group*Age,data=Mspeed)
outliers <- boxplot(sharpcurves~group*Age,data=Mspeed,plot=FALSE)$out
Mspeed1 <- Mspeed
Mspeed1 <- Mspeed1[-which(Mspeed1$sharpcurves %in% outliers),]
boxplot(sharpcurves~group*Age,data=Mspeed1)

Mspeed <- total %>%
  filter(Variable == "Mean Speed1")
ms <- aov(sharpcurves  ~ Age*group, 
          data = Mspeed1,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmeans(ms,pairwise ~ Age)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

leveneTest(ms)
test_sphericity(ms)
TukeyHSD(ms)

Mspeed2 <- Mspeed1 %>%
  select(one_of(c("ID", "Age", "group", "sharpcurves")))%>%
  group_by(group, Age)%>%
  filter(ID != '59')%>%
  filter(ID != '26')%>%
  filter(ID != '72')%>%
  filter(ID != '15')%>%
  summarise(
    n=n(),
    mean=mean(sharpcurves),
    sd=sd(sharpcurves)
  )%>%
  mutate(se=sd/sqrt(n-1))

p <- ggplot(Mspeed1, aes(x=group, y=sharpcurves, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin(outlier.alpha = 0)+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Mean Speed Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$sharpcurves)
SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$sharpcurves)
SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$sharpcurves)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$sharpcurves)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$sharpcurves)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$sharpcurves)

SDoa <- Mspeed1 %>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$gentlecurves)
SDoa <- Mspeed1 %>%
  filter(group == "Turn Table")
shapiro_test(SDoa$gentlecurves)
SDoa <- Mspeed1 %>%
  filter(group == "Full Motion")
shapiro_test(SDoa$gentlecurves)

SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")
shapiro_test(SDoa$gentlecurves)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")
shapiro_test(SDoa$gentlecurves)


##SD Speed
SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(sharpcurves~ Age*group, 
          data = SDspeed)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ scenario)
emmeans(ss, pairwise ~ group|Age)
emmeans(ss, pairwise ~ Age|group)
emmip(ss, ~ group|Age)

boxplot(sharpcurves~group*Age,data=SDspeed)
outliers <- boxplot(sharpcurves~group*Age,data=SDspeed,plot=FALSE)$out
SDspeed1 <- SDspeed
SDspeed1 <- SDspeed1[-which(SDspeed1$sharpcurves %in% outliers),]
boxplot(sharpcurves~group*Age,data=SDspeed1)


SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(sharpcurves~ Age*group, 
          data = SDspeed1)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ group)
emmeans(ss, pairwise ~ Age|group)
emmeans(ss, pairwise ~ Age)
emmeans(ss, pairwise ~ group|Age)
emmip(ss, ~ group|Age)
emmip(ss, ~ group)

SDoa <- SDspeed1 %>%
  filter(Age == "Older Adults")
shapiro_test(SDoa$gentlecurves)
SDoa <- SDspeed1 %>%
  filter(Age == "Younger Adults")
shapiro_test(SDoa$gentlecurves)

##Sharpe Curves
SDLP <- total %>%
  filter(Variable == "SDLP")
sdlp <- aov(sharpcurves ~ Age*group, data = SDLP,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmeans(sdlp,pairwise ~ group|Age )
emmeans(sdlp,pairwise ~ Age)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)

boxplot(sharpcurves~group*Age,data=SDLP)
outliers <- boxplot(sharpcurves~group*Age,data=SDLP,plot=FALSE)$out
SDLP1 <- SDLP
SDLP1 <- SDLP1[-which(SDLP1$sharpcurves %in% outliers),]
boxplot(sharpcurves~group*Age,data=SDLP1)

boxplot(sharpcurves~group*Age,data=SDLP1)
outliers <- boxplot(sharpcurves~group*Age,data=SDLP1,plot=FALSE)$out
SDLP2 <- SDLP1
SDLP2 <- SDLP1[-which(SDLP2$sharpcurves %in% outliers),]
boxplot(sharpcurves~group*Age,data=SDLP2)

sdlp <- aov(sharpcurves ~ Age*group, data = SDLP2,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmeans(sdlp,pairwise ~ group|Age)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
leveneTest(sdlp)

p <- ggplot(SDLP2, aes(x=group, y=sharpcurves, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin(outlier.alpha = 0)+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SDoa <- SDLP2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$sharpcurves)
SDoa <- SDLP2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$sharpcurves)
SDoa <- SDLP2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$sharpcurves)
SDoa <- SDLP2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$sharpcurves)
SDoa <- SDLP2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$sharpcurves)
SDoa <- SDLP2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$sharpcurves)

kruskal.test(sharpcurves ~ group, SR2)
p <- oneway.test(sharpcurves ~ group,data=SDLP1, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SDLP2, var.equal = FALSE)

SR2 %>%
  tapply(gentlecurves, c(group, Age), shapiro.test)

pairwise.wilcox.test(SR2$gentlecurves, SR2$group,
                     p.adjust.method = "BH")

eta_squared(p)
###Steering Reversals
SR <- total %>%
  filter(Variable == "Steer Rev")
sr <- aov(sharpcurves ~ Age*group, data = SR,anova_table = list(es = "pes"))
summary(sr)
eta_squared(sr)
emmeans(sr,pairwise ~ Age|group)
emmeans(sr,pairwise ~ group|Age)
emmeans(sr,pairwise ~ group)
emmip(sr, ~ group)
emmip(sr, Age ~ group)
emmip(sr, ~ scenario)
summary(sr)
test_levene(sr)
test_sphericity(sr)

boxplot(sharpcurves~group*Age,data=SR)
outliers <- boxplot(sharpcurves~group*Age,data=SR,plot=FALSE)$out
SR1 <- SR
SR1 <- SR1[-which(SR1$sharpcurves %in% outliers),]
boxplot(sharpcurves~group*Age,data=SR1)



sr <- aov(sharpcurves1 ~ Age*group, data = SR1,anova_table = list(es = "pes"))
summary(sr)
eta_squared(sr)
emmeans(sr,pairwise ~ Age|group)
emmeans(sr,pairwise ~ Age)
emmeans(sr,pairwise ~ group|Age)
emmeans(sr,pairwise ~ group)
emmip(sr, ~ group)
emmip(sr, Age ~ group)
emmip(sr, ~ scenario)
summary(sr)
test_levene(sr)
test_sphericity(sr)

leveneTest(sr)


SDoa <- SR1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$sharpcurves)
SDoa <- SR1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$sharpcurves)
SDoa <- SR1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$sharpcurves)
SDoa <- SR1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$sharpcurves)
SDoa <- SR1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$sharpcurves)
SDoa <- SR1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$sharpcurves)

oneway.test(sharpcurves ~ group,data=SR1, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)

pairwise.wilcox.test(SR1$sharpcurves, SR1$group,
                     p.adjust.method = "BH")

SR1 %>%
  group_by(Age)%>%
  dunn_test(sharpcurves~group)

p <- ggplot(SR1, aes(x=group, y=sharpcurves1, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin(outlier.alpha = 0)+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Steering Reversals")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SR2 <- SR1 %>%
  select(one_of(c("ID", "Age", "group", "sharpcurves1")))%>%
  group_by(group, Age)%>%
  filter(ID != '59')%>%
  filter(ID != '26')%>%
  filter(ID != '72')%>%
  filter(ID != '15')%>%
  summarise(
    n=n(),
    mean=mean(sharpcurves1),
    sd=sd(sharpcurves1)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(sr)
###Lane Departures
LD <- total %>%
  filter(Variable == "Lane departures")
ld <- aov(sharpcurves ~ Age*group, data = LD)
summary(ld)
eta_squared(ld)
emmeans(ld,pairwise ~ group|Age)
emmeans(ld,pairwise ~ group)
emmeans(ld,pairwise ~ Age|group)
emmip(ld, ~ group)
emmip(ld, Age ~ group)
emmip(ld, ~ scenario)
summary(ld)
test_levene(ld)
test_sphericity(ld)

boxplot(sharpcurves~group*Age,data=LD)
outliers <- boxplot(sharpcurves~group*Age,data=LD,plot=FALSE)$out
LD1 <- LD
LD1 <- LD1[-which(LD1$sharpcurves %in% outliers),]
boxplot(sharpcurves~group*Age,data=LD1)



LD <- total %>%
  filter(Variable == "Lane departures")
ld <- aov(sharpcurves1 ~ Age*group, data = LD1,anova_table = list(es = "pes"))
summary(ld)
eta_squared(ld)
emmeans(ld,pairwise ~ group|Age)
emmeans(ld,pairwise ~ Age|group)
emmeans(ld,pairwise ~ group)
emmeans(ld,pairwise ~ Age)
emmeans(ld,pairwise ~ scenario)
emmip(ld, ~ group)
emmip(ld, Age ~ group)
emmip(ld, ~ scenario)
summary(ld)
test_levene(ld)
test_sphericity(ld)

leveneTest(ld)


SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$sharpcurves)
SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$sharpcurves)
SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$sharpcurves)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$sharpcurves)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$sharpcurves)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$sharpcurves)

oneway.test(sharpcurves ~ group,data=LD1, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)

pairwise.wilcox.test(LD1$sharpcurves, LD1$group,
                     p.adjust.method = "BH")

LD1 %>%
  group_by(Age)%>%
  dunn_test(sharpcurves1~group)

p <- ggplot(LD1, aes(x=group, y=sharpcurves1, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Lane Departures")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="errorbar", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

LD2 <- LD1 %>%
  select(one_of(c("ID", "Age", "group", "sharpcurves1")))%>%
  group_by(group, Age)%>%
  filter(ID != '59')%>%
  filter(ID != '26')%>%
  filter(ID != '72')%>%
  filter(ID != '15')%>%
  summarise(
    n=n(),
    mean=mean(sharpcurves1),
    sd=sd(sharpcurves1)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(ld)




#################################################Winding

Mspeed <- total %>%
  filter(Variable == "Mean Speed")
ms <- aov(winding  ~ Age*group, 
          data = Mspeed,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

boxplot(winding~group*Age,data=Mspeed)
outliers <- boxplot(winding~group*Age,data=Mspeed,plot=FALSE)$out
Mspeed1 <- Mspeed
Mspeed1 <- Mspeed1[-which(Mspeed1$winding %in% outliers),]
boxplot(winding~group*Age,data=Mspeed1)

Mspeed <- total %>%
  filter(Variable == "Mean Speed1")
ms <- aov(winding  ~ Age*group, 
          data = Mspeed1,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ Age)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

leveneTest(ms)

TukeyHSD(ms)

SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(Mspeed1, aes(x=group, y=winding, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Mean Speed Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

Mspeed2 <- Mspeed1 %>%
  select(one_of(c("ID", "Age", "group", "winding")))%>%
  group_by(group, Age)%>%
 # filter(ID != '59')%>%
  #filter(ID != '26')%>%
  #filter(ID != '72')%>%
  #filter(ID != '15')%>%
  summarise(
    n=n(),
    mean=mean(winding),
    sd=sd(winding)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(ms)

##SD Speed
SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(winding~ Age*group, 
          data = SDspeed)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ scenario)
emmeans(ss, pairwise ~ group|Age)
emmip(ss, ~ group|Age)

boxplot(winding~group*Age,data=SDspeed)
outliers <- boxplot(winding~group*Age,data=SDspeed,plot=FALSE)$out
SDspeed1 <- SDspeed
SDspeed1 <- SDspeed1[-which(SDspeed1$winding %in% outliers),]
boxplot(winding~group*Age,data=SDspeed1)


SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(winding~ Age*group, 
          data = SDspeed1)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ Age|group)
emmeans(ss, pairwise ~ group|Age)
emmip(ss, ~ group|Age)
emmip(ss, ~ group)


##Sharpe Curves
SDLP <- total %>%
  filter(Variable == "SDLP")
sdlp <- aov(winding ~ Age*group, data = SDLP,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)

boxplot(winding~group*Age,data=SDLP)
outliers <- boxplot(winding~group*Age,data=SDLP,plot=FALSE)$out
SDLP1 <- SDLP
SDLP1 <- SDLP1[-which(SDLP1$winding %in% outliers),]
boxplot(winding~group*Age,data=SDLP1)

sdlp <- aov(winding ~ Age*group, data = SDLP1,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmeans(sdlp,pairwise ~ group|Age)
emmeans(sdlp,pairwise ~ Age)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)

leveneTest(sdlp)

TukeyHSD(ms)

SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(SDLP1, aes(x=group, y=winding, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

Mspeed2 <- Mspeed1 %>%
  select(one_of(c("ID", "Age", "group", "winding")))%>%
  group_by(group, Age)%>%
  # filter(ID != '59')%>%
  #filter(ID != '26')%>%
  #filter(ID != '72')%>%
  #filter(ID != '15')%>%
  summarise(
    n=n(),
    mean=mean(winding),
    sd=sd(winding)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(ms)

###Steering Reversals
SR <- total %>%
  filter(Variable == "Steer Rev")
sr <- aov(winding1 ~ Age*group, data = SR,anova_table = list(es = "pes"))
summary(sr)
eta_squared(sr)
emmeans(sr,pairwise ~ Age|group)
emmeans(sr,pairwise ~ group|Age)
emmeans(sr,pairwise ~ group)
emmip(sr, ~ group)
emmip(sr, Age ~ group)
emmip(sr, ~ scenario)
summary(sr)
test_levene(sr)
test_sphericity(sr)

boxplot(winding~group*Age,data=SR)
outliers <- boxplot(winding~group*Age,data=SR,plot=FALSE)$out
SR1 <- SR
SR1 <- SR1[-which(SR1$winding %in% outliers),]
boxplot(winding~group*Age,data=SR1)

boxplot(winding~group*Age,data=SR1)
outliers <- boxplot(winding~group*Age,data=SR1,plot=FALSE)$out
SR2 <- SR1
SR2 <- SR2[-which(SR2$winding %in% outliers),]
boxplot(winding~group*Age,data=SR2)



sr <- aov(winding1 ~ Age*group, data = SR2,anova_table = list(es = "pes"))
summary(sr)
eta_squared(sr)
emmeans(sr,pairwise ~ Age|group)
emmeans(sr,pairwise ~ Age)
emmeans(sr,pairwise ~ group|Age)
emmeans(sr,pairwise ~ group)
emmip(sr, ~ group)
emmip(sr, Age ~ group)
emmip(sr, ~ scenario)
summary(sr)
test_levene(sr)
test_sphericity(sr)


leveneTest(sr)

TukeyHSD(ms)

SDoa <- SR1 %>%
  filter(Age == "Older Adults")
#%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- SR1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- SR1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)
SDoa <- SR1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- SR1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- SR1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)

oneway.test(winding ~ Age,data=SR1, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)
kruskal.test(winding ~ Age, SR2)

pairwise.wilcox.test(SR1$winding1, SR1$Age,
                     p.adjust.method = "BH")

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(SR2, aes(x=group, y=winding, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Steering Reversals")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SR2 <- SR1 %>%
  select(one_of(c("ID", "Age", "group", "winding")))%>%
  group_by(group, Age)%>%
  filter(ID != '59')%>%
  summarise(
    n=n(),
    mean=mean(winding),
    sd=sd(winding)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(sr)



###Lane Departures
LD <- total %>%
  filter(Variable == "Lane departures")
ld <- aov(winding ~ Age*group, data = LD)
summary(ld)
eta_squared(ld)
emmeans(ld,pairwise ~ group|Age)
emmeans(ld,pairwise ~ group)
emmeans(ld,pairwise ~ Age|group)
emmip(ld, ~ group)
emmip(ld, Age ~ group)
emmip(ld, ~ scenario)
summary(ld)
test_levene(ld)
test_sphericity(ld)

boxplot(winding~group*Age,data=LD)
outliers <- boxplot(winding~group*Age,data=LD,plot=FALSE)$out
LD1 <- LD
LD1 <- LD1[-which(LD1$winding %in% outliers),]
boxplot(winding~group*Age,data=LD1)

boxplot(winding~group*Age,data=LD1)
outliers <- boxplot(winding~group*Age,data=LD1,plot=FALSE)$out
LD2 <- LD1
LD2 <- LD2[-which(LD2$winding %in% outliers),]
boxplot(winding~group*Age,data=LD2)

boxplot(winding~group*Age,data=LD2)
outliers <- boxplot(winding~group*Age,data=LD2,plot=FALSE)$out
LD3 <- LD2
LD3 <- LD3[-which(LD3$winding %in% outliers),]
boxplot(winding~group*Age,data=LD3)



LD <- total %>%
  filter(Variable == "Lane departures")
ld <- aov(winding ~ Age*group, data = LD1,anova_table = list(es = "pes"))
summary(ld)
eta_squared(ld)
emmeans(ld,pairwise ~ group|Age)
emmeans(ld,pairwise ~ Age)
emmeans(ld,pairwise ~ group)
emmeans(ld,pairwise ~ Age|group)
emmip(ld, ~ group)
emmip(ld, Age ~ group)
emmip(ld, ~ scenario)
summary(ld)
test_levene(ld)
test_sphericity(ld)

leveneTest(ld)

TukeyHSD(ms)

SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)

oneway.test(winding ~ Age,data=SR1, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(LD1$winding, LD1$group,
                     p.adjust.method = "BH")

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(LD1, aes(x=group, y=winding1, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Lane Departures")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

LD4 <- LD1 %>%
  select(one_of(c("ID", "Age", "group", "winding1")))%>%
  group_by(group)%>%
  filter(ID != '57')%>%
  summarise(
    n=n(),
    mean=mean(winding1),
    sd=sd(winding1)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(sr)





#################################################stops

Mspeed <- total %>%
  filter(Variable == "Mean Speed")
ms <- aov(stops  ~ Age*group, 
          data = Mspeed2,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ Age)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

boxplot(stops~group*Age,data=Mspeed)
outliers <- boxplot(stops~group*Age,data=Mspeed,plot=FALSE)$out
Mspeed1 <- Mspeed
Mspeed1 <- Mspeed1[-which(Mspeed1$stops %in% outliers),]
boxplot(stops~group*Age,data=Mspeed1)

boxplot(stops~group*Age,data=Mspeed1)
outliers <- boxplot(stops~group*Age,data=Mspeed1,plot=FALSE)$out
Mspeed2 <- Mspeed1
Mspeed2 <- Mspeed2[-which(Mspeed2$stops %in% outliers),]
boxplot(stops~group*Age,data=Mspeed2)

Mspeed <- total %>%
  filter(Variable == "Mean Speed")
ms <- aov(stops  ~ Age*group, 
          data = Mspeed2,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ Age)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

leveneTest(ms)

TukeyHSD(ms)

SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)

oneway.test(stops ~ Age,data=Mspeed2, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(LD1$winding, LD1$group,
                     p.adjust.method = "BH")

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(LD1, aes(x=group, y=winding1, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Lane Departures")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

LD4 <- LD1 %>%
  select(one_of(c("ID", "Age", "group", "winding1")))%>%
  group_by(group)%>%
  filter(ID != '57')%>%
  summarise(
    n=n(),
    mean=mean(winding1),
    sd=sd(winding1)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(sr)



##SD Speed
SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(stops~ Age*group, 
          data = SDspeed)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ scenario)
emmeans(ss, pairwise ~ group|Age)
emmip(ss, ~ group|Age)

boxplot(stops~group*Age,data=SDspeed)
outliers <- boxplot(stops~group*Age,data=SDspeed,plot=FALSE)$out
SDspeed1 <- SDspeed
SDspeed1 <- SDspeed1[-which(SDspeed1$stops %in% outliers),]
boxplot(stops~group*Age,data=SDspeed1)


SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(stops~ Age*group, 
          data = SDspeed1)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ group)
emmeans(ss, pairwise ~ group|Age)
emmip(ss, ~ group|Age)
emmip(ss, ~ group)

##Sharpe Curves
SDLP <- total %>%
  filter(Variable == "SDLP")
sdlp <- aov(stops ~ Age*group, data = SDLP,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)

boxplot(stops~group*Age,data=SDLP)
outliers <- boxplot(stops~group*Age,data=SDLP,plot=FALSE)$out
SDLP1 <- SDLP
SDLP1 <- SDLP1[-which(SDLP1$stops %in% outliers),]
boxplot(stops~group*Age,data=SDLP1)

sdlp <- aov(stops ~ Age*group, data = SDLP1,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmeans(sdlp,pairwise ~ group|Age)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)




###Lane Departures
LD <- total %>%
  filter(Variable == "Lane departures")
ld <- aov(stops ~ Age*group, data = LD)
summary(ld)
eta_squared(ld)
emmeans(ld,pairwise ~ group|Age)
emmeans(ld,pairwise ~ group)
emmeans(ld,pairwise ~ Age|group)
emmip(ld, ~ group)
emmip(ld, Age ~ group)
emmip(ld, ~ scenario)
summary(ld)
test_levene(ld)
test_sphericity(ld)

boxplot(stops~group*Age,data=LD)
outliers <- boxplot(stops~group*Age,data=LD,plot=FALSE)$out
LD1 <- LD
LD1 <- LD1[-which(LD1$stops %in% outliers),]
boxplot(stops~group*Age,data=LD1)

LD <- total %>%
  filter(Variable == "Lane departures")
ld <- aov(stops ~ Age*group, data = LD1,anova_table = list(es = "pes"))
summary(ld)
eta_squared(ld)
emmeans(ld,pairwise ~ group|Age)
emmeans(ld,pairwise ~ scenario)
emmip(ld, ~ group)
emmip(ld, Age ~ group)
emmip(ld, ~ scenario)
summary(ld)
test_levene(ld)
test_sphericity(ld)


###Mean Long ACC
MLO <- total %>%
  filter(Variable == "Mean Long acc")
mlo <- aov(stops ~ Age*group, data = MLO,anova_table = list(es = "pes"))
summary(mlo)
eta_squared(mlo)
emmeans
emmeans(mlo,pairwise ~ group|Age)
emmeans(mlo,pairwise ~ Age)
emmeans(mlo,pairwise ~ Age|group)
emmeans(mlo,pairwise ~ group)
emmip(mlo, ~ group)
emmip(mlo, Age ~ group)
emmip(mlo, ~ scenario)
summary(mlo)
test_levene(mlo)
test_sphericity(mlo)

boxplot(stops~group*Age,data=MLO)
outliers <- boxplot(stops~group*Age,data=MLO,plot=FALSE)$out
MLO1 <- MLO
MLO1 <- MLO1[-which(MLO1$stops %in% outliers),]
boxplot(stops~group*Age,data=MLO)

leveneTest(mlo)

TukeyHSD(ms)

SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)

oneway.test(stops ~ group,data=MLO, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(MLO$stops, MLO$group,
                     p.adjust.method = "BH")

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(MLO, aes(x=group, y=stops, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Mean Longitudinal Acceleration"~Km/h^2)+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

MLO1 <- MLO %>%
  select(one_of(c("ID", "Age", "group", "stops")))%>%
  group_by(group)%>%
  filter(ID != '59')%>%
  filter(ID != '15')%>%
  filter(ID != '26')%>%
  summarise(
    n=n(),
    mean=mean(stops),
    sd=sd(stops)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(slo)





###SD Long ACC
SLO <- total %>%
  filter(Variable == "SD Long acc")
slo <- aov(stops ~ Age*group, data = SLO,anova_table = list(es = "pes"))
summary(slo)
eta_squared(slo)
emmeans(slo,pairwise ~ Age|group)
emmeans(slo,pairwise ~ Age)
emmip(slo, ~ group)
emmip(slo, Age ~ group)
emmip(slo, ~ scenario)
summary(slo)
test_levene(slo)
test_sphericity(slo)
anova_test(slo)

boxplot(stops~group*Age,data=SLO)
outliers <- boxplot(stops~group*Age,data=SLO,plot=FALSE)$out
SLO1 <- SLO
SLO1 <- SLO1[-which(SLO1$stops %in% outliers),]
boxplot(stops~group*Age,data=SLO1)

slo <- aov(stops ~ Age*group, data = SLO1,anova_table = list(es = "pes"))
summary(slo)
eta_squared(slo)
emmeans(slo,pairwise ~ Age|group)
emmeans(slo,pairwise ~ Age)
emmip(slo, ~ group)
emmip(slo, Age ~ group)
emmip(slo, ~ scenario)
summary(slo)
test_levene(slo)
test_sphericity(slo)
anova_test(slo)


#################################################starts

Mspeed <- total %>%
  filter(Variable == "Mean Speed")
ms <- aov(starts  ~ Age*group, 
          data = Mspeed,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

boxplot(starts~group*Age,data=Mspeed)
outliers <- boxplot(starts~group*Age,data=Mspeed,plot=FALSE)$out
Mspeed1 <- Mspeed
Mspeed1 <- Mspeed1[-which(Mspeed1$starts %in% outliers),]
boxplot(starts~group*Age,data=Mspeed1)

boxplot(starts~group*Age,data=Mspeed1)
outliers <- boxplot(starts~group*Age,data=Mspeed1,plot=FALSE)$out
Mspeed2 <- Mspeed1
Mspeed2 <- Mspeed2[-which(Mspeed2$starts %in% outliers),]
boxplot(starts~group*Age,data=Mspeed2)

Mspeed <- total %>%
  filter(Variable == "Mean Speed1")
ms <- aov(starts  ~ Age*group, 
          data = Mspeed2,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

test_levene(ms)
test_sphericity(ms)

leveneTest(ms)

TukeyHSD(ms)

SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- LD1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)

oneway.test(stops ~ group,data=MLO, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(MLO$stops, MLO$group,
                     p.adjust.method = "BH")

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(Mspeed2, aes(x=group, y=starts, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Mean Speed Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

Mspeed3 <- Mspeed2 %>%
  select(one_of(c("ID", "Age", "group", "starts")))%>%
  group_by(group)%>%
  filter(ID != '59')%>%
  filter(ID != '15')%>%
  filter(ID != '26')%>%
  summarise(
    n=n(),
    mean=mean(starts),
    sd=sd(starts)
  )%>%
  mutate(se=sd/sqrt(n-1))


##SD Speed
SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(starts~ Age*group, 
          data = SDspeed)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ scenario)
emmeans(ss, pairwise ~ Age|group)
emmip(ss, ~ group|Age)

boxplot(starts~group*Age,data=SDspeed)
outliers <- boxplot(starts~group*Age,data=SDspeed,plot=FALSE)$out
SDspeed1 <- SDspeed
SDspeed1 <- SDspeed1[-which(SDspeed1$starts %in% outliers),]
boxplot(starts~group*Age,data=SDspeed1)


SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(starts~ Age*group, 
          data = SDspeed1)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ group)
emmeans(ss, pairwise ~ Age)
emmip(ss, ~ group|Age)
emmip(ss, ~ group)

##Sharpe Curves
SDLP <- total %>%
  filter(Variable == "SDLP")
sdlp <- aov(starts ~ Age*group, data = SDLP,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)

boxplot(starts~group*Age,data=SDLP)
outliers <- boxplot(starts~group*Age,data=SDLP,plot=FALSE)$out
SDLP1 <- SDLP
SDLP1 <- SDLP1[-which(SDLP1$starts %in% outliers),]
boxplot(starts~group*Age,data=SDLP1)

sdlp <- aov(starts ~ Age*group, data = SDLP1,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmeans(sdlp,pairwise ~ group|Age)
emmeans(sdlp,pairwise ~ Age)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)




###Mean Long ACC
MLO <- total %>%
  filter(Variable == "Mean Long acc")
mlo <- aov(starts ~ Age*group, data = MLO,anova_table = list(es = "pes"))
summary(mlo)
eta_squared(mlo)
emmeans(mlo,pairwise ~ Age)
emmeans(mlo,pairwise ~ scenario)
emmip(mlo, ~ group)
emmip(mlo, Age ~ group)
emmip(mlo, ~ scenario)
summary(mlo)
test_levene(mlo)
test_sphericity(mlo)

boxplot(starts~group*Age,data=MLO)
outliers <- boxplot(starts~group*Age,data=MLO,plot=FALSE)$out
MLO1 <- MLO
MLO1 <- MLO1[-which(MLO1$starts %in% outliers),]
boxplot(starts~group*Age,data=MLO1)

MLO <- total %>%
  filter(Variable == "Mean Long acc")
mlo <- aov(starts ~ Age*group, data = MLO1,anova_table = list(es = "pes"))
summary(mlo)
eta_squared(mlo)
emmeans(mlo,pairwise ~ Age)
emmeans(mlo,pairwise ~ Age|group)
emmeans(mlo,pairwise ~ group|Age)
emmip(mlo, ~ group)
emmip(mlo, Age ~ group)
emmip(mlo, ~ scenario)
summary(mlo)
test_levene(mlo)
test_sphericity(mlo)



###SD Long ACC
SLO <- total %>%
  filter(Variable == "SD Long acc")
slo <- aov(starts ~ Age*group, data = SLO,anova_table = list(es = "pes"))
summary(slo)
eta_squared(slo)
emmeans(slo,pairwise ~ Age|group)
emmeans(slo,pairwise ~ Age)
emmip(slo, ~ group)
emmip(slo, Age ~ group)
emmip(slo, ~ scenario)
summary(slo)
test_levene(slo)
test_sphericity(slo)
anova_test(slo)

boxplot(starts~group*Age,data=SLO)
outliers <- boxplot(starts~group*Age,data=SLO,plot=FALSE)$out
SLO1 <- SLO
SLO1 <- SLO1[-which(SLO1$starts %in% outliers),]
boxplot(starts~group*Age,data=SLO1)

slo <- aov(starts ~ Age*group, data = SLO1,anova_table = list(es = "pes"))
summary(slo)
eta_squared(slo)
emmeans(slo,pairwise ~group)
emmeans(slo,pairwise ~ Age|group)
emmeans(slo,pairwise ~ Age)
emmip(slo, ~ group)
emmip(slo, Age ~ group)
emmip(slo, ~ scenario)
summary(slo)
test_levene(slo)
test_sphericity(slo)
anova_test(slo)

###MLA
MLA <- total %>%
  filter(Variable == "Mean lat acc")
mlo<- aov(starts ~ Age*group, data = MLA,anova_table = list(es = "pes"))
summary(mlo)
emmeans(mlo,pairwise ~ Age)
emmeans(mlo,pairwise ~ scenario)
emmip(mlo, ~ group)
emmip(mlo, Age ~ group)
emmip(mlo, ~ scenario)
summary(mlo)
test_levene(mlo)
test_sphericity(mlo)

boxplot(starts1~group*Age,data=MLA)
outliers <- boxplot(starts~group*Age,data=MLA,plot=FALSE)$out
MLA1 <- MLA
MLA1 <- MLA1[-which(MLA1$starts %in% outliers),]
boxplot(starts ~group*Age,data=MLA1)

MLA <- total %>%
  filter(Variable == "Mean Lat acc")
mlo<- aov(starts ~ Age*group, data = MLA1,anova_table = list(es = "pes"))
summary(mlo)
emmeans(mlo,pairwise ~ Age)
emmeans(mlo,pairwise ~ group|Age)
emmeans(mlo,pairwise ~ group)
emmeans(mlo,pairwise ~ Age|group)
emmip(mlo, ~ group)
emmip(mlo, Age ~ group)
emmip(mlo, ~ scenario)
summary(mlo)
test_levene(mlo)
test_sphericity(mlo)

###SLA
SLA <- total %>%
  filter(Variable == "SD lat acc")
sla <- aov(starts ~ Age*group, data = SLA)
summary(sla)
emmeans(sla,pairwise ~ Age|group)
emmeans(sla,pairwise ~ Age)
emmip(sla, ~ group)
emmip(sla, Age ~ group)
emmip(sla, ~ scenario)
summary(sla)
test_levene(sla)
test_sphericity(sla)

boxplot(starts~group*Age,data=SLA)
outliers <- boxplot(starts~group*Age,data=SLA,plot=FALSE)$out
SLA1 <- SLA
SLA1 <- SLA1[-which(SLA1$starts %in% outliers),]
boxplot(starts~group*Age,data=SLA1)

SLA <- total %>%
  filter(Variable == "SD lat acc")
sla <- aov(starts ~ Age*group, data = SLA1)
summary(sla)
emmeans(sla,pairwise ~ Age|group)
emmeans(sla,pairwise ~ group|Age)
emmip(sla, ~ group)
emmip(sla, Age ~ group)
emmip(sla, ~ scenario)
summary(sla)
test_levene(sla)
test_sphericity(sla)


#################################################turn

Mspeed <- total %>%
  filter(Variable == "Mean Speed")
ms <- aov(turn  ~ Age*group, 
          data = Mspeed,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

boxplot(turn~group*Age,data=Mspeed)
outliers <- boxplot(turn~group*Age,data=Mspeed,plot=FALSE)$out
Mspeed1 <- Mspeed
Mspeed1 <- Mspeed1[-which(Mspeed1$turn %in% outliers),]
boxplot(turn~group*Age,data=Mspeed1)


boxplot(turn~group*Age,data=Mspeed1)
outliers <- boxplot(turn~group*Age,data=Mspeed1,plot=FALSE)$out
Mspeed2 <- Mspeed1
Mspeed2 <- Mspeed2[-which(Mspeed2$turn %in% outliers),]
boxplot(turn~group*Age,data=Mspeed2)

Mspeed <- total %>%
  filter(Variable == "Mean Speed")
ms <- aov(turn  ~ Age*group, 
          data = Mspeed2,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ Age)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

leveneTest(ms)

TukeyHSD(ms)

SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$winding)
SDoa <- Mspeed1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$winding)

oneway.test(winding ~ Age,data=SR1, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(LD1$winding, LD1$group,
                     p.adjust.method = "BH")

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(LD1, aes(x=group, y=winding1, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Lane Departures")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

LD4 <- LD1 %>%
  select(one_of(c("ID", "Age", "group", "winding1")))%>%
  group_by(group)%>%
  filter(ID != '57')%>%
  summarise(
    n=n(),
    mean=mean(winding1),
    sd=sd(winding1)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(sr)



##SD Speed
SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(turn~ Age*group, 
          data = SDspeed)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ scenario)
emmeans(ss, pairwise ~ group|Age)
emmip(ss, ~ group|Age)

boxplot(turn~group*Age,data=SDspeed)
outliers <- boxplot(turn~group*Age,data=SDspeed,plot=FALSE)$out
SDspeed1 <- SDspeed
SDspeed1 <- SDspeed1[-which(SDspeed1$turn %in% outliers),]
boxplot(turn~group*Age,data=SDspeed1)

boxplot(turn~group*Age,data=SDspeed1)
outliers <- boxplot(turn~group*Age,data=SDspeed1,plot=FALSE)$out
SDspeed2 <- SDspeed1
SDspeed2 <- SDspeed2[-which(SDspeed2$turn %in% outliers),]
boxplot(turn~group*Age,data=SDspeed2)


SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(turn~ Age*group, 
          data = SDspeed2)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ Age|group)
emmeans(ss, pairwise ~ Age)
emmeans(ss, pairwise ~ group|Age)
emmeans(ss, pairwise ~ group)
emmip(ss, ~ group|Age)
emmip(ss, ~ group)


leveneTest(ms)

TukeyHSD(ss)

SDoa <- SDspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- SDspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- SDspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)
SDoa <- SDspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- SDspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- SDspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)

oneway.test(winding ~ Age,data=SR1, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(LD1$winding, LD1$group,
                     p.adjust.method = "BH")

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(SDspeed2, aes(x=group, y=turn1, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("SD Speed Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SD1 <- SDspeed2 %>%
  select(one_of(c("ID", "Age", "group", "turn1")))%>%
  group_by(group)%>%
  filter(ID != '59')%>%
  filter(ID != '15')%>%
  filter(ID != '72')%>%
  filter(ID != '26')%>%
  filter(ID != '74')%>%
  summarise(
    n=n(),
    mean=mean(turn1),
    sd=sd(turn1)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(sr)


##Sharpe Curves
SDLP <- total %>%
  filter(Variable == "SDLP")
sdlp <- aov(turn ~ Age*group, data = SDLP,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)

boxplot(turn~group*Age,data=SDLP)
outliers <- boxplot(turn~group*Age,data=SDLP,plot=FALSE)$out
SDLP1 <- SDLP
SDLP1 <- SDLP1[-which(SDLP1$turn %in% outliers),]
boxplot(turn~group*Age,data=SDLP1)

sdlp <- aov(turn ~ Age*group, data = SDLP1,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmeans(sdlp,pairwise ~ group|Age)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)



leveneTest(ms)

TukeyHSD(ss)

SDoa <- SDspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- SDspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- SDspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)
SDoa <- SDspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- SDspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- SDspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)

oneway.test(winding ~ Age,data=SR1, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(LD1$winding, LD1$group,
                     p.adjust.method = "BH")

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(SDspeed2, aes(x=group, y=turn1, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SD1 <- SDspeed2 %>%
  select(one_of(c("ID", "Age", "group", "turn1")))%>%
  group_by(group)%>%
  filter(ID != '59')%>%
  filter(ID != '15')%>%
  filter(ID != '72')%>%
  filter(ID != '26')%>%
  filter(ID != '74')%>%
  summarise(
    n=n(),
    mean=mean(turn1),
    sd=sd(turn1)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(sr)



###Steering Reversals
SR <- total %>%
  filter(Variable == "Steer Rev")
sr <- aov(turn ~ Age*group, data = SR1,anova_table = list(es = "pes"))
summary(sr)
eta_squared(sr)
emmeans(sr,pairwise ~ Age|group)
emmeans(sr,pairwise ~ Age)
emmeans(sr,pairwise ~ group|Age)
emmeans(sr,pairwise ~ group)
emmip(sr, ~ group)
emmip(sr, Age ~ group)
emmip(sr, ~ scenario)
summary(sr)
test_levene(sr)
test_sphericity(sr)

boxplot(turn1~group*Age,data=SR)
outliers <- boxplot(turn1~group*Age,data=SR,plot=FALSE)$out
SR1 <- SR
SR1 <- SR1[-which(SR1$turn1 %in% outliers),]
boxplot(turn1~group*Age,data=SR1)





###Lane Departures
LD <- total %>%
  filter(Variable == "Lane departures")
ld <- aov(turn ~ Age*group, data = LD1)
summary(ld)
eta_squared(ld)
emmeans(ld,pairwise ~ group|Age)
emmeans(ld,pairwise ~ Age)
emmeans(ld,pairwise ~ group)
emmeans(ld,pairwise ~ Age|group)
emmip(ld, ~ group)
emmip(ld, Age ~ group)
emmip(ld, ~ scenario)
summary(ld)
test_levene(ld)
test_sphericity(ld)

boxplot(turn~group*Age,data=LD)
outliers <- boxplot(turn~group*Age,data=LD,plot=FALSE)$out
LD1 <- LD
LD1 <- LD1[-which(LD1$turn %in% outliers),]
boxplot(turn~group*Age,data=LD1)




###Mean Long ACC
MLO <- total %>%
  filter(Variable == "Mean Long acc")
mlo <- aov(turn ~ Age*group, data = MLO,anova_table = list(es = "pes"))
summary(mlo)
eta_squared(mlo)
emmeans(mlo,pairwise ~ Age)
emmeans(mlo,pairwise ~ scenario)
emmip(mlo, ~ group)
emmip(mlo, Age ~ group)
emmip(mlo, ~ scenario)
summary(mlo)
test_levene(mlo)
test_sphericity(mlo)

boxplot(turn~group*Age,data=MLO)
outliers <- boxplot(turn~group*Age,data=MLO,plot=FALSE)$out
MLO1 <- MLO
MLO1 <- MLO1[-which(MLO1$turn %in% outliers),]
boxplot(turn~group*Age,data=MLO1)

boxplot(turn~group*Age,data=MLO1)
outliers <- boxplot(turn~group*Age,data=MLO1,plot=FALSE)$out
MLO2 <- MLO1
MLO2 <- MLO2[-which(MLO2$turn %in% outliers),]
boxplot(turn~group*Age,data=MLO2)

MLO <- total %>%
  filter(Variable == "Mean Long acc")
mlo <- aov(turn ~ Age*group, data = MLO2,anova_table = list(es = "pes"))
summary(mlo)
eta_squared(mlo)
emmeans(mlo,pairwise ~ Age)
emmeans(mlo,pairwise ~ Age|group)
emmeans(mlo,pairwise ~ group|Age)
emmeans(mlo,pairwise ~ group)
emmip(mlo, ~ group)
emmip(mlo, Age ~ group)
emmip(mlo, ~ scenario)
summary(mlo)
leveneTest(mlo)

shapiro_test(MLO2,turn)

SDoa <- MLO2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- MLO2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- MLO2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)
SDoa <- MLO2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- MLO2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- MLO2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)

oneway.test(winding ~ Age,data=SR1, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(LD1$winding, LD1$group,
                     p.adjust.method = "BH")

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(MLO2, aes(x=group, y=turn1, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SD1 <- SDspeed2 %>%
  select(one_of(c("ID", "Age", "group", "turn1")))%>%
  group_by(group)%>%
  filter(ID != '59')%>%
  filter(ID != '15')%>%
  filter(ID != '72')%>%
  filter(ID != '26')%>%
  filter(ID != '74')%>%
  summarise(
    n=n(),
    mean=mean(turn1),
    sd=sd(turn1)
  )%>%
  mutate(se=sd/sqrt(n-1))



###SD Long ACC
SLO <- total %>%
  filter(Variable == "SD Long acc")
slo <- aov(turn ~ Age*group, data = SLO,anova_table = list(es = "pes"))
summary(slo)
eta_squared(slo)
emmeans(slo,pairwise ~ Age|group)
emmeans(slo,pairwise ~ Age)
emmip(slo, ~ group)
emmip(slo, Age ~ group)
emmip(slo, ~ scenario)
summary(slo)
test_levene(slo)
test_sphericity(slo)
anova_test(slo)

boxplot(turn~group*Age,data=SLO)
outliers <- boxplot(turn~group*Age,data=SLO,plot=FALSE)$out
SLO1 <- SLO
SLO1 <- SLO1[-which(SLO1$turn %in% outliers),]
boxplot(turn~group*Age,data=SLO1)

slo <- aov(turn ~ Age*group, data = SLO1,anova_table = list(es = "pes"))
summary(slo)
eta_squared(slo)
emmeans(slo,pairwise ~ Age|group)
emmeans(slo,pairwise ~ group)
emmeans(slo,pairwise ~ group|Age)
emmeans(slo,pairwise ~ Age)
emmip(slo, ~ group)
emmip(slo, Age ~ group)
emmip(slo, ~ scenario)
summary(slo)
test_levene(slo)
test_sphericity(slo)
anova_test(slo)

leveneTest(slo)

shapiro_test(MLO2,turn)

SDoa <- SLO1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- SLO1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- SLO1 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)
SDoa <- SLO1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- SLO1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- SLO1 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)

oneway.test(winding ~ Age,data=SR1, var.equal = FALSE)
oneway.test(turn ~ group,data=SLO1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(SLO1$turn, SLO1$group,
                     p.adjust.method = "BH")

TukeyHSD(slo)

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(SLO1, aes(x=group, y=turn1, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab((" Mean Longitudinal Acceleration"~Km/h^2))+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SLO3 <- SLO1 %>%
  select(one_of(c("ID", "Age", "group", "turn1")))%>%
  group_by(group)%>%
 filter(ID != '59')%>%
  filter(ID != '15')%>%
  filter(ID != '72')%>%
  filter(ID != '26')%>%
  filter(ID != '74')%>%
  summarise(
    n=n(),
    mean=mean(turn1),
    sd=sd(turn1)
  )%>%
  mutate(se=sd/sqrt(n-1))

###MLA
MLA <- total %>%
  filter(Variable == "Mean lat acc")
mlo<- aov(turn ~ Age*group, data = MLA,anova_table = list(es = "pes"))
summary(mlo)
emmeans(mlo,pairwise ~ Age)
emmeans(mlo,pairwise ~ scenario)
emmip(mlo, ~ group)
emmip(mlo, Age ~ group)
emmip(mlo, ~ scenario)
summary(mlo)
test_levene(mlo)
test_sphericity(mlo)

boxplot(turn1~group*Age,data=MLA)
outliers <- boxplot(turn~group*Age,data=MLA,plot=FALSE)$out
MLA1 <- MLA
MLA1 <- MLA1[-which(MLA1$turn %in% outliers),]
boxplot(turn ~group*Age,data=MLA1)

boxplot(turn1~group*Age,data=MLA1)
outliers <- boxplot(turn~group*Age,data=MLA1,plot=FALSE)$out
MLA2 <- MLA1
MLA2 <- MLA2[-which(MLA2$turn %in% outliers),]
boxplot(turn ~group*Age,data=MLA2)

boxplot(turn1~group*Age,data=MLA2)
outliers <- boxplot(turn~group*Age,data=MLA2,plot=FALSE)$out
MLA3 <- MLA2
MLA3 <- MLA3[-which(MLA3$turn %in% outliers),]
boxplot(turn ~group*Age,data=MLA3)

MLA <- total %>%
  filter(Variable == "Mean Lat acc")
mlo<- aov(turn ~ Age*group, data = MLA3,anova_table = list(es = "pes"))
summary(mlo)
emmeans(mlo,pairwise ~ Age)
emmeans(mlo,pairwise ~ group|Age)
emmeans(mlo,pairwise ~ group)
emmeans(mlo,pairwise ~ Age|group)
emmip(mlo, ~ group)
emmip(mlo, Age ~ group)
emmip(mlo, ~ scenario)
summary(mlo)
test_levene(mlo)
test_sphericity(mlo)

###SLA
SLA <- total %>%
  filter(Variable == "SD lat acc")
sla <- aov(turn ~ Age*group, data = SLA)
summary(sla)
emmeans(sla,pairwise ~ Age|group)
emmeans(sla,pairwise ~ Age)
emmip(sla, ~ group)
emmip(sla, Age ~ group)
emmip(sla, ~ scenario)
summary(sla)
test_levene(sla)
test_sphericity(sla)

boxplot(turn~group*Age,data=SLA)
outliers <- boxplot(turn~group*Age,data=SLA,plot=FALSE)$out
SLA1 <- SLA
SLA1 <- SLA1[-which(SLA1$turn %in% outliers),]
boxplot(turn~group*Age,data=SLA1)

boxplot(turn~group*Age,data=SLA1)
outliers <- boxplot(turn~group*Age,data=SLA1,plot=FALSE)$out
SLA2 <- SLA1
SLA2 <- SLA2[-which(SLA2$turn %in% outliers),]
boxplot(turn~group*Age,data=SLA2)

SLA <- total %>%
  filter(Variable == "SD lat acc")
sla <- aov(turn ~ Age*group, data = SLA2)
summary(sla)
eta_squared(sla)
emmeans(sla,pairwise ~ Age|group)
emmeans(sla,pairwise ~ group|Age)
emmeans(sla,pairwise ~ group)
emmeans(sla,pairwise ~ group|Age)
emmeans(sla,pairwise ~ Age)
emmip(sla, ~ group)
emmip(sla, Age ~ group)
emmip(sla, ~ scenario)
summary(sla)
test_levene(sla)
test_sphericity(sla)

TukeyHSD(sla)

leveneTest(sla)

shapiro_test(MLO2,turn)

SDoa <- SLA2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- SLA2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- SLA2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)
SDoa <- SLA2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- SLA2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- SLA2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)

oneway.test(winding ~ Age,data=SR1, var.equal = FALSE)
oneway.test(sharpcurves ~ Age,data=SR1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(LD1$winding, LD1$group,
                     p.adjust.method = "BH")

SR1 %>%
  group_by(Age)%>%
  dunn_test(turns~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(SLA2, aes(x=group, y=turn1, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("SD Lateral Acceleration"~Km/h^2)+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SLA4 <- SLA2 %>%
  select(one_of(c("ID", "Age", "group", "turn1")))%>%
  group_by(Age,group)%>%
  filter(ID != '59')%>%
  filter(ID != '15')%>%
  filter(ID != '72')%>%
  filter(ID != '26')%>%
  filter(ID != '74')%>%
  summarise(
    n=n(),
    mean=mean(turn1),
    sd=sd(turn1)
  )%>%
  mutate(se=sd/sqrt(n-1))

TukeyHSD(sla)



#################################################peaks


#################################################hills

Mspeed <- total %>%
  filter(Variable == "Mean Speed")
ms <- aov(hills  ~ Age*group, 
          data = Mspeed,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age|group)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

boxplot(hills~group*Age,data=Mspeed)
outliers <- boxplot(hills~group*Age,data=Mspeed,plot=FALSE)$out
Mspeed1 <- Mspeed
Mspeed1 <- Mspeed1[-which(Mspeed1$hills %in% outliers),]
boxplot(hills~group*Age,data=Mspeed1)

boxplot(hills~group*Age,data=Mspeed1)
outliers <- boxplot(hills~group*Age,data=Mspeed1,plot=FALSE)$out
Mspeed2 <- Mspeed1
Mspeed2 <- Mspeed2[-which(Mspeed2$hills %in% outliers),]
boxplot(hills~group*Age,data=Mspeed2)

Mspeed <- total %>%
  filter(Variable == "Mean Speed")
ms <- aov(hills  ~ Age*group, 
          data = Mspeed2,anova_table = list(es = "pes"))
summary(ms)
eta_squared(ms)
emmeans(ms,pairwise ~ Age)
emmeans(ms,pairwise ~ group|Age)
emmeans(ms,pairwise ~ group)
emmip(ms, ~ group)
emmip(ms, Age ~ group)

leveneTest(ms)
TukeyHSD(ms)

shapiro_test(MLO2,turn)

SDoa <- Mspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)

oneway.test(winding ~ Age,data=SR1, var.equal = FALSE)
oneway.test(turn ~ group,data=SLO1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(SLO1$turn, SLO1$group,
                     p.adjust.method = "BH")

TukeyHSD(ms)

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(Mspeed2, aes(x=group, y=hills, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("Mean Speed Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

Mspeed3 <- Mspeed2 %>%
  select(one_of(c("ID", "Age", "group", "hills")))%>%
  group_by(group)%>%
  filter(ID != '26')%>%
  #filter(ID != '15')%>%
  #filter(ID != '72')%>%
  #filter(ID != '26')%>%
  #filter(ID != '74')%>%
  summarise(
    n=n(),
    mean=mean(hills),
    sd=sd(hills)
  )%>%
  mutate(se=sd/sqrt(n-1))


##SD Speed
SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(hills~ Age*group, 
          data = SDspeed)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ scenario)
emmeans(ss, pairwise ~ group|Age)
emmip(ss, ~ group|Age)

boxplot(hills~group*Age,data=SDspeed)
outliers <- boxplot(hills~group*Age,data=SDspeed,plot=FALSE)$out
SDspeed1 <- SDspeed
SDspeed1 <- SDspeed1[-which(SDspeed1$hills %in% outliers),]
boxplot(hills~group*Age,data=SDspeed1)

boxplot(hills~group*Age,data=SDspeed1)
outliers <- boxplot(hills~group*Age,data=SDspeed1,plot=FALSE)$out
SDspeed2 <- SDspeed1
SDspeed2 <- SDspeed2[-which(SDspeed2$hills %in% outliers),]
boxplot(hills~group*Age,data=SDspeed2)


SDspeed <- total %>%
  filter(Variable == "SD Speed")
ss <- aov(hills~ Age*group, 
          data = SDspeed2)
summary(ss)
eta_squared(ss)
test_levene(ss)
test_sphericity(ss)
shapiro.test(ss)
emmeans(ss, pairwise ~ group)
emmeans(ss, pairwise ~ group|Age)
emmeans(ss, pairwise ~ Age|group)
emmip(ss, ~ group|Age)
emmip(ss, ~ group)

leveneTest(ss)
TukeyHSD(ss)

shapiro_test(MLO2,turn)

SDoa <- Mspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)

oneway.test(winding ~ Age,data=SR1, var.equal = FALSE)
oneway.test(turn ~ group,data=SLO1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(SLO1$turn, SLO1$group,
                     p.adjust.method = "BH")

TukeyHSD(ms)

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(SDspeed2, aes(x=group, y=hills, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("SD Speed Km/h")+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SDspeed3 <- SDspeed2 %>%
  select(one_of(c("ID", "Age", "group", "hills")))%>%
  group_by(group)%>%
  filter(ID != '26')%>%
  #filter(ID != '15')%>%
  #filter(ID != '72')%>%
  #filter(ID != '26')%>%
  #filter(ID != '74')%>%
  summarise(
    n=n(),
    mean=mean(hills),
    sd=sd(hills)
  )%>%
  mutate(se=sd/sqrt(n-1))


##Sharpe Curves
SDLP <- total %>%
  filter(Variable == "SDLP")
sdlp <- aov(hills ~ Age*group, data = SDLP,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)

boxplot(hills~group*Age,data=SDLP)
outliers <- boxplot(hills~group*Age,data=SDLP,plot=FALSE)$out
SDLP1 <- SDLP
SDLP1 <- SDLP1[-which(SDLP1$hills %in% outliers),]
boxplot(hills~group*Age,data=SDLP1)

sdlp <- aov(hills ~ Age*group, data = SDLP1,anova_table = list(es = "pes"))
summary(sdlp)
eta_squared(sdlp)
emmeans(sdlp,pairwise ~ group )
emmeans(sdlp,pairwise ~ Age|group)
emmeans(sdlp,pairwise ~ group|Age)
emmip(sdlp, ~ group)
emmip(sdlp, Age ~ group)
summary(sdlp)
test_levene(sdlp)
test_sphericity(sdlp)




###Mean Long ACC
MLO <- total %>%
  filter(Variable == "Mean Long acc")
mlo <- aov(hills ~ Age*group, data = MLO,anova_table = list(es = "pes"))
summary(mlo)
eta_squared(mlo)
emmeans(mlo,pairwise ~ Age)
emmeans(mlo,pairwise ~ scenario)
emmip(mlo, ~ group)
emmip(mlo, Age ~ group)
emmip(mlo, ~ scenario)
summary(mlo)
test_levene(mlo)
test_sphericity(mlo)

boxplot(hills~group*Age,data=MLO)
outliers <- boxplot(hills~group*Age,data=MLO,plot=FALSE)$out
MLO1 <- MLO
MLO1 <- MLO1[-which(MLO1$hills %in% outliers),]
boxplot(hills~group*Age,data=MLO1)

MLO <- total %>%
  filter(Variable == "Mean Long acc")
mlo <- aov(hills ~ Age*group, data = MLO1,anova_table = list(es = "pes"))
summary(mlo)
eta_squared(mlo)
emmeans(mlo,pairwise ~ Age)
emmeans(mlo,pairwise ~ Age|group)
emmeans(mlo,pairwise ~ group|Age)
emmip(mlo, ~ group)
emmip(mlo, Age ~ group)
emmip(mlo, ~ scenario)
summary(mlo)
test_levene(mlo)
test_sphericity(mlo)





###SD Long ACC
SLO <- total %>%
  filter(Variable == "SD Long acc")
slo <- aov(hills ~ Age*group, data = SLO,anova_table = list(es = "pes"))
summary(slo)
eta_squared(slo)
emmeans(slo,pairwise ~ Age|group)
emmeans(slo,pairwise ~ Age)
emmip(slo, ~ group)
emmip(slo, Age ~ group)
emmip(slo, ~ scenario)
summary(slo)
test_levene(slo)
test_sphericity(slo)
anova_test(slo)

boxplot(hills~group*Age,data=SLO)
outliers <- boxplot(hills~group*Age,data=SLO,plot=FALSE)$out
SLO1 <- SLO
SLO1 <- SLO1[-which(SLO1$hills %in% outliers),]
boxplot(hills~group*Age,data=SLO1)

boxplot(hills~group*Age,data=SLO1)
outliers <- boxplot(hills~group*Age,data=SLO1,plot=FALSE)$out
SLO2 <- SLO1
SLO2 <- SLO2[-which(SLO2$hills %in% outliers),]
boxplot(hills~group*Age,data=SLO2)

slo <- aov(hills ~ Age*group, data = SLO2,anova_table = list(es = "pes"))
summary(slo)
eta_squared(slo)
emmeans(slo,pairwise ~ Age|group)
emmeans(slo,pairwise ~ group|Age)
emmeans(slo,pairwise ~ group)
emmeans(slo,pairwise ~ Age)
emmip(slo, ~ group)
emmip(slo, Age ~ group)
emmip(slo, ~ scenario)
summary(slo)
test_levene(slo)
test_sphericity(slo)
anova_test(slo)

leveneTest(slo)
TukeyHSD(ss)

shapiro_test(MLO2,turn)

SDoa <- Mspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Older Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Fixed Base")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Turn Table")
shapiro_test(SDoa$turn1)
SDoa <- Mspeed2 %>%
  filter(Age == "Younger Adults")%>%
  filter(group == "Full Motion")
shapiro_test(SDoa$turn1)

oneway.test(hills ~ Age,data=SLO, var.equal = FALSE)
oneway.test(hills ~ group,data=SLO1, var.equal = FALSE)
kruskal.test(winding ~ group, LD1)

pairwise.wilcox.test(SLO1$hills, SLO1$group,
                     p.adjust.method = "BH")

TukeyHSD(ms)

SR1 %>%
  group_by(Age)%>%
  dunn_test(winding~group)

test_levene(ms)
test_sphericity(ms)

p <- ggplot(SLO2, aes(x=group, y=hills, fill=Age),colour="black") + 
  facet_wrap(~Age)+
  geom_violin()+
  geom_jitter(color="black", size=1, alpha=0.95)+
  theme(legend.position = "none")+
  xlab("Motion Group") + ylab("SD Longitudinal Acceleration"~Km/h^2)+
  theme(text=element_text(family="Arial", face="bold", size=10))+
  scale_fill_grey(start = 0.50, end = 0.90)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  stat_summary(fun.data=mean_se, geom="pointrange", 
               size=.4, colour="red", 
               position=position_dodge(width=.8), show.legend = FALSE)
p

SLO3<- SLO2 %>%
  select(one_of(c("ID", "Age", "group", "hills")))%>%
  group_by(group)%>%
  filter(ID != '26')%>%
  #filter(ID != '15')%>%
  #filter(ID != '72')%>%
  #filter(ID != '26')%>%
  #filter(ID != '74')%>%
  summarise(
    n=n(),
    mean=mean(hills),
    sd=sd(hills)
  )%>%
  mutate(se=sd/sqrt(n-1))




















