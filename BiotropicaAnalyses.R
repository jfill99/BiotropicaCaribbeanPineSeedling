### clear everything ###
rm(list=ls(all=TRUE)) 
cat("\014")

require(reshape2)
require(dplyr)
require(ggplot2)
require(lubridate)
library(lme4)
library(ResourceSelection)
library(gridExtra)
library(piecewiseSEM)
library(ggpubr)


#read in data
data_int1<-read.csv("Plotdata_Biotropica.csv",header=TRUE)


###########
#SURVIVAL##
###########

table(data_int1$fire,data_int1$surv)

data_int1_for<-data_int1[data_int1$Environment=="DenselyWooded",]
data_int1_sav<-data_int1[data_int1$Environment=="Savanna",]
data_int1_shr<-data_int1[data_int1$Environment=="Shrub",]

table(data_int1_for$fire,data_int1_for$surv)
table(data_int1_sav$fire,data_int1_sav$surv)
table(data_int1_shr$fire,data_int1_shr$surv)


#Test associations between predictors
#Height
kruskal.test(data_int1$height_start, data_int1$Environment)
kruskal.test(data_int1$height_start, data_int1$fire)


#Separate environments
kruskal.test(data_int1_for$fire, data_int1_for$height_start)
kruskal.test(data_int1_sav$fire, data_int1_sav$height_start)
kruskal.test(data_int1_shr$fire, data_int1_shr$height_start)



#Models of survival by fire and height where possible
mylogit_for <- glm(surv ~ fire, data = data_int1_for, family = "binomial")
summary(mylogit_for)
mylogit_for_rand <- glmer(surv ~ fire+(1|Plot), data = data_int1_for, family = "binomial")
summary(mylogit_for_rand)

mylogit_sav <- glm(surv ~ fire*height_start, data = data_int1_sav, family = "binomial")
summary(mylogit_sav)
mylogit_sav_rand <- glmer(surv ~ fire*height_start+(1|Plot), data = data_int1_sav, family = "binomial")
summary(mylogit_sav_rand)#overfitted

mylogit_shr <- glm(surv ~ fire, data = data_int1_shr, family = "binomial")
summary(mylogit_shr)
mylogit_shr_rand <- glmer(surv ~ fire+(1|Plot), data = data_int1_shr, family = "binomial")
summary(mylogit_shr_rand)#overfitted



##Separate densely wooded and shrubby environments AND fire/nofire
data_int1_for_fire<-data_int1_for[data_int1_for$fire==1,]
data_int1_for_nofire<-data_int1_for[data_int1_for$fire==0,]
data_int1_shr_fire<-data_int1_shr[data_int1_shr$fire==1,]
data_int1_shr_nofire<-data_int1_shr[data_int1_shr$fire==0,]


mylogit_for_fire <- glm(surv ~ height_start, data = data_int1_for_fire, family = "binomial")
summary(mylogit_for_fire)#overfitted if include plot
mylogit_for_nofire <- glm(surv ~ height_start, data = data_int1_for_nofire, family = "binomial")
summary(mylogit_for_nofire)#overfitted if include plot

mylogit_shr_fire <- glmer(surv ~ height_start+(1|Plot), data = data_int1_shr_fire, family = "binomial")
summary(mylogit_shr_fire)
mylogit_shr_nofire <- glm(surv ~ height_start, data = data_int1_shr_nofire, family = "binomial")
summary(mylogit_shr_nofire)#overfitted if include plot


modelList <- psem(
  mylogit_shr_fire
)
summary(modelList)

modelList <- psem(
  mylogit_for_rand
)
summary(modelList)



#########################
##  FIGURES FOR PAPER ###
#######################


##densely wooded##

no_na_for<-data_int1_for[!is.na(data_int1_for$height_start),]
vline_for <- summarise(group_by(no_na_for,fire), median = median(height_start))

F2<-ggplot(data_int1_for, aes(x=height_start))+xlim(c(0,400))+ylim(c(0,0.07))+
  stat_density(aes(color=fire),size=1, position="identity",geom="line")+
  labs(title = "Densely wooded", x = "Height (cm)", y = "")+
  guides(color=guide_legend(title=""))+
  scale_color_manual(labels=c("No Fire","Fire"),values=c("#00AFBB","#E7B800"))+
  theme_classic()+
  theme(axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y =element_blank(),
        axis.text = element_text(size = 16),
        legend.title=element_text(size=16, face = "bold"),
        legend.text = element_text(size = 16, face="bold"),
        legend.position = c(0.8, 0.8),
        plot.title=element_text(size=20, face="bold"))+
  geom_vline(data = vline_for, aes(xintercept = median,color = as.factor(fire)), size=c(1.2,1.2),linetype="dashed")


##savanna###

vline_sav <- summarise(group_by(data_int1_sav,fire), median = median(height_start))

S2<-ggplot(data_int1_sav, aes(x=height_start))+xlim(c(0,400))+ylim(c(0,0.07))+
  stat_density(aes(color=fire),size=1, position="identity",geom="line")+
  labs(title = "Open", x = "Height (cm)", y = "Density")+
  guides(color=guide_legend(title="Fire Occurrence"))+
  scale_color_manual(labels=c("No Fire","Fire"),values=c("#00AFBB","#E7B800"))+
  theme_classic()+
  theme(axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),
        legend.position = "none",
        plot.title=element_text(size=20, face="bold"))+
  geom_vline(data = vline_sav, aes(xintercept = median,color = as.factor(fire)), size=c(1.2,1.2),linetype="dashed")



##shrub###

no_na_shr<-data_int1_shr[!is.na(data_int1_shr$height_start),]
vline_shr <- summarise(group_by(no_na_shr,fire), median = median(height_start))


SH2<-ggplot(data_int1_shr, aes(x=height_start))+xlim(c(0,400))+ylim(c(0,0.07))+
  stat_density(aes(color=fire),size=1, position="identity",geom="line")+
  labs( title = "Shrubby", x = "Height (cm)", y = "")+
  guides(color=guide_legend(title="Fire Occurrence"))+
  scale_color_manual(labels=c("No Fire","Fire"),values=c("#00AFBB","#E7B800"))+
  theme_classic()+
  theme(axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text = element_text(size = 16),
        legend.position = "none",
        plot.title=element_text(size=20, face="bold"))+
  geom_vline(data = vline_shr, aes(xintercept = median,color = as.factor(fire)), size=c(1.2,1.2),linetype="dashed")


grid.arrange(S2,SH2,F2, nrow = 1)




###############
###char#####
##############


char<-read.csv("Char_Biotropica.csv", header=TRUE)
char$Census<-as.numeric(as.character(char$Char_height))
char_1<-char[!is.na(char$Char_height),]


#mean, se
char_census<-char_1%>%group_by(Environment) %>% summarize(avg = mean(Char_height), n = n(), 
                             sd = sd(Char_height), se = sd/sqrt(n))


#Separate environments
char_savanna<-char_1[char_1$Environment=="Open",c("Char_height","Environment")]
char_shrub<-char_1[char_1$Environment=="Shrubby",c("Char_height","Environment")]
char_forest<-char_1[char_1$Environment=="Densely wooded",c("Char_height","Environment")]

#Convert char heights to cm and format dataframe
char_savanna$Char_height1<-char_savanna$Char_height*100
char_shrub$Char_height1<-char_shrub$Char_height*100
char_forest$Char_height1<-char_forest$Char_height*100
char_savanna<-char_savanna[,-1]
char_shrub<-char_shrub[,-1]
char_forest<-char_forest[,-1]


#subset height distribution datasets
sav_boxplot<-data_int1_sav[,c("fire","height_start")]
sh_boxplot<-data_int1_shr[,c("fire","height_start")]
for_boxplot<-data_int1_for[,c("fire","height_start")]


#Combine char and height datasets
sav_boxplot<-rename(sav_boxplot, Char_height1="height_start")
sav_boxplot<-rename(sav_boxplot, Environment="fire")
sh_boxplot<-rename(sh_boxplot, Char_height1="height_start")
sh_boxplot<-rename(sh_boxplot, Environment="fire")
for_boxplot<-rename(for_boxplot, Char_height1="height_start")
for_boxplot<-rename(for_boxplot, Environment="fire")

sav_char<-rbind(char_savanna,sav_boxplot)
shr_char<-rbind(char_shrub,sh_boxplot)
for_char<-rbind(char_forest,for_boxplot)


#code column for x axis labels
for_char$Environment <- ifelse(for_char$Environment == "Densely wooded", "Char", 
                               ifelse(for_char$Environment=="0","No fire","Juveniles"))
sav_char$Environment <- ifelse(sav_char$Environment == "Open", "Char", 
                               ifelse(sav_char$Environment=="0","No fire","Juveniles"))
shr_char$Environment <- ifelse(shr_char$Environment == "Shrubby", "Char", 
                               ifelse(shr_char$Environment=="0","No fire","Juveniles"))

#subset just fire
sav_char_final<-sav_char[sav_char$Environment!="No fire",]
shr_char_final<-shr_char[shr_char$Environment!="No fire",]
for_char_final<-for_char[for_char$Environment!="No fire",]



wooded<-ggboxplot(for_char_final, x = "Environment", y = "Char_height1", 
          ylab = " ", xlab = " ", legend="blank", 
          title="Densely wooded",font="bold",outlier.shape=1,outlier.size=4,
          fill = "Environment",palette= c("#6A6E6D","#7ba151"), alpha=0.2)+ 
  theme(text=element_text(size=24))+
  geom_jitter(aes(color = ifelse(Environment=="Char", "#6A6E6D","#7ba151")), width=0.2)+
  theme(plot.title = element_text(face = "bold"))

savanna<-ggboxplot(sav_char_final, x = "Environment", y = "Char_height1", 
          ylab = "Height (cm)", xlab = " ", legend="blank",
          title="Open", ylim=c(0,600),outlier.shape=1,outlier.size=4,
          fill = "Environment",palette= c("#6A6E6D","#7ba151"), alpha=0.2)+
  theme(text=element_text(size=24))+
  theme(axis.title.y=element_text(size=28))+
  geom_jitter(aes(color = ifelse(Environment=="Char", "#6A6E6D","#7ba151")), width=0.2)+
  theme(plot.title = element_text(face = "bold"))

shrub<-ggboxplot(shr_char_final, x = "Environment", y = "Char_height1", 
                   ylab = " ", xlab = " ", legend="blank", 
                 title="Shrubby", ylim=c(0,600),outlier.shape=1,outlier.size=4,
                 fill = "Environment",palette= c("#6A6E6D","#7ba151"), alpha=0.2)+
  theme(text=element_text(size=24))+
  geom_jitter(aes(color = ifelse(Environment=="Char", "#6A6E6D","#7ba151")), width=0.2)+
  theme(plot.title = element_text(face = "bold"))


grid.arrange(savanna,shrub,wooded,nrow = 1)

