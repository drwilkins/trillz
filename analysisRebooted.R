require(pacman)
p_load(tidyverse,nlme,rsample,ggrepel)

#This is emily's branch

mountData<-read.csv("data/mount_color+ts.csv")
trialData<-read.csv("data/TrialDataRebooted2020.csv")
names(trialData)
#Select columns
d0<-trialData %>% select(Indiv,Site,Total.Fledged,WPY, EPY,Prop.WPY,CI.1.Julian,Trmt,PB.ID,B.Mount,H.Mount,LatVoc,LatSing,LatMov,PB_Norm_Tmp,PB_Fast_Tmp,Intrnsc.N,Intrnsc.Pulses,Intrnsc.RL,Intrnsc.CR.FB,Intrnsc.Tempo, Intrnsc.Rep,CapDate,Mass,RWL,maxTS,as.vector(sapply(c("T_","R_","B_","V_"),function(x) paste0(x,c("Avg.Brightness","Hue","Chrom")),USE.NAMES = F)) )

#streamline d0 even more...just breast color, etc
d <- d0 %>% select(-as.vector(sapply(c("T_","B_","V_"),function(x) paste0(x,c("Avg.Brightness","Hue","Chrom")))))

#collapse PB_Norm_tmp and PB_Fast_Tmp into 1 relevant column
d$PB_Tempo<-as.vector(unlist(sapply(1:nrow(d),function(x) {
  if(d$Trmt[x]=="F"){d$PB_Fast_Tmp[x]}else{
    if(d$Trmt[x]=="N"){d$PB_Norm_Tmp[x]}else{NA}  }
  },USE.NAMES = F)))

#Add response variable latency to vocalize or move
d$LatVoc_or_Mov<- as.vector(unlist(sapply(1:nrow(d),FUN=function(i) min(d$LatVoc[i],d$LatMov[i],na.rm=T))))
d$LatVoc_or_Mov[which(d$LatVoc_or_Mov==Inf)]<-NA


#Add relative mount measurements to appropriate rows
d$mount_R_Bri <- mountData$avg.bright[d$B.Mount]
d$mount_R_Chrom <- mountData$chrom[d$B.Mount]
d$rel_R_Chrom<-d$R_Chrom-d$mount_R_Chrom
d$mount_maxTS<- mountData$maxTS[d$B.Mount]
d$rel_maxTS<- d$maxTS-d$mount_maxTS
d$rel_Tempo<-d$Intrnsc.Tempo-d$PB_Tempo

## Make a nonduplicated copy of the data frame
d_nondup<-d %>% subset(!duplicated(Indiv))
length(unique(d_nondup$Indiv))==length(d_nondup$Indiv) #test

## Calculate delta_LatVoc across F-N
d_nondup$delta_LatVoc<-sapply(unique(d$Indiv),function(ID){
  df<-subset(d,Indiv==ID)
  F_Row<-which(df$Trmt=="F")
  N_Row<-which(df$Trmt=="N")
  delta_LatVoc<-df$LatVoc[N_Row]-df$LatVoc[F_Row]
  delta_LatVoc
    })



#################
# OK, overall, what happened it trials?

# Latency to vocalize is the most informative response metric we calculated. It varies a lot across trials
subset(d,Trmt!="H")  %>% ggplot(aes(x=Trmt,y=LatVoc,group=Indiv,col=Indiv))+geom_point()+geom_line()


d_nondup %>% ggplot(aes(x=delta_LatVoc,y=Total.Fledged))+geom_point()

#GRAPHS

subset(d,Trmt=="F") %>% ggplot(aes(x=rel_Tempo,LatVoc))+geom_point()
subset(d,Trmt=="F") %>% ggplot(aes(x=rel_R_Chrom,LatMov))+geom_point()
subset(d,Trmt!="H")  %>% ggplot(aes(x=rel_R_Chrom,LatVoc_or_Mov))+geom_point()+facet_grid(~Trmt)

subset(d,Trmt!="H")  %>% ggplot(aes(x=rel_Tempo,LatVoc))+geom_point()+facet_grid(~Trmt)+geom_label_repel(aes(label=Indiv))

subset(d,Trmt!="H")  %>% ggplot(aes(x=Trmt,y=LatVoc))+geom_boxplot()+geom_point()

## Preliminary Analysis
#No effect of treatment on the outset
lme(LatVoc~Trmt, random=~1|Indiv,data=subset(d,Trmt!="H"),na.action="na.omit") %>% summary()


# hmm...interesting, intuitive, significant finding <cross fingers>
lme(LatVoc~Trmt+rel_Tempo, random=~1|Indiv,data=subset(d,Trmt!="H"),na.action="na.omit") %>% summary()

#Q0: Is this driven by nonresponders????
d0 %>% subset(LatVoc==120) %>% group_by(Trmt) %>% summarize(length(LatVoc))

#Probs oughta remove 2600-27221 that didn't vocalize in either trial
d2<-d %>% subset(Indiv!="2600-27221")
lme(LatVoc~Trmt+rel_Tempo, random=~1|Indiv,data=subset(d2,Trmt!="H"),na.action="na.omit") %>% summary()


subset(d2,Trmt!="H")  %>% ggplot(aes(x=Intrnsc.Tempo,LatVoc))+geom_point()+facet_grid(~Trmt)+geom_smooth(method="loess")#+geom_label_repel(aes(label=Indiv))+ylim(0,80)



###
#Q1: Did any male sing faster than the fast trill treatment?
head(d)
d %>% subset(Trmt=="F") %>% ggplot()+geom_histogram(aes(x=rel_Tempo))

###
#Q2: Since 3 males did sing faster than the fast treatment, if you remove them, does this change your result?
d_slowSingers <- d %>% subset(rel_Tempo<0)
lme(LatVoc~Trmt+rel_Tempo, random=~1|Indiv,data=subset(d_slowSingers ,Trmt!="H"),na.action="na.omit") %>% summary()



#trying to visualize this better
d$residLatVoc<-resid(lm(LatVoc~rel_Tempo,data=d))

subset(d,Trmt!="H")  %>% ggplot(aes(x=Trmt,y=residLatVoc))+geom_boxplot()





lme(LatVoc~Trmt*rel_Tempo, random=~1|Indiv,data=subset(d,Trmt!="H"),na.action="na.omit") %>% summary()


