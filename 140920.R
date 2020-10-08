BaseTemp <- 39
Raw_RCP85$Season[Raw_RCP85$mm == '12' | Raw_RCP85$mm == '01' | Raw_RCP85$mm == '02' ] <- "Winter" 
Raw_RCP85$Season[Raw_RCP85$mm == '03' | Raw_RCP85$mm == '04' | Raw_RCP85$mm == '05'] <- "Spring" 
Raw_RCP85$Season[Raw_RCP85$mm == '06' | Raw_RCP85$mm == '07' | Raw_RCP85$mm == '08' ] <- "Summer" 
Raw_RCP85$Season[Raw_RCP85$mm == '09' | Raw_RCP85$mm == '10' | Raw_RCP85$mm == '11' ] <- "Fall" 
Raw_RCP8.5 <- Raw_RCP85[,-c(7)]



Raw_RCP8.5$tasmin.K. <- (Raw_RCP8.5$tasmin.K. - 273.15) * 9/5 + 32
Raw_RCP8.5$tasmax.K. <- (Raw_RCP8.5$tasmax.K. - 273.15) * 9/5 + 32
Raw_RCP8.5$GDD_Pot <- (Raw_RCP8.5$tasmax.K. + Raw_RCP8.5$tasmin.K.) / 2 - BaseTemp
Raw_RCP8.5$TF <- "ID"
Raw_RCP8.5 <- na.omit(Raw_RCP8.5)


Raw_RCP8.5$TF[Raw_RCP8.5$yyyy < 2021] <- "TF1" 
Raw_RCP8.5$TF[Raw_RCP8.5$yyyy > 2050] <- "TF3" 
Raw_RCP8.5$TF[Raw_RCP8.5$yyyy > 2020 & Raw_RCP8.5$yyyy < 2051 ] <- "TF2" 

 gr <- Raw_RCP8.5 %>%
  group_by(yyyy) %>%
  summarise(mean_run = mean(GDD_Pot))
 gr$TF <- "ID"
 gr$TF[gr$yyyy < 2021] <- "P" 
 gr$TF[gr$yyyy > 2050] <- "F2" 
 gr$TF[gr$yyyy > 2020 & gr$yyyy < 2051 ] <- "F1" 
library(ggplot2)

plot <- ggplot(Raw_RCP8.5, aes(x= GDD_Pot)) + geom_density(alpha=.3)
plot <- plot + theme(panel.grid.major = element_line(colour = "red"))+labs(title = "GDD Potatoes",
                                                                             subtitle = "Analog",
                                                                             caption = "Data source: ",
                                                                             x = "GDD", y = "Density"
)
plot
plot + facet_grid(Raw_RCP8.5$Location~ .)
plot + facet_wrap(Raw_RCP8.5$Location~., ncol=3)

plot1 <- ggplot(Raw_RCP8.5, aes(x= GDD_Pot)) + geom_density(alpha=.3)
plot1 <- plot + theme(panel.grid.major = element_line(colour = "red"))+labs(title = "GDD Potatoes",
                                                                             subtitle = "Analog",
                                                                             caption = "Data source: ",
                                                                             x = "GDD", y = "Density"
)
plot1
plot1 + facet_wrap(Raw_RCP8.5$Model~., ncol=3)
plot1 + facet_wrap(Raw_RCP8.5$TF~., ncol=3)


ggplot(Raw_RCP8.5, aes(x=pr.mm., y=GDD_Pot) ) +
  geom_hex(bins = 70) + scale_fill_continuous(type = "viridis") +   theme_bw()

plot2 <- ggplot(Raw_RCP8.5, aes(x=tasmax.K., y=pr.mm.) ) +
  geom_hex(bins = 70) + scale_fill_continuous(type = "viridis") +   theme_bw()
plot2 + facet_wrap(Raw_RCP8.5$TF~., ncol=3)

plot3 <- ggplot(Raw_RCP8.5, aes(x=tasmax.K., y=pr.mm.) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")
plot3 + facet_wrap(Raw_RCP8.5$Model~., ncol=3)

plot4 <- ggplot(gr, aes(x=yyyy, y=mean_run))+geom_line(color="red")+ geom_point()
plot4 + facet_wrap(gr$TF~., ncol=1)


groupB1 <- Raw_RCP8.5 %>%group_by(Location,Season,TF) %>%
  summarise(pr = sum(pr.mm.), Tmax = mean(tasmax.K.), Tmin = mean(tasmin.K.), GDD = mean(GDD_Pot))


Fall <- groupB1[which(groupB1$Season=='Fall'),]
Summer <- groupB1[which(groupB1$Season=='Summer'),]
Winter <- groupB1[which(groupB1$Season=='Winter'),]
Spring <- groupB1[which(groupB1$Season=='Spring'),]
library(ggrepel)
plot2 <- ggplot(Summer, aes(x=Tmax, y=pr) ) +
  geom_point(size=2) +  geom_label_repel(aes(label = Location),
                                         box.padding   = 0.35, 
                                         point.padding = 0.5,
                                         segment.color = 'grey50') +
  scale_fill_continuous(type = "viridis") +  geom_smooth()+ theme_bw()
plot2 + labs(title = "Summer Season") + facet_wrap(.~Summer$TF, ncol=3)


plot3 <- ggplot(Winter, aes(x=Tmax, y=pr) ) +
  geom_point(size=2) +  geom_label_repel(aes(label = Location),
                                         box.padding   = 0.35, 
                                         point.padding = 0.5,
                                         segment.color = 'grey50') +
  scale_fill_continuous(type = "viridis") +  geom_smooth()+ theme_bw()
plot3 + labs(title = "Winter Season") + facet_wrap(.~Winter$TF, ncol=3)

plot4 <- ggplot(Fall, aes(x=Tmax, y=pr) ) +
  geom_point(size=2) +  geom_label_repel(aes(label = Location),
                                         box.padding   = 0.35, 
                                         point.padding = 0.5,
                                         segment.color = 'grey50') +
  scale_fill_continuous(type = "viridis") +  geom_smooth()+ theme_bw()
plot4 + labs(title = "Fall Season") + facet_wrap(.~Fall$TF, ncol=3)

plot5 <- ggplot(Spring, aes(x=Tmax, y=pr) ) +
  geom_point(size=2) +  geom_label_repel(aes(label = Location),
                                         box.padding   = 0.35, 
                                         point.padding = 0.5,
                                         segment.color = 'grey50') +
  scale_fill_continuous(type = "viridis") +  geom_smooth()+ theme_bw()
plot5 + labs(title = "Spring") + facet_wrap(.~Spring$TF, ncol=3)


#HEATSTRESS CALCULATION
heatstressM <- (Raw_RCP8.5[Raw_RCP8.5$Model =='bcc-csm1-1-m',])
heatstress <- (heatstressM[heatstressM$tasmax.K. > 86,])

heatstresscount <- heatstress %>% group_by(yyyy,Location,Season,TF) %>% summarize(count=n())
SummerHSD <- heatstresscount[which(heatstresscount$Season=='Summer'),]
SummerHSD1 <- SummerHSD %>% group_by(TF,Location)%>% summarise(HSD = mean(count))

# ggplot(heatstresscount, aes(fill=yyyy, y=count, x=Location)) + 
#   geom_bar(position="stack", stat="identity")
# plot5 + facet_wrap(heatstresscount$Season~., ncol=1)
# 
# plot5 <- ggplot(heatstresscount[which(heatstresscount$Location=='AZ'),], aes(x=yyyy, y=count))+geom_line(color="red")+ geom_point()
# plot5 + facet_wrap(heatstresscount$Season~., ncol=5)

plot5 <- ggplot(SummerHSD, aes(x=Location, y=count))+geom_line(color="red")+ geom_point() 
plot5 + facet_wrap(SummerHSD$TF~., ncol=3)



# ggplot(SummerHSD1, aes(fill=TF, y=count, x=Location)) + 
# geom_bar(position="stack", stat="identity") +  geom_text(aes(label = (count)), size = 3, hjust = 0.5, vjust = 3, position = position_stack(vjust = 0.5)) 

p<-ggplot(data=SummerHSD1, aes(x=Location, y=HSD)) +
  geom_bar(stat="identity", fill="steelblue")+ geom_text(aes(label=HSD), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
p+facet_wrap(SummerHSD1$TF~., ncol=1)


SummerHSD_GDD <- merge(Summer,SummerHSD1,by=c("Location","TF"))

WinterHSD <- heatstresscount[which(heatstresscount$Season=='Winter'),]
WinterHSD1 <- WinterHSD %>% group_by(TF,Location)%>% summarise(WHSD = mean(count))
WinterHSD_GDD <- merge(Winter,WinterHSD1,by=c("Location","TF"))




setwd("E:/")

RawFall <- Raw_RCP8.5[sample(1:nrow(Raw_RCP8.5), 50,replace=FALSE),]
RawFall <- RawFall[which(RawFall$Season=='Fall'),]

RawSummer <- Raw_RCP8.5[sample(1:nrow(Raw_RCP8.5), 50,replace=FALSE),]
RawSummer <- RawSummer[which(RawSummer$Season=='Summer'),]


# RawSummer <- Raw_RCP8.5[which(Raw_RCP8.5$Season=='Summer'),]
# RawWinter <- Raw_RCP8.5[which(Raw_RCP8.5$Season=='Winter'),]
# RawSpring <- Raw_RCP8.5[which(Raw_RCP8.5$Season=='Spring'),]
# RawMerge <- merge(RawFall,RawSummer,by=c("Location","TF"))
# RawMerge <- merge(RawMerge,RawWinter,by=c("Location","TF"))
# RawMerge <- merge(RawMerge,RawSpring,by=c("Location","TF"))

Raw_RCP8.5$Season= as.factor(Raw_RCP8.5$Season)
Listseason= unique(Raw_RCP8.5$Season)
for(i in 1:length(Listseason)){
  SeasonWiseSample=Raw_RCP8.5[Raw_RCP8.5$Season%in%Listseason[i],]
  name= Listseason[i]
  write.csv(SeasonWiseSample, file = paste0(name,"_sample_GM.csv"))
}



GridmetYYMMDD$Season[GridmetYYMMDD$mm == '12' | GridmetYYMMDD$mm == '01' | GridmetYYMMDD$mm == '02' ] <- "Winter" 
GridmetYYMMDD$Season[GridmetYYMMDD$mm == '03' | GridmetYYMMDD$mm == '04' | GridmetYYMMDD$mm == '05'] <- "Spring" 
GridmetYYMMDD$Season[GridmetYYMMDD$mm == '06' | GridmetYYMMDD$mm == '07' | GridmetYYMMDD$mm == '08' ] <- "Summer" 
GridmetYYMMDD$Season[GridmetYYMMDD$mm == '09' | GridmetYYMMDD$mm == '10' | GridmetYYMMDD$mm == '11' ] <- "Fall" 

Winter_sample_GM$Tmax_Winter <- (Winter_sample_GM$Tmax_Winter - 273.15) * 9/5 + 32
Winter_sample_GM$Tmin_Winter <- (Winter_sample_GM$Tmin_Winter - 273.15) * 9/5 + 32
Winter_sample_GM$GDD_Winter <- (Winter_sample_GM$Tmax_Winter + Winter_sample_GM$Tmin_Winter) / 2 - BaseTemp

Winter_sample_GM <- Winter_sample_GM[,-c(1,2,7)]

#A Matrix

Fall_sample_GM <- Fall_sample_GM2 %>% 
  group_by(Loc) %>% 
  summarise(Pr_Fall = sum(Pr_Fall),
            Tmin_Fall=mean(Tmin_Fall),
            Tmax_Fall=mean(Tmax_Fall),
            GDD_Fall=mean(GDD_Fall))

Spring_sample_GM <- Spring_sample_GM2 %>% 
  group_by(Loc) %>% 
  summarise(Pr_Spring = sum(Pr_Spring),
            Tmin_Spring=mean(Tmin_Spring),
            Tmax_Spring=mean(Tmax_Spring),
            GDD_Spring=mean(GDD_Spring))

Summer_sample_GM <- Summer_sample_GM2 %>% 
  group_by(Loc) %>% 
  summarise(Pr_Summer = sum(Pr_Summer),
            Tmin_Summer=mean(Tmin_Summer),
            Tmax_Summer=mean(Tmax_Summer),
            GDD_Summer=mean(GDD_Summer))

Winter_sample_GM <- Winter_sample_GM2 %>% 
  group_by(Loc) %>% 
  summarise(Pr_Winter = sum(Pr_Winter),
            Tmin_Winter=mean(Tmin_Winter),
            Tmax_Winter=mean(Tmax_Winter),
            GDD_Winter=mean(GDD_Winter))

AllGM <- cbind(Fall_sample_GM,Spring_sample_GM,Summer_sample_GM,Winter_sample_GM)
AllGM <- AllGM[,-c(6,11,16)]

write.csv(AllGM, file = "A1.csv")

#B Matrix Selecting MOdel and Time Frame
AllModel_RCP8.5 <- AllModel_RCP8.5[-c(1,7,13:16,21:24,29:32)]
AllModel_RCP8.5_CanESM_TF2 <- AllModel_RCP8.5[which(AllModel_RCP8.5$Model=='bcc-csm1-1-m' & AllModel_RCP8.5$TF =='TF2'),]
AllModel_RCP8.5_CanESM_TF2 <- AllModel_RCP8.5_CanESM_TF2[-c(6)]


####### New PR SUM
AllModel_RCP8.5_BMatrix_V1 <- AllModel_RCP8.5_CanESM_TF2 %>% 
  group_by(Location) %>% 
  summarise(Pr_Fall = sum(Pr_Fall),Pr_Spring = sum(Pr_Spring),Pr_Summer = sum(Pr_Summer),Pr_Winter = sum(Pr_Winter),
            Tmin_Fall=mean(Tmin_Fall), Tmin_Spring=mean(Tmin_Spring),Tmin_Summer=mean(Tmin_Summer),Tmin_Winter=mean(Tmin_Winter),
            Tmax_Fall=mean(Tmax_Fall), Tmax_Spring=mean(Tmax__Spring),Tmax_Summer=mean(Tmax__Summer),Tmax_Winter=mean(Tmax_Winter),
            GDD_Fall=mean(GDD_Fall), GDD_Spring=mean(GDD_Spring),GDD_Summer=mean(GDD_Summer),GDD_Winter=mean(GDD_Winter))

#### 

AllModel_RCP8.5_BMatrix <- AllModel_RCP8.5_BMatrix[-c(5)]
write.csv(AllModel_RCP8.5_BMatrix, file = "B.csv")

#Cmatrix

####### New PR SUM
Fall_sample_GM1 <- Fall_sample_GM2 %>% 
  group_by(yyyy,Loc) %>% 
  summarise(Pr_Fall = sum(Pr_Fall),
            Tmin_Fall=mean(Tmin_Fall),
            Tmax_Fall=mean(Tmax_Fall),
            GDD_Fall=mean(GDD_Fall))

Spring_sample_GM1 <- Spring_sample_GM2 %>% 
  group_by(yyyy,Loc) %>% 
  summarise(Pr_Spring = sum(Pr_Spring),
            Tmin_Spring=mean(Tmin_Spring),
            Tmax_Spring=mean(Tmax_Spring),
            GDD_Spring=mean(GDD_Spring))

Summer_sample_GM1 <- Summer_sample_GM2 %>% 
  group_by(yyyy,Loc) %>% 
  summarise(Pr_Summer = sum(Pr_Summer),
            Tmin_Summer=mean(Tmin_Summer),
            Tmax_Summer=mean(Tmax_Summer),
            GDD_Summer=mean(GDD_Summer))

Winter_sample_GM1 <- Winter_sample_GM2 %>% 
  group_by(yyyy,Loc) %>% 
  summarise(Pr_Winter = sum(Pr_Winter),
            Tmin_Winter=mean(Tmin_Winter),
            Tmax_Winter=mean(Tmax_Winter),
            GDD_Winter=mean(GDD_Winter))

AllGM_C <- cbind(Fall_sample_GM1,Spring_sample_GM1,Summer_sample_GM1,Winter_sample_GM1)
AllGM_C <- AllGM_C[,-c(1,7,8,13,14,19,20)]
names(AllGM_C)[1] <- "Loc"
write.csv(AllGM_C, file = "C.csv")



Bmatrix <- AllModel_RCP8.5_BMatrix_V1
A <- AllGM 
B <- AllModel_RCP8.5_BMatrix_V1  
C <- AllGM_C

A <- A[,-c(1)]

