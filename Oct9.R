## CODE USED TO CREATE DENSITY PLOTS ##

GCMmodel <- "bcc-csm1-1-m"
Time <- "TF3" #NotTF3
County <- "WAd"
County1 <- "CAd"
Time1 <- "TF1"
chart_title <-  sprintf("Analogs of %s with its closest analog %s",County,County1)


SubData <- subset(RCP_DP_Sesaon, RCP_DP_Sesaon$Location == County 
            & RCP_DP_Sesaon$Model == GCMmodel & !RCP_DP_Sesaon$TF == Time)
SubDataA <- SubData[which(SubData$Location == County & SubData$TF == "TF1"),]
SubDataB <- SubData[which(SubData$Location == County & SubData$TF == "TF2"),]
SubDataA$ID <- "H1" #H1

SubDataB$ID <- "F1" #F1

SubData1 <- subset(RCP_DP_Sesaon, RCP_DP_Sesaon$Location == County1 
                  & RCP_DP_Sesaon$Model == GCMmodel & RCP_DP_Sesaon$TF == Time1)
SubData1$ID <- "H2" #H2

SubMerge <- rbind(SubDataA,SubDataB,SubData1)
SubMerge <- SubMerge[-c(2,3)]

tSubMerge <- melt(SubMerge)

# library(tidyr)
# tSubMerge <- SubMerge %>% pivot_longer(cols = c(2:18),names_to = "variable", values_to = "value")


jpeg("rplot.jpg", width = 1500, height = 1000)
plot <- ggplot(tSubMerge, aes(x= value, fill=ID)) + geom_density(alpha=.3)
plot <- plot + theme(panel.grid.major = element_line(colour = "black"))+labs(title = chart_title,
                                                                             subtitle = "Analog",
                                                                             caption = "Data source: ",
                                                                             x = "Values", y = "Density"
)
plot + facet_wrap(~ tSubMerge$variable, scales = "free") 
dev.off()




###### CREATING DATA FOR DENSITY PLOTS ###
RCP_DP <- Raw_RCP8.5 %>% 
  group_by(yyyy,Location,Season,TF,Model) %>% 
  summarise(Pr = sum(pr.mm.),
            Tmin=mean(tasmin.K.),
            Tmax=mean(tasmax.K.),
            GDD=sum(GDD_Pot))
RCP_DP$Season= as.factor(RCP_DP$Season)
Listseason= unique(RCP_DP$Season)
for(i in 1:length(Listseason)){
  SeasonWiseSample=RCP_DP[RCP_DP$Season%in%Listseason[i],]
  name= Listseason[i]
  write.csv(SeasonWiseSample, file = paste0(name,"_sample_DP.csv"))
}
RCP_DP_Sesaon <- cbind(Fall_sample_DP,Spring_sample_DP,Summer_sample_DP,Winter_sample_DP)
RCP_DP_Sesaon <- RCP_DP_Sesaon[-c(1,2,10:14,19:23,28:32)]
