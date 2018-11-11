#####DESCRIPTIVE ANALYSIS######

# First we set the path 
setwd("c:/Users/ce148hv/Desktop/CarlosIII/Primer_bimestre/Statistics")

#Now we import the libraries we are gonna use
library(ggplot2)
library(dplyr) #For data manipulation
library(plotrix) #For plots in 3D
library(tidyverse) #For data analytics in general

# We read the data file as a data.frame:
Data_project = read.csv("CleanData_CORRECT_NAMES.csv",sep = ";", dec = ",")
attach(Data_project)

#Now we can print the name of the columns and also see their class.
names(Data_project)
sapply(Data_project,class)


#Histogram of months living abroad
month_num <- as.integer(as.character(MonthsAbroad))
table(month_num)

ggplot(Data_project, aes(x=month_num)) +
  geom_histogram(binwidth=5,fill="darkblue",colour="white")+
  labs(title="Months living abroad", x = "Months", y="Number of people")+
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 18, color="black", face="bold"), axis.title.y = element_text(size = 18, color="black", face="bold"),
        plot.title = element_text(size = 20, face = "bold", color = "black", hjust=0.5))

#Pie plot in 3D for people abroad vs people never abroad
n_abroad<- nrow(filter(Data_project, MonthsAbroad!="0"))
n_home<- nrow(filter(Data_project, MonthsAbroad=="0"))

df4 <- data.frame(
  group = c("Living abroad", "Never"),
  value = c(n_abroad, n_home)
)

library(plotrix)
slices <- c(n_abroad, n_home) 
lbls <- c("Living abroad:", "Never:")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct, "%", sep=" ") # add percents to labels 
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart")

#Boxplot of the time streaming music
ggplot(Data_project, aes(y=MinutesStreamMusic)) + 
  stat_boxplot(geom="errorbar", width=0.2)+
  geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=2, fill="white", colour="black")+
  labs(title="Time streaming music", y = "Minutes")+
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 18, color="darkgreen", face="bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen", hjust=0.5))

#Boxplot of the time streaming video
ggplot(Data_project, aes(y=MinutesStreamVideo)) + 
  stat_boxplot(geom="errorbar", width=0.2)+
  geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=2, fill="white", colour="black")+
  labs(title="Time streaming video", y = "Minutes")+
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 18, color="darkgreen", face="bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen", hjust=0.5))


#Boxplot of the age
ggplot(Data_project, aes(y=Age))+ 
  stat_boxplot(geom="errorbar", width=0.2)+
  geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=2, fill="white", colour="black")+
  labs(title="Age of our sample", y = "Age")+
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 18, color="darkgreen", face="bold"),
        plot.title = element_text(size = 20, face = "bold", color = "darkgreen", hjust=0.5))


#Pie chart for nationality
n_Spain<- nrow(filter(Data_project, Country=="Spain"))
n_USA<- nrow(filter(Data_project, Country=="United States of America"))
n_italy<- nrow(filter(Data_project, Country=="Italy"))
n_other <-nrow(filter(Data_project, Country!="Spain" & Country!="Italy" & Country!="United States of America"))

prop.table(table(Country))*100

df <- data.frame(
  Group = c("Spain", "USA","Italy","Other"),
  value = c(n_Spain, n_USA, n_italy,n_other)
) %>% mutate(Group = fct_reorder(Group, value, .desc = TRUE))


#Pie chart for gender
n_males<- nrow(filter(Data_project, Gender=="Male"))
n_females<- nrow(filter(Data_project, Gender=="Female"))
df <- data.frame(
  group = c("Male", "Female"),
  value = c(n_males, n_females)
)
head(df)

ggplot(df, aes(x="", y=value, fill=group))+
scale_fill_manual(values = c("red", "seagreen3"))+
geom_bar(width = 1, stat = "identity")+
coord_polar("y")+
labs(title="Gender pie chart", x="", y="")+
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]),label = paste0(group, " : ", round(value / sum(value) * 100, 1), "%")))+
  theme(plot.title=element_text(size = 20, face = "bold", color = "darkblue", hjust=0.5))


#Bar plot for kind of high school
n_private<- nrow(filter(Data_project, HighSchool=="Private"))
n_public<- nrow(filter(Data_project, HighSchool=="Public School"))
n_partially_funded <-nrow(filter(Data_project, HighSchool=="Partially State-funded School"))

df2 <- data.frame(
  group = c("Private", "Public","Partially State-funded"),
  len = c(n_private, n_public,n_partially_funded)
)  
df3<- df2 %>%  mutate(group = fct_reorder(group, len, .asc = TRUE))

ggplot(df3, aes(x=group, y=len)) +
geom_bar(stat="identity", fill="steelblue")+
labs(title="High school attended", x = " ", y=" ")+
geom_text(aes(label=len), vjust=-0.3, size=3.5)+
theme( plot.title = element_text(size = 20, face = "bold", color = "darkblue", hjust=0.5))

#Pie plot of music streaming service
n_spotify<- nrow(filter(Data_project, MusicStreamingService=="Spotify"))
n_apple<- nrow(filter(Data_project, MusicStreamingService=="Apple Music"))
n_tidal <-nrow(filter(Data_project, MusicStreamingService=="Tidal"))
n_none<-nrow(filter(Data_project, MusicStreamingService=="None of them"))

df5 <- data.frame(
  Group = c("Spotify", "Apple Music","Tidal","None of them"),
  value = c(n_spotify, n_apple, n_tidal, n_none)
) %>% mutate(Group = fct_reorder(Group, value, .desc = TRUE))

ggplot(df5, aes(x="", y=value, fill=Group))+
  scale_fill_manual(values = c("Spotify"="blue","Apple Music"="seagreen3","Tidal"="yellow","None of them"="red"))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y")+
  labs(title="Music pie chart", x="", y="")+
  theme(axis.text.x=element_blank())+
  theme(plot.title=element_text(size = 20, face = "bold", color = "darkblue", hjust=0.5))

#Pie plof ot video streaming service
n_netflix<- nrow(filter(Data_project, VideoStreamingService=="Netflix"))
n_youtube<- nrow(filter(Data_project, VideoStreamingService=="YouTube"))
n_hbo <-nrow(filter(Data_project, VideoStreamingService=="HBO"))
n_amazon<-nrow(filter(Data_project, VideoStreamingService=="Amazon Prime"))
n_none<-nrow(filter(Data_project, VideoStreamingService=="None of them"))

df6 <- data.frame(
  Group = c("Netflix", "YouTube","HBO","Amazon Prime","None of them"),
  value = c(n_netflix, n_youtube, n_hbo, n_amazon, n_none)
) %>% mutate(Group = fct_reorder(Group, value, .desc = TRUE))


ggplot(df6, aes(x="", y=value, fill=Group))+
  scale_fill_manual(values = c("Netflix"="red","HBO"="blue","Amazon Prime"="yellow","YouTube"="green","None of them"="orange"))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0, direction=-1)+
  labs(title="Video pie chart", x="", y="")+
  theme_void()+
  theme(axis.text.x=element_blank())+
  theme(plot.title=element_text(size = 20, face = "bold", color = "darkblue", hjust=0.5))


#Bar plot of payment music
n_yes<- nrow(filter(Data_project,MusicStreamingService!="None of them" & PayMusic=="Yes"))
n_no<- nrow(filter(Data_project,MusicStreamingService!="None of them" & PayMusic=="No"))

df2 <- data.frame(
  group = c("Yes", "No"),
  len = c(n_yes, n_no)
)  
df3<- df2 %>%  mutate(group = fct_reorder(group, len, .asc = TRUE))

ggplot(df3, aes(x=group, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(title="Pay music services", x = " ", y=" ")+
  geom_text(aes(label=len), vjust=-0.3, size=3.5)+
  theme( plot.title = element_text(size = 20, face = "bold", color = "darkblue", hjust=0.5))


#Bar plot of payment video
n_yes<- nrow(filter(Data_project, VideoStreamingService!="None of them" & PayVideo=="Yes"))
n_no<- nrow(filter(Data_project, VideoStreamingService!= "None of them" & PayVideo=="No"))

df2 <- data.frame(
  group = c("Yes", "No"),
  len = c(n_yes, n_no)
)  

df3<- df2 %>%  mutate(group = fct_reorder(group, len, .desc  = TRUE))

ggplot(df3, aes(x=group, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  labs(title="Pay video services", x = " ", y=" ")+
  geom_text(aes(label=len), vjust=-0.3, size=3.5)+
  theme( plot.title = element_text(size = 20, face = "bold", color = "darkblue", hjust=0.5))