---
date: "2019-12-04T11:36:33+08:00"
draft: false
featuredImg: ""
tags:
- R
- datascience
- exploratory_data_analysis
title: Exploratory Data Analysis
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

```{r global_options, include=FALSE}
library(knitr)
opts_chunk$set(fig.align="center", fig.height=5, message=FALSE, warning=FALSE, fig.width=8, tidy=TRUE)
```
## Data Wrangling and Data Exploration
Andrew Lee ml45932
```{R}
##0 Introduction
getwd()
performance<-read.csv("exam.csv",TRUE,",")
medicalcost<-read.csv("insurance.csv",TRUE,",")
```
The first dataset is about student performance in three different(subjects) exams along with parental education level and completion of test preparation. The second dataset is about individual's medical cost charged by healthcare insurance companies. As I'm interested in healthcare and medicine, I'd like to know if there is any interesting relationship between student's performance and the cost of insurance with consideration of factors like economic, personal, and social.
One of my assumptions is that since people with free/lunch plan, low parental education, race, and no exam preparation course might indicate their socioeconomic status, they might not be able to maintain health lifestyle, thereby resulting in higher medical cost charged by insurance companies. 
```{R}
##1 Tidying: Rearranging Wide/Long
library(tidyverse)
performance <- performance%>%na.omit()
medicalcost <- medicalcost%>%na.omit() 


performance1 <- performance%>%pivot_longer(c(6:8))
performance2 <- performance1%>%pivot_wider(names_from = "name",values_from = "value")


medicalcost1<-medicalcost%>%select(sex,age,bmi,children,smoker,region,charges)
medicalcost2<-medicalcost1%>%pivot_longer(c(2:4))  
medicalcost3 <- medicalcost2%>%
  pivot_wider(names_from="name",values_from="value")


```
Since the data were already tidy, they were untidied and retidied by using pivot_longer and pivot_wider functions.
```{R}
##2 Joining/Merging
fulldata <- full_join(performance,medicalcost,by=c("gender"="sex"))

```
I used a full_join function as I did not want to drop any factors that may influence student performance as well as medical cost charged. 
```{R}
##3 Wrangling
fulldata1 <- fulldata%>%select(-region,) %>% mutate(race=race.ethnicity) %>%
  select(-race.ethnicity,) %>% arrange(desc(age))
#I took out region column as it contains only four across the U.S and becomes unnecessary facotr of my interest

fulldata1%>%group_by(gender) %>% summarize(mean_charges=mean(charges),mean_math=mean(math.score), mean_read=mean(reading.score), mean_write=mean(writing.score))
#Difference in exam scores and medical charges between two genders (1)

fulldata1%>%group_by(gender)%>%filter(age>mean(age,na.rm=TRUE))%>%
  summarize(mean_charges=mean(charges))
fulldata1%>%group_by(gender)%>%filter(age<mean(age,na.rm=TRUE))%>%
  summarize(mean_charges=mean(charges))
#Age could be a good indictor that might affect medical charges (2)

fulldata1%>%group_by(gender)%>%filter(smoker=="yes")%>%
  summarize(mean_charges=mean(charges))
fulldata1%>%group_by(gender)%>%filter(smoker=="no")%>%
  summarize(mean_charges=mean(charges))
#Smoking status could also be a good indicator that might affect medical charges (3)

fulldata1%>%group_by(gender)%>%filter(bmi>mean(bmi,na.rm=TRUE))%>%
  summarize(mean_charges=mean(charges))
fulldata1%>%group_by(gender)%>%filter(bmi<mean(bmi,na.rm=TRUE))%>%
  summarize(mean_charges=mean(charges))
#BMI could be a good indicator that might affect medical charges (4)

fulldata1%>%group_by(test.preparation.course,lunch)%>%
  filter(math.score>mean(math.score),reading.score>mean(reading.score),
         writing.score>mean(writing.score))%>% 
  summarize(mean_charges=mean(charges))
#economic status could affect medical charges (5)

fulldata1%>%group_by(parental.level.of.education)%>%
    summarize(mean_charges=mean(charges))%>% arrange(desc(mean_charges))
#parental level of education might affect medical charges (6)

fulldata1%>%group_by(children)%>%summarize(mean_charges=mean(charges))
#number of children might affect medical charges (7)

fulldata1%>%group_by(race) %>% summarize(mean_charges=mean(charges),mean_math=mean(math.score), mean_read=mean(reading.score), mean_write=mean(writing.score))
#Difference in exam scores and medical charges among five races (8)

fulldata1%>%group_by(race)%>%summarize(mean_bmi=mean(bmi))
#Difference in bmi among five races (9)   

fulldata1%>%group_by(smoker)%>%summarize(mean_math=mean(math.score), mean_read=mean(reading.score), mean_write=mean(writing.score))
#Difference in exam scores between smoker and non-smoker (10)

fulldata_nums <- fulldata1%>%select_if(is.numeric)%>%scale
data_pca <- prcomp(fulldata_nums)
names(data_pca)
summary(data_pca)
data_pca$sdev
eigen(cor(fulldata_nums))
#corrleation matrix of the numeric variables


```
Female group had higher reading and writing score but male group had higher medical cost and math score.For both of those who are older and yonger than the mean age, the male group had higher medical cost than the female group. For those who smoke, the male group also had higher medical cost but for non-smokers, the female group had higher medical cost. For both of those who have higher and lower BMI than the mean, the male group had higher medical cost. Medical cost was higher for the group with free/reduced lunch plan and no test preparation course but lower for the group with free/reduced plan completion of test preparation course. The higher the parental level of education is, the lower the medical cost is charged. The group with three children had the highest medical cost and the group with five kids had the lowest. All five race groups had similar BMI, though group A was the highest. Non-smoker group had higher reading and writing score than the smoker group.

```{R}
##4 Visualizing
library(ggplot2)

fulldata2 <- fulldata1%>%distinct()

fulldata2 %>% group_by(race,gender) %>% summarize(mean_char=mean(charges, na.rm=T),sd_char=sd(charges, na.rm=T),n=n(),se_char=sd_char/sqrt(n))%>%
ggplot(aes(gender,mean_char))+geom_bar(stat="identity")+
geom_errorbar(aes(y=mean_char, ymin = mean_char-se_char, 
                   ymax = mean_char+se_char))+ facet_wrap(~race)

fulldata2 %>% group_by(race,smoker) %>% summarize(mean_char=mean(charges, na.rm=T),sd_char=sd(charges, na.rm=T),n=n(),se_char=sd_char/sqrt(n))%>%
ggplot(aes(smoker,mean_char))+geom_bar(stat="identity")+
geom_errorbar(aes(y=mean_char, ymin = mean_char-se_char, 
                   ymax = mean_char+se_char))+ facet_wrap(~race)
```
Across all five races, every male group had a higher medical cost charged than the female group and smoker group had a higher medical cost charged than the non-smoker group. 
```{R}

ggplot(head(fulldata2,500), aes(math.score,charges))+
geom_point(aes(color=smoker, size=bmi))+
  scale_x_continuous(name="math exam score",breaks=c(20,40,60,80,100))+
  scale_y_continuous(name="medicalcost")


```
It is apparent that smoking status affects the medical costas well as BMI but there is no obvious relationship between excam score and medical cost. 
```{R}
##5 Dimensionality Reduction

fulldata_nums <- fulldata1%>%select_if(is.numeric)%>%scale
data_pca1 <- princomp(fulldata_nums)
summary(data_pca1,loadings=T)

eigval <- data_pca1$sdev^2
varprop=round(eigval/sum(eigval),2)

ggplot()+geom_bar(aes(y=varprop,x=1:7),stat="identity")+xlab("")+geom_path(aes(y=varprop,x=1:7))+
 geom_text(aes(x=1:7,y=varprop,label=round(varprop,2)),vjust=1,col="white",size=5)+
 scale_y_continuous(breaks=seq(0,.6,.2),labels = scales::percent)+
 scale_x_continuous(breaks=1:10)


round(cumsum(eigval)/sum(eigval),2)
eigval[1:7]

datadf<-data.frame(PC1=data_pca1$scores[,1],PC2=data_pca1$scores[,2])
ggplot(datadf,aes(PC1,PC2))+geom_point()


data_pca1$loadings[1:7,1:2]%>%as.data.frame%>%rownames_to_column%>%
ggplot()+geom_hline(aes(yintercept=0),lty=2)+
 geom_vline(aes(xintercept=0),lty=2)+ylab("PC2")+xlab("PC1")+
 geom_segment(aes(x=0,y=0,xend=Comp.1,yend=Comp.2),arrow=arrow(),col="red")+
 geom_label(aes(x=Comp.1*1.1,y=Comp.2*1.1,label=rowname))
```
PC1 and PC2 together can summarize 59% of the total variability.
Only 2 PCs have eigenvalues greater than 1 but PC3 and PC4 are close.
There seems to be a strong association of age, bmi, and the number of children with medical charges but not with exam scores. 