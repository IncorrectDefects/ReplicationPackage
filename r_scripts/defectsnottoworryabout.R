#Set working directory
#setwd("C:/Users/garet/OneDrive - Lancaster University/PhD/PaperStats/gaz/")
setwd("c:/Users/comqdhb/OneDrive - Lancaster University/gaz/")
data <- read.table("patchstats.csv", header=TRUE, stringsAsFactors=TRUE, sep=",", 
                   na.strings="NA", dec=".",strip.white=TRUE)
vnames<-c("Files","Classes","Methods",  "Added","Lines" 
         ,"Removed","Modified","Chunks", "Failingtests",      "RelevantTestCount"
         ,"StatementCoverage" ,"RepairActions",     "RepairPatterns" ,   "correct")
iv<-vnames[1:length(vnames)-1]
colnames(data)<-vnames
#need to remove a single outlier
data<-data[data$Files<7,]
library(Hmisc)
library(DescTools)
library(xtable)
library(GGally)
library(MASS)
library(tidyverse)
library(caret)

#perform the Mann Whitney U test
df<-NULL
pe<-NULL
for (i in 1:length(iv)){
  f<-as.formula(paste("correct~",iv[i],sep=""))
  x<-data[,iv[i]]
  y<-data$correct
  w<-wilcox.test(x,y,exact=FALSE, data=data, conf.int = FALSE, conf.level = 0.95,paired=TRUE)
  g<-glm(f,data=data, family=binomial)
  ag<-aov(g)
  sag<-summary(ag)
  pes<-sag[[1]][3][1,1]
  pess<-sag[[1]][5][1,1]
  pd<-data.frame(iv=iv[i],pes=pes,sig=pess)
  pe<-if(is.null(pe)) pd else rbind(pe,pd)
  p<-w$p.value
  s<-""
  if (p<.05) s<-"*"
  if (p<0.01) s<-"**"
  if (p<0.001) s<-"***"
  ps<-format(round(p, 5), nsmall = 5)
  d<-data.frame(iv=iv[i],p=ps,s=s)
  df<-if(is.null(df)) d else rbind(df,d)
}
# print the results of pairwise tests
print(df)
#sort the variables py partial eta
pe<- pe[order(-pe$pes),]
print(pe)
print(xtable(pe), include.rownames=FALSE)
#keep variables which are significant
pe<-pe[pe$sig<0.05,]
#build a vector of variable names to keep in the original data
#pe<-pe[!(pe$iv %in% c("Lines","Chunks","RepairActions")),]
#pe<-pe[!(pe$iv %in% c("Lines","Chunks","RepairPatterns","RepairActions")),]
#pe<-pe[!(pe$iv %in% c("Chunks","Added","Modified","Removed")),]
pe<-pe[!(pe$iv %in% c("Lines","RepairPatterns")),]
#pe<-pe[!(pe$iv %in% c("Lines")),]
v<-c("correct",pe$iv)
#keep only the variables of interest
data<-data[,v]
#build an interaction model
m<-glm(correct~(.)^2-1, data = data)
#m<-glm.nb(correct~(.)^2-1, data = data)
#m<-glm(correct~., data = data,family=binomial)
#m<-glm(correct~., data = data,family=binomial)
m<-glm(correct~., data = data)
#m<-glm(correct~(.)^2-1, data = data,family=binomial)

a<-aov(m)
sa<-summary(a)
print(summary(m))
print(sa)

print(xtable(summary(m)), include.rownames=TRUE)
print(xtable(sa), include.rownames=TRUE)
if (1==1){
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
vi<-car::vif(m)
print(vi)
print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print(summary(vi))
#print(xtable(summary(vi)), include.rownames=TRUE)
}
if(1==1){
  data$correctf<-as.factor(data$correct)
p_ <- GGally::print_if_interactive

g<-ggpairs(data,mapping=ggplot2::aes(colour = correctf),lower = list(continuous=wrap("points", position=position_jitter(height=.01, width=.01))))

pdf("correlationMatrix.pdf", height = 10, width = 15)

print(g)
dev.off()
p_(g)
}