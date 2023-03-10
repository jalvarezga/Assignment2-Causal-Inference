#taller2
#Joaquin Alvarez
#marzo 2023

install.packages('haven') #paquete para leer 
#archivos .sav
library(haven)
??heaven
getwd()
?read_sav
setwd('/Users/joaquinalvarez/Documents/decimoSemestre/InferenciaCausal/taller2')
getwd()
#path = file.path("/Users/joaquinalvarez/Documents/decimoSemestre/InferenciaCausal/taller2", "taller2", "Planea06_2018_Alumnos.sav")
dataset = read_sav('Planea06_2018_Alumnos.sav')
dataset
str(dataset)
dataset$W_FSTR100
dataset$EDAD
max(dataset$EDAD)
as.data.frame(dataset)
typeof(dataset)
dataset[,1]
dataset[2,]
#https://www.rebeccabarter.com/

#MATEMATICAS
dataset$PV1MAT
dataset$PV2MAT
dataset$PV3MAT
dataset$PV4MAT
dataset$PV5MAT
(dataset$PV1MAT+dataset$PV2MAT+dataset$PV3MAT+dataset$PV4MAT+dataset$PV5MAT)/5
names(dataset)


set.seed(123)
#we pick 200 schools randomly to be treated
dataset$ESCUELA
as.array(dataset$ESCUELA)

unique(dataset$ESCUELA)
tail(unique(dataset$ESCUELA))
#3573 in total 

set.seed(123)
#we pick 200 schools randomly to be treated
tratados=sample(1:3573, size = 200, replace=F)
tratados
2463%in%tratados
library(dplyr)
#just playing t understand functions
datosTratados=filter(dataset, dataset$ESCUELA==4)
datosTratados
dataset$ESCUELA
noTratados=setdiff(1:3573, tratados)
noTratados
datosTratados=c()
datosNoTratados=c()
for( j in tratados){
  datosTratados=rbind(datosTratados, filter(dataset, dataset$ESCUELA==j))
}
#for( j in noTratados){
  #datosNoTratados=rbind(datosNoTratados, filter(dataset, dataset$ESCUELA==j))
#}
#conviene otro approach para no andar guardando tanta basura porque son demasiadas variables
dim(dataset)
treatments=rep(0, 104973)
dataset$ESCUELA[123]%in%c(11,12)
for( i in 1:104973){
  if(dataset$ESCUELA[i]%in%tratados){treatments[i]=1}
}
treatments

sum(treatments)
#hence de average number of students per school in the treatment group is
sum(treatments)/200
head(datosTratados)
tail(datosTratados)
#which makes sense :)))



1452%in%tratados
1454%in%tratados



#let's run experiments

#Y_i= beta_0 +beta_1T_i

#beta_1 is the average treatment effect
#we will simulate for various treatment effects

mathScore=(dataset$PV1MAT+dataset$PV2MAT+dataset$PV3MAT+dataset$PV4MAT+dataset$PV5MAT)/5
dataset$MARGINC
#we can use the marginalization score as a control variable
#maybe also the age of the students and sex
df=data.frame(escuela=dataset$ESCUELA, tratado=treatments, calif=mathScore, marginacion=dataset$MARGINC, sexo=dataset$SEXO, edad=dataset$EDAD_AC)
head(df)
#marginacion es variable categorica pero ordinal (tiene orden/ jerarquia).
tail(df)
names(df)
summary(df)
str(df)
#df$marginacion=as.factor(df$marginacion)
#str(df)




#we want to simulate observations!!!!! Since these are all hypothetical scenarios and the data
#doesn't come from an experimental design as the one we are considering, we need to kind of simulate, 
#the scores of the students.

#First model: without stratifying the errors  and controlling on marginalization score
mean(df$calif)
summary(df)
#global average score for the students in the math test
beta_0=mean(df$calif)
#coaefficient for marginalization score
beta_1=-100
#we suppose that more marginalized students tend to get worse scores
#which is typically true, and a reasonable assumption
#treatment effect
beta_2=100
#interpretation: on average, students whose professors get a salary increase
#will perform 100 points better on the math test, compared to students
#whose professors didn't get a sallary increase, controlling on the marginalization 
#score for the schools.
set.seed(123)
Y=beta_0+beta_1*df$marginacion+beta_2*df$tratado+rnorm(dim(df)[1], sd=50)
model=cbind(df, Y)
head(model)

modelo1=lm(Y~marginacion+as.factor(tratado), data=model)
summary(modelo1)
#we get an estimate for the ATE close to 100
#and a very small p-value, which means we are able to detect the treatment effect.
#with significance level alpha=0.05


#now we want to study these ideas using different  magnitudes of tratment effects
#we want to campare the results when stratifying by school against the 
#results when we don't stratify by school, similar to what we did in class
#when stratifying errors, we should use the school number for doing so.
#and the data generating process should take the school numbers into accout

#a reasonable approch is to assign more variance to the grades in schools where there aren't many students
#or for the purposes of example and simulations, just make the variance something completely arbitrary
#or random.
beta2s=seq(10,100, length.out=10) #different tratment effects we will study
beta2s
c(beta2s)
beta2s=cbind(beta2s)
#we also want to consider small treatment effects and determine if  we are able to detect a 
#treatment effect, that is, reject the null hypothesis.
others=c(0,1,2,3,4,5)
treatmentEffects=c(others, beta2s)
treatmentEffects
beta_0=mean(df$calif[df$tratado==0]) #average score of non -treated
#will be our intercepts for our simulations
beta_0
beta_1=-100
#we suppose that more marginalized students tend to get worse scores
#which is typically true, and a reasonable assumption
#we will simulate error stucture stratified by school
stratifiedErrors=1:dim(df)[1]
length(stratifiedErrors)
unique(dataset$ESCUELA)
tail(unique(dataset$ESCUELA))
#3573 schools in total 
df
df[df$escuela==1,]
df[df$escuela==3,]
dim(df[df$escuela==3,])
dim(df[df$escuela==3,])[1] #4 students in school 3
aux=1 #an auxiliary variable
for(j in 1:3573){#for each school
  print(j)
  students=df[df$escuela==j,]
  #we generate a common error that affects all students in the same school
  Common_noise=rnorm(1,mean=0,sd=100/dim(students)[1])
  #if a school has many students it has less variance in the scores
  #this is an asusmption that we make in the data generating process
  #it can be changed of course.
  for(i in 1:dim(students)[1]){
    particularNoise=rnorm(1,mean=0, sd=runif(1, min=5,max=10))
    stratifiedErrors[aux]=particularNoise+Common_noise
    aux=aux+1
  }
}
stratifiedErrors
Y=beta_0+beta_1*df$marginacion+beta2s[1]*df$tratado+stratifiedErrors
Y
str(df)
names(df)
df=cbind(df, Y)
names(df)
modelo2=lm(Y~marginacion+tratado, data=df)
summary(modelo2)
df$escuela
#ok this works fine. Now let's simulate for different treatment effects!
treatmentEffects
#and let's make some plots similar to the ones we saw in the lectures

beta2s=seq(10,100, length.out=10) #different tratment effects we will study
beta2s
c(beta2s)
beta2s=cbind(beta2s)
#we also want to consider small treatment effects and determine if  we are able to detect a 
#treatment effect, that is, reject the null hypothesis.
others=c(0,2,4,6)
treatmentEffects=c(others, beta2s)
treatmentEffects


#WE ARE GOING TO USE A SLIGHTLT DIFFEREMT MODEL FOR THE ERRORS STRATIEFIED PER SCHOOL
#these implementations work fine. However they assume heteroskedastic errors between schools
#but we want something more: correlated scores betweeen students of the same school. We will explore this later on.

set.seed(123)
treatmentEffects#these are average treatment effects.
#we want to explore how well we are able to find an effect of increasing the salary of teachers in schools
#in the grades of the students of those schools
length(treatmentEffects)
TreatmentEstimate=rep(0,14)
c=1
nullRejections=rep(0,14)
for(beta in treatmentEffects){
  betass=1:100
  pvalues=1:100
  for(q in 1:100){
    print(q)
    #we pick 200 schools randomly to be treated
    tratados=sample(1:3573, size = 200, replace=F)
    treatments=rep(0, 104973)
    for( i in 1:104973){
      if(dataset$ESCUELA[i]%in%tratados){treatments[i]=1}
    }
    df=data.frame(escuela=dataset$ESCUELA, tratado=treatments,  marginacion=dataset$MARGINC)
    #now we simulate errors
    aux=1 #an auxiliary variable
    for(j in 1:3573){#for each school
      #print(j)
      students=df[df$escuela==j,]#students in j-th school
      #we give the same distribution to the errors
      #within a school
      Common_noise=rnorm(dim(students)[1],mean=0,sd=100/dim(students)[1])
      #if a school has many students it has less variance in the scores
      #this is an asusmption that we make in the data generating process
      #it can be changed of course.
      for(k in 1:dim(students)[1]){
        #particularNoise=rnorm(1,mean=0, sd=runif(1, min=5,max=10))
        stratifiedErrors[aux]=Common_noise[k]
        aux=aux+1
      }
    }
    #stratifiedErrors
    Y=beta_0+beta_1*df$marginacion+beta*df$tratado+stratifiedErrors
    df=cbind(df, Y)
    #we fit an ols  linear model without stratifying per school
    ols=lm(Y~marginacion+tratado, data=df)
    sum=summary(ols)
    betass[q]=sum$coefficients[3]
    pvalues[q]=sum$coefficients[3,4]
  }
  TreatmentEstimate[c]=mean(betass)
  nullRejections[c]=sum(pvalues<0.05)/100 #proportion of times we rejected the null
  #hence, this is an estimate for the probability of discovering a treatment 
  #effect!!!!!
  c=c+1
  print(c)
}
#it takes about 1 hour to run all the simulations in the for loop!!!!!
#because there's too much data 

plot(treatmentEffects, TreatmentEstimate)
cor(treatmentEffects, TreatmentEstimate) #perfect correlation!

plot(treatmentEffects, nullRejections)
abline(h=0.05)
var(Y)

#this is fine! But perhaps we are being to conservative at exploring VERY big values of the
#ATE
#and this is why everything seems to look perfect
#moreover, we should account for correlated socres within students from the same school in the 
#data generating process!! Becasue it is a more reasonable assumption in the context of the problem.
#Perhaps we should explore more beta2's closer to zero
#the problem is that our simulation approach takes way too much time.
#also the values that we assigned to the variances in each school may bee very small
#notice that students in the same school get very close grades in some cases.
#in particular in the cases in which there are many students in the school. The variance is just too small
#and  hence grades are almost identical for students in those schools
#we introduce these observations next.














#The important implementations and simulations
#
#
#
#
#
#
#
#
str(dataset)

summary(dataset)
#The important implementations and simulations
#we assume heteroskcedastic and correlated scores between studensts that belong to the same school
beta_0=mean(mathScore) #average score of non -treated
#will be our intercepts for our simulations
beta_0
#lower marginalization score can be interpreted as lower income level
beta_1=100
#we suppose that more marginalized students tend to get worse scores
#which is typically true, and a reasonable assumption
#Hence beta_1 is reasonable to be assumed positive

beta2s=seq(-5,5, length.out=20) #different treatment effects we will study
beta2s
beta2s=c(0, beta2s)
beta2s
beta2s=sort(beta2s)
beta2s
length(beta2s)
#we will even allow and consider a negative treatment effect! Which would be weird in reality
#it would be like getting worse grades is teachers get an increase in their salary
#we also want to consider small treatment effects and determine if  we are able to detect a 
#treatment effect, that is, reject the null hypothesis.
treatmentEffects=beta2s
#WE ARE GOING TO USE A SLIGHTLT DIFFEREMT MODEL FOR THE ERRORS STRATIEFIED PER SCHOOL
set.seed(123)
treatmentEffects#these are average treatment effects.
#we want to explore how well we are able to find an effect of increasing the salary of teachers in schools
#in the grades of the students of those schools
length(treatmentEffects)
TreatmentEstimate=rep(0,21)
c=1
nullRejections=rep(0,21)
lowerQuantiles=rep(0,21)
upperQuantiles=rep(0,21)
for(beta in treatmentEffects){
  betass=1:100
  pvalues=1:100
  for(q in 1:100){
    print(q)
    #we pick 200 schools randomly to be treated
    tratados=sample(1:3573, size = 200, replace=F)
    treatments=rep(0, 104973)
    for( i in 1:104973){
      if(dataset$ESCUELA[i]%in%tratados){treatments[i]=1}
    }
    df=data.frame(escuela=dataset$ESCUELA, tratado=treatments,  marginacion=dataset$MARGINC)
    #now we simulate errors
    aux=1 #an auxiliary variable
    for(j in 1:3573){#for each school
      #print(j)
      students=df[df$escuela==j,]#students in j-th school
      #we give the same distribution to the errors
      #within a school
      Common_noise=rnorm(1,mean=0,sd=sqrt(dim(students)[1]))
      #now more students implies more variance
      #if a school has many students it has less variance in the scores
      #this is an asusmption that we make in the data generating process
      #it can be changed of course.
      for(k in 1:dim(students)[1]){
        particularNoise=rnorm(1,mean=0, sd=5)
        stratifiedErrors[aux]=Common_noise+particularNoise
        #now we have clustered errors with correlations within the same school!
        #which is more realistic!!!
        aux=aux+1
      }
    }
    Y=beta_0+beta_1*df$marginacion+beta*df$tratado+stratifiedErrors
    df=cbind(df, Y)
    #we fit an ols  linear model without stratifying per school
    ols=lm(Y~marginacion+tratado, data=df)
    sum=summary(ols)
    betass[q]=sum$coefficients[3]
    pvalues[q]=sum$coefficients[3,4]
    
  }
  TreatmentEstimate[c]=mean(betass)
  nullRejections[c]=sum(pvalues<0.05)/100 #proportion of times we rejected the null
  #hence, this is an estimate for the probability of discovering a treatment 
  #effect!!!!!
  lowerQuantiles[c]=quantile(betass, 0.025) #represent quantiles of our estimator of beta_3
  upperQuantiles[c]=quantile(betass, 0.975)
  c=c+1
  print(c)
}
quantile(rnorm(1000), 0.025)
quantile(rnorm(1000), 0.975)
plot(treatmentEffects, TreatmentEstimate)
cor(treatmentEffects, TreatmentEstimate) #perfect correlation!
lowerQuantiles
upperQuantiles
treatmentEffects
treatmentEffects[11]
nullRejections[11] #very big!
plot(treatmentEffects, nullRejections)
plot(treatmentEffects, nullRejections, type='l')

#we can create confidence intervals qith the help of a blog
#https://statisticsglobe.com/draw-plot-with-confidence-intervals-in-r


library(ggplot2)
library(latex2exp) #for LaTEX code in R 
plotsin=data.frame(x=treatmentEffects, y=TreatmentEstimate, lower=lowerQuantiles, upper=upperQuantiles)
names(plotsin)
My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16))
p1=ggplot(plotsin, aes(x,y))+
  geom_point(lwd=2, col='red')+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  labs(title = 'ATE point estimates and Confidence Intervals without clustered errors',y = "Point estimations with a 95% confidence interval", 
       x = TeX(r"(\textbf{$\beta_2$})"))+
  theme(plot.title = element_text(hjust = 0.5),)+
  My_Theme
  
  
p1  
Y
min(Y)
max(Y)
#these are quite reasonable quantities compared to the real data
min(mathScore)
max(mathScore)



plotea=as.data.frame(cbind(nullRejections, treatmentEffects))
names(plotea)
ggplot(plotea, aes(x=treatmentEffects,y=nullRejections))+
  geom_line(lwd=2, col='orange')+
  labs(title = TeX(r"(\textbf{ Probability of rejecting the null for different average treatment effects with $\alpha=0.05$ confidence level})"), y = "probability of rejecting the null", 
       x = TeX(r"(\textbf{$\beta_2$})"))+
  geom_hline(yintercept=0.05, linetype="dashed", color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  My_Theme
  



#a very cool blog on the importance of  clustering in
#linear regression models



#este link me sirve mucho... Me he dado cuenta que recurro a el en casi todas mis tareas
#http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization
#da ideas muy basicas para plots





#
#
#
#Clustering errors!!!!

install.packages('lfe')
library(lfe)
install.packages("sandwich")
library(sandwich)
library(lmtest)
?sandwich
ols=lm(Y~marginacion+tratado, data=df)
sum=summary(ols)
sum
?coeftest
df
names(df)

#vcovCL: is a  Clustered Covariance Matrix Estimation
m1coeffs_cl <- coeftest(ols, vcov = vcovCL, cluster = ~escuela)
m1coeffs_cl
m1coeffs_cl[3,4]
m1coeffs_cl[3]
#notice the difference in p values!!!!
sum$coefficients[3,4]

#now we run similar implemantations but with clustered errors.
#and compare the two models
treatmentEffects=beta2s
#WE ARE GOING TO USE A SLIGHTLT DIFFEREMT MODEL FOR THE ERRORS STRATIEFIED PER SCHOOL
set.seed(123)
treatmentEffects#these are average treatment effects.
#we want to explore how well we are able to find an effect of increasing the salary of teachers in schools
#in the grades of the students of those schools
length(treatmentEffects)
ClusteredTreatmentEstimate=rep(0,21)
c=1
ClusterednullRejections=rep(0,21)
ClusteredlowerQuantiles=rep(0,21)
ClusteredupperQuantiles=rep(0,21)
for(beta in treatmentEffects){
  betass=1:100
  pvalues=1:100
  for(q in 1:100){
    print(q)
    #we pick 200 schools randomly to be treated
    tratados=sample(1:3573, size = 200, replace=F)
    treatments=rep(0, 104973)
    for( i in 1:104973){
      if(dataset$ESCUELA[i]%in%tratados){treatments[i]=1}
    }
    df=data.frame(escuela=dataset$ESCUELA, tratado=treatments,  marginacion=dataset$MARGINC)
    #now we simulate errors
    aux=1 #an auxiliary variable
    for(j in 1:3573){#for each school
      #print(j)
      students=df[df$escuela==j,]#students in j-th school
      #we give the same distribution to the errors
      #within a school
      Common_noise=rnorm(1,mean=0,sd=sqrt(dim(students)[1]))
      #now more students implies more variance
      #if a school has many students it has less variance in the scores
      #this is an asusmption that we make in the data generating process
      #it can be changed of course.
      for(k in 1:dim(students)[1]){
        particularNoise=rnorm(1,mean=0, sd=5)
        stratifiedErrors[aux]=Common_noise+particularNoise
        #now we have clustered errors with correlations within the same school!
        #which is more realistic!!!
        aux=aux+1
      }
    }
    Y=beta_0+beta_1*df$marginacion+beta*df$tratado+stratifiedErrors
    df=cbind(df, Y)
    #we fit an ols  linear model without stratifying per school
    ols=lm(Y~marginacion+tratado, data=df)
    m1coeffs_cl <- coeftest(ols, vcov = vcovCL, cluster = ~escuela)
    betass[q]=m1coeffs_cl[3]
    pvalues[q]=m1coeffs_cl[3,4]
    
  }
  ClusteredTreatmentEstimate[c]=mean(betass)
  ClusterednullRejections[c]=sum(pvalues<0.05)/100 #proportion of times we rejected the null
  #hence, this is an estimate for the probability of discovering a treatment 
  #effect!!!!!
  ClusteredlowerQuantiles[c]=quantile(betass, 0.025) #represent quantiles of our estimator of beta_3
  ClusteredupperQuantiles[c]=quantile(betass, 0.975)
  c=c+1
  print(c)
}


plotsin2=data.frame(x=treatmentEffects, y=ClusteredTreatmentEstimate, lower=ClusteredlowerQuantiles, upper=ClusteredupperQuantiles)
names(plotsin)
My_Theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16))
p2=ggplot(plotsin2, aes(x,y))+
  geom_point(lwd=3, col='forestgreen')+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  labs(title = 'ATE point estimates and Confidence Intervals considering clustered errors',y = "Point estimations with a 95% confidence interval", 
       x = TeX(r"(\textbf{$\beta_2$})"))+
  theme(plot.title = element_text(hjust = 0.5),)+
  My_Theme

print(p2)
library(patchwork) #to 'sum' ggplot objects
p1+p2


Y
min(Y)
max(Y)
#these are quite reasonable quantities compared to the real data
min(mathScore)
max(mathScore)



plotea2=as.data.frame(cbind(ClusterednullRejections, treatmentEffects))
names(plotea2)
ggplot(plotea2, aes(x=treatmentEffects,y=ClusterednullRejections))+
  geom_line(lwd=2, col='forestgreen')+
  labs(title = TeX(r"(\textbf{ Probability of rejecting the null for different average treatment effects with $\alpha=0.05$ confidence level})"), y = "probability of rejecting the null", 
       x = TeX(r"(\textbf{$\beta_2$})"))+
  geom_hline(yintercept=0.05, linetype="dashed", color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  My_Theme

compare=data.frame(ClusterednullRejections=ClusterednullRejections,nullRejections=nullRejections,treatmentEffects=treatmentEffects)
category=c('Clustered errors','non-clustered')
category  
p=ggplot(data=compare, aes(x=treatmentEffects))+
  geom_line(aes(y=ClusterednullRejections,group =1L,  color = "Clustered errors"), lwd=3)+
  geom_line(aes(y=nullRejections,group =1L, color='non-clustered'), lwd=3)+
  scale_color_manual(values=c("forestgreen","orange"),
                     breaks = category)+
  labs(x=TeX(r"(\textbf{True value of $\beta_2$})"), y='Probability of rejecting the null hypothesis', title=TeX(r"(\textbf{ Probability of rejecting the null for different average treatment effects with $\alpha=0.05$ confidence level})"))+
  theme(plot.title = element_text(hjust = 0.5))
p+My_Theme





#
#
#
#
#Randomized experiment
#will only consider non negative treatment effects
#for thee sake of simplicity and exploring ideas we will assume that the treatment effect is the same for everyone.
#however a more realistic approach would be to assign different treatment effects to each student.

treatmentEffects=seq(0,50, by=10)
treatmentEffects
length(treatmentEffects)
#we will use the original data and add the treatment effect
#we pick 200 schools randomly to be treated
set.seed(1)
tratados=sample(1:3573, size = 200, replace=F)
treatments=rep(0, 104973)
for( i in 1:104973){
  if(dataset$ESCUELA[i]%in%tratados){treatments[i]=1} #we assign 1 to all students in the school
  #where treatments willl be applied
}
dataframe=data.frame(escuela=dataset$ESCUELA, y0=mathScore, y=mathScore+treatmentEffects[2]*treatments, treatments=treatments)
#out test statistic will be the mean difference  of  the treated minus the mean in the controls

dataframe[dataframe$treatments==1,]
mean(dataframe[dataframe$treatments==1,]$y)
p_values=rep(1,6)

#first we implement an example with null treatment effect, then we generalize
getwd()
#we will use the original data and add the treatment effect
#we pick 200 schools randomly to be treated
set.seed(1)
tratados=sample(1:3573, size = 200, replace=F)
treatments=rep(0, 104973)
for( i in 1:104973){
  if(dataset$ESCUELA[i]%in%tratados){treatments[i]=1} #we assign 1 to all students in the school
  #where treatments willl be applied
}

treatmentEffects
#assume null treatment effect
dataframe=data.frame(escuela=dataset$ESCUELA, y0=mathScore, y=mathScore+treatmentEffects[1]*treatments, treatments=treatments)
#out test statistic will be the mean difference  of  the treated minus the mean in the controls

dataframe[dataframe$treatments==1,]
mean(dataframe[dataframe$treatments==1,]$y)
Estadistico_base=mean(dataframe[dataframe$treatments==1,]$y)-mean(dataframe[dataframe$treatments==0,]$y)
Estadistico_base
#estimate a p-value using simulation, for our test statistic 
set.seed(2)
statistics=c()
for(k in 1:1000){
  tratados=sample(1:3573, size = 200, replace=F)
  treatments=rep(0, 104973)
  for( i in 1:104973){
    if(dataset$ESCUELA[i]%in%tratados){treatments[i]=1} #we assign 1 to all students in the school
    #where treatments will be applied
  }
  dataframe=data.frame(escuela=dataset$ESCUELA, y0=mathScore, y=mathScore+treatmentEffects[1]*treatments, treatments=treatments)
  #usspose that the null is true
  #out test statistic will be the mean difference  of  the treated minus the mean in the controls
  statistics[k]=mean(dataframe[dataframe$treatments==1,]$y)-mean(dataframe[dataframe$treatments==0,]$y)
  print(k)
}
statistics
pvalue=sum(statistics>Estadistico_base)/1000
pvalue
nullEffect=data.frame(statistics)
nullEffect
ggplot(nullEffect, aes(x=statistics))+
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=Estadistico_base),
             color="red", linetype="dashed", size=1)+
  labs(x=TeX(r"(\textbf{Test statistic value})"), y='density', title=TeX(r"(\textbf{Density of the test statistic when the treatment effect is $\delta=0$})"))+
  theme(plot.title = element_text(hjust = 0.5))+My_Theme
#now using a positive treatment effect
set.seed(1)
tratados=sample(1:3573, size = 200, replace=F)
treatments=rep(0, 104973)
for( i in 1:104973){
  if(dataset$ESCUELA[i]%in%tratados){treatments[i]=1} #we assign 1 to all students in the school
  #where treatments willl be applied
}
treatmentEffects
#using delta=10
dataframe=data.frame(escuela=dataset$ESCUELA, y0=mathScore, y=mathScore+treatmentEffects[2]*treatments, treatments=treatments)
#out test statistic will be the mean difference  of  the treated minus the mean in the controls
Estadistico_base2=mean(dataframe[dataframe$treatments==1,]$y)-mean(dataframe[dataframe$treatments==0,]$y)
Estadistico_base2
ggplot(nullEffect, aes(x=statistics))+
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=Estadistico_base),
             color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=Estadistico_base2),
             color="black", linetype="dashed", size=1)+
  labs(x=TeX(r"(\textbf{Test statistic value})"), y='density', title=TeX(r"(\textbf{Density of the test statistic when the treatment effect is $\delta=0$})"))+
  theme(plot.title = element_text(hjust = 0.5))+My_Theme
pvalue2=sum(statistics>Estadistico_base2)/1000
pvalue2

#we want to repeat these sort of  simulations in order to get the power
#we explore the power for different treatment effects.
deltas=seq(0,20, length.out=100)
deltas
set.seed(2)
statistics=c()
for(k in 1:1000){
  tratados=sample(1:3573, size = 200, replace=F)
  treatments=rep(0, 104973)
  for( i in 1:104973){
    if(dataset$ESCUELA[i]%in%tratados){treatments[i]=1} #we assign 1 to all students in the school
    #where treatments will be applied
  }
  dataframe=data.frame(escuela=dataset$ESCUELA, y0=mathScore, y=mathScore+0*treatments, treatments=treatments)
  #suppose that the null is true
  #out test statistic will be the mean difference  of  the treated minus the mean in the controls
  statistics[k]=mean(dataframe[dataframe$treatments==1,]$y)-mean(dataframe[dataframe$treatments==0,]$y)
  print(k)
}
statistics
#now we got the distribution of the test statistic under the null. Now we want to explore the power
nullEffect=data.frame(statistics)
nullEffect
ggplot(nullEffect, aes(x=statistics))+
  geom_density(color="lightblue", fill="lightblue")
power=1:100
set.seed(123)
for(k in 1:100){
  #for each delta we estimate the power (prob of rejecting the null)
  rejections=0
  for(j in 1:100){
    #we simulate some treatments (randomization!!!!!)
    tratados=sample(1:3573, size = 200, replace=F)
    treatments=rep(0, 104973)
    for( i in 1:104973){
      if(dataset$ESCUELA[i]%in%tratados){treatments[i]=1} #we assign 1 to all students in the school
      #where treatments will be applied
    }
    dataframe=data.frame(y=mathScore+deltas[k]*treatments, treatments=treatments)
    #out test statistic will be the mean difference  of  the treated minus the mean in the controls
    statistic=mean(dataframe[dataframe$treatments==1,]$y)-mean(dataframe[dataframe$treatments==0,]$y)
    pval=sum(statistics>statistic)/1000
    if(pval<0.05){# we reject the null
      rejections=rejections+1
    }
  }
  power[k]=rejections/100 #estimate of the probability of rejecting the null
  #when usiong a significnace level of alpha=0.05
  print(k) #para ver como vamos  y hacernos una idea de cuanto tiempo falta :)
}
#this should take like an hour or more! 
#around 2 hours!!!
print(power)
rejects=data.frame(power, deltas)
ggplot(rejects, aes(x=deltas, y=power))+
  geom_line(col='tomato', lwd=2)+
  labs(x=TeX(r"(\textbf{$\delta$})"), y='Probability of rejecting the null hypothesis', title=TeX(r"(\textbf{Power function in the randomized experiment approach$})"))+
  theme(plot.title = element_text(hjust = 0.5))+My_Theme
  















#Problema2
getwd()
setwd( "/Users/joaquinalvarez/Downloads")
datos=read.csv('baseProblema2.csv', stringsAsFactors = T)
datos
head(datos)
summary(datos)

modelin=lm(datos$hosp~datos$hombre+datos$kit_administrativo, data=datos)
summary(modelin)
datos$sint_graves==datos$kit
datos$sint_graves==datos$kit_administrativo

#analyzing the population who received a kit
datos[datos$kit_administrativo==1, ]
treated=datos[datos$kit_administrativo==1, ]
dim(treated)

sum(treated$hombre)/dim(treated)[1] #proportion of treated male
1-sum(treated$hombre)/dim(treated)[1] #proportion of treated females

summary(treated)
#the data is relatively balance, suggesting no selection bias.

untreated=datos[datos$kit_administrativo==0, ]
sum(untreated$hombre)/dim(untreated)[1] #proportion of untreated male
1-sum(treated$hombre)/dim(treated)[1] #proportion of untreated females
summary(untreated) #dates, proportion of male and female 
#cormobility, suggest no selection bias, when comparing to the treated group


#controlling by sex
modelin=lm(datos$hosp~as.factor(datos$hombre)+as.factor(datos$kit_administrativo), data=datos)
summary(modelin)
#it seems reasonable that the coefficient associated to the ATE is negative. 
#the sign suggests that using the kit reduces the chances of hospitalization, 
#which is what the author of the report wanted to show


#we can compare those odds with the data
mean(treated$hosp)
mean(untreated$hosp)
mean(untreated$hosp)>mean(treated$hosp)
#chances of hospitalization were higher for the control group.
#suggesting that, with certain statistical significance the kit is effective.
#Of course that's what we were supposed to find. That was the purpose of the authors.
#of the paper.
#we can consider more control variables.
datos$sint_graves
modelin2=lm(datos$hosp~datos$sint_graves+as.factor(datos$hombre)+as.factor(datos$kit_administrativo), data=datos)
summary(modelin2)
#again, coherent with intuition, it makes sense that sint_graves
#is positive, suggesting that people who had lots of sypmtoms such as cough, etc,
#are more likely (on average) to be hospitalized.
