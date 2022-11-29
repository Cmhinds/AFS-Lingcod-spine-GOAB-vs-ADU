#TITLE:#### 
#American Fisheries Society Symposium 2022
#Chris Hinds, Fishery Biologist II, ADF&G, ADU 2022

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#HELPFUL WEBSITES:####
#Helpful websites: http://derekogle.com/fishR/2017-04-14-Modified_AgeBiasPlot
#Helpful websites: https://www.r-bloggers.com/2017/04/modified-age-bias-plot/
#Helpful websites: https://www.r-bloggers.com/2021/03/age-bias-plots-using-ggplot/
#Helpful websites: https://www.learnbyexample.org/r-bar-plot-base-graph/
#Helpful websites: https://www.sharpsightlabs.com/blog/barplot-r-geom_bar/

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#INSTALL PACKAGES/LIBRARY:####

getwd() #Working directory for this project is here. All data and projects should be saved here G:/ZZ Hinds/R

#install.packages("readxl") should already be installed
#install.packages("FSA") should already be installed
#install.packages ("tidyverse") should already be installed
#install.packages("ggplot2") #should already be installed

library(FSA) #FSA package runs the bias plot functions. Takes time to load. Might get Access Denied but is working if you can make the plots
library(tidyverse) #contains ggplot
library(ggplot2)

.libPaths() #Checks where your libraries are being stored if needed.Put it here C:/R/R-4.1.2/library

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#UPLOAD DATA:####
#Upload data from Import Dataset Environment wizard. Change the name of the file and copy the file name in the library and place it here. Any changes to the data will be reflected instead of deleting the data and uploading it again.

library(readxl) #For data in Excel spreadsheets

SEAK <- read_excel("AFS/Data/100 Lingcod SEAK Fin Spine Ford and ADU spine then Otos aged by ADU.xlsx", 
                   sheet = "Raw Data")

NGAK <- read_excel("AFS/Data/326 Lingcod NGAK Oto ADU and spines GOAB - Copy.xlsx", 
                   sheet = "Lingod Fin and Oto from Martin ")

Published_Agree_CV <- read_excel("AFS/Data/Delsa raw data ape cv.xlsx")

#UPLOAD DATA ANOTHER WAY:

library(readr) #For data in csv formats
read_csv  #Use read_csv and not read.csv. This is important. Replace read_excel above with read_csv

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#BIAS PLOTS:####

#SEAK: GOAB Spine vs SEAK Spine

#Write code for how the Bias plot should be set up in terms of X and Y
#The data after the ~ is the reference data or known age data
#ab1 is what the bias plot is called
#data is which dataset you want to pull the columns from
#ab1 <- ageBias(columnB~columnA,data=SEAK,nref.lab="Y Axis name",ref.lab="X axis name") 

ab1 <- ageBias(SEAKSpine~GOABSpine,data=SEAK, 
               nref.lab="SEAK Spine",ref.lab="GOAB Spine") 

#Code to tell the plot how to show the information within the plot

plotAB(ab1,col.CIsig="black",pch.mean.sig=19,sfrac=0.01,
       lty.agree=1,col.agree="black") 

#Code that shows significance of bias in P values. Outputs in Console and you can Export in Plots

summary(ab1, what="symmetry") 

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#NGAK: SEAK Otoliths vs GOAB Spines

ab1 <- ageBias(GOABSpine~SEAKOtolith,data=NGAK, 
               nref.lab="GOAB Spine",ref.lab="SEAK Otolith")

plotAB(ab1,col.CIsig="black",pch.mean.sig=19,sfrac=0.01,
       lty.agree=1,col.agree="black") 

summary(ab1, what="symmetry")

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#SEAK: SEAK Otolith vs GOAB Spine

ab1 <- ageBias(GOABSpine~SEAKOtolith,data=SEAK, 
               nref.lab="GOAB Spine",ref.lab="SEAK Otolith") 

plotAB(ab1,col.CIsig="black",pch.mean.sig=19,sfrac=0.01,
       lty.agree=1,col.agree="black") 

summary(ab1, what="symmetry") 

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#BAR PLOTS:####

#Published Data %Agreement across species vs %Average

#ggplot (data=, aes (x=, y=)) + geom_bar(color=, fill=, stat=) + geom_hline(yintercept = mean(), color="") 

#geom_bar codes for a bar plot
#data is the dataframe you want to pull from
#aes will map variables in your dataframe to the aesthetic attributes of your plot
#geom_bar codes for how the bar colors
#color is boarder of the bar
#fill is required and is the inside color of the bar
#stat sets the length of the bar. stat='identity' means the bar will conform to the height of what you set the Y axis to
#geom_hline is used to place a horizontal line on a bar graph
#the average is calculated using: mean(x, na.rm) where the column you want the average of is mean(" ")
#a horizontal line of " " color will be drawn at a height using the mean of ` ` column

ggplot (data=Published_Agree_CV, aes (x=SpecimenNumber, y=Agreement)) + 
  geom_bar(color='black', fill='black', stat='identity') +
  geom_hline(yintercept = mean(52),color='blue')

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#HISTOGRAMS:####

#Published Agreement and CV vs Published Average Agreement and CV

ggplot(data = Published_Agree_CV, aes(x = Agreement)) +
  geom_density(fill='black') + geom_vline(xintercept = mean(52),color='blue')

ggplot(data = Published_Agree_CV, aes(x = CV)) +
  geom_density(fill='black') + geom_vline(xintercept = mean(5.9),color='blue')

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Published Agreement and CV vs NGAK: SEAK Otoliths vs GOAB Spines

ggplot(data = Published_Agree_CV, aes(x = Agreement)) +
  geom_density(fill='black') + geom_vline(xintercept = mean(30.3),color='blue')

ggplot(data = Published_Agree_CV, aes(x = CV)) +
  geom_density(fill='black') + geom_vline(xintercept = mean(8.68),color='blue')

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Published Agreement and CV vs SEAK: GOAB Spine vs SEAK Spine

ggplot(data = Published_Agree_CV, aes(x = Agreement)) +
  geom_density(fill='black') + geom_vline(xintercept = mean(12),color='blue')

ggplot(data = Published_Agree_CV, aes(x = CV)) +
  geom_density(fill='black') + geom_vline(xintercept = mean(10.96),color='blue')

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Published Agreement and CV vs SEAK: SEAK Otoliths vs GOAB Spines

ggplot(data = Published_Agree_CV, aes(x = Agreement)) +
  geom_density(fill='black') + geom_vline(xintercept = mean(16),color='blue')

ggplot(data = Published_Agree_CV, aes(x = CV)) +
  geom_density(fill='black') + geom_vline(xintercept = mean(11.11),color='blue')

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#DENSITY PLOT WITH MEAN LINE (HISTOGRAM SHOWING BIAS):####

install.packages("ggplot2") #should already be installed

library(ggplot2)
####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#SEAK: GOAB Spine vs SEAK Oto

ggplot(SEAK,aes(GOABSpine, SEAKOtolith))+       #this plots the X, Y and data points show up
  geom_point()

SEAK$Bias<-SEAK$GOABSpine-SEAK$SEAKOtolith      #this calculates the bias.

ggplot(SEAK,aes(Bias))+                           #makes a bar graph of the showing bias data
  geom_bar()

?geom_bar()                                       #? asks for help with a function


ggplot(SEAK,aes(Bias))+                           #turned this from a bar graph to a squiggly line histogram
  geom_density()

mean(SEAK$Bias)                                   #calculates mean. outputs in console below

#Online Example to draw a vertical mean line and fill the histogram in as grey. Plugged in  my stuff in for "a" and for the "mean".
#a + geom_density(aes(y = stat(count)), fill = "lightgray") + geom_vline(aes(xintercept = mean(weight)), linetype = "dashed")

ggplot(SEAK,aes(Bias))+geom_density(aes(y = stat(count)), fill = "lightgray") +     
  geom_vline(aes(xintercept = mean(Bias)), linet = "dashed")

####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#SEAK: GOAB Spine vs SEAK Spine

ggplot(SEAK,aes(GOABSpine, SEAKSpine))+ geom_point()

SEAK$Bias<-SEAK$GOABSpine-SEAK$SEAKSpine  

ggplot(SEAK,aes(Bias))+ geom_bar()

ggplot(SEAK,aes(Bias))+ geom_density()

mean(SEAK$Bias) 

ggplot(SEAK,aes(Bias))+geom_density(aes(y = stat(count)), fill = "lightgray") +    
  geom_vline(aes(xintercept = mean(Bias)), linetype = "dashed")

####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#NGAK GOAB Spine age vs SEAK Oto Age

ggplot(NGAK,aes(GOABSpine, SEAKOtolith))+ geom_point()

NGAK$Bias<-NGAK$GOABSpine-NGAK$SEAKOtolith  

ggplot(NGAK,aes(Bias))+ geom_bar()

ggplot(NGAK,aes(Bias))+ geom_density()

mean(NGAK$Bias) 

ggplot(NGAK,aes(Bias))+geom_density(aes(y = stat(count)), fill = "lightgray") +    
  geom_vline(aes(xintercept = mean(Bias)), linetype = "dashed")
####+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#SAVE:#### 
# Save your project and name it. 
#Do not save the work space image/environment. It can be recreated with your code

 