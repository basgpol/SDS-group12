##EXERCICE 1
## Hello Guillaume 
install.packages("RCurl")
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/kosukeimai/qss/master/INTRO/turnout.csv")
y <- read.csv(text = x)

library("rio")
filepath ="https://raw.githubusercontent.com/kosukeimai/qss/master/INTRO/turnout.csv"
df = import(filepath)
summary(df)
dim(df)
(df$total*100)/(df$VAP+df$overseas)#turnout with VAP+overseas
(df$total*100)/(df$VEP)#turnout with VEP

df$ANES-((df$total*100)/(df$VAP+df$overseas))
df$ANES-((df$total*100)/(df$VEP))

df$ANES-((df$total*100)/(df$VAP+df$overseas-df$felons-df$noncit))

(df$total*100)/(df$VAP+df$overseas)
calculate_turnout_VAP=function(year){
  turnout_VAP=(total*100)/(VAP+overseas)
  return(turnout_VAP)
}
VAP_turnout=(df$total*100)/(df$VAP+df$overseas)
VEP_turnout=(df$total*100)/(df$VEP)
df["VAP_turnout"]=(df$total*100)/(df$VAP+df$overseas)
df["VEP_turnout"]=(df$total*100)/(df$VEP)

df$VAP_turnout[seq(from=1, to=7, by=1)]

mean(df$VAP_turnout[seq(from=1, to=7, by=1)])
mean(df$VAP_turnout[seq(from=8, to=14, by=1)])
df$VAP_turnout[seq(from=1, to=14, by=2)]

mean(df$VAP_turnout[seq(from=1, to=14, by=2)])
mean(df$VAP_turnout[seq(from=2, to=14, by=2)])
mean(df$ANES[seq(from=1, to=14, by=2)])
mean(df$ANES[seq(from=2, to=14, by=2)])

mean(df$ANES-((df$total*100)/(df$VEP)))
