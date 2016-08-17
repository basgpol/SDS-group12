library("readr")
library("dplyr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "kosukeimai/qss/"
branch = "master/"
link = "CAUSALITY/leaders.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)
#250 assassination attempts

?unique
unique(df$country)

print(unique(df$country)) ##88 country

df2<-data.frame(unique(df$country))

df.table= df %>%
  group_by(year)%>%
  summarise(n=n())
mean(df.table$n)

df%>%
  mutate(df,success) %>%
  paste0(ifelse(df$result=="dies within a day after the attack", 1, 0))
df.3 =df%>%
  mutate(success= ifelse((grepl("dies",result)==TRUE),1,0))

df.3=df%>%
  mutate(success=grepl("dies",result))

df.4=df.3%>%
  mutate(mean(success))

?tapply
tapply(df.3$politybefore, df.3$success,mean)#most autocratic= safest


tapply(df.3$age, df.3$success,mean)#younger=safer

#Not that random

#WARBEFORE
df.4=df.3%>%
  mutate(warbefore=ifelse((df.3$interwarbefore|df.3$civilwarbefore),1,0))

tapply(df.4$success,df.4$warbefore,mean)
mean(df.4$warbefore)

?mutate

#WARAFTER
df.4=df.4%>%
  mutate(warafter=ifelse((df.4$interwarafter|df.4$civilwarafter),1,0))

tapply(df.4$warafter, df.4$success,mean)

tapply(df.4$success,df.4$polityafter)

#change in polity

mean(df.4$polityafter)-mean(df.4$politybefore)

#subset failure and success
df.success= subset(df.4,success==1)
mean(df.success$polityafter)-mean(df.success$politybefore)

df.failure= subset(df.4,success==0)
mean(df.failure$polityafter)-mean(df.failure$politybefore)

#try new variable
df.failure= df.failure%>%
  mutate(politychange=df.failure$polityafter-df.failure$politybefore)
?tapply
