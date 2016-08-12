########################################################################################
##################################### Exercise 1 #######################################

library("readr")

## Opload data
library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "sebastianbarfort/sds_summer/"
branch = "gh-pages/"
link = "data/bechdel.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)

names(df)

## 1) Read the article and discuss in groups whether you find the argument and visualizations convincing


## 2) Generate the following new variables
library("dplyr")
df2 = mutate(df, 
             mean.male = count_male / count, 
             mean.female = count_female / count
)
View(df2)

statusfct = function(x) {
  status = vector()
  for (i in 1:7200) {
    if (x[i]==0) {
      status[i] = "All female"}
    if (x[i]==1) {
      status[i] = "All male"}
    else {
      status[i] = "Mixed"}
  }
  return (status)}

df3 = mutate(df2, 
             status = statusfct(mean.male))

## 3) Grouped operations: Discuss how best to investigate how the Bechdel score varies by the gender of the director/writer. 
#Write R code to carry out your ideas and visualize the results.

mean(df3$bechdel_test, 
     if(status = All male))  

mean(df3[df3$status=="All male","bechdel_test"], na.rm = TRUE)

##Splitter efter role og kønsstatus og udregner derefter gennemsnit
df_split = df3 %>% 
  group_by(role, status) %>% 
  summarise(mean_bech = mean(bechdel_test, na.rm = TRUE))

## remove actors-roles
df_noactors = filter(df_split, role != "actsin")

## samler status og gennemsnit i en kolonne
df_tilgraf = df_noactors
df_tilgraf$merged = paste(df_tilgraf$role, df_tilgraf$status, sep = ": ")

## visualiserer det
library(ggplot2)

ggplot(data = df_tilgraf) + 
  geom_bar(mapping = aes(x = merged, y = mean_bech, fill = role, color = role), stat = "identity") +
    labs(y = "Bechdel average", x = "Directors and writers gender", title = "Women give higher Bechdel average")
