library("readr")
gh.link = "https://raw.githubusercontent.com/"
user.repo = "sebastianbarfort/sds_summer/"
branch = "gh-pages/"
link = "data/bechdel.csv"
data.link = paste0(gh.link, user.repo, branch, link)
df = read_csv(data.link)

df.mutated= df %>%
  mutate(mean_male= count_male/count, mean_female= count_female/count)
df.mutated

df.status= df.mutated%>%
  mutate ()

status_print= function(x){
  status= vector()
  for(i in 1:7200){
    if(x[i]==0){ 
      status[i]="All female"} 
    if(x[i]==1){ 
      status[i]="All male"} 
    else{
      status[i]="Mixed"}
  }
  return(status)}    


gender = mutate(df.mutated,
                 Status= status_print(mean_male))

mean(gender[gender$Status=="All male", "gender$bechdel_test"],na.rm)

gender_mean=gender %>% 
  group_by(role, Status) %>% 
  summarise(mean_bech= mean(bechdel_test, na.rm=TRUE))

library(ggplot2)



gender_mean_coll= gender_mean
gender_mean_coll$merged= paste(gender_mean_coll$role, gender_mean_coll$Status, sep="-")
gender_mean_coll= filter(gender_mean_coll, role !="actsin")

ggplot(data = gender_mean_coll) + 
  geom_bar(mapping = aes(x = merged, y= mean_bech, fill=role), stat="identity")

