########################################################################################
#############################  Exercise 2: I Paid a Bribe ##############################
########################################################################################
install.packages("rvest")
library("rvest")

link.first.part = "http://www.ipaidabribe.com/reports/paid?page="
change.number = c((0:20)*10) # make vector with the numbers from 0:200 (in intervals of 10)
link.last.part = "#gsc.tab=0"

## vektor with all links
all.links = paste0(link.first.part, change.number, link.last.part)

## selector to each rapport
css.selector = ".heading-3 a"

## crating a function to find all link to all rapports
path_briberapport = function(link){
  output = link %>% 
    read_html(encoding = "UTF-8") %>% #inddrager special danish character 
    html_nodes(css = ".heading-3 a") %>% # html to link to each rapport
    html_attr(name = 'href')
  return(output)
}

## collecting the links to all 200 rapports
library(purrr)
link.rapports = all.links[1:21] %>% 
  map(path_briberapport)
link.rapports = unlist(link.rapports) #change from list to one big vector
link.rapports # show all the links


## create function that collects alle the wanted info
scrape_bribe_info = function(link){
  my.link = link %>% 
    read_html(encoding = "UTF-8")
  title = my.link %>% 
    html_nodes(".heading-3 a") %>% 
    html_text()
  title = title[1]
  amount = my.link %>% 
    html_nodes(".details .paid-amount span") %>% 
    html_text() 
  amount = amount[1]
  name_dep = my.link %>% 
    html_nodes(".name a") %>% 
    html_text() 
  name_dep = name_dep[1]
  trans_detail = my.link %>% 
    html_nodes(".body-copy-lg") %>% 
    html_text() 
  trans_detail = trans_detail[1]
  views = my.link %>% 
    html_nodes(".overview .views") %>% 
    html_text() 
  views = views[1]
  city = my.link %>% 
    html_nodes(".location") %>% 
    html_text() 
  city = city[1]
  return(data.frame(Title = title,
                    Amount = amount,
                    Name.of.Department = name_dep,
                    Transactions.details = trans_detail,
                    Views = views,
                    City = city))
}

##  create dataframe by applying the scrape function 
df_bribe = link.rapports[1:200]  %>% 
  map_df(scrape_bribe_info)

View(df_bribe) # Show the final dataframe
