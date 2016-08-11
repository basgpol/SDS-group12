########################################################################################
#############################  Exercise 2: I Paid a Bribe ##############################
########################################################################################

library("rvest")

link = "http://www.ipaidabribe.com/reports/paid#gsc.tab=0"
css.selector = ".heading-3 a"

bribe.links = link %>% 
  read_html(encoding = "UTF-8") %>% #inddrager special danish character 
  html_nodes(css = css.selector) %>%
  html_attr(name = 'href') #tager den attribut med navnet hret

## laver funktion som henter stilling og navn
scrape_bribe = function(link){
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

library("purrr")
bribe = bribe.links[1:2]  %>% 
  map_df(scrape_bribe)