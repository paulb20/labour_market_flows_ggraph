library(tidyverse)
library(rio)
library(glue)
library(readxl)
library(rvest)
library(lubridate)
library(scales)
library(ggraph)
library(tidygraph)
library(igraph)
library(ragg)
ons_month <- "aug2022"
ons_quarter <- "aug2022"



## Read file in from  ONS ----

#x02
x02_scrape <- read_html("https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/labourforcesurveyflowsestimatesx02")

html_nodes(x02_scrape, xpath=".//a[contains(@href, '.xls')]") %>% 
  html_attr("href") %>% 
  sprintf("https://www.ons.gov.uk%s", .) -> excel_links


download.file(excel_links, paste0("x02",ons_month, ".xlsx"), mode="wb", method="libcurl")


X02 <- import(paste0("x02",ons_quarter,".xlsx"), sheet="Labour market flows SA", skip=6) %>% 
  filter(!is.na(`Still in Employment`)) %>% 
  mutate(...1=str_trunc(...1, 12, side="right", ellipsis="")) %>% 
  separate(...1, into=c("month1", "month3"), sep="-", convert=TRUE) %>% 
  mutate(month= as.POSIXct(strptime(paste(1, .data$month3), "%d %b %Y"))) %>% 
  mutate(UE_HR = `Unemployment to Employment`/(`Still in Unemployment` + `Unemployment to Employment` + `Unemployment to Economic Inactivity`),
         NE_HR = `Economic Inactivity to Employment` / (`Still in Economic Inactivity` + `Economic Inactivity to Employment` + `Economic Inactivity to Unemployment`),
         J2J_HR = Levels / (`Still in Employment` + `Employment to Unemployment` + `Employment to Economic Inactivity`)) %>% 
  rename(`Job to job`=Levels, `Job to job HR`= Rates)

X02headline <- X02 %>% select(c(29,4,5,11,15,16,23,24)) %>% 
  pivot_longer(!month, names_to="Category", values_to="Index")
X02headline <- X02headline %>% filter(Category != "Unknown", !is.na(Index))
X02headline_ends <- group_by(X02headline, Category) %>% filter(month == max(month))

## Graph presentation ----

test <- X02headline_ends %>% ungroup() %>% 
  #filter(Quarter == "4") %>% 
  mutate(
    From=case_when(
      str_detect(Category,"^Employment")~"Employment",
      str_detect(Category,"^Unemployment")~"Unemployment", 
      str_detect(Category,"^Economic")~"Inactivity"),
    To= case_when(
      str_detect(Category, "Employment$")~"Employment",
      str_detect(Category, "Unemployment$")~"Unemployment",
      str_detect(Category, "Inactivity$")~"Inactivity")) %>% 
  select(From, To, Index) %>% 
  filter(!is.na(From), !is.na(To))


test3 <- tibble(From ="Employment", To="Employment", Index = pull(X02headline_ends[3,3]))
names(test3)<- names(test)
#test3 <- unnest(test3, cols=c(flows))
test2 <- bind_rows(test3, test)
test2 <- as_tbl_graph(test, directed=TRUE)
test2 <- activate(test2, edges)
test3 <- as_tbl_graph(test3)
test3 <- activate(test3, edges)
test4 <- graph_join(test2, test3)
test4 <- activate(test4, edges)

latest_loop <- ggraph(test4,layout="linear") + 
  geom_edge_loop(aes(width=Index,label=round(Index/1000,0), direction =180, alpha=stat(index)),
                 angle_calc = 'along',
                 label_dodge = unit(5, 'mm'),
                 #arrow = arrow(length = unit(1, 'mm')),
                 start_cap = circle(7, 'mm'),
                 end_cap = circle(7, 'mm'), show.legend=FALSE) + 
  geom_edge_arc(aes(width=Index,label=round(Index/1000,0), alpha=stat(index)),
                angle_calc = 'along',
                label_dodge = unit(-5, 'mm'),
                #arrow = arrow(length = unit(1, 'mm')),
                start_cap = circle(7, 'mm'),
                end_cap = circle(7, 'mm'), show.legend=FALSE) + 
  geom_node_text(aes(label=name)) + 
  theme_graph() + labs(title="Labour market flows, UK, Thousands", subtitle = "Latest quarter", caption="Data from ONS sheet X02")
agg_png(glue("X02loop.png"), width=20, height=12, unit="cm", res=300)
print(latest_loop)
dev.off()




