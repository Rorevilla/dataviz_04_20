library(dplyr)
library(magrittr)
library(scales)
library(lubridate)
library(ggplot2)
library(hrbrthemes)

Sys.setlocale("LC_ALL","English")


download.file(url = "https://github.com/dovedevic/memonavirus/archive/master.zip", destfile = "memonavirus-master.zip")
unzip(zipfile = "memonavirus-master.zip")


setwd(dir = "~/memonavirus-master/data/")

# examine the contents
logs<-list.files("~/memonavirus-master/data/raw",pattern="*infections*",full.names = TRUE)

for (file in logs){
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <-read.table(file, header=FALSE, sep="\t")
  }
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    tryCatch({
    temp_dataset <-read.table(file, header=FALSE, sep="\t")
    if (ncol(temp_dataset)>8){print(file)}
    dataset<-bind_rows(dataset,temp_dataset,.id="id")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

   
colnames(dataset)<- c("id","id2","datetime","infected_user","infected_comment","cause_user","cause","cause_type")
dataset <- dataset %>% mutate(fecha=as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S"))
dataset<-dataset %>% group_by(infected_user) %>% filter(fecha==min(fecha)) %>% slice(1) %>% ungroup()
dataset2<- dataset
dataset <- dataset%>% group_by(cause_type,hora=floor_date(fecha, "12 hours")) %>% arrange(cause_type,hora) %>%  summarise(infection_no=n()) %>% ungroup()
dataset<-dataset %>% group_by(cause_type)%>% mutate(cumsum=cumsum(infection_no))

ggplot(dataset)+
  geom_line(aes(x=hora,y=cumsum,color=cause_type),size=1)+
  scale_x_datetime(breaks = seq(min(dataset$hora),max(dataset$hora)+ hours(3),by="7 days"), date_labels = "%b %d")+
  scale_y_continuous(labels = comma,breaks = seq(0,140000,20000))+
  theme_ipsum_rc()+
  labs(x="Date",y="Number of infected users",title="Spread of Memonavirus over Reddit")
  


comment_post <- read.csv2("C:/Users/Revi/Desktop/DataVizApril2020/Comment-Post/comments2_all.csv",sep=",",header=TRUE)
dataset2 <- dataset2 %>% left_join(comment_post, by=c("infected_comment"))
unique(comment_post$infected_comment) %>% data.frame() %>% write.csv2("comment_post.csv",row.names = FALSE)

