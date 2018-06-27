# serve trained model to the public

library(rvest)
library(xgboost)
library(jsonlite)
library(V8)
library(lubridate)
library(dplyr)
library(httr)

# get time feature
getTimeFeature<-function(x){
  created_at_dt<-as.POSIXct(x$created_at, origin="1970-01-01",tz="GMT")
  launched_at_dt<-as.POSIXct(x$launched_at, origin="1970-01-01",tz="GMT")
  deadline_dt<-as.POSIXct(x$deadline, origin="1970-01-01",tz="GMT")
  created_to_launched_days<- as.numeric(difftime(launched_at_dt, created_at_dt,units='days'))
  launched_to_deadline_days<- as.numeric(difftime(deadline_dt, launched_at_dt,units='days'))
  return(c(created_to_launched_days, launched_to_deadline_days))
}

# function to get pledge tier info
getRewards<-function(x){
  page<-read_html(x)
  
  # get script data
  script<-page %>% html_nodes("script") %>% html_text(trim=TRUE)
  script_ind <- grep("window.current_ip",script)
  script<-script[script_ind]
  ctx <- v8(global="window")
  ctx$eval(script)
  current<-fromJSON(gsub('&quot;','"',ctx$get("window.current_project")))
  goal<-current$goal
#  goal<-getPledgeAmnt(page %>% html_node('.lh3-lg .money') %>% xml_text())
#  if(is.na(goal)){
#    goal<-getPledgeAmnt(page %>% html_node('.mb0 .money') %>% xml_text())
#  }
  category<-getCategory(current$category$slug)
  money<-unlist(lapply(page %>% html_nodes('.pledge__amount .money') %>% xml_text(),
                       getPledgeAmnt),use.names = FALSE)
  count<-unlist(lapply(page %>% html_nodes('.pledge__backer-count') %>% xml_text(),
                       function(x){return(strtoi(gsub("[\n backers ,]","",x)))}),
                use.names = FALSE)
  ebf<-sum(unlist(lapply(page %>% html_nodes('.pledge__title') %>% xml_text(),
                         function(x){return(grepl('early|bird',tolower(gsub("[\n]","",x))))})))/length(count)
  lf <- length(page %>% html_nodes('.pledge__limit') %>% html_nodes('.pledge__limit')) / length(count)
  inl<- as.integer("Anywhere in the world" %in% (page %>% html_nodes('.pledge__detail-info') %>% html_text()))
  return(list(money, count, ebf, lf, inl, goal, category, current))
}

# function to extract pledge amount
getPledgeAmnt<-function(text) {
  ind<-gregexpr(pattern='$', fixed = TRUE, text=text)[[1]][1]
  subtext<-substring(text, ind+1)
  subtextr<-gsub(",","",subtext)
  return(strtoi(subtextr))
}

# get tier level
getTierLevel <- function(x) {
  if(length(x) == 0) {
    return(c(0,0,0))
  }
  else {
    low<-sum(x <= 100)
    med<-sum(x > 100 & x <= 1000)
    high<-sum(x > 1000)
    return(c(low, med, high)) 
  }
}

getCategory <- function(x) {
  df<-data.frame(category_cleancomics=0, category_cleancrafts=0, category_cleandance=0, category_cleandesign=0,
                 category_cleanfashion=0, category_cleanfilm=0, category_cleanfood=0, category_cleangames=0,
                 category_cleanjournalism=0, category_cleanmusic=0, category_cleanphotography=0,
                 category_cleanpublishing=0, category_cleantechnology=0, category_cleantheater=0)
  cate<-x
  if(grepl('art',cate)) {
    # art is when all categories are 0
  }
  else if (grepl('comics',cate)) {
    df$category_cleancomics<-1
  }
  else if (grepl('crafts',cate)){
    df$category_cleancrafts<-1
  }
  else if (grepl('dance',cate)){
    df$category_cleandance<-1
  }
  else if (grepl('design',cate)){
    df$category_cleandesign<-1
  }
  else if (grepl('fashion',cate)){
    df$category_cleanfashion<-1
  }
  else if (grepl('film',cate)){
    df$category_cleanfilm<-1
  }
  else if (grepl('food',cate)){
    df$category_cleanfood<-1
  }
  else if (grepl('games',cate)){
    df$category_cleangames<-1
  }
  else if (grepl('journalism',cate)){
    df$category_cleanjournalism<-1
  }
  else if (grepl('music',cate)){
    df$category_cleanmusic<-1
  }
  else if (grepl('photography',cate)){
    df$category_cleanphotography<-1
  }
  else if (grepl('publishing',cate)){
    df$category_cleanpublishing<-1
  }
  else if (grepl('technology',cate)){
    df$category_cleantechnology<-1
  }
  else if (grepl('theater',cate)){
    df$category_cleantheater<-1
  }
  else{
    # if none of above, default to art
  }
  return(df)
}

# get unemployment rate
getUnemploymentRate<-function(datetime){
  page <- read_html('https://data.bls.gov/timeseries/LNS14000000')
  tbs <- page %>% html_table(fill=TRUE)
  ind <- grep('Year', tbs)
  tb <- tbs[[ind]]
  value <- NA
  datetime<-as.POSIXct(datetime, origin="1970-01-01",tz="GMT")
  datetime <- datetime %m+% months(1)
  while (is.na(value)) {
    datetime <- datetime %m-% months(1)
    month<-months(datetime, TRUE)
    year <- format(datetime, "%Y")
    value<- as.numeric(tb %>% filter(Year == year) %>% select(month))
  }
  return(value)
}

getProjectData<-function(link) {
  webscrape<-getRewards(link)
  df<-webscrape[[7]]
  df$goal<-webscrape[[6]]
  timefeature<-getTimeFeature(webscrape[[8]])
  df$created_to_launched_days<-timefeature[1]
  df$launched_to_deadline_days<-timefeature[2]
  df$unemployment_rate<-getUnemploymentRate(webscrape[[8]]$launched_at)
  pledge.tier<-webscrape[[1]]
  pledge.tier.count<-webscrape[[2]]
  df$early.bird.frac<-webscrape[[3]]
  df$limited.frac<-webscrape[[4]]
  df$ship.intl1<-webscrape[[5]]
  df$median.tier.cost<-median(pledge.tier)
  tier.level<-getTierLevel(pledge.tier)
  df$tier.low<-tier.level[1]
  df$tier.med<-tier.level[2]
  df$tier.high<-tier.level[3]
  return(df)
}

getProjectPrediction <- function(link) {
  if(validateLink(link)) {
    out <- tryCatch(
      {
        col.order<-readRDS("./www/col.order")
        df<-getProjectData(link)[col.order]
        xdf<-xgb.DMatrix(as.matrix(df))
        xgb_model <- readRDS("./www/webserve.model")
        pred<-predict(xgb_model, xdf)
      },
      error=function(cond) {
        return(FALSE)
      }
    )    
    return(out)
  } else {
    return(FALSE)
  }
}



validateLink<-function(link) {
  return(grepl("https://www.kickstarter.com/", link))
}