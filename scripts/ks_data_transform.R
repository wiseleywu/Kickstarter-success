#' ---
#' title: Data Compliation Script
#' ---

library(tidyverse)
library(chron)
library(zoo)
library(lubridate)
library(timeDate)
library(readr)
library(jsonlite)

# set seed for later
set.seed(123)

# read combined file if available, otherwise download, extract, and combine the data from the web.
if(file.exists('./data/kickstarter_combined_data.csv')) {
  df_master<-read_csv("data//kickstarter_combined_data.csv")
  df_master <- df_master[,-1]
} else {
  zip.file <- 'https://s3.amazonaws.com/weruns/forfun/Kickstarter/Kickstarter_2018-02-15T03_20_44_743Z.zip'
  filename=str_split(zip.file, pattern = '/')[[1]][7]
  destlink=paste('./data/', filename, sep='')
  download.file(zip.file, destfile=destlink)
  exdir=paste('./data/', str_split(filename, pattern='.zip')[[1]][1], sep='')
  unzip(destlink, exdir=exdir)
  fp<-list.files(exdir, full.names = TRUE)
  df_master<-do.call(rbind, lapply(fp, read_csv))
  write.csv(df_master,'/data/kickstarter_combined_data.csv')
}

#filter for only US countries 
#155376 count for Us Countries
df_master <- filter(df_master, country == 'US')

#139933 count for unique id 
df_master<-df_master[!duplicated(df_master$id),]


#convert unixtime to UTC datetime
df_master$created_at_dt <- as.POSIXct(df_master$created_at, origin="1970-01-01",tz="GMT")
df_master$launched_at_dt <- as.POSIXct(df_master$launched_at, origin="1970-01-01",tz="GMT")
df_master$deadline_dt <- as.POSIXct(df_master$deadline, origin="1970-01-01",tz="GMT")

#convert datetime to UTC date 
df_master$created_at_d <- as.Date(df_master$created_at_dt,tz="GMT")
df_master$launched_at_d <- as.Date(df_master$launched_at_dt,tz="GMT")
df_master$deadline_d <- as.Date(df_master$deadline_dt,tz="GMT")


###################Brenton: stock, holiday, date time data
df_stg_time <- df_master[,c("id","created_at_dt","launched_at_dt","deadline_dt","created_at_d","state","launched_at_d","deadline_d")]


##get day of week laucnhed at and deadline
df_stg_time$launched_at_day <- weekdays(as.Date(df_stg_time$launched_at_dt))
df_stg_time$deadline_day <- weekdays(as.Date(df_stg_time$deadline_dt))

#get hour
df_stg_time$launched_at_hour <- hour(df_stg_time$launched_at_dt)
df_stg_time$deadline_hour <- hour(df_stg_time$deadline_dt)

#get month
df_stg_time$launched_at_month<- month(df_stg_time$launched_at_dt,label=TRUE,abbr=TRUE)
df_stg_time$deadline_month <- month(df_stg_time$deadline_dt,label=TRUE,abbr=TRUE)

#Get time between created at launched at
df_stg_time$created_to_launched_days<- difftime(df_stg_time$launched_at_dt,df_stg_time$created_at_dt,units='days')
df_stg_time$launched_to_deadline_days<- difftime(df_stg_time$deadline_dt,df_stg_time$launched_at_dt,units='days')


#Get holiday
holiday_data <- read_csv("./data//holiday_data.csv")
holiday_data$holiday <- as.Date(holiday_data$holiday,format = '%m/%d/%y')
df_stg_time$deadline_holiday <- ifelse(df_stg_time$created_at_d %in% holiday_data$holiday,1,0)
df_stg_time$launched_at_holiday <- ifelse(df_stg_time$launched_at_d %in% holiday_data$holiday,1,0)

#Get stock, #if NA, then make it the previous date
sp500 <- read_csv("./data/sp500.csv")
sp500$Date <- as.Date(sp500$Date)
sp500 <- sp500[,c(1,5)]
sp500$close_previous_day <- lag(sp500$Close,1)
sp500$sp500_close_percent_change_launched_at <- (sp500$Close - sp500$close_previous_day)/ sp500$close_previous_day
sp500 <- sp500[,c(1,4)]

#Get all dates that are possible and fill the weekend with friday % change 
dim_date <- data.frame(Date = (seq(from = min(sp500$Date),to = max(sp500$Date),by = 'day')))
sp500<-left_join(dim_date,sp500,by = c('Date'))
#Fill in the first date as NA, since there is no kickstarter data, the NA is only there to replace the field with the weekend data
sp500$sp500_close_percent_change_launched_at <- c(NA,na.locf(sp500$sp500_close_percent_change_launched_at))


df_stg_time <- inner_join(df_stg_time,sp500,by=c('launched_at_d' = 'Date')) 
#colnames(df_stg_time)[which(names(df_stg_time) == "sp500_close_percent_change_launched_at")] <- "sp500_close_percent_change_deadline"


#df_stg_time <- inner_join(df_stg_time,sp500,by=c('deadline_d' = 'Date')) 

#Remove not needed columns
df_stg_time <- df_stg_time[, -c(2:8)]

#join time stg dataframe of holiday,datetime data to master
df_master <- inner_join(df_master,df_stg_time,by=c('id' = 'id')) 


#get unemployment
df_us_emp <- read_csv("./data/us_unemp.csv")
df_us_emp <- gather(df_us_emp,key='month',un_rate=Jan:Dec)
df_us_emp$date_emp<-as.Date(paste(df_us_emp$Year,df_us_emp$month,'1',sep='-'),format = '%Y-%b-%d')
df_us_emp <- df_us_emp[,c(3:4)]
colnames(df_us_emp) <-c('unemployment_rate','date_emp')
df_master$floor_launch_date_month <- floor_date(df_master$launched_at_d, unit="month")
df_master <- inner_join(df_master,df_us_emp,by=c('floor_launch_date_month' = 'date_emp'))

########################Sucheta Popularity stg
df_pop_stg <- read_csv("./data/weekly_search.csv")
df_pop_stg$date_popularity <- as.Date(df_pop_stg$date_popularity, '%Y-%m-%d')
df_pop_stg$popularity <- as.numeric(df_pop_stg$popularity)

#join on week date with Sunday being the floor date
df_master$launched_at_floor_week <- floor_date(df_master$launched_at_d, unit="week", week_start = getOption("lubridate.week.start", 7))
df_master <-inner_join(df_master,df_pop_stg, by =c('launched_at_floor_week' = 'date_popularity'))


######################## Wiseley Webscrap stg
if(file.exists("./data/final_df.rda")){
  load(file = './data/final_df.rda')
}else{
  source("webscrape_comm_rewards.R")
}
df_master<-inner_join(df_master,final_df,by=c('id' = 'id'))


#######################Aish category and location

# helper fucntion to get category form json string
getCategory <- function(x) {
  cate<-strsplit(fromJSON(x)$urls$web$discover, "categories/")[[1]][2]
  if(grepl('art',cate)) {
    cate <- 'art'
  }
  else if (grepl('comics',cate)) {
    cate <-'comics'
  }
  else if (grepl('crafts',cate)){
    cate <- 'crafts'
  }
  else if (grepl('dance',cate)){
    cate <- 'dance'
  }
  else if (grepl('design',cate)){
    cate <- 'design'
  }
  else if (grepl('fashion',cate)){
    cate <- 'fashion'
  }
  else if (grepl('film',cate)){
    cate <- 'film'
  }
  else if (grepl('food',cate)){
    cate <- 'food'
  }
  else if (grepl('games',cate)){
    cate <- 'games'
  }
  else if (grepl('journalism',cate)){
    cate <- 'journalism'
  }
  else if (grepl('music',cate)){
    cate <- 'music'
  }
  else if (grepl('photography',cate)){
    cate <- 'photography'
  }
  else if (grepl('publishing',cate)){
    cate <- 'publishing'
  }
  else if (grepl('technology',cate)){
    cate <- 'technology'
  }
  else if (grepl('theater',cate)){
    cate <- 'theater'
  }
  else{
    cate <- NA
  }
  return(cate)
}

df_master$category_clean<-unlist(lapply(lapply(df_master$category, getCategory),"[",1))


################Final transform 

#colnames(df_master)
#Keep  needed columns
#Convert to correct data types
df_master$goal <- as.numeric(df_master$goal)
df_master$category_clean <- as.factor(df_master$category_clean)
df_master$launched_at_month <- as.character(df_master$launched_at_month)
df_master$created_to_launched_days <- as.numeric(df_master$created_to_launched_days)
df_master$launched_to_deadline_days <- as.numeric(df_master$launched_to_deadline_days)
df_master$launched_at_holiday <- as.factor(df_master$launched_at_holiday)
df_master$deadline_holiday <- as.factor(df_master$deadline_holiday)
df_master$sp500_close_percent_change_launched_at <- as.numeric(df_master$sp500_close_percent_change_launched_at)
df_master$unemployment_rate <- as.numeric(df_master$unemployment_rate)
df_master$early.bird.frac <- as.numeric(df_master$early.bird.frac)
df_master$limited.frac <- as.numeric(df_master$limited.frac)
df_master$ship.intl <- as.factor(as.integer(as.logical(df_master$ship.intl)))
df_master$popularity <- as.numeric(df_master$popularity)
df_master$median.tier.cost <- as.numeric(df_master$median.tier.cost)
df_master$tier.low <- as.numeric(df_master$tier.low)
df_master$tier.med <- as.numeric(df_master$tier.med)
df_master$tier.high <- as.numeric(df_master$tier.high)
df_master$state <- as.factor(df_master$state)



df_initital_success <- select(df_master,c( 
  goal,
  category_clean,
  launched_at_month,
  created_to_launched_days,
  launched_to_deadline_days,
  launched_at_holiday,
  deadline_holiday,
  sp500_close_percent_change_launched_at,
  unemployment_rate,
  early.bird.frac,
  limited.frac,
  ship.intl,
  popularity,
  median.tier.cost,
  tier.low,
  tier.med,
  tier.high,
  state
))

#Create dummy variables
df_initital_success<-data.frame(model.matrix(state~., data=df_initital_success),state =df_initital_success$state )
df_initital_success<-df_initital_success[,2:ncol(df_initital_success)]

df_live_project <- filter(df_initital_success,state =='live')
df_initital_success <- filter(df_initital_success,state != 'live')
#convert state to binary sucessful or not
df_initital_success$state <- ifelse(df_initital_success$state == 'successful',1,0)
df_live_project$state <- ifelse(df_live_project$state == 'successful',1,0)

saveRDS(df_master, file="./rdata/df_master.rds")


spec = c(train = .80, test = .10, validate = .10)
data_split = sample(cut(seq(nrow(df_initital_success)), nrow(df_initital_success)*cumsum(c(0,spec)), labels = names(spec)))
final_datasets <- split(df_initital_success, data_split)

df_initital_success_train <- final_datasets$train
df_initital_success_validate <- final_datasets$validate
df_initital_success_test <- final_datasets$test

##Save files
saveRDS(df_initital_success_train, file="./rdata/df_initital_success_train.rds")
saveRDS(df_initital_success_validate, file="./rdata/df_initital_success_validate.rds")
saveRDS(df_initital_success_test, file="./rdata/df_initital_success_test.rds")
saveRDS(df_live_project, file="./rdata/df_live_project.rds")