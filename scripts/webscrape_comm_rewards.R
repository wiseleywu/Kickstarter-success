library(rvest)
library(jsonlite)

######## definte helper function ########

# try-catch wrapper to read community page
readUrl <- function(url) {
  out <- tryCatch(
    {
      getBacker(url)
    },
    error=function(cond) {
      return(NA)
    }
  )    
  return(out)
}

# function to get pledge tier info
getRewards<-function(x){
  page<-read_html(x)
  money<-unlist(lapply(page %>% html_nodes('.money') %>% xml_text(),
                       function(x){return(strtoi(gsub("[$ US ,]","",x)))}),
                use.names = FALSE)
  count<-unlist(lapply(page %>% html_nodes('.pledge__backer-count') %>% xml_text(),
                       function(x){return(strtoi(gsub("[\n backers ,]","",x)))}),
                use.names = FALSE)
  #pledge_df <- data.frame(pledge=money, count=count)
  ebf<-sum(unlist(lapply(page %>% html_nodes('.pledge__title') %>% xml_text(),
                         function(x){return(grepl('early|bird',tolower(gsub("[\n]","",x))))})))/length(count)
  lf <- length(page %>% html_nodes('.pledge__limit') %>% html_nodes('.pledge__limit')) / length(count)
  inl<- "Anywhere in the world" %in% (page %>% html_nodes('.pledge__detail-info') %>% html_text())
  return(list(money, count, ebf, lf, inl))
}

# function to get number of new/existing backers
getBacker<-function(x){
  page<-read_html(x)
  new<-strtoi(gsub("[\n ,]", "", page %>% html_node('.new-backers') %>% html_node('.count') %>% xml_text()))
  exist<-strtoi(gsub("[\n ,]", "", page %>% html_node('.existing-backers') %>% html_node('.count') %>% xml_text()))
  return(list(new,exist))
}

# replace NA value from the community webscrape with default value (# of backers)
replaceNA<-function(row) {
  if (is.na(row['new.backers'])){
    return(c(row['backers_count'],0))
  }
  else {
    return(c(row['new.backers'], row['exist.backers']))
  }
}

# fix some bad webscrapping
replaceBadValue<-function(row) {
  if(row['pledge.tier.na'] == TRUE) {
    return(getRewards(as.character(row['rewards_html']))[[1]])
  }
  else {
    return(row['pledge.tier'])
  }
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

# get empty tier
getEmptyTier <- function(x) {
  if(is.null(x)) {
    return(0)
  }
  else {
    return(sum(x == 0))
  }
}

# get popular tier
getPopularTier <- function(row) {
  if(is.null(unlist(row['pledge.tier.count']))) {
    return(0)
  }
  else if(length(unlist(row['pledge.tier.count']))==1) {
    return(0)
  }
  else if(sum(unlist(row['pledge.tier.count']))==0) {
    return(0)
  }
  else {
    mi <- which.max(as.numeric(unlist(row['pledge.tier.count'])))
    return(as.numeric(unlist(row['pledge.tier']))[mi])
  }
}

######## webcrapping starts here ########

# define column dtype and skip useless columns
colClasses=c("NULL", "integer", rep("character",2),"numeric","factor","integer","character","factor","character","factor","factor","integer", "factor", "numeric", "numeric", "numeric", "factor", "integer", rep("character",2), "numeric", rep("character", 3), rep("factor",2), "factor", "integer", "numeric", "character", "numeric", "factor", "character", rep("NULL",4))

# read in csv with colClasses
df<-read.csv('./data/kickstarter_combined_data.csv', colClasses = colClasses)

# remove non-US and duplicated entry
df <- df %>% filter(country=='US') %>% distinct(id, .keep_all = TRUE)

# get rewards and community page
df$rewards_html<-apply(df, 1, function(x){return(fromJSON(x['urls'])$web$rewards)})
df$community_html<-apply(df, 1, function(x){return(gsub("/rewards","/community",fromJSON(x['urls'])$web$rewards))})

# using lapply to webscrape community page
x<-lapply(df$community_html, readUrl)
df$new.backers<-as.numeric(unlist(lapply(x,"[",1)))
df$exist.backers<-as.numeric(unlist(lapply(x,"[",2)))

# using lapply to webscrape rewards page
x<-lapply(df$rewards_html, getRewards)
df$pledge.tier<-lapply(lapply(x,"[",1),unlist)
df$pledge.tier.count<-lapply(lapply(x,"[",2),unlist)
df$early.bird.frac<-as.numeric(unlist(lapply(x,"[",3)))
df$limited.frac<-as.numeric(unlist(lapply(x,"[",4)))
df$ship.intl<-as.logical(unlist(lapply(x,"[",5)))

# fill in NA values
df<- df %>% mutate(early.bird.frac =if_else(is.na(early.bird.frac), 0, early.bird.frac))
df<- df %>% mutate(limited.frac =if_else(is.na(limited.frac), 0, limited.frac))

# create new columns
df$tier.level<-unlist(lapply(df$pledge.tier, length))
df$median.tier.cost<-lapply(df$pledge.tier, median)
df<- df %>% mutate(median.tier.cost=if_else(is.na(median.tier.cost), 0, as.numeric(median.tier.cost)))

t<-lapply(df$pledge.tier, getTierLevel)
df$tier.low<-unlist(lapply(t,"[",1),unlist)
df$tier.med<-unlist(lapply(t,"[",2),unlist)
df$tier.high<-unlist(lapply(t,"[",3),unlist)

t<-lapply(df$pledge.tier.count, getEmptyTier)
df$empty.tier<-unlist(lapply(t,"[",1),unlist)

t<-apply(dropdf, 1, getPopularTier)
df$popular.tier<-unlist(lapply(t,"[",1),unlist)

# output file
final_df <- df
save(final_df, 'final_df.rda')