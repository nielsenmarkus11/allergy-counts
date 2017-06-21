### Pull the public facebook page allergy count data from sandy allergy clinic
library(Rfacebook)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(lubridate)
library(reshape2)

source('fbauth.R')

fb_oauth <- fbOAuth(app_id=app_id, app_secret=app_secret)

pages <- searchPages(string="intermountain", token=fb_oauth)
pages[grep("sandy",pages$name,ignore.case = T),]

allergy <- getPage(page="sandyclinicallergy", token = fb_oauth,n=1500)

# Filter posts to only include those that have "pollen count" in the message text
counts <- allergy%>%
  select(message,created_time)%>%
  filter(grepl("pollen count",message,ignore.case=T))

# Parse the date from the message
datecuts <-regexpr("\\d+\\/\\d+\\/\\d+|\\w+ \\d+, \\d+",counts$message,perl=T)
dates <- ifelse(datecuts==-1,NA,substr(counts$message,datecuts,datecuts + attr(datecuts, "match.length")-1))
dates.new <- ifelse(nchar(dates)==8,as.Date(strptime(dates,"%m/%d/%y")),as.Date(strptime(dates,"%B %d, %Y")))

# Now parse the remainder of the text for pollen counts
pollen <- ifelse(datecuts==-1,counts$message,substr(counts$message,datecuts + attr(datecuts, "match.length"),nchar(counts$message)))
pollen.split <- gsub(" \\(.*?\\)","",gsub("\n"," ",iconv(pollen,"latin1", "ASCII",sub=".")))
pollen.split <- gsub("tune\\.\\.\\.","",pollen.split)
pollen.split <- gsub("moderate high","high",pollen.split,ignore.case = T)
pollen.split <- gsub("extra","very",pollen.split,ignore.case = T)

#convert levels to numbers
pollen.split <- gsub("very high","5",pollen.split,ignore.case = T)
pollen.split <- gsub("very low","1",pollen.split,ignore.case = T)
pollen.split <- gsub("extra low","1",pollen.split,ignore.case = T)
pollen.split <- gsub("\\<high\\>","4",pollen.split,ignore.case = T)
pollen.split <- gsub("\\<moderate\\>","3",pollen.split,ignore.case = T)
pollen.split <- gsub("\\<low\\>","2",pollen.split,ignore.case = T)

#remove extra spaces after ellipses and other text
pollen.split <- gsub("mold: ","mold\\.\\.\\.",pollen.split,ignore.case=T)
pollen.split <- gsub("grass: ","grass\\.\\.\\.",pollen.split,ignore.case=T)
pollen.split <- gsub("(\\b)(\\.\\.\\.\\s+)","\\1\\.\\.\\.",pollen.split)
pollen.split <- gsub("trees","",pollen.split,ignore.case = T)
pollen.split <- gsub("weeds\\.\\.\\.\\d","",pollen.split,ignore.case = T)
pollen.split <- gsub("weeds","",pollen.split,ignore.case = T)
pollen.split <- gsub("lindon","linden",pollen.split,ignore.case = T)
pollen.split <- gsub("grasses","grass",pollen.split,ignore.case = T)
pollen.split <- gsub("chenopods","chenopod",pollen.split,ignore.case = T)
pollen.split <- gsub("\\*chenopod","chenopod",pollen.split,ignore.case = T)
pollen.split <- gsub("\\*sagebrush","sagebrush",pollen.split,ignore.case = T)
pollen.split <- gsub("catttail","cattail",pollen.split,ignore.case = T)
pollen.split <- gsub("maple\\/boxelder","maple",pollen.split,ignore.case = T)
pollen.split <- gsub("elder\\/maple","maple",pollen.split,ignore.case = T)
pollen.split <- gsub("elder","maple",pollen.split,ignore.case = T)
pollen.split <- gsub("cedar\\/juniper","cedar",pollen.split,ignore.case = T)
pollen.split <- gsub("cottonwood\\/aspen","cottonwood",pollen.split,ignore.case = T)
pollen.split <- gsub("aspen\\/cottonwood","cottonwood",pollen.split,ignore.case = T)
pollen.split <- gsub("(maple\\/mulberry)(\\.\\.\\.\\d)","maple\\2 mulberry\\2",pollen.split,ignore.case = T)
pollen.split <- gsub("juniper","cedar",pollen.split,ignore.case = T)
pollen.split <- gsub("ginko","ginkgo",pollen.split,ignore.case = T)
pollen.split <- gsub("gingko","ginkgo",pollen.split,ignore.case = T)
pollen.split <- gsub("sagebrush","sage",pollen.split,ignore.case = T)
pollen.split <- gsub("sage","sagebrush",pollen.split,ignore.case = T)
pollen.split <- gsub("walnt","walnut",pollen.split,ignore.case = T)
pollen.split <- gsub("cattalil","cattail",pollen.split,ignore.case = T)
pollen.split <- gsub("\\. ","\\.",pollen.split,ignore.case = T)
pollen.split <- gsub("(\\b)(\\.+)(\\d)","\\1\\.\\.\\.\\3",pollen.split,ignore.case = T)
pollen.split <- gsub("(\\d)\\.\\.\\.cedar","cedar\\.\\.\\.\\1",pollen.split,ignore.case = T)



# Clean up levels
pollen.split <- gsub("5!","5",pollen.split,ignore.case = T)
pollen.split <- gsub("4!!!!","4",pollen.split,ignore.case = T)
pollen.split <- gsub("4!","4",pollen.split,ignore.case = T)
pollen.split <- gsub("(\\d)(-|\\()relative","\\1",pollen.split,ignore.case = T)
pollen.split <- gsub("(\\d)(\\(|\\r)","\\1",pollen.split,ignore.case = T)
pollen.split <- gsub("(\\d)\\r","\\1",pollen.split,ignore.case = T)



clean.split <- strsplit(pollen.split,split=" ")
my.reduce <- function(x) x[grepl("\\.\\.\\.",x) & !grepl("none",x,ignore.case=T)]
clean.split2 <- lapply(clean.split,my.reduce)

myclean <- function(x) {
  if(length(x)>0){
    myx<-unlist(strsplit(x,"\\.\\.\\."))
    myx<-myx[myx!=""]
    mymat <- matrix(myx,ncol=2,byrow=T)
    mymat[,2]<- gsub("\\.","",mymat[,2])
    return(mymat)
  }
  else mymat <- NULL
}

final.clean <-lapply(clean.split2,myclean)

#get list of all pollen sources
mynames <- function(x) x[,1]
unique.names <-unique(tolower(unlist(lapply(final.clean,mynames))))
# Check to see if there are any funny names or numbers.
unique.names[order(unique.names)]

mylevels <- function(x) x[,2]
unique.counts <-unique(tolower(unlist(lapply(final.clean,mylevels))))
# Check to see if there are any funny numbers.
unique.counts[order(unique.counts)]

# populate the data now
counts[unique.names] <- 0

for (i in 1:length(final.clean)){
  print(i)
  for (j in unique.names){
    if (!is.null(final.clean[[i]])) {
      myrow <- grepl(j,tolower(final.clean[[i]][,1]))
      if(sum(myrow)>0){
        tmp <- as.numeric(final.clean[[i]][which(myrow),2])
        counts[i,j] = mean(tmp)
      }
    }
  }
}

counts$created_time <- as.Date(substr(counts$created_time,1,10))

# remove weed since it is too general
counts$`maple/mulberry`<-NULL
counts$weed<-NULL


trees <- c("alder","ash","beech","birch","cedar","chestnut","cottonwood",
           "elm","ginkgo","linden","maple","mulberry","oak","olive","pecan",
           "pine","privet","sycamore","walnut","willow")
# counts$trees <- rowMeans(counts[,trees], na.rm = TRUE)
counts$trees <- apply(counts[,trees],1,max,na.rm = TRUE)

weeds <- c("cattail","chenopod","dock","ephedra","hemp",
           "nettle","plantain","ragweed","sagebrush","sedge")
# counts$weeds <- rowMeans(counts[,weeds], na.rm = TRUE)
counts$weeds <- apply(counts[,weeds],1,max,na.rm = TRUE)

describe(counts)

final <- melt(counts[,-1],id="created_time")

# get day of week and week of year
final$day <- ordered(weekdays(final$created_time),
                      levels=rev(c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")))

final$week <- lubridate::week(final$created_time)
final$year <- year(final$created_time)

# Now plot pollen counts over time
all.trees <- final[final$variable %in% trees,]
ggplot(all.trees,aes(week,day,fill=value)) + geom_tile() + 
  facet_grid(variable~year) + 
  scale_fill_gradient(low = "white", high = "blue") 

all.weeds <- final[final$variable %in% weeds,]
ggplot(all.weeds,aes(week,day,fill=value)) + geom_tile()  + 
  facet_grid(variable~year) + 
  scale_fill_gradient(low = "white", high = "blue") 

pdf("img/overall-trend.pdf",width=11.5, height=7)
summary <- final[final$variable %in% c("grass","mold","trees","weeds"),]
ggplot(summary,aes(week,day,fill=value)) + geom_tile() +
  facet_grid(year~variable) + 
  scale_fill_gradient(low = "white", high = "blue", breaks=0:5,
                      labels=c("None","Very Low","Low","Moderate","High","Very High")) +
  labs(title="Seasonal Allergies Pollen Trends",
       subtitle="By Pollen Type and Year",
       caption="Source: Intermountain Allergy & Asthma",
       x="Week", y="", fill = "Pollen Count")
dev.off()

