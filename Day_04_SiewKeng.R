library("dplyr")
library("readxl")
library("ggplot2")
library("stringr")
library("lubridate")
library("data.table")

setwd("D:/ADAPT")

df_Data = data.frame()
Port_code = data.frame()

path="D:/ADAPT"
#load ADAPT_Sample_dataset*.csv into df_Data 
myfiles = dir(path)

for (file in myfiles)
{
  if(grepl("csv",file))
  {
    indata<- fread(file,check.names = FALSE, stringsAsFactors = FALSE)
    df_Data<-rbind(df_Data,indata)
  }

}


#load Portcodes Dictionary.xlsx into Port_code 
Port_code<-read_excel("Portcodes Dictionary.xlsx")


##R graphs
hist(df_Data$DWELL_DAYS,
main="DWELL DAYS", 
xlab="Days", 
border="blue", 
col="green",
las=1, 
breaks=5)


boxplot(df_Data$DWELL_DAYS)
plot(as.numeric(as.factor(df_Data$CNTR_FOE)),df_Data$DWELL_DAYS)


#Group by, sorting ....
df_Data %>% group_by(POL) %>% 
  summarise(CNTR_WGT_KG_mean = mean(CNTR_WGT_KG)) %>% 
  arrange(CNTR_WGT_KG_mean) %>% 
  top_n(wt=CNTR_WGT_KG_mean, n=5)


#Reshape 
df_Data_Cntr_Wgt_Grp<-df_Data %>% group_by(POL,CNTR_FOE) %>% 
  summarise(CNTR_WGT_KG_mean = mean(CNTR_WGT_KG))

df_Data_Cntr_Wgt_Grp_Wide<-dcast(df_Data_Cntr_Wgt_Grp,POL ~ CNTR_FOE, value=CNTR_WGT_KG_mean)


#filter
df_Data %>% filter(POL == "IDBUN")
df_Data %>% filter(POL %in% c("IDBUN","SGSIN"))
df_Data_IDBUN<-df_Data[df_Data$POL=="IDBUN",]


df_Data %>% filter(grepl(pattern ="BUN",ignore.case = TRUE, x =POL))
filter(df_Data, grepl("BUN", POL, fixed = TRUE))
df_Data[POL %like% 'BUN'] #works but has warning


#Sample
df_Data %>% sample_n(5)
sample(df_Data, 5, replace = FALSE, prob = NULL)

df_Data %>% sample_frac(.05)
sample(df_Data, as.integer(nrow(df_Data)*0.05), replace = TRUE, prob = NULL)



#Merge
df_Data_Merge<-merge(x = df_Data, y = Port_code, by.x = 'POL',by.y='PORT CODE', all.x = TRUE)
names(df_Data_Merge)[names(df_Data_Merge)== "COUNTRY CODE"]="POL_COUNTRY_CODE"
names(df_Data_Merge)[names(df_Data_Merge)== "PORT NAME"]="POL_PORT_NAME"
names(df_Data_Merge)[names(df_Data_Merge)== "COUNTRY NAME"]="POL_COUNTRY_NAME"
names(df_Data_Merge)[names(df_Data_Merge)== "City Name"]="POL_CITY_NAME"


df_Data_Merge<-merge(x = df_Data_Merge, y = Port_code, by.x = 'POD',by.y='PORT CODE', all.x = TRUE)
names(df_Data_Merge)[names(df_Data_Merge)== "COUNTRY CODE"]="POD_COUNTRY_CODE"
names(df_Data_Merge)[names(df_Data_Merge)== "PORT NAME"]="POD_PORT_NAME"
names(df_Data_Merge)[names(df_Data_Merge)== "COUNTRY NAME"]="POD_COUNTRY_NAME"
names(df_Data_Merge)[names(df_Data_Merge)== "City Name"]="POD_CITY_NAME"


#Duplicate
df_Data_Merge[duplicated(df_Data[,c("CNTR_N",
                                    "CNTR_WGT_KG",
                                    "POL",
                                    "POD",
                                    "LOAD_VSL_NAME",
                                    "DISC_VSL_NAME")])]

df_Data_Merge[!duplicated(df_Data[,c("CNTR_N",
                                    "CNTR_WGT_KG",
                                    "POL",
                                    "POD",
                                    "LOAD_VSL_NAME",
                                    "DISC_VSL_NAME")])]

df_Data_Merge %>% distinct(CNTR_N,
                           CNTR_WGT_KG,
                           POL,
                           POD,
                           LOAD_VSL_NAME,
                           DISC_VSL_NAME, .keep_all = TRUE)


#ggplot
df_Data %>% ggplot(aes(CNTR_FOE,CNTR_WGT_KG))+geom_boxplot()
df_Data %>% ggplot(aes(CNTR_WGT_KG, color=CNTR_FOE)) + geom_histogram(binwidth = 500)


#mtcars

mtcar

##EDA mtcars
View(mtcars)
dim(mtcars)

str(mtcars)
sapply(mtcars, class)
summary(mtcars)

colSums(is.na(mtcars))
rowSums(is.na(mtcars))


##Convert col "AM"
mtcars$am=factor(mtcars$am,labels=c("Automatric","manual"))


##Stats
attach(mtcars)
summary(mtcars)
IQR(mpg)
quantile(mpg)
mean(mpg)
median(mpg)
var(mpg)
sd(mpg)
cor(mtcars)

tabulate(match(am,unique(am)))

getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}


getmode(am)

##graph
ggplot(mtcars,aes(am))+geom_bar()
ggplot(mtcars,aes(mpg)) + geom_histogram(binwidth = 5)
ggplot(mtcars,aes(mpg, color=am)) + geom_histogram(binwidth = 5)
ggplot(mtcars,aes(mpg,wt)) + geom_point()


##correclation
cov(mpg,wt)
cor(mpg,wt)
cor.test(mpg,wt,method = "pearson")

View(cor(mtcars[ , -which(names(mtcars) %in% c("am"))]))
df_car_cor<-cor(mtcars[ , -which(names(mtcars) %in% c("am"))])



#Welch T test
t.test(mpg ~ am,var.equal=FALSE)

#Student T test
t.test(mpg ~ am,var.equal=TRUE)


#test normal
qqnorm(mpg)
qqplot(mpg,am)


#linear
lm.mpg_am<-lm(mpg ~ am)
summary(lm.mpg_am)

lm.mpg_wt<-lm(mpg ~ wt)
predict(lm.mpg_wt,c(3),interval = "confidence")



