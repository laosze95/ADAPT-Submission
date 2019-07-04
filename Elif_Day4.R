setwd("C:/Users/edursun/Desktop/ADAPT/ADAPT_3")
df_Data <- read.csv(file="C:/Users/edursun/Desktop/ADAPT/ADAPT_3/ADAPT_data_for_day3.csv",
                  header=TRUE, sep=",")
library(ggplot2)
hist(df_Data$DWELL_DAYS)
boxplot(df_Data$DWELL_DAYS)
plot(df_Data$CNTR_WGT_KG,df_Data$DWELL_DAYS)

#CNTR_WGT BY POL # CNTR_WGT AND ASCENDING TOP 5

library(dplyr)


a=df_Data %>%
  group_by(POL) %>%
  summarise(avg=mean(CNTR_WGT_KG)) %>% arrange(avg) %>% top_n(wt=avg,n=5)

#GROUP BY POL & CNTR_FOE

b= df_Data %>%
  group_by(POL,CNTR_FOE) %>%
  summarise(avg=mean(CNTR_WGT_KG))

library(reshape2)

b

# DCAST by POL&CNTR_FOE DCAST=LONG FORMAT TO WIDE FORMAT
DCAST_TABLE= dcast(b, POL ~ CNTR_FOE, value.var = "avg")




library(dplyr)

# VERÝYÝ FÝLTRELEMEK ÝÇÝN IDBUN FÝLTRELEDÝK

df_BUN= df_Data %>% filter(POL == "IDBUN")
df_BUN= df_Data %>% filter(POL %in% c("IDBUN","SGSIN")) # BÝRDEN FAZLA DEÐERÝ ARAMAK ÝÇÝN

# BUN ÝÇERENLERÝ POL ÝÇÝNDE BULMAK ÝÇÝN
df_Data %>% filter( grepl(pattern ="BUN", ignore.case = TRUE, x=POL))


#random five rows
smple= df_Data %>% sample_n(5)

#5% of rows
smple= df_Data %>% sample_frac(0,05)

library(readxl)
Port_Dictionary <- read_excel("C:/Users/edursun/Desktop/ADAPT/ADAPT_3/Portcodes Dictionary.xlsx", sheet= "Portcode Dictionary")

library(dplyr)
#merge with left outer join by. koyarak farklý sütun isimlerini refere edebilirsin.
df_Data= merge(df_Data,Port_Dictionary, by.x ="POL", by.y= "PORT CODE",all.x = TRUE)
ncol(df_Data)


#omit duplicates
df_Data %>% distinct(CNTR_N,CNTR_WGT_KG,POL,POD, LOAD_VSL_NAME, DISC_VSL_NAME, .keep_all = TRUE) #dplyr ile yapýlýyor


View(table(df_Data$CNTR_N))


df_Data[!duplicated(df_Data[c("CNTR_N","CNTR_WGT_KG","POL","POD", "LOAD_VSL_NAME", "DISC_VSL_NAME")]),] #duplicateleri kaldýrmak için
View(df_Data)

df_CON= df_Data %>% filter(CNTR_N == "AAMU 7000400") # cntr number filter control
View(df_CON)
View(table(df_Data$CNTR_N)) # to view the number of values in each <row

library(ggplot2)

df_Data %>% ggplot(aes(CNTR_FOE,CNTR_WGT_KG))+geom_boxplot() #box plot

df_Data %>% ggplot(aes(CNTR_FOE,CNTR_WGT_KG))+geom_boxplot()
df_Data %>% ggplot(aes(CNTR_WGT_KG)) + geom_histogram()

df_Data %>% ggplot(aes(CNTR_FOE))+geom_histogram()
ggplot(aes(CNTR_WGT_KG))+geom_histogram()

mtcars
dim(mtcars)
str(mtcars) # verilerin tiplerini görmek için
mtcars <- within(mtcars, {
am <- factor(am,labels=c("automatic","manual")) #numerikleri faktöre çevirmek için
vs <- factor(vs,labels=c("v","s"))
})
mtcars$am=factor(mtcars$am,labels = c("automatic","manual"))
class(mtcars)
sapply(mtcars, class)
summary(mtcars)
sapply(mtcars, function(x) sum(length(which(is.na(x))))) 

#mode (R kendisi hesaplayamýyor)
getmode <-function(v) {
uniqv <- unique(v)
unique(v)[which.max(tabulate(match(v,uniqv)))]
}
getmode(am)

mtcars$am 
var(mtcars$mpg) #varyans
sd(mtcars$mpg)  #standart sapma
mtcars
attach(mtcars)
density.default(x=mtcars$mpg)

mtcars %>% select("mpg","cyl","disp","hp","drat","wt","qsec","gear","carb") %>% cor() #korelasyon için
cor.test(mtcars$mpg,mtcars$wt)


# t.test için 

mtcars

t.test(mtcars$mpg ~ mtcars$am, var.equal=TRUE)

#linear regression

linearMod <- lm(mtcars$mpg ~ mtcars$am)
summary(linearMod)

linearMod2 <- lm(mtcars$mpg ~ mtcars$wt)
summary(linearMod2)
