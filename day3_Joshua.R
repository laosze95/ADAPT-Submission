#load the data to df_Data
df_Data = read.csv("C:/container/ADAPT_data_for_day3.csv")

#create histogram for dwell days
hist(df_Data$DWELL_DAYS)

#adding colour to the bar
hist(df_Data$DWELL_DAYS, col="BLUE")

#summarizing data by grouping POL and averge weight, showing top 5
df_Data %>% group_by(POL) %>% summarize(mn = mean(CNTR_WGT_KG)) %>% arrange(mn) %>% top_n(wt=mn, n=5)

#adding the group by with CNTR_FOE
agg_table = df_Data %>% group_by(POL,CNTR_FOE) %>% summarize(mn = mean(CNTR_WGT_KG)) 

# use library reshape2
library(reshape2)

#convert long to wide table
dcast(agg_table, POL ~ CNTR_FOE, var.value=mn)

#create a table with POL = df_IBUN
df_IDBUN <- df_Data %>% filter(POL== "IDBUN")

#filter POL with BUN in the data
df_BUN <- df_Data %>% filter(grepl("BUN",POL))

#random select 5 rows of data
sample = df_Data %>% sample_n(5)

#random select 5% of data
sample = df_Data %>% sample_frac(0.05)

#Merge of info from dictionary for POL
POL_m = merge(df_Data, Port_codes, by.x="POL", by.y = "PORT CODE", all.x = TRUE)

#Merge of info from dictionary for POD
POL_POD_m = merge(POL_m, Port_codes, by.x="POD", by.y = "PORT CODE", all.x = TRUE)

#remove dupiicated record
newdf_Data = df_Data[!duplicated(df_Data[c("CNTR_N", "CNTR_WGT_KG", "POL", "POD","LOAD_VSL_NAME", "DISC_VSL_NAME")]),]

#create ggplot 
df_Data %>% ggplot(aes(CNTR_FOE, CNTR_WGT_KG)) + geom_boxplot()

#create histogram using ggplot
df_Data %>% ggplot(aes(DWELL_DAYS)) + geom_histogram(binwidth = 5)

#find out info for mtcars
str(mtcars)

#more info for mtcars
summary(mtcars)

#change numeric to factor
mtcars$am = factor(mtcars$am, labels = c("Automatic","Manual"))

#check for missing data
sapply(mtcars, function(x) sum(is.na(x)))


#find IQR
IQR(mpg)

#set the quantile level required
quantile(mpg, prob = c(0.15,0.25,0.35))

#create function to find mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv [which.max(tabulate(match(v, uniqv)))]
}
getmode(am)

#plot for type of transmission
ggplot(mtcars, aes(am)) + geom_histogram(binwidth = 1)

hist(mtcars$wt)

ggplot(mtcars, aes(am,mpg)) + geom_boxplot()

var(mtcars$mpg)
sd(mtcars$mpg)

#positive relationship to mpg are drat, qsec
#negative relationship to mpg are disp, hp, wt

pairs(mtcars)
cov(mtcars$mpg,mtcars$wt)
cor(mtcars$mpg,mtcars$wt)
cor.test(mtcars$mpg, mtcars$wt, method = "pearson")
