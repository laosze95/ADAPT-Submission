df=read.csv('C:/Users/maloise/Desktop/ADAPT/Day 3/ADAPT_data_for_day3.csv')
View(df)
hist(df$DWELL_DAYS)
boxplot(df$DWELL_DAYS)
plot(df$CNTR_WGT_KG, df$DWELL_DAYS)
df %>% group_by(POL) %>% summarize(mean(CNTR_WGT_KG)) %>% arrange(CNTR_WGT_KG)
df %>% group_by(POL) %>% summarize(mn=mean(CNTR_WGT_KG)) %>% arrange(mn)
df %>% group_by(POL) %>% summarize(mn=mean(CNTR_WGT_KG)) %>% arrange(mn) %>%top_n(wt=mn, n=10)
TEST=df %>% group_by(POL,CNTR_FOE) %>% summarize(mn=mean(CNTR_WGT_KG))
CAST=dcast(TEST, POL ~ CNTR_FOE, value.var = "mn")
DF_BUN=df[df$POL=="IDBUN",]
DF_BUN=df %>% filter(POL == "IDBUN")
DF_BUN=df %>% filter(grepl(pattern = "BUN", x=POL, ignore.case=TRUE))
Sample=sample_n(df, 5)
Sample=sample_frac(df, 0.05)
df_Merge=read_excel('C:/Users/maloise/Desktop/ADAPT/Day 3/Portcodes Dictionary.xlsx')
df=merge(x=df, y=df_Merge, by.x="POL",by.y ="PORT CODE",all.x = TRUE)
df=merge(x=df, y=df_Merge, by.x="POD",by.y ="PORT CODE",all.x = TRUE)
Duplicated=df[duplicated(df[c("CNTR_N","CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME")]),]
Duplicated_OUT=df[!duplicated(df[c("CNTR_N","CNTR_WGT_KG","POL","POD","LOAD_VSL_NAME","DISC_VSL_NAME")]),]
library(ggplot2)
df %>% ggplot(aes(CNTR_FOE, CNTR_WGT_KG)) + geom_boxplot()
df %>% ggplot(aes(CNTR_WGT_KG)) + geom_histogram(color="black", fill="blue")
MTG=mtcars
nrow(MTG)
ncol(MTG)
str(mtcars)
sapply(mtcars, function(x) sum(is.na(x)))
rowSums(is.na(mtcars))
mtcars$am=factor(mtcars$am,labels = c("Automatic","manual"))
summary(mtcars)
quantile(mpg)
IRQ(mpg)
mean(mpg)
median(mpg)
quantile(mpg, probs = c(0.15, 0.25, 0.35))




getmode <- function(v) {
  uniqv <- unique(v)
  unique[which.max(tabulate(match(v, uniqv)))]
}
getmode(am)

var(mpg)
sd(mpg)

pairs(mtcars)
pairs(mtcars2[,c(1,3,5,6,7)])

cov(mpg, wt)
Cor(mpg, wt)
cor.test(mpg,wt,method = "pearson")
