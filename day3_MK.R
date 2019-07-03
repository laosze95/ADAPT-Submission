df_Data=read.csv("Adapt_data_for_day3.csv")
hist(df_Data$DWELL_DAYS, col = "red")
boxplot(df_Data$DWELL_DAYS, col = "blue")
stripchart(df_Data$DWELL_DAYS, col = "blue")
plot(df_Data$CNTR_WGT_KG,df_Data$DWELL_DAYS, col = "blue")
qqnorm(df_Data$DWELL_DAYS, col = "blue")

df_Data %>%
  group_by(POL) %>%
  summarize(Mean=mean(CNTR_WGT_KG)) %>%
  arrange(desc(Mean)) %>%
 top_n(wt = Mean, 10) #use head() or tails () as a alternative

longtable = df_Data %>%
  group_by(POL, CNTR_FOE) %>%
  summarize(mn=mean(CNTR_WGT_KG)) 
widetable= dcast(longtable,POL~CNTR_FOE,value.var = "mn")
 
df_IDBUN = filter(df_Data, POL=="IDBUN")

df_IDBUN = df_Data %>% filter(grepl(x = POL, pattern = "BUN",ignore.case = FALSE))


sample = df_Data[sample(1:nrow(df_Data), 5),]
sample_frac(df_Data, 0.05)
sample = df_Data %>% sample_n(5)

Portcodes = read_excel("Portcodes dictionary.xlsx")
df_Data = merge(df_Data, Portcodes, by.x ="POD", by.y = "PORT CODE", all.x=TRUE)
colnames(df_Data)[colnames(df_Data)=="COUNTRY CODE"]="POD_COUNTRY CODE"
colnames(df_Data)[colnames(df_Data)=="PORT NAME"]="POD_PORT NAME"
colnames(df_Data)[colnames(df_Data)=="COUNTRY NAME"]="POD_COUNTRY NAME"
colnames(df_Data)[colnames(df_Data)=="City Name"]="POD_CITY NAME"

df_Data = df_Data %>% # need to assign df_Data before starting because otherwise the remaining codes will keep using the old df_Data
  merge(Portcodes, by.x ="POL", by.y = "PORT CODE", all.x=TRUE)
colnames(df_Data)[colnames(df_Data)=="COUNTRY CODE"]="POL_COUNTRY CODE"
colnames(df_Data)[colnames(df_Data)=="PORT NAME"]="POL_PORT NAME"
colnames(df_Data)[colnames(df_Data)=="COUNTRY NAME"]="POL_COUNTRY NAME"
colnames(df_Data)[colnames(df_Data)=="City Name"]="POL_CITY NAME"

#! makes the list shows all that is not duplicated. without !, it will show the rows that is duplicated, it is the 2nd entry, not the first one where the 2nd entry is duplicated from.
df_Data[!duplicated(df_Data[c("CNTR_N", "CNTR_WGT_KG", "POL", "POD", "LOAD_VSL_NAME", "DISC_VSL_NAME")]), ] 

ggplot(df_Data,aes(CNTR_FOE, CNTR_WGT_KG)) + geom_boxplot() + labs(x = "FOE", title = "test")

my_plot = ggplot(df_Data,aes(CNTR_FOE, CNTR_WGT_KG))

my_plot + geom_col(color = "red") + labs(title = "test")

mtcars$am = factor(mtcars$am, labels = c("Automatic", "manual"))

sapply(mtcars, function(x) sum(is.na(x))) #number of missing data in columns

attach(mtcars)

plot(mtcars$am)
plot(mtcars$mpg)
plot(mtcars$am, mtcars$mpg)
plot(mtcars$mpg, mtcars$wt)

hist(mtcars$mpg)
var(mtcars$mpg)
sd(mtcars$mpg)
plot(density(mpg))
cov(mtcars$vs, mtcars$mpg)
cor(mtcars$wt, mtcars$mpg)
pairs(mtcars[,1:4])
cor(mtcars)
cor(mtcars[,-9])
