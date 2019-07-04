#Set the working path directory
filenames = list.files("C:/Users/zhangdj/Desktop/DataForExcellence/Workshop_Dataset/Day 3")

#Creating dataframe for csv files
df_Data_CSV = data.frame()

for(file in filenames)
{
  if(grepl(".csv",file,ignore.case = TRUE))
  {
    temp=read.csv(file)
    df_Data_CSV = rbind(df_Data_CSV,temp)
  }
}

#Creating dataframe for xlsx files
df_Data_XLSX = data.frame()

for(file in filenames)
{
  if(grepl(".xlsx",file,ignore.case = TRUE))
  {
    temp= read_excel(file)
    df_Data_XLSX = rbind(df_Data_XLSX,temp)
  }
}

#Check the dimension of the dataset
dim(df_Data_CSV) 

#Check the head/tail of the dataset
head(df_Data_CSV)
tail(df_Data_CSV)

#Random taken numbers from the dataset
sample(1:nrow(df_Data_CSV),10)
sample(1:5,3)

#Getting columns from the dataset
myCols=colnames(df_Data_CSV)
myCols[1:3]

#Rename of the columns in the dataset 1st method
names(df_Data_CSV)[names(df_Data_CSV) == "LOAD_PORT_C"] <- "POL"
names(df_Data_CSV)[names(df_Data_CSV) == "DISC_PORT1_C"] <- "POD"
names(df_Data_CSV)[names(df_Data_CSV) == "LOAD_ABBR_VESSEL_M"] <- "LOAD_VSL_NAME"
names(df_Data_CSV)[names(df_Data_CSV) == "DISC_ABBR_VESSEL_M"] <- "DISC_VSL_NAME"
names(df_Data_CSV)[names(df_Data_CSV) == "CNTR_STATUS.C"] <- "CNTR_FOE"
names(df_Data_CSV)[names(df_Data_CSV) == "LENGTH_Q"] <- "CNTR_LENGTH"
names(df_Data_CSV)[names(df_Data_CSV) == "WT_Q"] <- "CNTR_WGT_KG"
names(df_Data_CSV)[names(df_Data_CSV) == "HEIGHT_Q"] <- "CNTR_HEIGHT"

#Rename of the columns in the dataset 2nd method
#The new name infront and old name behind
df_Data_CSV=df_Data_CSV %>% rename("CNTR_N"="ZDJ")

#Check the column names in the dataset
names(df_Data_CSV)

#Convert scientific data to non-scientific data
df_Data_CSV$DISC_DT=format(df_Data_CSV$DISC_DT,scientific = FALSE)
df_Data_CSV$LOAD_DT=format(df_Data_CSV$LOAD_DT,scientific = FALSE)

#Convert num to time and create 2 new columns
df_Data_CSV$dt_LOAD_DT = as.POSIXct(df_Data_CSV$LOAD_DT, format="%Y%m%d%H%M%S")
df_Data_CSV$dt_DISC_DT = as.POSIXct(df_Data_CSV$DISC_DT, format="%Y%m%d%H%M%S")

#Obtain data type of all columns
sapply(df_Data_CSV, class)

#Create summary table to understand the data spread
summary(df_Data_CSV)

#Find number of NAs in a specific column
sum(is.na(df_Data_CSV$RF_TEMP))

#Find number of NAs in each column by 1 command
colSums(is.na(df_Data_CSV))

#Finding the % of NA in every Column nearest to 2 decimal places
naData=colSums(is.na(df_Data_CSV))/nrow(df_Data_CSV)
round(naData,2)

#Calculate the average CNTR_Wight by CNTR_FOE
df_Data_CSV %>%
  group_by(CNTR_FOE) %>%
  summarise(mean(CNTR_WGT_KG))

#Sample of T-Test
t.test(mpg ~ am, var.equal=FALSE)

#Test of Equal Variances
library(car)
leveneTest(mpg~am)

#Test of Normality
shapiro.test(mtcars$mpg)

#Normal Q-Q Plot
qqnorm(mpg)
qqline(mpg)

#Simple Linear Regression
model1 <- lm(mpg ~ am)
summary(model1)

model2 <- lm(mpg ~ wt)
summary(model2)


