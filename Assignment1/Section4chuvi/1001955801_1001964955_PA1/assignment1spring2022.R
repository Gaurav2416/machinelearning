#Before starting with the tasks in the assignment, we need to remove the rows with missing values
#write the code for it here and save the new data frame with the same name as df_census.
data <- read.csv('C:/Users/16824/OneDrive/Desktop/Project/Data Mining/Assignment1/Section4chuvi/Section4chuvi/census1994.csv')
idx <- data == " ?"
is.na(data)<- idx
data <- na.omit(data)
data <- na.omit(data)
nrow(data)
class(data)
#Task 1-a: Print the details of the df_census data frame (information such as number of rows,columns, name of columns, etc)
summary(data)

#Task 1-b: Find the number of rows and columns in the df_census data frame.
dim(data)
#Task 1-c: Print the descriptive details (min, max, quartiles etc) for 'Age' column of the df_census
summary(data$Age)
#Task 1-d: Print the number of unique values for 'education_num' and 'hours-per-week' columns
unique(data$education.num)
unique(data$hours.per.week)

#Task 2-a: Find out the sum of Captial Gain for people with education level as Bachelors and HS-Grad.
#condition <- table(data$education,data$capital.gain)
#sum("Total Bachelors:",condition$[" Bachelors"])
sum(subset(data, education == " Bachelors")$capital.gain)
sum(subset(data, education == " HS-grad")$capital.gain)


#Task 2-b: Find out the total number of people surveyed in months may, october and december.
#Create a new column for 'Survey_Month' by using 'Date' column
#write the code for extracting the month from the date column here
###############begin your code here
dates <- as.Date(data$ï..Date, "%m/%d/%Y")
data$Survey_Month<-months(dates)
###############send you code here
table(data$Survey_Month == 'May')

table(data$Survey_Month == 'October')

table(data$Survey_Month == 'December')

# Find out the total number of surveys in september and november with workclass as private and age less than 50.

table(data$Survey_Month == 'September' & data$WorkClass == ' Private' & data$Age < 50)

df <- table(cut(data$Age,seq(0,90,15)))
barplot(df, beside=TRUE, legend=TRUE, main="Age Interval", horiz = FALSE,xlab = "count",ylab = "Age")
table(data$Survey_Month == 'November' & data$WorkClass == ' Private' & data$Age < 50)
#Task 2-d: Find out 3 least surveyed education categories, print their names and corresponding number of surveys for periods January-June and July-December.
num<-match(data$Survey_Month, month.name)
dataFirstSix<-subset(data, num < 7)
sort(table(dataFirstSix$education),decreasing= F) %>% head(3)
dataLastSix<-subset(data, num >6 & num < 13 )
sort(table(dataLastSix$education), decreasing = F) %>% head(3)

#Task 2-e: Find out top 5 native-countries besides United-States, print their names and number of surveys belonging to each.
data2e <- subset(data,data$native.country != ' United-States')
sort(table(data2e$native.country),decreasing = T) %>% head(5)

#Task 2-f: Find out Top-5 native-countries with the most number of samples belonging to class >50K
data2F <- subset(data,data$class == ' >50K')
sort(table(data2F$native.country),decreasing = T) %>% head(5)


#Task 3-a: Draw a histogram for total number of surveys taken each month. Dislpay months with their corresponding numbers(Eg: January is 1)
x <- table(data$Survey_Month)
hist(x,
     main="Survey Per month",
     xlab="Month",
     col="darkmagenta",
     freq=FALSE
)

#Task 3-g: Draw a line chart showing average capital gain for each education category.
q<-subset(data,select = c('capital.gain','education'))
#table(q$education) %>% mean(q$capital.gain, trim = 0, na.rm = FALSE)
avg <- aggregate(x = q$capital.gain,                # Specify data column
                 by = list(q$education),              # Specify group indicator
                 FUN = mean)  
plot(avg$x,type = "l",xlab = "Education", ylab = "Mean")

attach(data)
plot(data$Age, data$hours.per.week, main="Scatterplot Example", xlab="Age ", ylab="Hour per week ", pch=19)

data3b<-subset(data,select = c('Survey_Month','gender')) 
lay<-table(data3b$Survey_Month,data3b$gender)
barplot(lay)

