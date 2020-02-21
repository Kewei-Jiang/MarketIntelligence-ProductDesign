data <- read.csv("Beverages_November 8, 2019_13.37.csv")
View(data)

######## Data Cleaning #######################################
dim(data)

## Remove unneccessary columns
rem_col <- subset(data,select=-c(StartDate,EndDate,Status,IPAddress,Progress,Finished,RecordedDate,ResponseId
                                 ,RecipientLastName,RecipientFirstName,RecipientEmail,ExternalReference,
                                 DistributionChannel,UserLanguage,Q16))

## Remove unneccessary rows
ind <- seq(0,3)

# make subset --> this would choose every `ind` row
rem_col[ind, ]

# --> this would exclude ale `ind` row
almost_there <- rem_col[-ind, ]
View(almost_there)

#Renaming columns for convenience
colnames(almost_there)[colnames(almost_there)=="Duration..in.seconds."] <- "Duration_in_seconds"
colnames(almost_there)[colnames(almost_there)=="LocationLatitude"] <- "Latitude"
colnames(almost_there)[colnames(almost_there)=="LocationLongitude"] <- "Longitude"
colnames(almost_there)[colnames(almost_there)=="Q1_1"] <- "Water"
colnames(almost_there)[colnames(almost_there)=="Q1_2"] <- "Coffee_or_Tea"
colnames(almost_there)[colnames(almost_there)=="Q1_3"] <- "Energy_Drinks"
colnames(almost_there)[colnames(almost_there)=="Q1_4"] <- "Soft_Drinks"
colnames(almost_there)[colnames(almost_there)=="Q1_5"] <- "Juice/Juice_Beverages"
colnames(almost_there)[colnames(almost_there)=="Q1_6"] <- "Dairy/Dairy_Substitutes"
colnames(almost_there)[colnames(almost_there)=="Q1_7"] <- "Alcoholic_Beverages"
colnames(almost_there)[colnames(almost_there)=="Q4"] <- "Important_Attributes"
colnames(almost_there)[colnames(almost_there)=="Q5_1"] <- "Bai_(Rank)"
colnames(almost_there)[colnames(almost_there)=="Q5_3"] <- "Hint_Water_(Rank)"
colnames(almost_there)[colnames(almost_there)=="Q5_5"] <- "Ayala's_(Rank)"
colnames(almost_there)[colnames(almost_there)=="Q5_6"] <- "Zpirit_(Rank)"
colnames(almost_there)[colnames(almost_there)=="Q8"] <- "How_many_bottles_of_the_best_ranked?"
colnames(almost_there)[colnames(almost_there)=="Q11"] <- "Likeliness_to_buy_AIR_WATER"
colnames(almost_there)[colnames(almost_there)=="Q12"] <- "Likeliness_to_pay"
colnames(almost_there)[colnames(almost_there)=="Q13"] <- "Gender"
colnames(almost_there)[colnames(almost_there)=="Q18"] <- "Age"
colnames(almost_there)[colnames(almost_there)=="Q17"] <- "Level_of_education"
colnames(almost_there)[colnames(almost_there)=="Q18.1"] <- "Occupation"
colnames(almost_there)[colnames(almost_there)=="Q7"] <- "Motivation"

View(almost_there)

#Convert factors to integers
almost_there$Duration_in_seconds <- type.convert(almost_there$Duration_in_seconds)
almost_there$Latitude <- type.convert(almost_there$Latitude)
almost_there$Longitude <- type.convert(almost_there$Longitude)
almost_there$Likeliness_to_buy_AIR_WATER <- type.convert(almost_there$Likeliness_to_buy_AIR_WATER)
almost_there$Likeliness_to_pay <- type.convert(almost_there$Likeliness_to_pay)

### Impute missing data for "Likeliness to Buy" and "Likeliness to pay"
library(rpart)
library(randomForest)
install.packages('simputation')
library(simputation)
d1<-impute_cart(almost_there,Likeliness_to_pay~.)
View(d1)
d2<-impute_cart(d1, Likeliness_to_buy_AIR_WATER~.)
View(d2)
Final_Data <- d2
View(Final_Data)
dim(Final_Data)
View(data)

write.csv(Final_Data,"FinalData.csv",row.names = FALSE)

########### Data Analysis and EDA ####################
library(ggplot2)
ggplot(Final_Data,aes(Soft_Drinks)) + geom_bar()
ggplot(Final_Data,aes(Coffee_or_Tea)) + geom_bar() + facet_wrap(~Occupation)
ggplot(Final_Data,aes(Likeliness_to_pay,Likeliness_to_buy_AIR_WATER)) + geom_point()
ggplot(Final_Data,aes(Likeliness_to_pay)) + geom_bar()
ggplot(Final_Data,aes(Likeliness_to_buy_AIR_WATER)) + geom_bar()

#Distrubution Plots
ggplot(Final_Data,aes(x=Likeliness_to_pay)) +  geom_histogram(fill = "orange")+
  labs(x= "Likeliness to Pay",y = "Count", title = paste("Distribution of", "Likeliness to Pay")) + theme_bw()

ggplot(Final_Data,aes(x=Likeliness_to_buy_AIR_WATER)) +  geom_histogram(fill = "orange")+
  labs(x= "Likeliness to Buy AIR WATER",y = "Count", title = paste("Distribution of", "Likeliness to buy AIR WATER")) + theme_bw()

#plot between 
ggplot(Final_Data, aes(x= as.factor(Age), y= Likeliness_to_buy_AIR_WATER)) + geom_boxplot()


