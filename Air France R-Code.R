# #############################################################################
# Team 12: Mengting Lin, Sneha Ramu, Alessandro Bruni Conter, Fernando Patron #
#                  Data Science R: Air France Business Case                   #
###############################################################################

#Summary

# -Challenge
#  Which of the search engines are the ones that give the most return on investment to Air France.
#  Which strategy is the most efficient and the one that gives the best performance.

# -Insights
#  Our priority is to determine which SEM gives a better ROA*, CTR** and Conversion Rate.
#  What Keyword strategy has  the best performance (branded or unbranded)

#- Recommendations & Next Steps
#  Invest advertising budget to Yahoo-US and Google-US in match types of “Advanced” and “Broad” keywords. 
#  Reduce expenses in “Impressions”. 
#  Relocate the budget from Overture to Yahoo-US and Google-US.

#-------------------------------------------------------------------------------

#Approach

# -Data importing and preprocessing
# -Descriptive analysis and feature engineering
# -Logistic Regression, Gini Decision Tree parameter tuning and result interpretation
# -Validating recommendations by using confusion matrix

#Key Findings

# -EDA
#  Google-US and Yahoo-US have the higher average "Engine Click Through Rate".
#  Yahoo-US, Google-US and MSN-Global performs better in "Trans.Conv.%".
#  Yahoo-US has the highest cost performance. Follow by Google-Global and Google-US.
#  MSN-Global has the highest "Total Cost" and lower "Trans.Conv.Rate".
#  Yahoo-US has the lowest "Total Cost" and highest "Trans.Conv.Rate".
#  Keywords with is important for search engine marketing campaign.
#  Yahoo-US performs best in RoA.
#  Overture-US performs worst in RoA.

#-Modeling
#  Business Success (ROA >=1) Conditions:
#     Tran.Conv. % > 120% and Search Engine Bid < $3.8
#     Trans. Conv. % is between 43% and 81% with Search Engine Bid < $3.1
#  Business Failure (ROA < 1) Conditions
#     Trans. Conv. % < 43% and Search Engine Bid >=  $3.1
#     Trans. Conv. % > 120% and Search Engine Bid >= $3.8

#-Recommendations & Next Steps
#  - Invest advertising budget to Yahoo-US and Google-US in match types of “Advanced” and “Broad” keywords. 
#  - Reduce expenses in “Impressions”. 
#  - Relocate the budget from Overture to Yahoo-US and Google-US.

#-------------------------------------------------------------------------------

#Index
# 1.Data Preprocessing
# 2.EDA
#   - Feature Engineering
#   - Marketing Campaign Analysis
#   - Publisher Analysis
#   - Keyword Analysis
#   - RoA Analysis
# 3.Data Modeling
#   - Data Preparation
#   - Linear Regression
#   - Logistic Regression
# 4. Recommendations & Next Steps  

#-------------------------------------------------------------------------------

################################
#      Data Preprocessing      #
################################

#####Importing data#####
#Install packages
library(readxl)
library(ROCR)
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)
library(plotly)
library(ggplot2)

#Load dataset
AF <- read_excel("Desktop/Air France Case Spreadsheet Supplement.xls", 
                 sheet = "DoubleClick")

#####Data Cleaning#####
#Identifying the problem in my data
head(AF, n=10) #Print the first 10 rows of AF
summary(AF) 
AF <- as.data.frame(AF)

#Check if there is missing values
sum(is.na(AF)) #There are 1224 missing values
apply(is.na(AF), 2, which) #"Bid Strategy" has missing values

table(AF$`Publisher Name`)
prop.table(table(AF$`Publisher Name`))
round(prop.table(table(AF$`Publisher Name`)), 2) * 100


################################
#             EDA              #
################################

#####RoA#####
#Implementing the RoA(Return on Advertising Expenditure)
AF$RoA <- c()
for(i in 1:nrow(AF)){
  AF$RoA[i] <- (AF$Amount[i] - AF$`Total Cost`[i]) / AF$`Total Cost`[i]
}#closing the for loop

#clean up missing value
AF$RoA <- gsub("Inf","0",AF$RoA)

#change datatype
AF$RoA<-as.numeric(AF$RoA)


#####Booking_Rate#####
AF$Booking_Rate <- c()
for(i in 1:nrow(AF)){
  AF$Booking_Rate[i] <- AF$`Total Volume of Bookings`[i] /AF$Impressions[i]
}#closing the for loop

#clean up missing value
AF$Booking_Rate <- gsub("Inf","0",AF$Booking_Rate)


#####Match Type#####
AF$Match_Type <- gsub("N/A","",AF$`Match Type`)


#####Keyword_AirFrance#####
#create new column Group and give the label 0 and 1
AF$Keyword_AirFrance<- AF$Keyword
AF$Keyword_AirFrance <- gsub(".*air france.*","1", AF$Keyword_AirFrance)
AF$Keyword_AirFrance <- gsub(".*airfrance.*","1", AF$Keyword_AirFrance)
AF$Keyword_AirFrance[AF$Keyword_AirFrance !=1]<-0

#change the datatype
AF$Keyword_AirFrance<-as.numeric(AF$Keyword_AirFrance)


#####Marketing Campaign Analysis#####
#Engine Click Thru %
ggplot(data=AF) +
  stat_summary(
    mapping=aes(x=`Publisher Name`, y=`Engine Click Thru %`),
    fun.ymin=min,
    fun.ymax=max,
    fun.y=mean
  )

#Trans.Conv.%
ggplot(data=AF) +
  stat_summary(
    mapping=aes(x=`Publisher Name`, y=`Trans. Conv. %`),
    fun.ymin=min,
    fun.ymax=max,
    fun.y=mean
  )

#RoA
ggplot(data = AF, aes(x=`Publisher Name`,y=RoA)) +
  geom_bar(stat="identity", width = 0.5)

#Insights
# -Google-US and Yahoo-US have the higher average “Engine Click Through Rate”
# -Yahoo-US, Google-US and MSN-Global performs better in “Trans.Conv.%”.
# -Yahoo-US and Google-US have the higher RoA.



#####Publisher Analysis#####
#Impressions#
ggplot(data = AF, aes(x=`Publisher Name`,y=Impressions)) +
  geom_bar(stat="identity", width = 0.5)
#Amount#
ggplot(data = AF, aes(x=`Publisher Name`,y=Amount)) +
  geom_bar(stat="identity", width = 0.5)
#Total Cost#
ggplot(data = AF, aes(x=`Publisher Name`,y=`Total Cost`)) +
  geom_bar(stat="identity", width = 0.5)
#RoA
ggplot(data = AF, aes(x=`Publisher Name`,y=RoA)) +
  geom_bar(stat="identity", width = 0.5)


#Impression vs. Total Volume of Bookings
ggplot(data=AF,aes(x=Impressions, y=`Total Volume of Bookings`, color=`Publisher Name`))+geom_point()+geom_smooth()
#Impression vs.RoA
ggplot(data=AF,aes(x=Impressions, y=RoA, color=`Publisher Name`))+geom_point()+geom_smooth()
#Total Cost vs.Impressions
ggplot(data=AF,aes(x=Impressions, y=`Total Cost`, color=`Publisher Name`))+geom_point()+geom_smooth()

#Total Cost vs.Amount
ggplot(data=AF,aes(x=`Total Cost`, y=Amount, color=`Publisher Name`))+geom_point()+geom_smooth()
#Total Cost vs.Trans.Conv.%
ggplot(data=AF,aes(x=`Trans. Conv. %`, y=`Total Cost`, color=`Publisher Name`))+geom_point()+geom_smooth()

#Insights
# -Yahoo - US has the highest cost performance. Follow by are Google-Global and Google US.
# -MSN-Global has the highest “Total Cost” and lower Trans.Conv. Rate. 
# -Yahoo-US has the lowest “Total Cost” and highest Trans.Conv. Rate.
# -Impressions it is not an useful KPI in this case.
# -MSN-Global has high cost but low trans.conv.%.



#####Keyword Analysis#####
#Match Type
ggplot(data = AF, aes(x=`Publisher Name`,y=`Match Type`)) +
  geom_point()
ggplot(data = AF, aes(x=`Match Type`,y=RoA),) +
  geom_bar(stat="identity", width = 0.5)

#Average Position(Avg.Pos)
Plot_AvgPos<- plot_ly() %>% 
  add_trace(data = AF,
            y = ~`Avg. Pos.`,
            color = ~`Publisher Name`,
            type = "box") %>% 
  layout(title = "Average Position",
         xaxis = list(title = "Publisher Name"))
Plot_AvgPos

#RoA
ggplot(data = AF, aes(x=`Publisher Name`,y=RoA),) +
  geom_bar(stat="identity", width = 0.5)

#keyword_AirFrance vs. Avg.Pos.
ggplot(data=AF,aes(x=Keyword_AirFrance, y=`Avg. Pos.`, color=`Publisher Name`))+geom_point()+geom_smooth()
#keyword_AirFrance vs. Total Volume of Bookings
ggplot(data=AF,aes(x=Keyword_AirFrance, y=`Total Volume of Bookings`, color=`Publisher Name`))+geom_point()+geom_smooth()
#keyword_AirFrance vs. RoA
ggplot(data=AF,aes(x=Keyword_AirFrance, y=RoA, color=`Publisher Name`))+geom_point()+geom_smooth()

#Insights
#Keyword includes AirFrance has higher average position
#Keyword includes AirFrance has higher volume of bookings
#Keyword includes AirFrance has higher RoA
#Keyword includes AirFrance is really important!!!!
#Keyword with the match type of "Advance", "Broad", and "Standard" have higher RoA.
#Yahoo-US and Google can offer all these three keyword match types business.


#######RoA Analysis#######
Google_US <- AF[which(AF$`Publisher Name`=='Google - US'), ]
Google_Global <- AF[which(AF$`Publisher Name`=='Google - Global'), ]
Yahoo_US <- AF[which(AF$`Publisher Name`=='Yahoo - US'), ]
MSN_US <- AF[which(AF$`Publisher Name`=='MSN - US'), ]
MSN_Global <- AF[which(AF$`Publisher Name`=='MSN - Global'), ]
Overture_US <- AF[which(AF$`Publisher Name`=='Overture - US'), ]
Overture_Global <- AF[which(AF$`Publisher Name`=='Overture - Global'), ]

#create vectors 
Publisher_Name <- c('Google_US', 'Google_Global', 'Yahoo_US','MSN_US', 'MSN_Global','Overture_US', 'Overture_Global')

#convert datatype
Google_US$RoA <- as.numeric(Google_US$RoA)
Google_Global$RoA <- as.numeric(Google_Global$RoA)
Yahoo_US$RoA <- as.numeric(Yahoo_US$RoA)
MSN_US$RoA <- as.numeric(MSN_US$RoA)
MSN_Global$RoA <- as.numeric(MSN_Global$RoA)
Overture_US$RoA <- as.numeric(Overture_US$RoA)
Overture_Global$RoA <- as.numeric(Overture_Global$RoA)

Google_US$Booking_Rate <- as.numeric(Google_US$Booking_Rate)
Google_Global$Booking_Rate <- as.numeric(Google_Global$Booking_Rate)
Yahoo_US$Booking_Rate <- as.numeric(Yahoo_US$Booking_Rate)
MSN_US$Booking_Rate <- as.numeric(MSN_US$Booking_Rate)
MSN_Global$Booking_Rate <- as.numeric(MSN_Global$Booking_Rate)
Overture_US$Booking_Rate <- as.numeric(Overture_US$Booking_Rate)
Overture_Global$Booking_Rate <- as.numeric(Overture_Global$Booking_Rate)

#create new columns
total_revenue <- c(sum(Google_US$Amount),sum(Google_Global$Amount),sum(Yahoo_US$Amount),
                   sum(MSN_US$Amount),sum(MSN_Global$Amount),sum(Overture_US$Amount),
                   sum(Overture_Global$Amount))

total_cost <- c(sum(Google_US$`Total Cost`),sum(Google_Global$`Total Cost`),sum(Yahoo_US$`Total Cost`),
                sum(MSN_US$`Total Cost`),sum(MSN_Global$`Total Cost`),sum(Overture_US$`Total Cost`),
                sum(Overture_Global$`Total Cost`))

total_impressions <- c(sum(Google_US$Impressions),sum(Google_Global$Impressions),
                       sum(Yahoo_US$Impressions),sum(MSN_US$Impressions),
                       sum(MSN_Global$Impressions),sum(Overture_US$Impressions),
                       sum(Overture_Global$Impressions))

total_bookings <- c(sum(Google_US$`Total Volume of Bookings`),sum(Google_Global$`Total Volume of Bookings`),
                    sum(Yahoo_US$`Total Volume of Bookings`),sum(MSN_US$`Total Volume of Bookings`),
                    sum(MSN_Global$`Total Volume of Bookings`),sum(Overture_US$`Total Volume of Bookings`),
                    sum(Overture_Global$`Total Volume of Bookings`))

total_clicks <- c(sum(Google_US$Clicks),sum(Google_Global$Clicks),sum(Yahoo_US$Clicks),
                  sum(MSN_US$Clicks),sum(MSN_Global$Clicks),sum(Overture_US$Clicks),
                  sum(Overture_Global$Clicks))

total_click_charges <- c(sum(Google_US$`Click Charges`),sum(Google_Global$`Click Charges`),
                         sum(Yahoo_US$`Click Charges`),sum(MSN_US$`Click Charges`),
                         sum(MSN_Global$`Click Charges`),sum(Overture_US$`Click Charges`),
                         sum(Overture_Global$`Click Charges`))

Avg_RoA <- c(mean(Google_US$RoA),mean(Google_Global$RoA),mean(Yahoo_US$RoA),mean(MSN_US$RoA),
             mean(MSN_Global$RoA),mean(Overture_US$RoA),mean(Overture_Global$RoA))

Avg_cost_per_click <- c(mean(Google_US$`Avg. Cost per Click`),mean(Google_Global$`Avg. Cost per Click`), 
                        mean(Yahoo_US$`Avg. Cost per Click`),mean(MSN_US$`Avg. Cost per Click`),
                        mean(MSN_Global$`Avg. Cost per Click`),mean(Overture_US$`Avg. Cost per Click`),
                        mean(Overture_Global$`Avg. Cost per Click`))

Avg_Booking_Rate <- c(mean(Google_US$Booking_Rate),mean(Google_Global$Booking_Rate), 
                      mean(Yahoo_US$Booking_Rate),mean(MSN_US$Booking_Rate),
                      mean(MSN_Global$Booking_Rate),mean(Overture_US$Booking_Rate),
                      mean(Overture_Global$Booking_Rate))

matrix <- matrix(c(Publisher_Name, total_revenue, total_bookings,total_impressions,
                   total_cost,total_clicks,total_click_charges,Avg_RoA, Avg_Booking_Rate, 
                   Avg_cost_per_click),nrow=7, ncol=10)

AF_Table <- as.data.frame(matrix)
names(AF_Table) <- c("Publisher Name", "Total Revenue", "Total Bookings", "Total impressions", "Total Cost",
                     "Total Clicks", "Total Click Charges", "Avg. RoA", "Avg. Booking Rate", "Avg. Cost/Click" )

#change the datatype
AF_Table$`Total Bookings` <- as.numeric(AF_Table$`Total Bookings`)
AF_Table$`Total impressions` <- as.numeric(AF_Table$`Total impressions`)
AF_Table$`Total Cost` <- as.numeric(AF_Table$`Total Cost`)
AF_Table$`Total Clicks` <- as.numeric(AF_Table$`Total Clicks`)
AF_Table$`Total Click Charges` <- as.numeric(AF_Table$`Total Click Charges`)
AF_Table$`Avg. RoA` <- as.numeric(AF_Table$`Avg. RoA`)
AF_Table$`Avg. Booking Rate` <- round(as.numeric(AF_Table$`Avg. Booking Rate`), digits=4)
AF_Table$`Avg. Cost/Click` <- as.numeric(AF_Table$`Avg. Cost/Click`)
AF_Table$`Total Revenue` <- as.numeric(AF_Table$`Total Revenue`)


#create new ratios 
AF_Table$RoA <- round(AF_Table$`Total Revenue`/AF_Table$`Total Cost`, digits = 2)
AF_Table$BR <- AF_Table$`Total Bookings`/AF_Table$`Total impressions`
AF_Table$CPC <- round(AF_Table$`Total Click Charges`/AF_Table$`Total Clicks`, digits = 4)
AF_Table$CONV <- round(AF_Table$`Total Bookings`/AF_Table$`Total Clicks`, digits = 4)
AF_Table$CTR <- round(AF_Table$`Total Clicks`/AF_Table$`Total impressions`, digits = 4)

#combine Kayak data with AF_Table data
AF_Table[nrow(AF_Table)+1,]=c("Kayak",233694,208,NA,NA,2839,NA,NA,NA,NA,NA,NA,NA,0.0733,NA)

#convert datatype
AF_Table$RoA <- as.numeric(AF_Table$RoA)
AF_Table$BR <- as.numeric(AF_Table$BR)
AF_Table$CPC <- as.numeric(AF_Table$CPC)
AF_Table$CONV <- as.numeric(AF_Table$CONV)
AF_Table$CTR <- as.numeric(AF_Table$CTR)

#######Bubble Graph#######
# CONV vs.ROA Bubble Graph
BG_CONV <- plot_ly(AF_Table, x = ~CONV, y = ~RoA, textposition = "auto", type = 'scatter', 
                   mode = 'markers', size = ~CONV, color = ~`Publisher Name`, colors = 'Blues',
                   marker = list(opacity = 1.0, sizemode = 'diameter')) %>%
           layout(title = 'Trans.Convert Rate vs. RoA', xaxis = list(title = "Trans.Convert Rate", showgrid = FALSE),
                  yaxis = list(title = "ROA", showgrid = FALSE),
                  showlegend = TRUE)

BG_CONV

# CTR vs.RoA Bubble Graph
BG_CTR <- plot_ly(AF_Table, x = ~CTR, y = ~RoA, textposition = "auto", type = 'scatter', 
                  mode = 'markers', size = ~CTR, color = ~`Publisher Name`, colors = 'Blues',
                  marker = list(opacity = 1.0, sizemode = 'diameter')) %>%
          layout(title = 'Click Through Rate vs. RoA', xaxis = list(title = "Click Through Rate", showgrid = FALSE),
                 yaxis = list(title = "ROA", showgrid = FALSE),
                 showlegend = TRUE)

BG_CTR

# CPC vs. RoA Bubble Graph
BG_CPC <- plot_ly(AF_Table, x = ~CPC, y = ~RoA, textposition = "auto",
                  type = 'scatter', mode = 'markers', size = ~CPC, color = ~`Publisher Name`, 
                  colors = 'Blues', marker = list(opacity = 1.0, sizemode = "diameter")) %>%
          layout(title = 'Charge Per Click vs.RoA', xaxis = list(title = "Charge Per Click", showgrid = FALSE),
                 yaxis = list(title = "ROA", showgrid = FALSE),
                 showlegend = TRUE)

BG_CPC

# BR vs. RoA Bubble Graph
BG_BR <- plot_ly(AF_Table, x = ~BR, y = ~RoA, textposition = "auto",type = 'scatter', 
                 mode = 'markers', size = ~BR, color = ~`Publisher Name`, colors = 'Blues', 
                 marker = list(opacity = 1.0, sizemode = "diameter")) %>%
         layout(title = 'Booking Rate vs.RoA', xaxis = list(title = "Booking Rate", showgrid = FALSE),
                 yaxis = list(title = "ROA", showgrid = FALSE),
                 showlegend = TRUE)

BG_BR

#Insights
# -Overall, Yahoo_US performs best in RoA(Return in Advertising)
# -Overall, Overture_US performs worst in RoA


################################
#        Data Modeling         #
################################
#create ROA binary
# adding additional logic based on an if statement
AF$ROA <- c()
for(i in 1:nrow(AF)){
  if(AF$RoA[i] > mean(AF$RoA, na.rm=TRUE)){
    AF$ROA[i] <- 1
  }else{AF$ROA[i] <- 0} # closing if statement
}# closing the i-loop

training_idx <- sample(1:nrow(AF), size=0.8*nrow(AF))
#step two
AF_train <- AF[training_idx,]
AF_test <- AF[-training_idx,]

#linear regression model
AF_linear <- lm(ROA~Keyword_AirFrance, data = AF_train)
summary(AF_linear)
exp(0.239273)-1

#create logit model
AF_logit <- glm(ROA~`Search Engine Bid`+Clicks+`Click Charges`+`Avg. Cost per Click`+
                     Impressions+`Engine Click Thru %`+`Avg. Pos.`+`Trans. Conv. %`+
                     `Total Cost/ Trans.`+Amount+`Total Cost`+`Total Volume of Bookings`+
                     +Booking_Rate+Keyword_AirFrance,
                data=AF, family="binomial")
summary(AF_logit)

# AF_logit_rr: delete p-value > 0.05
AF_logit_rr <- glm(ROA~`Search Engine Bid`+Clicks+`Trans. Conv. %`+`Total Volume of Bookings`+
                        Keyword_AirFrance,
                   data=AF, family="binomial")
summary(AF_logit_rr)

#session of confusion matrix
my_prediction_testing <- predict(AF_logit_rr, AF_test, type="response")
confusionMatrix(data= as.factor(as.numeric(my_prediction_testing>0.5)),
                reference= as.factor(as.numeric(AF_test$ROA)))

my_prediction_training <- predict(AF_logit_rr, AF_train, type="response")
confusionMatrix(data= as.factor(as.numeric(my_prediction_training>0.5)),
                reference= as.factor(as.numeric(AF_train$ROA)))

#create in an AUC ROC for AF logit
pred_val_logit <- prediction(my_prediction_testing, AF_test$ROA)
perf_logit <- performance(pred_val_logit, "tpr", "fpr")
plot(perf_logit)

#plot decision tree
AF_tree <- rpart(ROA ~ `Search Engine Bid`+Clicks+`Trans. Conv. %`+`Total Volume of Bookings`+
                   Keyword_AirFrance,
                 data=AF_train, method="class", cp=0.010)
rpart.plot(AF_tree, type=1, extra=1)

#using this tree to predict on testing customers
AF_tree_predict <- predict(AF_tree, AF_test, 
                                type="prob")
AF_tree_prediction <- prediction(AF_tree_predict[,2], 
                                      AF_test$ROA)
AF_tree_perf <- performance(AF_tree_prediction,
                            "tpr", "fpr")
plot(perf_logit, col="blue")
plot(AF_tree_perf, col="green4", add=TRUE)



################################
# Recommendations & Next Steps #
################################
# 1.Invest advertising budget to Yahoo-US and Google-US in match types of “Advanced” and “Broad” keywords. 

# 2. Reduce expenses in “Impressions”. 

# 3. Relocate the budget from Overture to Yahoo-US and Google-US.








