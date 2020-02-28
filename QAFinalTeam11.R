# WebQA Analysis Readme ----------------------------------------------------------

"

Each one can write our final code in each section. 
For simplicity-sake lets use the the same name for the merged dataframe: qaDf
qaDf has the merged visits and financials files and the column date with the 
proper format.

The qaDf is saved in the github repository.
The pre-processing section is for the professor to check how we massaged the data.

NOTE: period IS TRANSFORMED AS A FACTOR IN THE PREPROCESSING STEP

"




# Packages ----------------------------------------------------------------

library(readxl)
library(magrittr)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)
library(plotly)
library(tidyr)
library(readr)
library(MASS)
library(caret)

source('helperFunctions.R')




# PreProcessing -----------------------------------------------------------



visitsWeek <- read_excel("webAnalyticsCase.xls",
                         sheet = "Weekly Visits")

financials <- read_excel('webAnalyticsCase.xls',
                         sheet = 'Financials')

# Create date column

qaDf <- convertQAWeeksToDate(dataFrameVisits = visitsWeek,
                     dfVisitsAndFinance = financials,
                     mergeDfs = TRUE)


# Initial Period: 1
# Pre Promotion: 2
# Promotion: 3
# Post Promotion: 4
# convertQAWeeksToPeriod only works if the dataframe has the original amount of rows
  # as in the case. 

qaDf <- convertQAWeeksToPeriod(qaDf)

# Write CSV
# write.csv(qaDf, 'mergedVisitsFinancialsWithDateCol.csv',
#           row.names = FALSE)


# Load Data ---------------------------------------------------------------

qaDf <- read_csv('mergedVisitsFinancialsWithDateCol.csv')

# Q1
dfregion <- read_excel("webAnalyticsCase.xls",sheet ='TopTenGeographicSources' )
trafficSourcesDf <- read_excel("webAnalyticsCase.xls", sheet = 'allTrafficSources') 
topTenRefSitesDf <- read_excel('webAnalyticsCase.xls', sheet = 'TopTenReferringSites')
dfsearcheng <- read_excel('webAnalyticsCase.xls', sheet = 'TopTenSearchEngineSourcesofVisits')

#DF only used in Q2 for additional column "Rev/UV"
FV_P <- read_excel("Web_FinVists_Periods.xlsx")

# Carolina Q1 -------------------------------------------------------------

# Unique visits by date ---------------------------------------------------


#Unique Visits by Period

# In case we want to do compare by percentage
# qaDf <- qaDf %>% mutate(percentVisits = Visits/sum(Visits) )


# By period 
uvisitsbar<-ggplot(qaDf, aes(as.factor(period),
                             `Unique Visits`,
                             fill = as.factor(period)))
uvisitsbar +
  geom_bar(stat = 'identity') +
  xlab("Period") +
  ylab("Unique Visitors") +
  ggtitle("Number of Unique visitors per Period")+ 
  scale_x_discrete(labels = c('Initial Period',
                              'Pre-promotion',
                              'Promotion',
                              'Post-promotion'))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'),
        legend.position = 'none')



# By week, important to see the trend line 

uvisitsline<-ggplot(qaDf)
uvisitsline + geom_line(aes(qaDf$date,qaDf$`Unique Visits`)) + 
  scale_x_date(date_labels="%d%b",date_breaks  ="3 week") +  
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Date") +
  ylab("Unique Visitors") +
  ggtitle("Number of Unique visitors per week") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'),
        legend.position = 'none')

# Plotly Version
p <- plot_ly(qaDf, x = ~qaDf$date, y = qaDf$`Unique Visits`,type = 'scatter', mode='lines')
p


#From where they visit

# Data prep to create percentages and aggregate the groups 

# The dataframe is ordered in descending 
dfregion <- dfregion[order(-dfregion$Visits), ]

# Sum of the visits is created to find the percentage 
totalRegionVisits <- sum(dfregion$Visits)

# Percentage of visits created
dfregion <- dfregion %>% mutate(percentVisits = Visits/sum(Visits) )
 
# All Asian countries grouped
asiaIndex <- grep('Asia',x = dfregion$`Top Ten Geographic Sources by Sub Continent Region` )
asiaSum <- dfregion[asiaIndex,'Visits'] %>% sum() 

# All European countries grouped
europIndex <- grep('Europe',x = dfregion$`Top Ten Geographic Sources by Sub Continent Region` )
europSum <- dfregion[europIndex,'Visits'] %>% sum() 

# A row is added in order to plot the final df
dfregion <- add_row(dfregion, 
                    `Top Ten Geographic Sources by Sub Continent Region` = c("Europe",'Asia'), 
                    Visits = c(europSum,asiaSum),percentVisits = c(europSum/totalRegionVisits ,
                                                                   asiaSum/totalRegionVisits ))

# The dataframe is orderd again by descending (Visits Column)
dfregion <- dfregion[order(-dfregion$Visits), ]


# Creating the plot with the relevant labels
ggplot(dfregion[1:5,], aes(x = reorder(`Top Ten Geographic Sources by Sub Continent Region`,
                                       -percentVisits),
                                    y = percentVisits,
                           fill = `Top Ten Geographic Sources by Sub Continent Region`,
                           label = paste0(round(percentVisits*100,1),'%'))) + 
  geom_bar(stat = 'identity') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab('Regions') +
  ylab('Percentage of Visits') +
  ggtitle('Percentage of Visits per Region') + 
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'),
        legend.position = 'none') +
  geom_text(vjust = 2.5, size = 5)

ggplot(dfregion, aes(x = reorder(`Top Ten Geographic Sources by Sub Continent Region`,-percentVisits),
                           y = percentVisits)) + 
  geom_bar(stat = 'identity') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab('Regions') +
  ylab('Percentage of Visits') +
  ggtitle('Percentage of Visits per Region') + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'))



#Traffic source

trafficSourcesDf <- trafficSourcesDf %>% 
  mutate(percentVisits = Visits/sum(Visits))


sourcebar<-ggplot(trafficSourcesDf[1:3,], 
                  aes(x = reorder(`All Traffic Sources`,-percentVisits),
                                              percentVisits,
                  fill = as.factor(`All Traffic Sources`),
                  label = paste0(round(percentVisits*100),'%')))
sourcebar + 
  geom_bar(stat = 'identity') +
  xlab("Traffic Source") +
  ylab("Visits %") +
  ggtitle("Total Visits per Traffic Sources") + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'),
        legend.position = 'none') +
  geom_text(vjust = 2.5, size = 5)




#Referring sites

View(topTenRefSitesDf)
topTenRefSitesDf <- topTenRefSitesDf %>% 
  mutate(percentVisits = Visits/sum(Visits))

otherRefSitesPercent <- topTenRefSitesDf %>% 
  subset(percentVisits < 0.1,select = percentVisits) %>% sum()

otherRefSitesSum <- topTenRefSitesDf %>% 
  subset(percentVisits < 0.1,select = Visits) %>% sum()


topTenRefSitesDf <- topTenRefSitesDf %>% 
  add_row(`Top Ten Referring Sites` = 'Other',
          Visits = otherRefSitesSum,
          percentVisits = otherRefSitesPercent)

topTenRefSitesDf <- topTenRefSitesDf[order(-topTenRefSitesDf$Visits), ]


topTenRefSitesDf[1:4,] %>% 
  ggplot(aes(x = reorder(`Top Ten Referring Sites`, - percentVisits),
             y = percentVisits,
             fill = as.factor(`Top Ten Referring Sites`),
             label = paste0(round(percentVisits*100),'%'))) +
  geom_bar(stat = 'identity') + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'),
        legend.position = 'none') +
  xlab("Traffic Source") +
  ylab("Visits %") +
  ggtitle("Percentage of Total Visits per Traffic Sources") + 
  geom_text(vjust = 1.8, size = 5)



#Search engine

dfsearcheng <- dfsearcheng %>%
  mutate(percentVisits = Visits/sum(Visits))

otherSearchEngSum <- dfsearcheng %>% 
  subset(percentVisits < 0.8,select = Visits) %>% sum()

otherSearchEngPerc <- dfsearcheng %>% 
  subset(percentVisits < 0.8,select = percentVisits) %>% sum()

dfsearcheng <- dfsearcheng %>% 
  add_row(`Top Ten Search Engine Sources of Visits` = 'Other',
          Visits = otherSearchEngSum,
          percentVisits = otherSearchEngPerc)

dfsearcheng <- dfsearcheng[order(-dfsearcheng$Visits), ]


dfsearcheng[1:2,] %>% 
  ggplot(aes(x = reorder(`Top Ten Search Engine Sources of Visits`, - percentVisits),
             y = percentVisits, 
             label = paste0(round(percentVisits * 100,2),'%'),
             fill = as.factor(percentVisits))) +
  geom_bar(stat = 'identity') + 
 geom_text(vjust = 2.5, size = 5)+ 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'),
        legend.position = 'none') +
  xlab("Traffic Source") +
  ylab("Visits %") +
  ggtitle("Percentage of Total Visits per Traffic Sources")  


searchengbar<-plot_ly(dfsearcheng, x = ~dfsearcheng$`Top Ten Search Engine Sources of Visits`, y = ~dfsearcheng$Visits, mode='lines')

searchengbar %>% layout(xaxis = list(title="Search Engine Source"), yaxis = list(title = "Visits"))




# Talya Q2 a,b ------------------------------------------------------------
#A How much time (sec)  they spent on the website per period?
T1 <- qaDf$`Avg. Time on Site (secs.)`[1:14]
T2 <- qaDf$`Avg. Time on Site (secs.)`[15:35]
T3 <- qaDf$`Avg. Time on Site (secs.)`[36:52]
T4 <- qaDf$`Avg. Time on Site (secs.)`[53:66]

#Took the avg of secs per period vistors spent on the website (secs)
tm1 <- mean(T1) # 80.28571 
tm2 <- mean(T2) # 95.85714 
tm3 <- mean(T3) # 48.7647
tm4 <- mean(T4) # 70.00000

#BAR CHART for AVg Time per period
# Did conversion manually just to confirm that scale_y_time was converting appropriately
#qaDf$AvgTimeOnSite_Minutes <- qaDf$`Avg. Time on Site (secs.)` / 60

timebar <- ggplot(qaDf, aes(as.factor(qaDf$period), as_datetime(qaDf$`Avg. Time on Site (secs.)`)))
timebar + geom_col() +
  xlab("Periods") +
  ylab("Avg Time (Mins)") +
  ggtitle("Avg Time on Website per Period") +
  scale_x_discrete(labels = c('Initial Period',
                              'Pre-promotion',
                              'Promotion',
                              'Post-promotion')) +
  scale_y_time(labels = function(l) strftime(l, '%M')) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'))


#B How many pageviews per visits per period?
PVP1 <- FV_P$`Pages/Visit`[1:14]
PVP2 <- FV_P$`Pages/Visit`[15:35]
PVP3 <- FV_P$`Pages/Visit`[36:52]
PVP4 <- FV_P$`Pages/Visit`[53:66]

#Took the avg of pgs/vist and periods 
PM1 <- mean(PVP1) #2.282857
PM2 <- mean(PVP2) #2.669048
PM3 <- mean(PVP3) #1.794706
PM4 <- mean(PVP4) #2.181429

#BAR CHART: Pg views per visit per period
pageviewsbar <- ggplot(qaDf, aes(as.factor(qaDf$period), qaDf$`Pages/Visit`))
pageviewsbar + geom_col() +
  xlab("Periods") +
  ylab("Number of Pageviews per Visit") +
  ggtitle("Average PageViews per Visit") + 
  scale_x_discrete(labels = c('Initial Period',
                              'Pre-promotion',
                              'Promotion',
                              'Post-promotion')) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'))


# Elmir Q2 c, d -----------------------------------------------------------
for (i in 1:nrow(FV_P)){FV_P$`Rev/UV`[i] <- FV_P$Revenue[i]/FV_P$`Unique Visits`[i]}

Rev_Unq <- ggplot(FV_P, aes(FV_P$period,FV_P$`Rev/UV`)) + geom_bar(stat = "summary")

AVG_T <- ggplot(qaDf, aes(qaDf$period, qaDf$`Avg. Time on Site (secs.)`)) + geom_bar(stat = "summary")

Page_V<- ggplot(qaDf, aes(qaDf$period, qaDf$Pageviews)) + geom_bar(stat = "summary")

Page_per_Vis<-ggplot(qaDf, aes(qaDf$period, qaDf$`Pages/Visit`)) + geom_bar(stat = "summary")

Rev <- ggplot(qaDf, aes(qaDf$period, qaDf$Revenue)) + geom_bar(stat = "summary")

UNQ <- ggplot(qaDf, aes(qaDf$period, qaDf$`Unique Visits`)) + geom_bar(stat = "summary")

#Plotting the graphs together
ggarrange(Rev, AVG_T, Page_V, Page_per_Vis, UNQ, Rev_Unq)


#My graphs used NB/Retention/Rev_Unq ------

Retention_graph + labs(title = "Retention by period",  x= "Period", y= "Retention" )

Rev_Unq + labs(title = "Unique Revenue by period ",  x= "Period", y= "Revenue/Unique" )

NB_graph + geom_bar(stat = "identity")+ labs(title = "Interested Web Visitors by period",  x= "Period", y= "Interest" )


# Diego Q3 ----------------------------------------------------------------


# Do the traditional promos drive web traffic, and in turn drive incremental sales?
#   
#   Define traditional promos (mail, magazines adds. New paid listings - December 2008)
# Traditional promotion -> from (may 25-may31) to  (nov 23-nov 29) 
# Web traffic: total visits (question 2)
# Incremental sales: differences of sales (revenues and pounds sold) between weeks/periods 
# Change in incremental sales vs total visits

#   Assumptions: 
# 1) during the 4 periods, QA has done all the traditional promotions
# 2) The brochures is the only traditional promotion that affected the increase in web visits. 

# a) How visits varied from pre promo to post promo?
# I will do a line chart with X axis as Date, Y axis as Visits and the periods will be lines
# from (may 25-may31) to  (nov 23-nov 29) 

# Initial Period: May 25 - Aug 30 (First 14 Rows)
# Pre Promotion: Aug 31 - Jan 24 [15:35]
# Promotion: Jan 25 - May 23 [36:52]
# Post Promotion: May 24 - Aug 29 [53:]




# The peak of visitors is noticeable in the promotion period

# PreProcessing -----------------------------------------------------------


visitsFinancials <- qaDf

colnames(visitsFinancials) <- str_replace_all(colnames(visitsFinancials),' ', '_')


# Part A ------------------------------------------------------------------



visitsFinancialsForVisits <- visitsFinancials[15:66,]

visitsFinancialsForVisits %>% 
  ggplot(aes(date, Visits)) + 
  # geom_bar(stat = 'identity') +
  geom_line() +
  scale_x_date(date_labels="%d%b",date_breaks  ="2 week") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  geom_vline(xintercept = as.numeric(as.Date('2009-01-14')), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(as.Date('2009-05-21')),
             colour = 'red') +
  xlab('Weeks') +
  ylab('Total Visits') +
  ggtitle('Visits Variation Among Periods') + 
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"))


# Part B Pre Processing ------------------------------------------------------------------


# b)  How incremental sales varied from pre promo to post promo?


incrementalDate <- visitsFinancials[15:66,]





# Incremental Revenue  
# First, a dataframe all the 1-week lagged variables is created
# This is done in order to reuse any lagged difference variable in the future


# This function creates the lagged Difference of a given Dataframe
# It outputs a List, this is done in order to be able to store
# different variable lengths.

# DiffLag lags this variables and returns a list
laggedList <- diffLag(2,c("Visits",
            "Unique_Visits",
            "Pageviews",
            "Pages/Visit",
            "Avg._Time_on_Site_(secs.)",
            "Bounce_Rate","%_New_Visits",
            "Revenue",
            "Profit",
            "Lbs._Sold",
            "Inquiries"),incrementalDate) 

# Convert the list to a dataframe, 
 # if the elements have different lengths this will throw an error

laggedDf <- data.frame(matrix(unlist(laggedList), 
                  nrow=length(laggedList[[1]]), 
                  byrow=FALSE)) 

# Create appropriate names of the columns in order to distinguish them 
  # if merged with original dataset 

laggedNames <- c("lagged_Visits", 
                 "lagged_Unique_Visits", 
                 "lagged_Pageviews", 
                 "lagged_Pages/Visit", 
                 "lagged_Avg._Time_on_Site_(secs.)", 
                 "lagged_Bounce_Rate", 
                 "lagged_%_New_Visits", 
                 "lagged_Revenue", 
                 "lagged_Profit", 
                 "lagged_Lbs._Sold", 
                 "lagged_Inquiries")

colnames(laggedDf) <- laggedNames



# Rename Variables for ggplot to have a more descriptive name
  # New columns in incrementalDate are created
  # NOTE: Incremental date contains the observations from prepromotion to 
    # Postpromotion. We are considireing the brochure as the main traditional promotion
      #thus,  the initial period is not included in the following graphs

incrementalDate$incrementalSalesLbsSold <- laggedDf$lagged_Lbs._Sold
incrementalDate$incrementalRevenue <- laggedDf$lagged_Revenue



# Part B.1 Incremental Sales ----------------------------------------------------------------


# plot: Line chart, y axis Incremental Sales in Lbs, x axis Weeks
# Incremental sales vary more during the promotion period, i n general is not
# Stable
# I wouldn't include this graph in the main analysis, there's no actionable insight

incrementalDate %>% 
  ggplot(aes(date, incrementalSalesLbsSold)) + 
  geom_line() +
  scale_x_date(date_labels="%d%b",date_breaks  ="2 week") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  geom_vline(xintercept = as.numeric(as.Date('2009-01-14')), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(as.Date('2009-05-21')),
             colour = 'red') +
  xlab('Weeks') +
  ylab('Incremental Sales') +
  ggtitle('Incremental Sales Variation Among Periods') + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'))

# Boxplot among periods
# Same result as before, more variation during the promotion
incrementalDate %>% 
  ggplot(aes(factor(period), incrementalSalesLbsSold)) +
  geom_boxplot() + 
  xlab('Period') +
  ylab('Incremental Sales (Lbs. Sold)') +
  ggtitle('Incremental Sales Variation Among Periods',
          subtitle = 'Incremental Sales: Future Lbs. Sold - Past Lbs. Sold') + 
  scale_x_discrete(labels = c('Pre-promotion','Promotion', 'Post-promotion')) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'))



# Part B.2 Inc Revenue ----------------------------------------------------------------

# Yield Same results as before, more variation in the promotion period, 
# The prepromotion had higher revenue
# Not sure if to include this, no actionable insight here

# Incremental Revenue
incrementalDate %>% 
  ggplot(aes(date, Revenue)) + 
  geom_line() +
  scale_x_date(date_labels="%d%b",date_breaks  ="2 week") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  geom_vline(xintercept = as.numeric(as.Date('2009-01-14')), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(as.Date('2009-05-21')),
             colour = 'red') +
  xlab('Weeks') +
  ylab('Incremental Revenue') +
  ggtitle('Incremental Sales Variation Among Periods') + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'))

# Boxplot among periods
incrementalDate %>% 
  ggplot(aes(factor(period), incrementalRevenue)) +
  geom_boxplot() + 
  xlab('Period') +
  ylab('Incremental Revenue') +
  ggtitle('Incremental Revenue Variation Among Periods',
          subtitle = 'Incremental Revenue: Future Revenue - Past Revenue') + 
  scale_x_discrete(labels = c('Pre-promotion','Promotion', 'Post-promotion')) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,colour = '#8e918f'))



# Modelling  --------------------------------------------------------------

#### THE ROLLING DIFFERENCE AND THE LAGGED REVENUE NEEDS TO BE SHIFTER UPWARDS!!!!!

# However, this analysis is just for the difference of one week!! 
# Question: How incremental sales vary when we extend the lag week?
# We can compute a moving average of the difference to know how much
# sales vary among periods

# Goal: Find best way to model incremental sales and future sales


# Incremental Revenue Modelling -------------------------------------------


columnNames <-c("Visits",
                "Unique_Visits",
                "Pageviews",
                "Pages/Visit",
                "Avg._Time_on_Site_(secs.)",
                "Bounce_Rate","%_New_Visits",
                "Inquiries") 

# This function crates a dataframe with 
  # n lagged weeks Revenue.
   #  I.e. Revenue shifts m rows. 
  # Then n is indexed from m+1 to nrow(df)
# The output of this function is the DF that 
 # is going to be used in the modelling


# Incremental Sales have the best fit on the 2nd week lag R2 = 0.20
# Data better to predict values in 2 weeks. This is the prediction to of 
# incremental revenue

for (i in 1:10){
  
  laggedLmDf <- laggedModelRevPrep(lagWeeks = i, 
                                   columnName = "Revenue",
                                   dataFrameName = visitsFinancials)
  
  laggedModelTemp <- lm(laggedLmDf$Lagged_Revenue ~ . ,
                        data = laggedLmDf) %>% 
                            summary()
  
  paste('Lag:',1,' --- ', print(laggedModelTemp$adj.r.squared),'\n')
  
  
}

# THIS IS CREATING A MODEL FOR INCREMENTAL REVENUE
# 2 WEEKS IS THE OPTIMAL POINT IN TIME

# lm(formula = laggedIncRevDf$Lagged_Revenue ~ Visits + Pageviews + 
#      Lbs._Sold, data = laggedIncRevDf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -450157  -94312   19704  100249  333602 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -6.130e+05  1.012e+05  -6.055 9.93e-08 ***
#   Visits      -3.381e+02  1.398e+02  -2.418   0.0187 *  
#   Pageviews    2.591e+02  1.087e+02   2.384   0.0203 *  
#   Lbs._Sold    2.337e+01  3.504e+00   6.671 9.09e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 162100 on 60 degrees of freedom
# Multiple R-squared:  0.5309,	Adjusted R-squared:  0.5074 
# F-statistic: 22.63 on 3 and 60 DF,  p-value: 6.366e-10

laggedIncRevDf <- laggedModelRevPrep(lagWeeks = 2, 
                                     columnName = "Revenue",
                                     dataFrameName = visitsFinancials)


laggedModel <- lm(laggedIncRevDf$Lagged_Revenue ~ . ,data = laggedIncRevDf) 
laggedModel %>% summary()


# Variable selection through stepwise regression
stepAIC(laggedModel, direction = 'backward')

laggedIncRevDf <- laggedIncRevDf %>% 
    scale() %>% 
      as.data.frame()

laggedIncRevFinal <-lm(formula = laggedIncRevDf$Lagged_Revenue ~ Visits +
                         Pageviews + 
                         Lbs._Sold, 
                       data = laggedIncRevDf)

laggedIncRevFinal  %>% 
  summary()



# NOTE TO FUTURE SELF: COMPARE THIS MODEL with the one on line 464 ---> 
  # lm(formula = laggedIncRevDf$Lagged_Revenue ~ Visits + Pageviews + 
    # Lbs._Sold, data = laggedIncRevDf) # F p-value: 6.366e-10
# The adj.R is 0.51, slightly worse than the previous model. 
# The focus is not having the highest R squared, 
  # The focus is to find the most parsimonious model in order to extract
  # actionable insights!! This second model has less variables and a 
  # lower overall p value (F test), it's more generalizable
# Key insights: Our revenue is mostly affected after 2 weeks of users navigating the 
# webpage. 
# Moreover, with the average of number of visitors, pageviews and Lbs.Sold, 
# we can explain 50 % of the future revenue (2 weeks). We can recommend which elements 
# of the new webpage are needed to be optimized in order to have the desired output! 
# TO DO: Think about which other variables might be useful in modelling the future revenue



# Variable importance, standardized coeffs vs Caret Variable Importance
  # Caret's variable importance evaluates the absolute value of the T statistic 
    # of each variable
scaledLagIncRevDF <- scale(laggedIncRevDf) %>% as.data.frame()
standardCoeffIncRevDf <- lm(laggedIncRevDf$Lagged_Revenue ~ 
                                Visits + 
                              Unique_Visits +
                              Pageviews + 
                              `Avg._Time_on_Site_(secs.)` + 
                              Lbs._Sold, 
                          data = laggedIncRevDf) %>%  
                      coefficients() %>% 
                    as.data.frame() 

# Standardized Coeffs
standardCoeffIncRevDf %>% round(2)
# Caret's variable importance
varImp(laggedIncRevFinal)

# We already now that Lbs. Sold is the most important variable of the model, 
# it explainsaround 40% of the variance in Revenue 

#________________________________________
# INTERPRETATION: 
# The less amount of weekly visits and the greater amount of weekly page views 
# will yield a better revenue in approximately 2 weeks
#________________________________________

# Let's take a look if we can model the Lbs. Sold! # This was previously done
# Because it was one of the most

# Incremental Sales (LBS.Sold model):
# THIS IS CREATING A MODEL FOR INCREMENTAL SALES - Lbs.Sold (4 weeks)
# VISITS AND UNIQUE VISITS ARE THE ONLY SIGNIFICANT VARIABLES < 0.05 
# THE INTERPRETATION HAS NO LOGIC BUSINESS IMPLICATION, 
# E.G. THE HIGHER THE BOUNCE RATE, THE HIGHER THE SALES 
# ADJ R SQUARED IS 0.122 AND MULTIPLE R SQUARED IS 0.194, MODEL P VALUE: 0.02981


# Lead Revenue Model ----------------------------------------------------


# Now let's see how we can predict the future revenue magnitude # 

# We print the output up to a 15-week lead

for(leadNum in 1:3){
  
  # CHANGED LAGGED TO LEAD
  leadNumber <- leadNum
  leadRev <- lead(qaDf$Revenue,n = leadNum)
  leadRev[is.na(leadRev)] <- 0
  leadQa <- qaDf[,c(-1,-9,-13,-14,-8,-10)]
  leadQa$leadRev <- leadRev
  leadQa <- leadQa %>% scale() %>% as.data.frame()
  stopIndex1 <- nrow(leadQa) - leadNumber 
  leadQa <-leadQa[1:stopIndex1, ]
  
  # print(cor(qaModel$leadRev, qaModel$`Unique Visits`))
  tempModel <- lm(leadQa$leadRev ~ . , data = leadQa)
  library(MASS)
  steps1 <- stepAIC(tempModel, direction = 'backward', trace = 0) %>% summary()
  # tempModel$adj.r.squared %>% print()
  print(paste(leadNumber, ' ', round(steps1$adj.r.squared,3)))
}

# MODEL BEFORE: More in detail, best model .16 R squared 3 week lag
# MODEL AFTER: Best model 0.46 R and 0.4 adj. R Squared

# THE MODEL IMPROVES A LOT WHEN USING THE LEAD INSTEAD OF THE LAG, 
# THE LAG SHOULDNT HAVE BEEN USED BECAUSE WE WERE TRYING TO PREDICT 
# PASTA DATA WITH FUTURE DATA (LAG ROLLS ROWS UP, NOT DOWN)
# http://b2b-marketingblog.activeconversion.com/industrial/high-bounce-rate/

# Call:
#   lm(formula = qaModel$leadRev ~ `Pages/Visit` + `Avg._Time_on_Site_(secs.)` + 
#        Bounce_Rate + `%_New_Visits` + Inquiries, data = qaModel)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -296115 -124070   -3810  119567  314462 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 -5624645    1325904  -4.242 8.74e-05 ***
#   `Pages/Visit`                 464518     238628   1.947  0.05679 .  
# `Avg._Time_on_Site_(secs.)`     8983       3221   2.789  0.00729 ** 
#   Bounce_Rate                  2972582    1118517   2.658  0.01033 *  
#   `%_New_Visits`               2511603    1145660   2.192  0.03269 *  
#   Inquiries                      24008       8290   2.896  0.00544 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 163900 on 54 degrees of freedom
# Multiple R-squared:  0.4565,	Adjusted R-squared:  0.4062 
# F-statistic: 9.073 on 5 and 54 DF,  p-value: 2.624e-06

qaDf %>%
  ggplot() + 
  geom_line()

leadNumber <- 2
qaModel <- visitsFinancials[,c(-1,-9,-13,-14,-10)]
leadRev <- lead(qaDf$Revenue,n = leadNumber) # CHANGED LAG TO LEAD
leadRev[is.na(leadRev)] <- 0
qaModel$leadRev <- leadRev
stopIndex <- nrow(qaModel) - leadNumber 
qaModel <-qaModel[1:stopIndex, ]
qaModel<- scale(qaModel) %>% as.data.frame()

# Check Bounce Rate and Inquiries
qaLeadRev <- lm(qaModel$leadRev ~ . , data = qaModel)
qaLeadRev  %>% summary()


# Variable Selection
library(MASS)
steps<- stepAIC(qaLeadRev, direction = 'backward')

# BEST MODEL 
  # THe lagged Difference model won
leadBestMod <- lm(formula = qaModel$leadRev ~ Visits + Unique_Visits + `Pages/Visit`, 
   data = qaModel)

leadBestMod %>% summary()

colnames(qaModel)[c(4,5,7)] <- c('pagesPerVisit', 'avgTimeOnSite','percentNewVisits' )
# 
# Call:
#   lm(formula = qaModel$leadRev ~ Visits + Unique_Visits + `Pages/Visit`, 
#      data = qaModel)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -284532 -104709  -10660   99071  375850 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   -200926.5   207340.3  -0.969  0.33668   
# Visits           1886.4      929.1   2.030  0.04709 * 
#   Unique_Visits   -1870.9      963.9  -1.941  0.05731 . 
# `Pages/Visit`  242070.7    74419.2   3.253  0.00194 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 144900 on 56 degrees of freedom
# Multiple R-squared:  0.2877,	Adjusted R-squared:  0.2495 
# F-statistic: 7.539 on 3 and 56 DF,  p-value: 0.0002533


# Checking nonlinear model 
# library(randomForest)
# colnames(qaModel)[4] <- 'pages_visit'
# colnames(qaModel)[5] <- 'avgTime'
# colnames(qaModel)[7] <- 'percNewVisits'
# rf<-randomForest(qaModel$leadRev ~ .,data = qaModel)
# rf$importance


qaModel$date <- visitsFinancials$date[7:nrow(visitsFinancials)]

library(ggpubr)

p1 <-  ggplot(data = qaModel, mapping = aes(x = date, y = leadRev)) + 
  geom_line(stat = "identity") 
p1

p2 <-  ggplot(data = qaModel, mapping = aes(x = date, y = Visits)) + 
  geom_line(stat = "identity")
p2

subplot(p1, p2, nrows = 2, margin = 0.04, heights = c(0.6, 0.4),
        shareX = TRUE,
        shareY = TRUE)


ggplotly(qaModel %>%
  ggplot() +
  geom_point(aes(x = Visits, 
             y = leadRev)))

library(plotly)
ggplotly(qaModel %>%
  ggplot() +
    geom_point(aes(x = Bounce_Rate, y =  leadRev)))

# Example of how double Y axis can deform the data!!  
ggplot(qaModel, aes(x = date)) + 
  geom_line(aes(y = leadRev, colour = "Revenue")) + 
  geom_line(aes(y = Inquiries*100000, colour = "Inquiries")) +
  scale_y_continuous(sec.axis = sec_axis( ~ . /100000, name = "Inquiries"))



 




# Exploration Data Analysis -----------------------------------------------

# DESCRIPTIVE STATISTICS

" 1) Using data in the Weekly Visits and Financials worksheets, create four column charts (like
                                                                                         Figure 1: Visits to the QA Website per Week) for unique visits over time, revenue over time,
profit over time, and pounds sold over time. You do not have to indicate on these charts the
cutoffs for the four periods.

2) Using the same data, calculate the following summary statistics for visits, unique visits,
revenue, profit, and pounds sold: mean, median, standard deviation, minimum, and
maximum, for the initial, pre-promotion, promotion, and post-promotion periods. So, for
each period you should provide 25 values: five summary measures for each of five variables,
as per the table below for the initial period."

# RELATIONSHIPS BETWEEN VARIABLES

"
5) Start by taking a look at revenue and pounds sold. (Before proceeding, what does your
intuition say about the relationship between these two variables?) Create a scatter diagram of
revenue versus pounds sold. (Revenue should be on the y, or vertical, axis.) Determine the
correlation coefficient of revenue and pounds sold.


6) Now create the scatter diagram of revenue versus visits. (Given your previous work, what
do you expect this plot to look like?) Determine the correlation coefficient of revenue and
visits.

7) Summarize your results. In particular, elaborate on the implications of the relationship
between revenue and number of visits to the website. Feel free to examine any other variable
pairs you think might be important.
"
#Question 1- Daily Visits and Financials 
#Unique Visits 
head(qaDf)
UniqueVisitsbar <- ggplot(qaDf, aes(qaDf$`Week (2008-2009)`, qaDf$`Unique Visits`))
UniqueVisitsbar + geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Date") +
  ylab("Unique Visitors") +
  ggtitle("Number of Unique visitors per week")

#Revenue 
revenuebar <- ggplot(qaDf, aes(qaDf$`Week (2008-2009)`, Revenue))
revenuebar + geom_col()+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Date") +
  ylab("Revenue") +
  ggtitle("Revenue Generation per week")

#Profit 
profitbar <- ggplot(qaDf, aes(qaDf$`Week (2008-2009)`, Profit))
profitbar + geom_col()+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Date") +
  ylab("Profit") +
  ggtitle("Profits per week")

#Pounds 
poundsbar <- ggplot(qaDf, aes(qaDf$`Week (2008-2009)`, qaDf$`Lbs. Sold`))
poundsbar + geom_col() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Date") +
  ylab("Lbs Sold") +
  ggtitle("Lbs Sold per week")



#Question 2- Summary Stats
#Initial Period
#visits
summary(qaDf$Visits[ncol(qaDf):14])
sd(qaDf$Visits[ncol(qaDf):14])

#UniqueVisits
summary(qaDf$`Unique Visits`[ncol(qaDf):14])
sd(qaDf$`Unique Visits`[ncol(qaDf):14])

#Revenue
summary(qaDf$Revenue[ncol(qaDf):14])
sd(qaDf$Revenue[ncol(qaDf):14])

#Profit
summary(qaDf$Profit[ncol(qaDf):14])
sd(qaDf$Profit[ncol(qaDf):14])

#LBs Sold
summary(qaDf$`Lbs. Sold`[ncol(qaDf):14])
sd(qaDf$`Lbs. Sold`[ncol(qaDf):14])

#Pre Promo
#Visits
summary(qaDf$Visits[15:35])
sd(qaDf$Visits[15:35])

#Unique Visits 
summary(qaDf$`Unique Visits`[15:35])
sd(qaDf$`Unique Visits`[15:35])

#Revenue
summary(qaDf$Revenue[15:35])
sd(qaDf$Revenue[15:35])

#Profit
summary(qaDf$Profit[15:35])
sd(qaDf$Profit[15:35])

#Lbs Sold
summary(qaDf$`Lbs. Sold`[15:35])
sd(qaDf$`Lbs. Sold`[15:35])

#Promo
#Visits
summary(qaDf$Visits[36:52])
sd(qaDf$Visits[36:52])

#Unique Visits 
summary(qaDf$`Unique Visits`[36:52])
sd(qaDf$`Unique Visits`[36:52])

#Revenue
summary(qaDf$Revenue[36:52])
sd(qaDf$Revenue[36:52])

#Profit
summary(qaDf$Profit[36:52])
sd(qaDf$Profit[36:52])

#LBs Sold 
summary(qaDf$`Lbs. Sold`[36:52])
sd(qaDf$`Lbs. Sold`[36:52])

#Post Promo
#Visits
summary(qaDf$Visits[53:nrow(FV)])
sd(qaDf$Visits[53:nrow(fv)])

#Unique Visits 
summary(qaDf$`Unique Visits`[53:nrow(FV)])
sd(qaDf$`Unique Visits`[53:nrow(FV)])

#Revenue
summary(qaDf$Revenue[53:nrow(FV)])
sd(qaDf$Revenue[53:nrow(FV)])

#Profit
summary(qaDf$Profit[53:nrow(FV)])
sd(qaDf$Profit[53:nrow(FV)])

#LBs Sold 
summary(qaDf$`Lbs. Sold`[53:nrow(FV)])
sd(qaDf$`Lbs. Sold`[53:nrow(FV)])



#Question 3
PrdRev <- qaDf %>% ggplot(aes(x = period,y = Revenue)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

PrdProf <- qaDf %>% ggplot(aes(x = period,y = Profit)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

PrdLbs <- qaDf %>% ggplot(aes(x = period,y = `Lbs. Sold`)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

PrdVisits <- qaDf %>% ggplot(aes(x = period,y = Visits)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

PrdUniqueVisits<- qaDf %>% ggplot(aes(x = period,y = `Unique Visits`)) +
  geom_bar(stat = 'summary',fun.y = 'mean')

library(ggpubr)
ggarrange(PrdVisits,PrdUniqueVisits,PrdLbs,PrdProf,PrdRev)


#Question 4: Insights from Qs 1-3
# Cost Column
qaDf <- qaDf %>% 
  mutate(cost = Revenue - Profit)
# Price
qaDf <- qaDf %>% 
  mutate(price = Revenue/Lbs._Sold)

# Price Plot 
qaDf %>% ggplot(aes(y = price, x = date)) +
  geom_line() + 
  geom_vline(xintercept = as.numeric(qaDf$date[15]), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(qaDf$date[36]), 
             colour = 'red') +
  geom_vline(xintercept = as.numeric(qaDf$date[53]), 
             colour = 'red')

#Correlations between Revenue, Price, and Lbs Sold
cor(qaDf$price, qaDf$Revenue)
cor(qaDf$Lbs._Sold, qaDf$Revenue) 

#Our Insights
  # Price has decreased from quarter 1 to quarter 2 ($32 to $29) 
  # Cost are fluctuating at a similar level, they are not affecting the proffits as much as Revenue (view corr with profit)
  # Hypotheis -- Because Lbs sold went up by 47% in the second quarter, this may be due to the price drop 
    # Revenues didn't increase by same amount!
    # Demand is elastic, affects a lot of the price
    # Price elasticity = Percentage change in quantity demanded / percentage change in price

  # Revenue is what's driving the profit and Lbs Sold is what's driving the revenue (REFER to correlation and Elmir's explanation)  
  # Sticky note -> These are our variables to watch out


#Question 5- Scatter Plot for Rev and Lbs Sold
corr1 <- ggplot(qaDf, aes(qaDf$`Lbs. Sold`, Revenue))
corr1 +geom_point()
cor(qaDf$`Lbs. Sold`,qaDf$Revenue) #Positive Correlation #0.87


#Question 6- Scatter Plot for Rev and Visits
corr2 <- ggplot(qaDf, aes(Visits, Revenue))
corr2 + geom_point()
cor(qaDf$Visits, qaDf$Revenue) #Negative Correlation #-0.06


#Question 7: Insights from Qs 5 & 6
#Revenue vs Lbs Sold
#From our previous analysis and insights from Qs 1-3 we know that Revenue is driven by Lbs Sold 
#Question 5 confirms this further with a strong correlation of about 0.87 

# Revenue vs Visits
# Company is paying for payperclick, people that are entering the website are not buying 
  #products because there is no relationship with the Revenues.
# Hypothesis: QA's Website may not be intuitive, may not be atractive, this is caused by something. 
  # I.e. The website is not good enough in terms of quality and content 
# Possible suggestions: 
  # Based on the hypothesis: Do A/B testing to improve the website. Make it more interactive.  
  # Based on insight: Keep track inquires from website to know sales conversion. In order to know.
    #how many visitors are placing orders through the website. This is to know the ROI of the website.
# Extra suggestion: 
  #Include "How did you hear about us?" in order to better allocate resources in marketing channels.
    #In return -> Giving incentive (discount or a dollar)

qaDf %>%
  ggplot(aes(Visits,Revenue)) +
  geom_point() 
cor(qaDf$Visits, qaDf$Revenue)
cor(log(qaDf$Visits), log(qaDf$Revenue))

qaDf %>% 
 exploration
  ggplot(aes(`Lbs. Sold`)) +
  geom_histogram(bins = 15, col = 'white') +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(limits = c(0,max(qaDf$`Lbs. Sold`)),  
                     breaks = seq(0, max(qaDf$`Lbs. Sold`), by = 5000))

hist(qaDf$`Lbs. Sold`, breaks = 15
)