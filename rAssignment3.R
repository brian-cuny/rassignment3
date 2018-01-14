require(ggplot2)
require(RColorBrewer)
require(MASS)
require(plyr)

birthData <- read.table(file='C:\\Users\\Brian\\Desktop\\Births.csv', header=TRUE, sep=',', stringsAsFactors=FALSE)
workingData <- birthData[,c(5,9,3)]
combinedBirthData <- aggregate(births ~ day_of_week + year, workingData, sum)[,c(2,1,3)]

#points and mean all years by day of week
original <- ggplot(combinedBirthData, aes(x=factor(day_of_week), y=births)) + geom_point(aes(color=factor(year))) + 
  labs(title='Births By Day of Week (1969-1988) With Trend Line', caption='Data From: http://vincentarelbundock.github.io/Rdatasets/',x='Day Of Week', y='Births', color='Year') +
  scale_x_discrete(labels=c('Mon', 'Tues','Wed','Thurs','Fri','Sat','Sun')) 

original + stat_summary(aes(y=births,group=1,size=2), fun.y=mean, geom='line',group=1, show.legend=FALSE)

#seperated by year
original + facet_wrap(~year) + theme(legend.position='none') + scale_x_discrete(labels=c('M','T','W',"Th",'F','Sa','Su'))

chisq.test(aggregate(births ~ day_of_week, workingData, mean)[,c(2)])

#There is statistically significant difference between the groups.


#Linear Regression: Has the trend been increasing recently?

weekDayFunction <- function(data){
  subData <- data[which(data[,2] %in% 1:5),]
  sum(subData$births) / nrow(subData)
}

weekEndFunction <- function(data){
  subData <- data[which(data[,2] %in% 6:7),]
  sum(subData$births) / nrow(subData)
}

ratioData <- ddply(workingData, .variables='year', .fun=c(weekDayFunction,weekEndFunction))
ratioData <- cbind(ratioData, (ratioData$V1 - ratioData$V2)/ratioData$V1)
names(ratioData) <- c('year', 'average_weekday', 'average_weekend', 'percent_decrease')

ggplot(ratioData, aes(x=year, y=percent_decrease)) + geom_point(aes(color=percent_decrease)) + scale_color_gradient(low='blue', high='red') +
  labs(title='Percent Difference in Weekend to Weekday Births (1969-1988)', x='Year', y='Percent Decrease', caption='Data From: http://vincentarelbundock.github.io/Rdatasets/') +
  theme(legend.position='none') +
  scale_y_continuous(labels= scales::percent) + 
  geom_smooth(method='lm')

ratioData$year <- ratioData$year - 1969
#lm(formula = percent_decrease ~ year, data=ratioData)
summary(lm(formula = percent_decrease ~ year, data=ratioData))










