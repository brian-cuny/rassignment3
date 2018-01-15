require(ggplot2)
require(RColorBrewer)
require(MASS)
require(plyr)
require(scales)

birthData <- read.table(file='C:\\Users\\Brian\\Desktop\\Births.csv', header=TRUE, sep=',', stringsAsFactors=FALSE)

summary(birthData$births)
sd(birthData$births)

ggplot(birthData, aes(x=births)) + geom_histogram(aes(y=..density.., fill=..count..), bins=25) + scale_fill_gradient('Count', low='#DCDCDC', high='#7C7C7C') + 
  stat_function(fun=dnorm, color='red', args=list(mean=mean(birthData$births), sd=sd(birthData$births)), aes(size=2), show.legend=FALSE) + theme(legend.position='none') +
  scale_y_continuous('Proportion', labels=scales::comma) + scale_x_continuous('Births', labels=scales::comma) + 
  labs(title='Frequency of Births in Single Day (1969-1988)', caption='Data From: http://vincentarelbundock.github.io/Rdatasets/') 


workingData <- birthData[,c(5,9,3)]
combinedBirthData <- aggregate(births ~ day_of_week + year, workingData, sum)[,c(2,1,3)]

#points and mean all years by day of week
original <- ggplot(combinedBirthData, aes(x=factor(day_of_week), y=births)) + geom_point(aes(color=factor(year))) + 
  labs(title='Births By Day of Week (1969-1988) With Trend Line', caption='Data From: http://vincentarelbundock.github.io/Rdatasets/',x='Day Of Week', y='Births', color='Year') 

original + stat_summary(aes(y=births,group=1,size=1), fun.y=mean, geom='line',group=1, show.legend=FALSE) +
  scale_x_discrete(labels=c('Mon', 'Tues','Wed','Thurs','Fri','Sat','Sun')) 

#seperated by year
original + facet_wrap(~year) + theme(legend.position='none') + scale_x_discrete(labels=c('M','T','W',"Th",'F','Sa','Su'))

summary(aov(births ~ day_of_week, data=workingData))

ggplot(combinedBirthData, aes(x=factor(day_of_week), y=births)) + geom_boxplot() + scale_x_discrete(labels=c('Mon', 'Tues','Wed','Thurs','Fri','Sat','Sun')) + 
  labs(title='Births By Day of Week (1969-1988)', caption='Data From: http://vincentarelbundock.github.io/Rdatasets/',x='Day Of Week', y='Births') 

#There is statistically significant difference between the groups.


#Linear Regression: Has the trend been increasing recently?

weekDayFunction <- function(data){
  subData <- data[which(data[,2] %in% 1:5),]
  sum(subData$births) / sum(data$births)
}

ratioData <- ddply(workingData, .variables='year', .fun=weekDayFunction)
names(ratioData) <- c('year', 'proportion_weekday')

ggplot(ratioData, aes(x=year, y=proportion_weekday)) + geom_point(aes(color=proportion_weekday)) + scale_color_gradient(low='blue', high='red') +
  labs(title='Proportion of Births during Weekday (1969-1988)', x='Year', y='Proprotion of Babies Born', caption='Data From: http://vincentarelbundock.github.io/Rdatasets/') +
  theme(legend.position='none') +
  scale_y_continuous(labels= scales::percent) + 
  geom_smooth(method='lm')

ratioData$year <- ratioData$year - 1969
summary(lm(formula = proportion_weekday ~ year, data=ratioData))

cor(ratioData$proportion_weekday, ratioData$year)








