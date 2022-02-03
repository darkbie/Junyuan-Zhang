#preprocess the data
library(tidyverse)
library(lubridate)
covid = read.csv("illinois_shield_covid_data.csv", header = TRUE)
begin = ymd(20200817)
days = c(begin)
for (i in 1:457) {
  days[i+1] = begin +i
}
covid$days = days

write.csv(covid, "covid1.csv")

ggplot(data = covid, mapping = aes(x = days, y = totalNewCases)) + geom_point(color = "red") +
  labs(title = "Date vs Number of case or test",y = "Case or Test", x = "date") + geom_smooth()

