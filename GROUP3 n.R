library(ggplot2)
library(tidyverse)
library(moderndive)
library(gapminder)
library(sjPlot)
library(stats)
library(jtools)
library(janitor)

house<- read.csv('dataset3.csv')
median(house$Total.Number.of.Family.members)
median(as.numeric(house$Total.Household.Income))
median(as.numeric(house$Total.Food.Expenditure))

 #The median number of family members and expenditure is divided into two categories
house$Total.Number.of.Family.members[house$Total.Number.of.Family.members <= 4] = 0
house$Total.Number.of.Family.members[house$Total.Number.of.Family.members > 4] = 1

# delete Region as they are all the same
house <- house[,-2]
house$Total.Number.of.Family.members <- as.factor(house$Total.Number.of.Family.members)
levels(house$Total.Number.of.Family.members) <- c("less than 4 members","greater than 4 members")
house$Household.Head.Sex <- as.factor(house$Household.Head.Sex)
house$Type.of.Household <- as.factor(house$Type.of.Household)

#boxplot of head age and members
ggplot(data = house, aes(x = Total.Number.of.Family.members, y =Household.Head.Age, fill = Total.Number.of.Family.members)) + 
  geom_boxplot() +
  labs(x = "members", y = "head age") + theme(legend.position = "none")


#ggplot(data = house, aes(x = members, y =expenditure, fill = members)) + geom_boxplot() +labs(x = "members", y = "income") + theme(legend.position = "none")
# gender distribution
house %>%
  tabyl(Household.Head.Sex, Total.Number.of.Family.members) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() # To show original counts

#plot of gender and members
ggplot(data = house, aes(x = Total.Number.of.Family.members, group = Household.Head.Sex)) +
  geom_bar(aes(y = ..prop.., fill = Household.Head.Sex), stat = "count", position = "dodge") + labs(x = "members", y = "gender")

house %>%
  tabyl(Type.of.Household, Total.Number.of.Family.members) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% adorn_ns() # To show original counts
ggplot(data = house, aes(x = Total.Number.of.Family.members, group = Type.of.Household)) + geom_bar(aes(y = ..prop.., fill = Type.of.Household),
                                                                              stat = "count", position = "dodge") + 
  labs(x = "members", y = "type")
mod.house <- glm(Total.Number.of.Family.members ~ Household.Head.Sex + Type.of.Household+Total.Food.Expenditure , data = house, family = binomial(link = "logit"))
mod.house %>% 
  summary()
#plot_model(mod.house, show.values = TRUE, title = "", show.p = FALSE, value.offset = 0.25)
