gitcreds::gitcreds_set()
library(ggplot2)
house<- read.csv('dataset3.csv')
median(house$Total.Number.of.Family.members)
median(as.numeric(house$Total.Household.Income))
median(as.numeric(house$Total.Food.Expenditure))

 #The median number of family members and expenditure is divided into two categories
house=house %>%
  dplyr:::mutate.data.frame(members=cut(Total.Number.of.Family.members,c(-Inf,4,Inf),right=FALSE,labels=c("less than 4 members","greater than 4 members")))
head(house)    
house=house %>%
  dplyr:::mutate.data.frame(expenditure=cut(Total.Food.Expenditure,c(-Inf,54594,Inf),right=FALSE,labels=c("low","high")))
head(house)                            
house <- house %>%
  select(members,Household.Head.Sex,Total.Household.Income,Type.of.Household,Household.Head.Age,expenditure) 
house$members <- as.factor(house$members)
levels(house$members) <- c("less than 4 members","greater than 4 members")
house$Total.Household.Income <- as.factor(house$Total.Household.Income)
house$Household.Head.Sex <- as.factor(house$Household.Head.Sex)
house$Type.of.Household <- as.factor(house$Type.of.Household)

#boxplot of head age and members
ggplot(data = house, aes(x = members, y =Household.Head.Age, fill = members)) + 
  geom_boxplot() +
  labs(x = "members", y = "head age") + theme(legend.position = "none")


#ggplot(data = house, aes(x = members, y =expenditure, fill = members)) + geom_boxplot() +labs(x = "members", y = "income") + theme(legend.position = "none")
# gender distribution
house %>%
  tabyl(Household.Head.Sex, members) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() # To show original counts

#plot of gender and members
ggplot(data = house, aes(x = members, group = Household.Head.Sex)) +
  geom_bar(aes(y = ..prop.., fill = Household.Head.Sex), stat = "count", position = "dodge") + labs(x = "members", y = "gender")

house %>%
  tabyl(Type.of.Household, members) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% adorn_ns() # To show original counts
ggplot(data = house, aes(x = members, group = Type.of.Household)) + geom_bar(aes(y = ..prop.., fill = Type.of.Household),
                                                                              stat = "count", position = "dodge") + 
  labs(x = "members", y = "type")
mod.house <- glm(members ~ Household.Head.Sex + Type.of.Household+expenditure , data = house, family = binomial(link = "logit"))
mod.house %>% 
  summary()
#plot_model(mod.house, show.values = TRUE, title = "", show.p = FALSE, value.offset = 0.25)
