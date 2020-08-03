library(tidyverse)
library(knitr)

d <- data[-c(1), -c(4, 5, 6, 7, 18, 19, 20, 21, 22, 23, 24)]
names(data) <- c("relationship", "length", "sleepTogetherFrequency", 
                 "remove", "remove", "remove", "remove", "snores", "bathroomTrips", "sick", "noSex", 
                 "temperature", "argument", "noSpace", "blanketHog", "child", "workHours", "remove",
                 "remove", "stayTogether", "sleepBetter", "sexLife", "occupation", "occupationOther",
                 "gender", "ageGroup", "income", "education", "cencusLocation")

#turn the checkboxes into booleans
d <- d %>% mutate(snores = ifelse(is.na(snores), FALSE, TRUE))
d <- d %>% mutate(bathroomTrips = ifelse(is.na(bathroomTrips), FALSE, TRUE))
d <- d %>% mutate(sick = ifelse(is.na(sick), FALSE, TRUE))
d <- d %>% mutate(noSex = ifelse(is.na(noSex), FALSE, TRUE))
d <- d %>% mutate(temperature = ifelse(is.na(temperature), FALSE, TRUE))
d <- d %>% mutate(argument = ifelse(is.na(argument), FALSE, TRUE))
d <- d %>% mutate(noSpace = ifelse(is.na(noSpace), FALSE, TRUE))
d <- d %>% mutate(blanketHog = ifelse(is.na(blanketHog), FALSE, TRUE))
d <- d %>% mutate(child = ifelse(is.na(child), FALSE, TRUE))
d <- d %>% mutate(workHours = ifelse(is.na(workHours), FALSE, TRUE))

#turn respective character columns to factor columns (they were multiple choice originally)
d$relationship <- factor(d$relationship, levels = c("Single", "Single, but cohabiting with a significant other",
                                                       "In a domestic partnership or civil union", "Married",
                                                       "Seperated", "Divorced", "Widowed"))
d$length <- factor(d$length, levels = c("Less than 1 year", "1-5 years", "6-10 years", "11-15 years",
                                           "16-20 years", "More than 20 years"))
d$sleepTogetherFrequency <- factor(d$sleepTogetherFrequency, levels = c("Never", "Once a year or less", "Once a month or less", "A few times per month",
                                                                           "A few times per week", "Every night"))
d$gender <- as.factor(d$gender)
d$ageGroup <- factor(d$ageGroup, levels = c("18-29", "30-44", "45-60", "> 60"))
d$income <- factor(d$income, levels = c("$0 - $24,999", "$25,000 - $49,999", "$50,000 - $99,999", "$100,000 - $149,999", "$150,000+"))
d$education <- factor(d$education, levels = c("Less than high school degree", "High school degree", 
                                              "Some college or Associate degree", "Bachelor degree", "Graduate degree"))
d$cencusLocation <- as.factor(d$cencusLocation)

#what kind of people do we have info on, anyways? Let's have an overview of our demographics
d %>% filter(!is.na(gender)) %>% ggplot(aes(x = gender)) + geom_bar() #even split, slightly more male
d %>% filter(!is.na(education)) %>% ggplot(aes(education)) + geom_bar() + coord_flip() #we have a HIGHLY educated sample#
#d %>% filter(!is.na(ageGroup)) %>% ggplot(aes(x = ageGroup)) + geom_bar() #mostly middle aged people
#d %>% filter(!is.na(income)) %>% ggplot(aes(x = income)) + geom_bar() #weighted towards a richer demographic, though with a median around 100K
#d %>% filter(!is.na(relationship)) %>% ggplot(aes(x = relationship)) + geom_bar() + coord_flip() #we don't have enough of each to commend on anything
#d %>% filter(!is.na(length)) %>% ggplot(aes(length)) + geom_bar()
#overall: our dataset is educated, wealthy, married, and around the middle age

#add in columns for number of reasons
d$numReasons <- rowSums(d == TRUE, na.rm = TRUE)
d <- d %>% mutate(hasReason = ifelse(numReasons > 0, TRUE, FALSE)) #1 is true
withReason <- d[d[, "numReasons"] > 0, ] #this set ticked off one of the reasons listed above
withoutReason <-d[d[, "numReasons"] <= 0, ] #the dataset that ticked off,,, nothing?

##what's the most common reason in the OneReason catagory?
oneReason <- d[d[, "numReasons"] == 1, ]

#need to mutate a new column with if elses that finds the one and puts it in a factored column
oneReason <- oneReason %>% mutate(reason = ifelse(snores, "snores", 
                                     ifelse(bathroomTrips, "bathroom", 
                                     ifelse(sick, "sick",
                                     ifelse(noSex, "noSex",
                                     ifelse(temperature, "temperature",
                                     ifelse(argument, "argument",
                                     ifelse(noSpace, "noSpace",
                                     ifelse(blanketHog, "blanketHog",
                                     ifelse(child, "child",
                                     ifelse(workHours, "work",NA)))))))))))
oneReason$reason <- as.factor(oneReason$reason)

#what's the most common reasons people don't sleep together?
base <- oneReason %>% group_by(reason) %>% summarize(counts = n()) 
(commonOne <- base %>% ggplot(aes(reason, counts)) + geom_bar(stat = "identity") +
  geom_text(aes(label = counts), vjust = -.3)) +ggtitle("Reasons Why Partners Sleep Seperately") +
  labs(y="Number of People", x = "Reason Why")

#length of relationships and reason for NOT sleeping together by length of relationship
oneReason %>% filter(!is.na(reason) & !is.na(length)) %>%filter(length != "Less than 1 year") %>% group_by(length) %>% 
  ggplot(aes(x = length, fill = reason)) + 
  geom_bar(position = "fill") + ylab("Percent") + coord_flip() +
  ggtitle("Sleeping Together by Length of Relationship") +
  labs(x="Length of Relationship", y = "Percent") + labs(fill = "Reason Why They Don't Sleep Together")

#length of relationships and reason for NOT sleeping together by age
oneReason %>% filter(!is.na(reason) & !is.na(ageGroup)) %>% group_by(length) %>% 
  ggplot(aes(x = ageGroup, fill = reason)) + 
  geom_bar(position = "fill") + ylab("Percent") + coord_flip() +
  ggtitle("Sleeping Together by Age Group") +
  labs(x="Age Group of Partners", y = "Percent") + labs(fill = "Reason Why They Don't Sleep Together")



#Plots the percentage breakdown of frequency of sleeping together by length, removing for people who never sleep together
withReason %>% filter(!is.na(sleepTogetherFrequency)) %>% filter(length != "Less than 1 year") %>%
  ggplot(aes(length, fill = sleepTogetherFrequency)) + ylab("Percent") +
  geom_bar(position = "fill") + coord_flip() + scale_fill_manual(values=c("#540D6E", 
                                                                          "#EE4266", "#FFD23F", "#3BCEAC", "#06823C")) +
  ggtitle("Sleeping Together by Length of Relationship") +
  labs(x="Length of Relationship", y = "Percent") + labs(fill = "Frequency of Sleeping Together")

#education affects frequency of sleeping together
withReason %>% filter(!is.na(sleepTogetherFrequency)) %>% filter(education != "Less than high school degree") %>% 
  ggplot(aes(education, fill = sleepTogetherFrequency)) + ylab("Percent") +
  geom_bar(position = "fill") + coord_flip() + scale_fill_manual(values=c("#540D6E", 
                                                                          "#EE4266", "#FFD23F", "#3BCEAC", "#06823C")) +
  ggtitle("Sleeping Together by Education of Partners") +
  labs(x="Education Level", y = "Percent") + labs(fill = "Frequency of Sleeping Together")

#possible point of bias - wealthier, more educated people
d %>% filter(!is.na(education)) %>% ggplot(aes(education)) + geom_bar() + 
  ggtitle("Respondent Education Distribution") + coord_flip() +
  labs(x="Education Level", y = "Number of Respondents") + labs(fill = "Frequency of Sleeping Together")
d %>% filter(!is.na(income)) %>% ggplot(aes(income)) + geom_bar() + 
  ggtitle("Respondent Income Distribution") + coord_flip() +
  labs(x="Income Level", y = "Number of Respondents") + labs(fill = "Frequency of Sleeping Together")