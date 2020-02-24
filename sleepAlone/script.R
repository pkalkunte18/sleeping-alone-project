library(tidyverse)

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
d %>% filter(!is.na(education)) %>% ggplot(aes(education)) + geom_bar() + coord_flip() #we have a HIGHLY educated sample
d %>% filter(!is.na(ageGroup)) %>% ggplot(aes(x = ageGroup)) + geom_bar() #mostly middle aged people
d %>% filter(!is.na(income)) %>% ggplot(aes(x = income)) + geom_bar() #weighted towards a richer demographic, though with a median around 100K
d %>% filter(!is.na(relationship)) %>% ggplot(aes(x = relationship)) + geom_bar() + coord_flip() #we don't have enough of each to commend on anything
d %>% filter(!is.na(length)) %>% ggplot(aes(length)) + geom_bar()
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

#what's the most common reasons?
base <- oneReason %>% group_by(reason) %>% summarize(counts = n()) 
(commonOne <- base %>% ggplot(aes(reason, counts)) + geom_bar(stat = "identity") +
  geom_text(aes(label = counts), vjust = -.3)) #snoring is the most common reason, then illness

#Plots the percentage breakdown of frequency of sleeping together by length
d %>% filter(!is.na(length)) %>% 
  ggplot(aes(length, fill = sleepTogetherFrequency)) + ylab("Percent") +
  geom_bar(position = "fill") + coord_flip() + scale_fill_manual(values=c("#540D6E", "#EE4266", "#000000", "#FFD23F", "#3BCEAC", "#06823C"))

withReason %>% filter(!is.na(sleepTogetherFrequency)) %>% 
  ggplot(aes(length, fill = sleepTogetherFrequency)) + ylab("Percent") +
  geom_bar(position = "fill") + coord_flip() + scale_fill_manual(values=c("#540D6E", 
                                    "#EE4266", "#FFD23F", "#3BCEAC", "#06823C"))

#does income lend trneds in sleeping together frequency?
d %>% filter(!is.na(sleepTogetherFrequency) & !is.na(income)) %>% 
  ggplot(aes(income, fill = sleepTogetherFrequency)) + ylab("Percent") +
  geom_bar(position = "fill") + coord_flip() + scale_fill_manual(values=c("#540D6E", "#EE4266", "#000000", "#FFD23F", "#3BCEAC", "#06823C"))

withReason %>% filter(!is.na(sleepTogetherFrequency) & !is.na(income)) %>% 
  ggplot(aes(income, fill = sleepTogetherFrequency)) + ylab("Percent") +
  geom_bar(position = "fill") + coord_flip() + scale_fill_manual(values=c("#540D6E", 
                                                                          "#EE4266", "#FFD23F", "#3BCEAC", "#06823C"))

#what's the major factors by education?
withReason %>% filter(!is.na(sleepTogetherFrequency) & !is.na(education) & education != "Less than high school degree") %>% 
  ggplot(aes(education, fill = sleepTogetherFrequency)) + ylab("Percent") +
  geom_bar(position = "fill") + coord_flip() + scale_fill_manual(values=c("#540D6E", 
                                                                          "#EE4266", "#FFD23F", "#3BCEAC", "#06823C"))
#To investigate by agegroup:
oneReason %>% filter(!is.na(reason) & !is.na(ageGroup)) %>% group_by(ageGroup) %>% ggplot(aes(x = ageGroup, fill = reason)) + geom_bar(position = "fill") + ylab("Percent") + coord_flip()

oneReason %>% filter(!is.na(reason) & !is.na(ageGroup)) %>% group_by(ageGroup) %>% ggplot(aes(x = ageGroup, fill = reason)) + geom_bar() + coord_flip()

#To investigate this stration by education:
oneReason %>% filter(!is.na(reason) & !is.na(education)) %>% group_by(education) %>% ggplot(aes(x = education, fill = reason)) + geom_bar(position = "fill") + ylab("Percent") + coord_flip()

oneReason %>% filter(!is.na(reason) & !is.na(education)) %>% group_by(education) %>% ggplot(aes(x = education, fill = reason)) + geom_bar() + coord_flip()

#To investigate this stration by income:
oneReason %>% filter(!is.na(reason) & !is.na(income)) %>% group_by(income) %>% ggplot(aes(x = income, fill = reason)) + geom_bar(position = "fill") + ylab("Percent") + coord_flip()

oneReason %>% filter(!is.na(reason) & !is.na(income)) %>% group_by(income) %>% ggplot(aes(x = income, fill = reason)) + geom_bar() + coord_flip()

#Let's see if there are reason differences between the sexes:
oneReason %>% filter(!is.na(reason) & !is.na(gender)) %>% group_by(reason) %>% ggplot(aes(x = reason, fill = gender)) + geom_bar(position = "fill") + ylab("Percent") 
oneReason %>% filter(!is.na(reason) & !is.na(gender)) %>% group_by(reason) %>% ggplot(aes(x = reason, fill = gender)) + geom_bar()