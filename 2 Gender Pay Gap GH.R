# Introduction #####

# This R code was used for the articles: 
# https://www.littalics.com/gender-pay-gap-and-people-analytics-a-practice-with-open-data/
# https://www.littalics.com/finding-hidden-patterns-in-gender-pay-gap-data/
# Data were anonymized, Annual salary data were randomized (multiplied by random number between 0.98-1.02)



# Setting environment #####

rm(list=ls())
#setwd("~/...")

library(tidyverse)
library(lubridate)
library(summarytools)
library(broom)
library(gridExtra)



# Loading and munging #####

Employees <- read_csv("https://raw.githubusercontent.com/Littal/Gender-Pay-Gap/main/2%20Gender%20Pay%20Gap%20GH.csv")

Employees <- Employees %>%
  mutate(Gender=as.factor(Gender)) %>%
  mutate(Department=as.factor(Department)) %>%
  mutate(Assignment=as.factor(Assignment))  %>%
  mutate(Date.Hired=mdy(Date.Hired)) %>%
  mutate(Tenure.Years=((mdy("12/31/2017")-Date.Hired)/365)) %>%
  mutate(Tenure = as.factor(case_when(
    Tenure.Years >= 15 ~ "15-20+",
    Tenure.Years >= 10 & Tenure.Years < 15 ~ "10-15",
    Tenure.Years >= 5 & Tenure.Years < 10 ~ "5-10",
    Tenure.Years <5 ~ "0-5"))) %>%
    mutate(Tenure = factor(Tenure, levels = c("0-5", "5-10", "10-15", "15-20+")))

# Nice tibble? YES
glimpse(Employees)

# Missing values? NO
Employees %>%
  summarise(Gender_mv = sum(is.na(Gender)),
            Annual.Salary_mv = sum(is.na(Annual.Salary)),
            Department_mv = sum(is.na(Department)),
            Department.Name_mv = sum(is.na(Department.Name)),
            Assignment_mv = sum(is.na(Assignment)),
            Position.Title_mv = sum(is.na(Position.Title)),
            Date.Hired_mv = sum(is.na(Date.Hired)),
            Tenure.Years_mv = sum(is.na(Tenure.Years)),
            Tenure = sum(is.na(Tenure))) 



# Background variables #####

# Independent variables: Gender, Assignment, Tenure
# Frequencies, cross-tabs (dependent?)

Employees %>%
  freq(Gender, report.nas = FALSE, round.digits = 1, headings = TRUE) 
Employees %>%
  freq(Assignment, report.nas = FALSE, round.digits = 1, headings = TRUE)
Employees %>%  
  freq(Tenure, report.nas = FALSE, round.digits = 1, headings = TRUE)

ctable(Employees$Gender, Employees$Assignment, method = 'render')
chisq.test(table(Employees$Gender, Employees$Assignment))

ctable(Employees$Gender, Employees$Tenure, method = 'render')
chisq.test(table(Employees$Gender, Employees$Tenure))

ctable(Employees$Assignment, Employees$Tenure, method = 'render')
chisq.test(table(Employees$Assignment, Employees$Tenure))



# Research variable #####

# Dependent variable: Annual.Salary
# Distribution, descriptive and exploration by Gender


# Explore all employees' annual salary

descr(Employees$Annual.Salary, style = "rmarkdown")

par(mfrow = c(2, 1))
hist(Employees$Annual.Salary, breaks = 100, freq = FALSE, xlab = NULL, 
     main = "Current Annual Salary", col = "grey")
curve(dnorm (x, mean = mean(Employees$Annual.Salary), 
             sd = sd(Employees$Annual.Salary)), add = TRUE, col = "red", lwd = 2)
boxplot(Employees$Annual.Salary, horizontal = TRUE, col = "grey")


# Explore genders' annual salary (base R)

with(Employees, stby(data = Annual.Salary, INDICES = Gender, 
                   FUN = descr, stats = c("mean", "sd", "min", "med", "max")))
tidy(t.test(Annual.Salary ~ Gender, data = Employees))

par(mfrow = c(2, 2))
hist(Employees$Annual.Salary[Employees$Gender=="F"], 
     col="orange", xlim=c(0,300000), breaks = 50, freq = FALSE, xlab = NULL, 
     main = "Current Annual Salary - Women, Avg 73K")
curve(dnorm (x, mean = mean(Employees$Annual.Salary), 
             sd = sd(Employees$Annual.Salary)), add = TRUE)
boxplot(Employees$Annual.Salary[Employees$Gender=="F"], 
        horizontal = TRUE, col="orange", ylim = c(0,300000))
hist(Employees$Annual.Salary[Employees$Gender=="M"], 
     col="lightblue", xlim=c(0,300000), breaks = 50, freq = FALSE, xlab = NULL, 
     main = "Current Annual Salary - Men, Avg 77K")
curve(dnorm (x, mean = mean(Employees$Annual.Salary), 
             sd = sd(Employees$Annual.Salary)), add = TRUE)
boxplot(Employees$Annual.Salary[Employees$Gender=="M"], 
        horizontal = TRUE, col="lightblue", ylim = c(0,300000))


# two-ways Anova to explain annual salary by background variables

anova1 <- aov(Annual.Salary ~ Gender + Assignment + Gender:Assignment, data = Employees)
summary(anova1)

anova2 <- aov(Annual.Salary ~ Gender + Tenure + Gender:Tenure, data = Employees)
summary(anova2)

anova3 <- aov(Annual.Salary ~ Assignment + Tenure + Assignment:Tenure, data = Employees)
summary(anova3)


# old interaction plots

interaction1 <- aggregate(Employees$Annual.Salary, 
                          by=list(Employees$Gender, Employees$Assignment), 
                          FUN=mean, na.rm=TRUE)
colnames(interaction1) <-c("Gender", "Assignment", "Salary")
interaction1

interaction2 <- aggregate(Employees$Annual.Salary, 
                          by=list(Employees$Gender, Employees$Tenure), 
                          FUN=mean, na.rm=TRUE)
colnames(interaction2) <-c("Gender", "Tenure", "Salary")
interaction2

interaction3 <- aggregate(Employees$Annual.Salary, 
                          by=list(Employees$Assignment, Employees$Tenure), 
                          FUN=mean, na.rm=TRUE)
colnames(interaction3) <-c("Assignment", "Tenure", "Salary")
interaction3


interaction1plot <- ggplot(data=interaction1, mapping=aes(x=Assignment, y=Salary, color=Gender)) +
                      geom_point() + geom_line(aes(group = Gender)) +
                      scale_y_continuous(name="Annual Salary", limits=c(30000,100000), labels = scales::comma)
interaction2plot <- ggplot(data=interaction2, mapping=aes(x=Tenure, y=Salary, color=Gender)) +
                      geom_point() + geom_line(aes(group = Gender)) +
                      scale_y_continuous(name="Annual Salary", limits=c(30000,100000), labels = scales::comma)
interaction3plot <- ggplot(data=interaction3, mapping=aes(x=Tenure, y=Salary, color=Assignment)) +
                      geom_point() + geom_line(aes(group = Assignment)) +
                      scale_y_continuous(name="Annual Salary", limits=c(30000,100000), labels = scales::comma)

grid.arrange(interaction1plot, interaction2plot, interaction3plot, nrow=3)


# new interaction plots

interaction2plot <- ggplot(data=interaction2, mapping=aes(x=Tenure, y=Salary, color=Gender)) +
  geom_point(size = 5) + geom_line(aes(group = Gender), size=1) +
  scale_y_continuous(name="Annual Salary", limits=c(30000,100000), labels = scales::comma) +
  scale_color_manual(values=c("orange", "skyblue")) + theme_bw()
interaction2plot

# (not used)
interaction2facet <- aggregate(Employees$Annual.Salary, 
                          by=list(Employees$Gender, Employees$Tenure, Employees$Assignment), 
                          FUN=mean, na.rm=TRUE)
colnames(interaction2facet) <-c("Gender", "Tenure", "Assignment", "Salary")
interaction2facet

interaction2plotfacet <- ggplot(data=interaction2facet, mapping=aes(x=Tenure, y=Salary, color=Gender)) +
  geom_point() + geom_line(aes(group = Gender)) +
  scale_y_continuous(name="Annual Salary", limits=c(30000,100000), labels = scales::comma) + 
  facet_wrap(~Assignment)
interaction2plotfacet


# Exploring gender and tenure with linear regression (not used)

reg2plot <- Employees %>%
  ggplot(aes(Tenure.Years, Annual.Salary, color=Gender)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, aes(colour=Gender)) + 
  ylim(0,200000)
reg2plot

reg2 <- lm(Annual.Salary ~ Tenure.Years*Gender, data = Employees)
reg2
summary(reg2)


# Analysis for diverse roles only #####

# First find position with both genders

DiversePositions <- Employees %>%
  group_by(Position.Title) %>%
  summarize(Male=sum(Gender=="M"), Female=sum(Gender=="F")) %>%
  filter(Male>0 & Female>0) %>%
  select(Position.Title) %>%
  pull(Position.Title)

EmployeesDiverse <- Employees %>%
  mutate(Diverse = ifelse(Position.Title %in% DiversePositions, 1, 0)) %>%
  filter(Diverse == 1)


# Explore again genders' annual salary (base R)

with(EmployeesDiverse, stby(data = Annual.Salary, INDICES = Gender, 
                     FUN = descr, stats = c("mean", "sd", "min", "med", "max")))
tidy(t.test(Annual.Salary ~ Gender, data = EmployeesDiverse))

par(mfrow = c(2, 2))
hist(EmployeesDiverse$Annual.Salary[EmployeesDiverse$Gender=="F"], 
     col="orange", xlim=c(0,300000), breaks = 50, freq = FALSE, xlab = NULL, main = "Current Annual Salary - Women, Avg 72K")
curve(dnorm (x, mean = mean(EmployeesDiverse$Annual.Salary), 
             sd = sd(EmployeesDiverse$Annual.Salary)), add = TRUE)
boxplot(EmployeesDiverse$Annual.Salary[EmployeesDiverse$Gender=="F"], 
        horizontal = TRUE, col="orange", ylim = c(0,300000))
hist(EmployeesDiverse$Annual.Salary[EmployeesDiverse$Gender=="M"], 
     col="lightblue", xlim=c(0,300000), breaks = 50, freq = FALSE, xlab = NULL, main = "Current Annual Salary - Men, Avg 78K")
curve(dnorm (x, mean = mean(EmployeesDiverse$Annual.Salary), 
             sd = sd(EmployeesDiverse$Annual.Salary)), add = TRUE)
boxplot(EmployeesDiverse$Annual.Salary[EmployeesDiverse$Gender=="M"], 
        horizontal = TRUE, col="lightblue", ylim = c(0,300000))


# Sort roles by gender pay gap

EmployeesDiverseSort <- EmployeesDiverse %>%
  group_by(Gender, Position.Title) %>%
  summarize(Salary = mean(Annual.Salary)) %>%
  arrange(Position.Title) %>% 
  group_by(Position.Title) %>%
  mutate(Gap = Salary - lag(Salary)) %>%
  filter(Gender == "M") %>%
  arrange(desc(Gap))

# new interaction plots

interaction2diverse <- aggregate(EmployeesDiverse$Annual.Salary, 
                          by=list(EmployeesDiverse$Gender, EmployeesDiverse$Tenure), 
                          FUN=mean, na.rm=TRUE)
colnames(interaction2diverse) <-c("Gender", "Tenure", "Salary")
interaction2diverse

interaction2plotdiverse <- ggplot(data=interaction2diverse, mapping=aes(x=Tenure, y=Salary, color=Gender)) +
  geom_point(size = 5) + geom_line(aes(group = Gender), size=1) +
  scale_y_continuous(name="Annual Salary", limits=c(30000,100000), labels = scales::comma) +
  scale_color_manual(values=c("orange", "skyblue")) + theme_bw()
interaction2plotdiverse

# (not used)
interaction2facetdiverse <- aggregate(EmployeesDiverse$Annual.Salary, 
                               by=list(EmployeesDiverse$Gender, EmployeesDiverse$Tenure, EmployeesDiverse$Assignment), 
                               FUN=mean, na.rm=TRUE)
colnames(interaction2facetdiverse) <-c("Gender", "Tenure", "Assignment", "Salary")
interaction2facet

interaction2plotfacetdiverse <- ggplot(data=interaction2facetdiverse, mapping=aes(x=Tenure, y=Salary, color=Gender)) +
  geom_point() + geom_line(aes(group = Gender)) +
  scale_y_continuous(name="Annual Salary", limits=c(30000,100000), labels = scales::comma) + 
  facet_wrap(~Assignment)
interaction2plotfacetdiverse


# Exploring gender and tenure with linear relationship

reg2plotdiverse <- EmployeesDiverse %>%
  ggplot(aes(Tenure.Years, Annual.Salary, color=Gender)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = lm, se = FALSE, aes(colour=Gender), size=1.75) + 
  ylim(0,200000) +
  scale_color_manual(values=c("orange", "skyblue")) + theme_bw()
reg2plotdiverse

reg2Aplotdiverse <- EmployeesDiverse %>%
  ggplot(aes(Tenure.Years, Annual.Salary, color=Gender, size=Assignment)) +  #Part-time dots enlarged
  geom_point(alpha = 0.3) + 
  geom_smooth(method = lm, se = FALSE, aes(colour=Gender), size=1.75) + 
  ylim(0,200000) +
  scale_color_manual(values=c("orange", "skyblue")) + theme_bw()
reg2Aplotdiverse

reg2Bplotdiverse <- EmployeesDiverse %>%
  ggplot(aes(Tenure.Years, Annual.Salary, color=Gender, size=Assignment)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = lm, se = FALSE, aes(colour=Gender, linetype=Assignment), size=1.75) + #Part-time line separated
  ylim(0,200000) +
  scale_color_manual(values=c("orange", "skyblue")) + theme_bw()
reg2Bplotdiverse


# Presenting interactions in ANOVA and Regression

ANOVAtwo <- aov(Annual.Salary~Tenure*Gender, data = EmployeesDiverse)
ANOVAtwo
summary(ANOVAtwo)

ANOVAthree <- aov(Annual.Salary~Tenure*Gender*Assignment, data = EmployeesDiverse)
ANOVAthree
summary(ANOVAthree)


REGtwo <- lm(Annual.Salary ~ Tenure.Years*Gender, data = EmployeesDiverse)
REGtwo
summary(REGtwo)

REGthree <- lm(Annual.Salary ~ Tenure.Years*Gender*Assignment, data = EmployeesDiverse)
REGthree
summary(REGthree)

