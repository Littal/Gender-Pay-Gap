# Introduction #####

# This R code was used for the article: 
# https://www.littalics.com/gender-pay-gap-and-people-analytics-a-practice-with-open-data/
# Annual salary was randomized (multipled by random number between 0.98-1.02)


# Setting environment #####

rm(list=ls())
#setwd("~/...")

library(tidyverse)
library(lubridate)
library(summarytools)
library(broom)
library(gridExtra)


# Loading and munging #####

Employees <- read_csv("2 Gender Pay Gap GH.csv")

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
# Frequencies, crosstubs (dependent?)

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

# Explore genders' annual salary 
with(Employees, stby(data = Annual.Salary, INDICES = Gender, 
                   FUN = descr, stats = c("mean", "sd", "min", "med", "max")))
tidy(t.test(Annual.Salary ~ Gender, data = Employees))

par(mfrow = c(2, 2))
hist(Employees$Annual.Salary[Employees$Gender=="F"], 
     col="orange", xlim=c(0,300000), breaks = 50, freq = FALSE, xlab = NULL, main = "Current Annual Salary - Women")
curve(dnorm (x, mean = mean(Employees$Annual.Salary), 
             sd = sd(Employees$Annual.Salary)), add = TRUE)
boxplot(Employees$Annual.Salary[Employees$Gender=="F"], 
        horizontal = TRUE, col="orange", ylim = c(0,300000))
hist(Employees$Annual.Salary[Employees$Gender=="M"], 
     col="lightblue", xlim=c(0,300000), breaks = 50, freq = FALSE, xlab = NULL, main = "Current Annual Salary - Men")
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


# interaction plots

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

library(gridExtra)
grid.arrange(interaction1plot, interaction2plot, interaction3plot, nrow=3)

