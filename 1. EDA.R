#Load the dataset and packages and set seed
bank.full <- read.csv("~/Desktop/Assignment 2/Bank-additional/bank-additional-full.csv", sep=";")
set.seed(123)

library(ggplot2)
library(tidyverse)
library(dplyr)

#Underdstand the data -> numerical/categorical data
head(bank.full)
tail(bank.full)
summary(bank.full)
str(bank.full)

#Change the name of output y to Deposit
bank.full <- bank.full %>% rename(Deposit = y)
colnames(bank.full)

#Show the percentage distributions of two output classes
count <- table(bank.full$Deposit)
count

no_class_percent <- (count[1] / sum(count[]))
no_class_percent

yes_class_percent <- (count[2] / sum(count[]))
yes_class_percent

#Find the mean values of numerical attributes for the binary output
mean_summary <- bank.full %>% group_by(Deposit) %>% summarise(campaign_mean=mean(campaign),previous_mean=mean(previous),emp.var.rate_mean=mean(emp.var.rate),cons.price.idx_mean=mean(cons.price.idx),cons.conf.idx_mean=mean(cons.conf.idx),age_mean=mean(age), nremployed_mean=mean(nr.employed), duration_mean=mean(duration),pdays_mean=mean(pdays),euribor3m_mean=mean(euribor3m))
mean_summary

#To understand the distribution in categorical attributes: job
bank.full_job <- bank.full %>% group_by(job) %>% summarise(frequency=n()) %>% arrange(desc(frequency))
bank.full_job$job <- factor(bank.full_job$job, levels=bank.full_job$job[order(-bank.full_job$frequency)])

ggplot(data = bank.full_job, mapping = aes(x=job, y=frequency)) +
  geom_bar(stat="identity", width=0.7, fill="steelblue") +
  labs(title="Job type distribution") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=12,angle = -20)) +
  geom_text(aes(label=frequency), size=5,vjust=-0.3)

bank_job <- bank.full %>% group_by(job,Deposit) %>% summarize(count=n())

ggplot(data = bank_job, mapping = aes(x=job, y=count, fill=Deposit)) +
  geom_bar(stat = "identity", width=0.8,position = "dodge") +
  labs(title="Job type to Deopsit binary distribution", size=10) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=12,angle = -20)) +
  geom_text(aes(label=count), size=5, vjust=-0.5, position = position_dodge(0.9))

#To understand the distribution in categorical attributes: marital status
bank.full_marital <- bank.full %>% group_by(marital) %>% summarise(frequency=n()) %>% arrange(desc(frequency))
bank.full_marital$marital <- factor(bank.full_marital$marital, levels=bank.full_marital$marital[order(-bank.full_marital$frequency)])

ggplot(data = bank.full_marital, mapping = aes(x=marital, y=frequency)) +
  geom_bar(stat="identity", width=0.4, fill="steelblue") +
  labs(title="Marital status distribution") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=12,angle = -20)) +
  geom_text(aes(label=frequency), size=5,vjust=-0.3)

bank_marital <- bank.full %>% group_by(marital,Deposit) %>% summarize(count=n())

ggplot(data = bank_marital, mapping = aes(x=marital, y=count, fill=Deposit)) +
  geom_bar(stat = "identity", width=0.6,position = "dodge") +
  labs(title="Marital status to Deopsit binary distribution", size=10) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=15)) +
  geom_text(aes(label=count), size=5, vjust=-0.5, position = position_dodge(0.6))

#To understand the distribution in categorical attributes: education
bank.full_education <- bank.full %>% group_by(education) %>% summarise(frequency=n()) %>% arrange(desc(frequency))
bank.full_education$education <- factor(bank.full_education$education, levels=bank.full_education$education[order(-bank.full_education$frequency)])

ggplot(data = bank.full_education, mapping = aes(x=education, y=frequency)) +
  geom_bar(stat="identity", width=0.5, fill="steelblue") +
  labs(title="Education level distribution") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=12,angle = -10)) +
  geom_text(aes(label=frequency), size=5,vjust=-0.3)

bank_education <- bank.full %>% group_by(education,Deposit) %>% summarize(count=n())

ggplot(data = bank_education, mapping = aes(x=education, y=count, fill=Deposit)) +
  geom_bar(stat = "identity", width=0.8,position = "dodge") +
  labs(title="Education level to Deopsit binary distribution", size=10) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=15,angle = -20)) +
  geom_text(aes(label=count), size=5, vjust=-0.5, position = position_dodge(0.7))

#To understand the distribution in categorical attributes: default credit
bank.full_default <- bank.full %>% group_by(default) %>% summarise(frequency=n()) %>% arrange(desc(default))
bank.full_default$default <- factor(bank.full_default$default , levels=bank.full_default$default[order(-bank.full_default$frequency)])

ggplot(data = bank.full_default, mapping = aes(x=default, y=frequency)) +
  geom_bar(stat="identity", width=0.3, fill="steelblue") +
  labs(title="Default credit distribution") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=15)) +
  geom_text(aes(label=frequency), size=5,vjust=-0.3)

bank_default <- bank.full %>% group_by(default,Deposit) %>% summarize(count=n())

ggplot(data = bank_default, mapping = aes(x=default, y=count, fill=Deposit)) +
  geom_bar(stat = "identity", width=0.6,position = "dodge") +
  labs(title="Default credit to Deopsit binary distribution", size=10) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=15)) +
  geom_text(aes(label=count), size=5, vjust=-0.5, position = position_dodge(0.6))

#To understand the distribution in categorical attributes: housing loan
bank.full_housing <- bank.full %>% group_by(housing) %>% summarise(frequency=n()) %>% arrange(desc(housing))
bank.full_housing$housing <- factor(bank.full_housing$housing, levels=bank.full_housing$housing[order(-bank.full_housing$frequency)])

ggplot(data = bank.full_housing, mapping = aes(x=housing, y=frequency)) +
  geom_bar(stat="identity", width=0.4, fill="steelblue") +
  labs(title="Housing loan distribution") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=12)) +
  geom_text(aes(label=frequency), size=5,vjust=-0.3)

bank_housing <- bank.full %>% group_by(housing,Deposit) %>% summarize(count=n())

ggplot(data = bank_housing, mapping = aes(x=housing, y=count, fill=Deposit)) +
  geom_bar(stat = "identity", width=0.6,position = "dodge") +
  labs(title="Housing loan to Deopsit binary distribution", size=10) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=15)) +
  geom_text(aes(label=count), size=5, vjust=-0.5, position = position_dodge(0.6))

#To understand the distribution in categorical attributes: personal loan
bank.full_loan <- bank.full %>% group_by(loan) %>% summarise(frequency=n()) %>% arrange(desc(loan))
bank.full_loan$loan <- factor(bank.full_loan$loan, levels=bank.full_loan$loan[order(-bank.full_loan$frequency)])

ggplot(data = bank.full_loan, mapping = aes(x=loan, y=frequency)) +
  geom_bar(stat="identity", width=0.4, fill="steelblue") +
  labs(title="Personal loan distribution") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=12)) +
  geom_text(aes(label=frequency), size=5,vjust=-0.3)

bank_loan <- bank.full %>% group_by(loan,Deposit) %>% summarize(count=n())

ggplot(data = bank_loan, mapping = aes(x=loan, y=count, fill=Deposit)) +
  geom_bar(stat = "identity", width=0.6,position = "dodge") +
  labs(title="Personal loan to Deopsit binary distribution", size=10) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=15)) +
  geom_text(aes(label=count), size=5, vjust=-0.5, position = position_dodge(0.6))

#To understand the distribution in categorical attributes: contact type
bank.full_contact <- bank.full %>% group_by(contact) %>% summarise(frequency=n()) %>% arrange(desc(contact))
bank.full_contact$contact <- factor(bank.full_contact$contact, levels=bank.full_contact$contact[order(-bank.full_contact$frequency)])

ggplot(data = bank.full_contact, mapping = aes(x=contact, y=frequency)) +
  geom_bar(stat="identity", width=0.3, fill="steelblue") +
  labs(title="Contact type distribution") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=12)) +
  geom_text(aes(label=frequency), size=5,vjust=-0.3)

bank_contact <- bank.full %>% group_by(contact,Deposit) %>% summarize(count=n())

ggplot(data = bank_contact, mapping = aes(x=contact, y=count, fill=Deposit)) +
  geom_bar(stat = "identity", width=0.5,position = "dodge") +
  labs(title="Contact type to Deopsit binary distribution", size=10) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=15)) +
  geom_text(aes(label=count), size=5, vjust=-0.5, position = position_dodge(0.5))

#To understand the distribution in categorical attributes: last contact month
bank.full_month <- bank.full %>% group_by(month) %>% summarise(frequency=n()) %>% arrange(desc(month))
bank.full_month$month <- factor(bank.full_month$month, levels=bank.full_month$month[order(-bank.full_month$frequency)])

ggplot(data = bank.full_month, mapping = aes(x=month, y=frequency)) +
  geom_bar(stat="identity", width=0.6, fill="steelblue") +
  labs(title="Last contact month distribution") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=12)) +
  geom_text(aes(label=frequency), size=5,vjust=-0.3)

bank_month <- bank.full %>% group_by(month,Deposit) %>% summarize(count=n())

ggplot(data = bank_month, mapping = aes(x=month, y=count, fill=Deposit)) +
  geom_bar(stat = "identity", width=0.8,position = "dodge") +
  labs(title="Last contact month to Deopsit binary distribution", size=10) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=15)) +
  geom_text(aes(label=count), size=4, vjust=-0.5, position = position_dodge(0.7))

#To understand the distribution in categorical attributes: last contact day
bank.full_day_of_week <- bank.full %>% group_by(day_of_week) %>% summarise(frequency=n()) %>% arrange(desc(day_of_week))
bank.full_day_of_week$day_of_week <- factor(bank.full_day_of_week$day_of_week, levels=bank.full_day_of_week$day_of_week[order(-bank.full_day_of_week$frequency)])

ggplot(data = bank.full_day_of_week, mapping = aes(x=day_of_week, y=frequency)) +
  geom_bar(stat="identity", width=0.4, fill="steelblue") +
  labs(title="Last contact day of week distribution") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=12)) +
  geom_text(aes(label=frequency), size=5,vjust=-0.3)

bank_day <- bank.full %>% group_by(day_of_week,Deposit) %>% summarize(count=n())

ggplot(data = bank_day, mapping = aes(x=day_of_week, y=count, fill=Deposit)) +
  geom_bar(stat = "identity", width=0.6,position = "dodge") +
  labs(title="Last contact day of week to Deopsit binary distribution", size=10) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=15)) +
  geom_text(aes(label=count), size=5, vjust=-0.5, position = position_dodge(0.6))

#To understand the distribution in categorical attributes: outcome of previous campaign
bank.full_poutcome <- bank.full %>% group_by(poutcome) %>% summarise(frequency=n()) %>% arrange(desc(poutcome))
bank.full_poutcome$poutcome <- factor(bank.full_poutcome$poutcome, levels=bank.full_poutcome$poutcome[order(-bank.full_poutcome$frequency)])

ggplot(data = bank.full_poutcome, mapping = aes(x=poutcome, y=frequency)) +
  geom_bar(stat="identity", width=0.3, fill="steelblue") +
  labs(title="Outcome of previous campaign distribution") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=12)) +
  geom_text(aes(label=frequency), size=5,vjust=-0.3)

bank_poutcome <- bank.full %>% group_by(poutcome,Deposit) %>% summarize(count=n())

ggplot(data = bank_poutcome, mapping = aes(x=poutcome, y=count, fill=Deposit)) +
  geom_bar(stat = "identity", width=0.6,position = "dodge") +
  labs(title="Outcome of previous campaign to Deopsit binary distribution", size=10) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(axis.text.x = element_text(face="bold.italic",size=15)) +
  geom_text(aes(label=count), size=5, vjust=-0.5, position = position_dodge(0.6))

#To delete the default column for preparing training_set and test_set -> 8597 unknown
bank.full <- bank.full %>% select(-default)


# Find correlation from data by correlation coefficient between attributes
library(Hmisc)
bank.rcorr = rcorr(as.matrix(bank.full))
bank.coeff = bank.rcorr$r

bank_y <- bank.coeff[,20]

bank_y <- bank_y[-20]

barplot(bank_y[order(bank_y)],
        main = "Correlation coefficient of different variables to Deposit",
        xlab = "Correlartion coefficient",
        ylab = "Variables",
        font.lab = "2",
        font.main = "4",
        las = "1",
        names.arg = c("age", "job", "mar", "edu","hou", "loan","cont","mon","day","dura","camp","pday","prev","pout","emp","cons","cons","euri","nr.em"),
        col = "darkred",
        horiz = TRUE)


library(corrplot)
corrplot(bank.coeff)
