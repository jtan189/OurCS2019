#!/usr/bin/env Rscript
##
## https://github.com/jtan189/OurCS2019
##
## In an interactive R session or from another R script, run using:
## > source('prepare_data.R')

options(error=recover)

## install packages with, e.g.:
## > install.packages("dplyr")

suppressPackageStartupMessages(library(dplyr))
## suppressPackageStartupMessages(library(ggplot2))

kSurveyFilename <- "Survey Results.csv"

kSurveyColnames <- c(
    "start.date",
    "end.date",
    "response.type",
    "ip.address",
    "progress",
    "duration.sec",
    "finished",
    "recorded.date",
    "response.id",
    "recipient.last.name",
    "recipient.first.name",
    "recipient.email",
    "external.data.reference",
    "location.latitude",
    "location.longitude",
    "distribution.channel",
    "user.language",
    "month",
    "year",
    "times.use.device.in.day",
    "use.device.for.audio",
    "use.device.for.info",
    "use.device.for.control.other.devices",
    "use.device.for.conversation",
    "use.device.for.shopping",
    "use.device.for.alarm",
    "use.device.for.weather",
    "use.device.for.time",
    "use.device.for.reminders",
    "use.device.for.other",                        
    "use.device.for.other.text",
    "initial.privacy.concerns",
    "current.privacy.concerns",
    "what.privacy.concerns",
    "mic.on.by.default",
    "recordings.stored.by.company",
    "can.access.command.history",
    "device.option.delete.data",
    "no.device.privacy.settings",
    "age",
    "gender",
    "gender.self.describe",
    "work.it.or.cs.degree",
    "education.level"
)


survey.data <- read.csv(kSurveyFilename,
                        header=FALSE,
                        skip=3,
                        stringsAsFactors=FALSE,
                        col.names=kSurveyColnames)

survey.data <- survey.data %>%
    mutate(start.date = as.POSIXct(start.date),
           end.date = as.POSIXct(end.date),
           recorded.date = as.POSIXct(recorded.date)) %>%
    filter(response.type == "IP Address") %>%
    select(-response.type,
           -progress,
           -finished,
           -ip.address,
           -recipient.last.name,
           -recipient.first.name,
           -recipient.email,
           -external.data.reference,
           -location.latitude,
           -location.longitude,
           -distribution.channel,
           -user.language,
           -gender.self.describe)

survey.data$month <- as.numeric(survey.data$month)
survey.data$year <- as.numeric(survey.data$year)
survey.data <- survey.data %>%
  mutate(months.owned = (2019 - year) * 12 +
                        (10 - month))

## bin privacy knowledge
survey.data <- survey.data %>%
  mutate(mic.on.by.default.correct = mic.on.by.default %in% c("Probably correct",
                                                              "Definitely correct"),
         recordings.stored.by.company.correct = recordings.stored.by.company %in% 
                                           c("Probably correct",
                                             "Definitely correct"),
         can.access.command.history.correct = can.access.command.history %in% 
                                          c("Probably correct",
                                            "Definitely correct"),
         device.option.delete.data.correct = device.option.delete.data %in% 
                                        c("Probably correct",
                                          "Definitely correct"),
         no.device.privacy.settings.correct = no.device.privacy.settings %in%
                                                 c("Probably incorrect",
                                                   "Definitely incorrect"),
         total.correct = mic.on.by.default.correct +
           can.access.command.history.correct +
           device.option.delete.data.correct + 
           no.device.privacy.settings.correct)
         

survey.data$times.use.device.in.day <- as.factor(survey.data$times.use.device.in.day)

survey.data$use.device.for.audio <- survey.data$use.device.for.audio != ""
survey.data$use.device.for.info <- survey.data$use.device.for.info != ""
survey.data$use.device.for.conversation <- survey.data$use.device.for.conversation != ""
survey.data$use.device.for.control.other.devices <- survey.data$use.device.for.control.other.devices != ""
survey.data$use.device.for.shopping <- survey.data$use.device.for.shopping != ""
survey.data$use.device.for.alarm <- survey.data$use.device.for.alarm != ""
survey.data$use.device.for.weather <- survey.data$use.device.for.weather != ""
survey.data$use.device.for.time <- survey.data$use.device.for.time != ""
survey.data$use.device.for.reminders <- survey.data$use.device.for.reminders != ""
survey.data$use.device.for.other <- survey.data$use.device.for.other != ""

survey.data$gender <- as.factor(survey.data$gender)
survey.data$education.level <- as.factor(survey.data$education.level)

kLikertAgree <- c(
    "Strongly disagree",
    "Somewhat disagree",
    "Neither agree nor disagree",
    "Somewhat agree",
    "Strongly agree"
)

survey.data$initial.privacy.concerns <- factor(survey.data$initial.privacy.concerns,
                                               levels=kLikertAgree,
                                               ordered=TRUE)
survey.data$current.privacy.concerns <- factor(survey.data$current.privacy.concerns,
                                               levels=kLikertAgree,
                                               ordered=TRUE)

kLikertCorrect <- c(
    "Definitely incorrect",
    "Probably incorrect",
    "I don't know",
    "Probably correct",
    "Definitely correct"
)

survey.data$mic.on.by.default <- factor(survey.data$mic.on.by.default,
                                       levels=kLikertCorrect,
                                       ordered=TRUE)

survey.data$recordings.stored.by.company <- factor(survey.data$recordings.stored.by.company,
                                                   levels=kLikertCorrect,
                                                   ordered=TRUE)

survey.data$can.access.command.history <- factor(survey.data$can.access.command.history,
                                                 levels=kLikertCorrect,
                                                 ordered=TRUE)

survey.data$device.option.delete.data <- factor(survey.data$device.option.delete.data,
                                                levels=kLikertCorrect,
                                                ordered=TRUE)

survey.data$no.device.privacy.settings <- factor(survey.data$no.device.privacy.settings,
                                                 levels=kLikertCorrect,
                                                 ordered=TRUE)
kAge <- c("18 - 24",
          "25 - 34",
          "35 - 44",
          "45 - 54",
          "55 - 64")

survey.data$age <- factor(survey.data$age,
                          levels=kAge,
                          ordered=TRUE)

survey.data$work.it.or.cs.degree <- factor(survey.data$work.it.or.cs.degree,
                                                 levels=c("Yes", "No"))

rm(kAge,
   kLikertAgree,
   kLikertCorrect,
   kSurveyColnames,
   kSurveyFilename)
