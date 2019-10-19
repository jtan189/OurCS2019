#!/usr/bin/env Rscript

source('prepare_data.R')

cat("## Descriptive statistics\n")

cat("\nTotal participants:", nrow(survey.data), "\n")

cat("\nAge:\n")
print(summary(survey.data$age))

cat("\nGender:")
print(table(survey.data$gender))

cat("\nWork in IT or CS degree:")
print(table(survey.data$work.it.or.cs.degree))

cat("\nEducation level:")
print(table(survey.data$education.level))

cat("\nMonths owned device:\n")
print(summary(survey.data$months.owned))

cat("\nTimes use device in a day:\n")
print(summary(survey.data$times.use.device.in.day))

cat("\nDevice usage:\n")
for (device.usage in device.usage.types) {

    cat(paste0("    ",
        device.usage, ": ",
        sum(survey.data[, device.usage]), " (",
        round(sum(survey.data[, device.usage]) / nrow(survey.data) * 100,
              digits=2),
        "%)\n"))
}

cat("\nInitial privacy concerns:\n")
print(table(survey.data$initial.privacy.concerns))

cat("\nCurrent privacy concerns:\n")
print(table(survey.data$current.privacy.concerns))

cat("\nParticipant belief in data practices:\n")

for (practice in privacy.practices.beliefs) {

    cat(paste0("    ",
        practice, ": ",
        sum(survey.data[, practice]), " (",
        round(sum(survey.data[, practice]) / nrow(survey.data) * 100,
              digits=2),
        "%)\n"))
}

cat("\nParticipant disbelief in data practices:\n")


for (practice in privacy.practices.nonbeliefs) {

    cat(paste0("    ",
        practice, ": ",
        sum(survey.data[, practice]), " (",
        round(sum(survey.data[, practice]) / nrow(survey.data) * 100,
              digits=2),
        "%)\n"))
}

cat("\nParticipant correctness about data practices (unsure considered incorrect):\n")
for (practice in privacy.practices.was.correct) {

    cat(paste0("    ",
        practice, ": ",
        sum(survey.data[, practice]), " (",
        round(sum(survey.data[, practice]) / nrow(survey.data) * 100,
              digits=2),
        "%)\n"))
}


cat("\n\n## Hypothesis tests\n")

cat("\nH1: people who have owned these devices longer know more")
## own device longer -> know more about privacy?
## vars: total.correct, months.owned
## test name: Spearman's rank correlation coefficient
h1.result <- cor.test(survey.data$months.owned,
                      survey.data$total.correct,
                      type="spearman")
print(h1.result)



cat("\nH2: education and tech expertise are positively correlated with knowledge")
## vars: total.correct, education.level, work.it.or.cs.degree
cat("\n  Education level:\n")
h2.edu.result <- cor.test(as.numeric(survey.data$education.level),
                          survey.data$total.correct,
                          type="spearman")
print(h2.edu.result)

cat("\n  Technical expertise:\n")
h2.tech.result <- chisq.test(table(survey.data$work.it.or.cs.degree,
                                  survey.data$total.correct))
print(h2.tech.result)



cat("\nH3: users with different levels of knowledge view/use devices differently\n")
## vars: total.correct (our level of knowledge proxy)
##       use diff:
##          could be: frequency of use, types of use, amount of use, etc.
##
## We'll see if different level of knowledge leads to different usages
## Note: In pracitce, if perform many hypothesis tests, we should
##       apply a multiple-testing correction (e.g. Holm-Bonferonni)
##       and interpret the corrected p-values

usage.summary <- survey.data %>%
    group_by(total.correct) %>%
    summarize(num.use.for.audio = sum(use.device.for.audio),
              num.use.for.info = sum(use.device.for.info),
              num.use.for.control.other.devices = sum(use.device.for.control.other.devices),
              num.use.for.conversation = sum(use.device.for.conversation),
              num.use.for.shopping = sum(use.device.for.shopping),
              num.use.for.alarm = sum(use.device.for.alarm),
              num.use.for.weather = sum(use.device.for.weather),
              num.use.for.time = sum(use.device.for.time),
              num.use.for.reminders = sum(use.device.for.reminders))

num.use.fields <- c(
    "num.use.for.audio",
    "num.use.for.info",
    "num.use.for.control.other.devices",
    "num.use.for.conversation",
    "num.use.for.shopping",
    "num.use.for.alarm",
    "num.use.for.weather",
    "num.use.for.time",
    "num.use.for.reminders"
)

for (use.field in num.use.fields) {
    cat("  ", use.field, ":\n")
    print(cor.test(usage.summary$total.correct,
                   usage.summary[, use.field][[1]]))
}

cat("\nH4: more privacy concerns -> use less?")
h4.result <- cor.test(as.numeric(survey.data$current.privacy.concern),
                      survey.data$total.uses,
                      type="spearman")
print(h4.result)
