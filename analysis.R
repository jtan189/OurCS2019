#!/usr/bin/env Rscript

source('prepare_data.R')

## compare people that use device for 1 purpose vs many
survey.data$total.uses <- with(survey.data,
                               use.device.for.audio +
                               use.device.for.info +
                               use.device.for.control.other.devices +
                               use.device.for.conversation +
                               use.device.for.shopping +
                               use.device.for.alarm +
                               use.device.for.weather +
                               use.device.for.time +
                               use.device.for.reminders +
                               use.device.for.other)

hist(survey.data$months.owned)

## own device longer -> know more about privacy?
## vars: total.correct, months.owned
scatter
           
                               