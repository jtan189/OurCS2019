#!/usr/bin/env Rscript

source('prepare_data.R')

## > install.packages("ggplot2")
## > install.packages("reshape2")
## > install.packages("RColorBrewer")
library(ggplot2)
library(reshape2)
library(RColorBrewer)

likert.blues <- rev(brewer.pal(9,"Blues")[c(5,7)])
likert.greens <- brewer.pal(9,"Greens")[c(5,7)]
likert.grey <- brewer.pal(3,"Greys")[2]
likert.colors <- c(likert.blues, likert.grey, likert.greens)

## cookbook-r.com/Graphs/

## bar plots
##
## example:
##
## ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
##     geom_bar(stat="identity")


## scatter plots
##
## examples:
##
## ggplot(dat, aes(x=xvar, y=yvar)) +
##     geom_point(shape=1)      # Use hollow circles
##
## ggplot(dat, aes(x=xvar, y=yvar)) +
##     geom_point(shape=1) +    # Use hollow circles
##     geom_smooth(method=lm)   # Add linear regression line 
##                              #  (by default includes 95% confidence region)
h4.plot <- ggplot(survey.data, aes(x=current.privacy.concerns, y=initial.privacy.concerns)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  geom_line()

## histograms:
##
## example:
##
##
## ggplot(data=chol, aes(chol$AGE)) + geom_histogram()


## stacked bar plots:
##

## turn wide-format data into long-format data

long.data <- melt(survey.data[, c("response.id", privacy.practices)], id="response.id")
long.data$belief.correct <- factor(long.data$value,
                                   levels=kLikertCorrect,
                                   ordered=TRUE)

long.data$privacy.practice <- long.data$variable

stacked.data <- long.data %>%
    group_by(privacy.practice,belief.correct) %>%
    summarize(count=n(),
              percent=count/nrow(survey.data) * 100) %>%
    select(percent,
           privacy.practice,
           belief.correct)

stacked.plot <- ggplot(stacked.data,
                       aes(x=privacy.practice,
                           y=percent,
                           fill=belief.correct)) +
    theme_bw() +
    geom_bar(stat = "identity", width = .7) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(color = "black", size=13),
          aspect.ratio = .7,
          legend.position="bottom",
          legend.text = element_text(size=13),
          plot.margin = unit(c(0,1,0,1), "cm"),
          plot.title = element_text(hjust = 0.5)) +
    ## ggtitle("Title name") +
    scale_fill_manual(values=likert.colors) +
    scale_y_continuous(breaks = c(0, 50, 100),
                       minor_breaks = NULL,
                       labels = c("0%", "50%", "100%"),
                       expand = c(0,0)) +
    guides(fill=guide_legend(title=NULL,
                             reverse=TRUE)) +
    geom_hline(yintercept=c(0,100)) +
    coord_flip()    

survey.data$concern.diff <- as.numeric(survey.data$current.privacy.concerns) -
  +                         as.numeric(survey.data$initial.privacy.concerns)

hist(survey.data$concern.diff)

ggplot(survey.data, aes(months.owned, concern.diff)) + geom_point() + geom_smooth()

ggplot(survey.data, aes(as.numeric(times.use.device.in.day), concern.diff)) + geom_point() + geom_smooth()


