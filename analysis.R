########################################################################
##
## ANALYSES AND GRAPHS
##
## AI-Mediated Communication: How Profile Generation by AI Affects 
## Perceived Trustworthiness. Forthcoming in Proceedings of the 2019 
## CHI Conference on Human Factors in Computing Systems. ACM.
## Jakesch, M., French, M., Ma, X., Hancock, J.T. & Naaman, M. 
##
## Created by Maurice Jakesch
## Version: 20 December 2018
##
########################################################################

## Evironment
# Set relative paths
path = sub("[^/]*.R", "", rstudioapi::getActiveDocumentContext()$path)
setwd(path)

# Load packages
library(ggplot2)
library(RColorBrewer)
library(stats)
library(nlme)
library(psy)
library(MuMIn)
library(Rmisc)

# Color scheme
colors <- c('dodgerblue2', 'gray20')

#####################################################################################
#####################################################################################
###################################### STUDY 1 ###################################### 
#####################################################################################
#####################################################################################

## Load data
data_1 = read.csv('data_study1.csv')
data_1 <- data_1[-c(1, 2),] # Remove metadata
data_1 <- data_1[data_1$Finished == TRUE,]

## Convert data types
data_1[] <- lapply(data_1, gsub, pattern = "Highly confident  100%", replacement = "100")
data_1[] <- lapply(data_1, gsub, pattern = "Not confident 0%", replacement = "0")
data_1[] <- lapply(data_1, gsub, pattern = "Strongly agree 100%", replacement = "100")
data_1[] <- lapply(data_1, gsub, pattern = "Strongly disagree 0%", replacement = "0")
temp <- c(sprintf("General_%s",seq(1:3)), sprintf("Attitude_%s",seq(1:4)), sprintf("P%s_1",seq(1:10)), c('Dem_Birth'),
          sprintf("P%s_2",seq(1:10)), sprintf("P%s_3",seq(1:10)), sprintf("P%s_T_Page.Submit",seq(1:10)))
data_1[temp] <- sapply(data_1[,temp], function(x) {as.numeric(x)})
for (i in seq(1:10)){
  data_1[,sprintf("rating_%s", i)] <- rowMeans(data_1[,sprintf("P%s_%s",i,seq(1:3))])
}
data_1$age <- 2018 - data_1$Dem_Birth

## User level stats
data_1$subject <- rownames(data_1)
data_1$Attitude_3_R <- 100 - data_1$Attitude_3 # Inverse code
data_1$Attitude_4_R <- 100 - data_1$Attitude_4 # Inverse code
data_1$general <- rowMeans(data_1[,sprintf("General_%s",seq(1:3))]) # General trust index
data_1$attitude <- rowMeans(data_1[,c('Attitude_1', 'Attitude_2', 'Attitude_4_R')]) # General attitude index
data_1$rating_mean <- rowMeans(data_1[,sprintf("rating_%s",seq(1:10))])
data_1$rating_sd <- transform(data_1[,sprintf("rating_%s",seq(1:10))], SD=apply(data_1[,sprintf("rating_%s",seq(1:10))],1, sd, na.rm = TRUE))$SD
data_1$time_median <- apply(data_1[,sprintf("P%s_T_Page.Submit",seq(1:10))], 1, median)
mean(data_1$age, na.rm = TRUE)
table(data_1$Dem_Gender)/nrow(data_1)

# Exclusions based on response stats (clickthrough)
mask <- data_1$rating_mean < mean(data_1$rating_mean) + 2 * sd(data_1$rating_mean) & data_1$rating_mean > mean(data_1$rating_mean) - 2 * sd(data_1$rating_mean) 
mask <- mask & data_1$rating_sd < mean(data_1$rating_sd) + 2 * sd(data_1$rating_sd) & data_1$rating_sd > mean(data_1$rating_sd) - 2 * sd(data_1$rating_sd) 
mask <- mask & data_1$rating_sd >= 5
mask <- mask & data_1$time_median >= 5
data_1 <- data_1[mask,]

## Exclusions based on scenario understanding
data_1 <- data_1[data_1$Val_2 == 'John is easy to see.',]
data_1 <- data_1[data_1$Val_1 == 'I am traveling and the person in the profile offers to host me.',]
#data_1 <- data_1[data_1$Demo_Reject == '',] # no exclusions based on free-form

# Convert to long
data_1l <- reshape(data_1,
                   varying = list(sprintf("rating_%s",seq(1:10)), sprintf("P%s_T_Page.Submit",seq(1:10)),
                                  sprintf("P%s_1",seq(1:10)), sprintf("P%s_2",seq(1:10)), sprintf("P%s_3",seq(1:10))),
                   timevar = c("profile"),
                   times = seq(1,10),
                   v.names = c('rating', 'time', 'rating_1', 'rating_2', 'rating_3'),
                   direction = "long")
data_1l$profile_type <- ifelse(data_1l$profile > 5, "low", "high")

# Basic statistics
cronbach(data_1l[,c('rating_1', 'rating_2', 'rating_3')])
summarySE(data_1l, 'rating')
cronbach(data_1[,sprintf("General_%s",seq(1:3))])
summarySE(data_1, 'general')
cronbach(data_1[,c('Attitude_1', 'Attitude_2', 'Attitude_4_R')])
summarySE(data_1, 'attitude')
summarySE(data_1l, measurevar="rating", groupvars=c("condition"))
summarySE(data_1l, measurevar="rating", groupvars=c("profile_type"))

# Plot 1-1: Trust rating by profile and group
plot_d1 <- summarySE(data_1l, measurevar="rating", groupvars=c("profile","condition", 'profile_type'))
plot_d1$profile_type <- revalue(plot_d1$profile_type, c( "low"='Low baseline profiles', "high"='High baseline profiles'))
plot_d1$condition <- revalue(plot_d1$condition, c( "baseline"='Control (all human)', "ai"='Treatment (all AI)'))
plot_d1$profile_rank <- revalue(factor(plot_d1$profile), c('5'='1', '2'='2', '1'='3', '3'='4', '4'='5', '10'='6', '6'='7',
                                                      '9'='8', '7'='9', '8'='10')) # Renamed profiles for plots
plot_d1$profile_rank <- paste("P", plot_d1$profile_rank, sep='')
plot_d1$profile_rank = factor(plot_d1$profile_rank, levels=c(sprintf("P%s",seq(1:10))))
pd = position_dodge(0.6)
ggplot(plot_d1, aes(x=profile_rank, y=rating, color=condition)) + geom_point(shape = 15, size  = 3, position = pd) +
  geom_errorbar(aes(ymin  = rating - ci,ymax  = rating + ci), width = 0.5, size  = 0.7,position = pd) + 
  scale_x_discrete(name = 'Host Profile') + ylab('Mean trustworthiness') + facet_grid(~profile_type, scales="free_x") +
  scale_color_manual(values=colors, name="Group") +  theme_bw() + theme(legend.position = c(0.18, 0.2)) 

# Analysis 1-1: ANOVA
aov_1 <- aov(rating ~ profile_type * condition + Error(subject), data=data_1l)
summary(aov_1)

# Analysis 1-2: OLS Regression
lme_1 <- lme(rating ~ profile_type * condition + attitude + general, random =~1|subject, data=data_1l)
summary(lme_1)

#####################################################################################
#####################################################################################
###################################### STUDY 2 ###################################### 
#####################################################################################
#####################################################################################

## Load data
data_2 = read.csv('data_study2.csv')
data_2 <- data_2[-c(1, 2),] # Remove metadata
data_2 <- data_2[data_2$Finished == TRUE,]

## Convert data types
data_2[] <- lapply(data_2, gsub, pattern = "Highly confident  100%", replacement = "100")
data_2[] <- lapply(data_2, gsub, pattern = "Not confident 0%", replacement = "0")
data_2[] <- lapply(data_2, gsub, pattern = "Strongly agree 100%", replacement = "100")
data_2[] <- lapply(data_2, gsub, pattern = "Strongly disagree 0%", replacement = "0")
data_2[] <- lapply(data_2, gsub, pattern = "Definitely human-written1", replacement = "1")
data_2[] <- lapply(data_2, gsub, pattern = "Definitely A.I.-generated6", replacement = "6")
temp <- c(sprintf("General_%s",seq(1:3)), sprintf("Attitude_%s",seq(1:4)), sprintf("P%s_1",seq(1:10)),
          sprintf("P%s_2",seq(1:10)), sprintf("P%s_3",seq(1:10)), sprintf("P%s_G",seq(1:10)),
          sprintf("P%s_T_Page.Submit",seq(1:10)), 'Birthyear')
data_2[temp] <- sapply(data_2[,temp], function(x) {as.numeric(x)})
for (i in seq(1:10)){
  data_2[,sprintf("rating_%s", i)] <- rowMeans(data_2[,sprintf("P%s_%s",i,seq(1:3))])
}
data_2$age <- 2018 - data_2$Birthyear

## User level stats
data_2$subject <- rownames(data_2)
data_2$Attitude_3_R <- 100 - data_2$Attitude_3 # Inverse code
data_2$Attitude_4_R <- 100 - data_2$Attitude_4 # Inverse code
data_2$general <- rowMeans(data_2[,sprintf("General_%s",seq(1:3))]) # General trust index
data_2$attitude <- rowMeans(data_2[,c('Attitude_1', 'Attitude_2', 'Attitude_4_R')]) # General attitude index
data_2$rating_mean <- rowMeans(data_2[,sprintf("rating_%s",seq(1:10))])
data_2$rating_sd <- transform(data_2[,sprintf("rating_%s",seq(1:10))], SD=apply(data_2[,sprintf("rating_%s",seq(1:10))],1, sd, na.rm = TRUE))$SD
data_2$time_median <- apply(data_2[,sprintf("P%s_T_Page.Submit",seq(1:10))], 1, median)
mean(data_2$age, na.rm = TRUE)
table(data_2$Gender)/nrow(data_2)

# Exclusions based on response stats (clickthrough)
mask <- data_2$rating_mean < mean(data_2$rating_mean) + 2 * sd(data_2$rating_mean) & data_2$rating_mean > mean(data_2$rating_mean) - 2 * sd(data_2$rating_mean) 
mask <- mask & data_2$rating_sd < mean(data_2$rating_sd) + 2 * sd(data_2$rating_sd) & data_2$rating_sd > mean(data_2$rating_sd) - 2 * sd(data_2$rating_sd) 
mask <- mask & data_2$rating_sd >= 5
mask <- mask & data_2$time_median >= 5
data_2 <- data_2[mask,]

## Exclusions based on scenario understanding
data_2 <- data_2[data_2$Val_2 == 'John is easy to see.',]
data_2 <- data_2[data_2$Val_1 == 'I am traveling and the person in the profile offers to host me.',]
#data_2 <- data_2[data_2$Demo_Reject == '',] # no exclusions based on free-form
286-nrow(data_2)

# Convert to long and compute cronbach's alpha
data_2l <- reshape(data_2,
                   varying = list(sprintf("rating_%s",seq(1:10)), sprintf("P%s_G",seq(1:10)), sprintf("P%s_T_Page.Submit",seq(1:10)),
                                  sprintf("P%s_1",seq(1:10)), sprintf("P%s_2",seq(1:10)), sprintf("P%s_3",seq(1:10))),
                   timevar = c("profile"),
                   times = seq(1,10),
                   v.names = c('rating', 'aiscore', 'time', 'rating_1', 'rating_2', 'rating_3'),
                   direction = "long")
data_2l$profile_type <- ifelse(data_2l$profile > 5, "low", "high")
cronbach(data_2l[,c('rating_1', 'rating_2', 'rating_3')])
summarySE(data_2l, 'rating')

# Plot 2-1: Profile trends over AI-ness
plot_d2 <- data_2l
plot_d2$profile_rank <- revalue(factor(plot_d2$profile), c('5'='1', '2'='2', '1'='3', '3'='4', '4'='5', '10'='6', '6'='7',
                                                           '9'='8', '7'='9', '8'='10')) # Renamed profiles for plots
plot_d2$full_profile <- paste("Profile ", plot_d2$profile_rank, '\n[', data_2l$profile_type, ']', sep ='' )
plot_d2$full_profile = factor(plot_d2$full_profile, levels=
        c(sprintf("Profile %s\n[high]",seq(1:5)), sprintf("Profile %s\n[low]",seq(from=6, to=10))))
ggplot(data = plot_d2, mapping = aes(x= aiscore, y = rating, color = aiscore)) + 
  theme_bw() + facet_grid(~full_profile) + geom_point(alpha = 0.5, position=position_jitterdodge()) +
  ylab('Trustworthiness rating') + scale_x_continuous(name = 'Belief about profile generation', breaks = c(1,2,3,4,5,6), labels = c('',' Human','','','','AI')) + 
  scale_colour_gradientn(colours = colorRampPalette(colors)(100), limits=c(1, 6), breaks=c(1.2, 2.73, 4.266, 5.8), name='AI Score', 
  labels=c("Definitely human", "Probably human", "Probably AI" ,"Definitely AI")) + geom_smooth(method = 'lm', size = 0.8, color = 'black') +
  ylim(15, 100) + scale_shape_discrete(name = "", labels = c("Linear fit")) + theme(axis.title.x = element_text(margin = margin(t = 6, r = 0, b = 0, l = 0)))

# Analysis 2-1: OLS Regression
lme_2a <- lm(rating ~ aiscore * profile_type , data=data_2l)
summary(lme_2a)

lme_2b <- lm(rating ~ aiscore * profile_type + general, data=data_2l)
summary(lme_2b)

lme_2c <- lm(rating ~ aiscore * profile_type + general + attitude, data=data_2l)
summary(lme_2c)

lme_2d <- lme(rating ~ aiscore * profile_type, random =~ 1|subject, data=data_2l)
summary(lme_2d)

#####################################################################################
#####################################################################################
###################################### STUDY 3 ###################################### 
#####################################################################################
#####################################################################################

## Load data
data_3 = read.csv('data_study3.csv')
data_3 <- data_3[-c(1, 2),] # Remove metadata
data_3 <- data_3[data_3$Finished == TRUE,]

## Convert data types
data_3[] <- lapply(data_3, gsub, pattern = "Highly confident  100%", replacement = "100")
data_3[] <- lapply(data_3, gsub, pattern = "Not confident 0%", replacement = "0")
data_3[] <- lapply(data_3, gsub, pattern = "Strongly agree 100%", replacement = "100")
data_3[] <- lapply(data_3, gsub, pattern = "Strongly disagree 0%", replacement = "0")
data_3[] <- lapply(data_3, gsub, pattern = "Definitely human-written1", replacement = "1")
data_3[] <- lapply(data_3, gsub, pattern = "Definitely A.I.-generated6", replacement = "6")
temp <- c(sprintf("General_%s",seq(1:3)), sprintf("Attitude_%s",seq(1:4)), 'Birthyear', 
              sprintf("X%s_P1_T_Page.Submit",seq(1:10)), sprintf("ProfileID_%s",seq(1:10)))
data_3[temp] <- sapply(data_3[,temp], function(x) {as.numeric(x)})
for (i in seq(1:10)){
  data_3[,sprintf("rating_%s", i)] <- rowMeans(sapply(data_3[,sprintf("X%s_R%s",i,seq(1:3))], function(x) {as.numeric(x)}))
  data_3[,sprintf("aiscore_%s", i)] <- rowMeans(sapply(data_3[,sprintf("X%s_AI",i,seq(1:3))], function(x) {as.numeric(x)}))
}

## Rename groups (we changed their names throughout the project)
data_3$condition <- gsub("uncertainty", "unlabeled", data_3$condition)
data_3$condition <- gsub("expectation", "labeled", data_3$condition)
data_3$condition <- gsub("problematize", "primed", data_3$condition)

## User level stats
data_3$subject <- rownames(data_3)
data_3$Attitude_3_R <- 100 - data_3$Attitude_3 # Inverse code
data_3$Attitude_4_R <- 100 - data_3$Attitude_4 # Inverse code
data_3$general <- rowMeans(data_3[,sprintf("General_%s",seq(1:3))]) # General trust index
data_3$attitude <- rowMeans(data_3[,c('Attitude_1', 'Attitude_2', 'Attitude_4_R')]) # General attitude index
data_3$rating_mean <- rowMeans(data_3[,sprintf("rating_%s",seq(1:10))])
data_3$rating_sd <- transform(data_3[,sprintf("rating_%s",seq(1:10))], SD=apply(data_3[,sprintf("rating_%s",seq(1:10))],1, sd, na.rm = TRUE))$SD
data_3$time_median <- apply(data_3[,sprintf("X%s_P1_T_Page.Submit",seq(1:10))], 1, median)
data_3$age <- 2018 - data_3$Birthyear
mean(data_3$age, na.rm = TRUE)
table(data_3$Gender)/nrow(data_3)


# Exclusions based on response stats (clickthrough)
mask <- data_3$rating_mean < mean(data_3$rating_mean) + 2 * sd(data_3$rating_mean) & data_3$rating_mean > mean(data_3$rating_mean) - 2 * sd(data_3$rating_mean) 
mask <- mask & data_3$rating_sd < mean(data_3$rating_sd) + 2 * sd(data_3$rating_sd) & data_3$rating_sd > mean(data_3$rating_sd) - 2 * sd(data_3$rating_sd) 
mask <- mask & data_3$rating_sd >= 5
mask <- mask & data_3$time_median >= 5
data_3 <- data_3[mask,]

## Exclusions based on scenario understanding
data_3 <- data_3[data_3$Val_1 == 'John is easy to see.',]
data_3 <- data_3[data_3$Val_2 == 'I am traveling and the person in the profile offers to host me.',]
#data_3 <- data_3[data_3$Val_3 == 'Some were generated by the AI system, some were written by the host.' | data_3$Val_3 == '',] # at beginning
#data_3 <- data_3[data_3$Val_4 == 'Some were generated by the AI system, some were written by the host.' | data_3$Val_4 == '',] # at end
#data_3 <- data_3[data_3$Demo_Fail == '',] # no exclusions based on free-form
323-nrow(data_3)

# Convert to long and compute cronbach's alpha
data_3l <- reshape(data_3,
               varying = list(sprintf("rating_%s",seq(1:10)), sprintf("aiscore_%s",seq(1:10)), 
                              sprintf("X%s_P1_T_Page.Submit",seq(1:10)), sprintf("ProfileID_%s",seq(1:10)),
                              sprintf("X%s_R1",seq(1:10)), sprintf("X%s_R2",seq(1:10)), sprintf("X%s_R3",seq(1:10))),
               timevar = c("order"),
               times = seq(1,10),
               v.names = c('rating', 'aiscore', 'time', 'profile', 'rating_1', 'rating_2', 'rating_3'),
               direction = "long")
data_3l$type <- ifelse(data_3l$profile <= 15, 'ai', 'human')
data_3l[c('rating_1', 'rating_2', 'rating_3')] <- sapply(data_3l[c('rating_1', 'rating_2', 'rating_3')], function(x) {as.numeric(x)})

# Basic stats
cronbach(data_3l[,c('rating_1', 'rating_2', 'rating_3')])
summarySE(data_3l, 'rating')

# Plot 3-1: Absolute differences between groups and profiles
plot_d31 <- summarySE(data_3l, 'rating', groupvars = c('type','condition'))
# Unfortunately, the CRAN summarySEwithin produces faulty results. As a work-around, and based on its functioning
# (see http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/), we compute SE & CI 
# based on data that's normalized by subjects' mean rating to account for the repeated measures per subject
data_3l$normed_rating <- data_3l$rating - data_3l$rating_mean
plot_d31_within <- summarySE(data_3l, 'normed_rating', groupvars = c('type','condition'))
plot_d31 <- merge(plot_d31[,c("type","condition","N","rating")],plot_d31_within[,c("type","condition","sd","se","ci")],by=c("type","condition")) 
plot_d31$type <- revalue(plot_d31$type, c( "human"='"Human"', "ai"='"AI"'))
plot_d31$type <- factor(plot_d31$type, levels = c( '"Human"', '"AI"'))
plot_d31$condition <- revalue(plot_d31$condition, c( "control"="Control", "labeled"="Labeled", "unlabeled"='Unlabeled', 'primed'='Primed'))
plot_d31$condition <- factor(plot_d31$condition, levels = c('Control', 'Unlabeled', 'Labeled', 'Primed'))
pd <- position_dodge(0.25)
sh <- c(15, 19, 15, 19, 15, 19, 15, 19)
sh <- c(19, 15, 19, 15, 19, 15, 19, 15)
ggplot(plot_d31, aes(x=condition, y=rating, colour=type, group=type)) + #geom_line(position=pd) + 
  geom_errorbar(aes(ymin=rating-ci, ymax=rating+ci), colour="black", width=.2, position=pd) +
  geom_point(position=pd, shape = sh, size  = 4) + theme_bw(base_size = 15) + xlab('Condition') + ylab('Mean trustworthiness') +
  theme(legend.position="top", legend.box = "horizontal", legend.box.margin=margin(0,0,-10,0), legend.background=element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 0))) +
  labs(color='Profile type') +  scale_color_manual(values=colors) 
summarySE(data_3l, 'rating', groupvars = c('condition'))
plot_d31

# Plot 3-2: Primed group only -- profile ratings over AI scores
plot_d32 <- summarySE(data_3l[data_3l$condition == 'primed',], 'rating', groupvars = c('aiscore'))
# Unfortunately, the CRAN summarySEwithin produces faulty results. As a work-around, and based on its functioning
# (see http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/), we compute SE & CI 
# based on data that's normalized by subjects' mean rating to account for the repeated measures per subject
plot_d32_within <- summarySE(data_3l[data_3l$condition == 'primed',], 'normed_rating', groupvars = c('aiscore'))
plot_d32 <- merge(plot_d32[,c("N","rating","aiscore")],plot_d32_within[,c("aiscore","sd","se","ci")],by=c("aiscore")) 
plot_d32$aiscore_r <- revalue(factor(plot_d32$aiscore), c("1"="1\nDefinitely\nhuman-written", "2"="2", "3"="3", "4"="4", "5"="5", "6"="6\nDefinitely\nAI-generated"))
ggplot(plot_d32, aes(x=aiscore_r, y=rating)) +
  geom_errorbar(aes(ymin=rating-ci, ymax=rating+ci), colour="black", width=.2, position=pd) + geom_line(aes(x = aiscore, color = aiscore)) +
  geom_point(aes(x = aiscore, color = aiscore), shape = 19, size  = 4) + theme_bw(base_size = 15) + xlab('AI score') + 
  ylab('Mean trustworthiness') + theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), 
                                         axis.title.x = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+guides(colour=FALSE) +
  scale_colour_gradientn(colours = colorRampPalette(colors)(100), limits=c(1, 6), breaks=c(1.2, 2.73, 4.266, 6))

# Analysis 3-1: 
aov_3 <- aov(rating ~ condition * type + Error(subject), data=data_3l)
summary(aov_3)

# Analysis 3-2:
data_3l$type <- factor(data_3l$type, levels = c('human', 'ai'))
lme_31 <- lm(rating ~ type * condition, data=data_3l)
summary(lme_31)

lme_32 <- lme(rating ~ type * condition, random=~ 1|subject, data=data_3l)
anova(lme_32)
summary(lme_32)
r.squaredGLMM(lme_32)

# Analysis 3-3:
summarySE(data_3l[data_3l$condition == 'primed',], 'aiscore', groupvars = c('type'))
summarySE(data_3l[data_3l$condition == 'primed',], 'normed_rating', groupvars = c('type'))
t.test(data_3l[data_3l$condition == 'primed' & data_3l$type == 'human',]$aiscore,
       data_3l[data_3l$condition == 'primed' & data_3l$type == 'ai',]$aiscore)
plot_d32
