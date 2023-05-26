library(haven)
library(dplyr)

setwd("E:/education/thesis")


df <- read_wls_dataset(include_pgs_datasets = TRUE)

#read turley pgs
turley_df = read.csv(file="data/polygenic scores/Turley/Turley_idpub_shuffled.txt", sep="\t")
#read Lee pgs
lee_df = read.csv(file="data/polygenic scores/Lee/Lee_idpub_shuffled.txt", sep="\t")

df = merge(x=df, y=turley_df, by=c("idpub","rtype"), all.x=TRUE)
df = merge(x=df, y=lee_df, by=c("idpub","rtype"), all.x=TRUE)
remove(turley_df)
remove(lee_df)

#z_ru001re	R4 Respondent is in 80% random sample for depression and alcohol sections.
#z_mu001rec	R4 Summary score for psychological distress/depression - modified CES-D.
#nn038rer	R4 In the last 6 months have you experienced depression and how much has it bothered you?
#z_ru018re	R4 What is the usual length of time between the end of one period of depression to the beginning of the next?
#z_ru019re	R4 What is the unit for usual length of time between the end of one period of depression to the beginning of the next?
#z_ru014re	R4 If you have had only one period of depression, how long did you feel sad, blue, or depressed?
#z_ru015re	R4 If you have had only one period of depression, what is the unit for length of period when you felt sad, blue, or depressed?
#  z_ru016re	R4 If you have had more than one period of depression, what is the average length of periods when you felt sad, blue, or depressed?
#z_ru003re	R4 Were your experience with depression always caused by alcohol, drugs, medications, or physical illness?
#z_ru004re	R4 What age were you when you experienced the worst, particularly bad, or the most recent period of depression?
#  z_ru005re	R4 Was this depression the worst, particularly bad, or the most recent episode?
#  z_ru006re	R4 During this episode of depression, did the you lose weight without trying to, as much as 2 pounds a week for several weeks or as much as 10 pounds altogether?
#z_mu002re	R4 Number of psychological distress/depression - modified CES-D items answered.
#z_ru024rec	R4 Number of depression symptoms.
#z_rua24re	R4 Number of valid responses for depression symptoms.



df2 = df[(df$z_ru024rec > -2) & !is.na(df$z_ru024rec) &
           (df$z_mu001rec > -3) & !is.na(df$z_mu001rec),]
df2 = df2[, c("z_ru024rec", "z_mu001rec", "pgs_dep_gwas")]
table(df2$z_mu001rec)
cor(x=df2$z_ru024rec, y=df2$z_mu001rec, use="complete.obs")
plot(df2$z_ru024rec,df2$z_mu001rec)

df2_sorted = df2[order(df2$z_ru024rec, df2$z_mu001rec),]
df2_sorted = df2_sorted[df2_sorted$z_ru024rec > 0,]

#scores vs number of depressions for R4

ggplot(df2_sorted[, c("z_ru024rec", "z_mu001rec")], aes(x=z_mu001rec, color = as.factor(z_ru024rec)))+
  #geom_histogram( color='#e9ecef', alpha=0.6, position='identity')
  geom_density(adjust=2.5, alpha=.0)  +
  labs(title = "density of depression test scores", 
       x = "score", 
       y = "density") +
  scale_color_discrete(name = "Number of depressions") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))
  
table(df2_sorted$z_ru024rec)

cor(df2_sorted$z_ru024rec, df2_sorted$z_mu001rec)


df2_sorted_pgs = df2_sorted[!is.na(df2_sorted$pgs_dep_gwas),]
min(df2_sorted_pgs$pgs_dep_gwas)

df_pgs = df[(df$z_ru024rec > -2) & !is.na(df$z_ru024rec) &
           !is.na(df$pgs_dep_gwas),]
df_pgs_sorted = df_pgs[order(df_pgs$z_ru024rec, df_pgs$pgs_dep_gwas),]

cor(df_pgs$z_ru024rec, df_pgs$pgs_dep_gwas)
hist(df_pgs[df_pgs$z_ru024rec == 1,]$pgs_dep_gwas)

ggplot(df_pgs_sorted[, c("z_ru024rec", "pgs_dep_gwas")], aes(x=pgs_dep_gwas, color = as.factor(z_ru024rec)))+
  #geom_histogram( color='#e9ecef', alpha=0.6, position='identity')
  geom_density(adjust=2.5, alpha=.0)  +
  labs(title = "density of depression pgs", 
       x = "pgs", 
       y = "density") +
  scale_color_discrete(name = "Number of depressions") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))


df_pgs_sorted = df_pgs[order(df_pgs$pgs_dep_gwas),]
df_pgs_sorted$high_pgs_dep = 0 

min_index = floor(nrow(df_pgs_sorted) * 0.025)
max_index = floor(nrow(df_pgs_sorted) * 0.975)
df_pgs_sorted[1:min_index,]$high_pgs_dep <- "low" 
df_pgs_sorted[min_index:max_index,]$high_pgs_dep <- "average"
df_pgs_sorted[max_index:nrow(df_pgs_sorted),]$high_pgs_dep <- "high"
table(df_pgs_sorted$high_pgs_dep)


ggplot(df_pgs_sorted[, c("z_ru024rec", "high_pgs_dep")], aes(x=z_ru024rec, color = as.factor(high_pgs_dep)))+
  #geom_histogram( color='#e9ecef', alpha=0.6, position='identity')
  geom_density(adjust=1.5, alpha=.0)  +
  labs(title = "density of depression number by pgs factor", 
       x = "count", 
       y = "density") +
  scale_color_discrete(name = "PGS factor") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))



#-------------------------------------------------------------------------------
pgs_col_name = "pgs_dep_mtag"


#Have you ever had a time in life lasting two weeks or more when nearly every day you felt sad...
#1	YES
#2	NO

#Were your experience with depression always caused
#1	YES	1
#2	SOMETIMES	
#3	NO

#R4
#z_ru002re	R4 Have you ever had a time in life lasting two weeks or more when nearly every day you felt sad, blue, depressed, or when you lost interest in most things like work, hobbies, or things you usually liked to do for fun?
#z_ru003re	R4 Were your experience with depression always caused by alcohol, drugs, medications, or physical illness?

#R5  
#z_au002re	R5 Have you ever had a time in life lasting two weeks or more when nearly every day you felt sad, blue, depressed, or when you lost interest in most things like work, hobbies, or things you usually liked to do for fun?
#z_gu003re	R5 Were your experiences with depression always caused by alcohol, drugs, medications, or physical illness?

#R6
#z_hu002re	R6 Since last interview has Participant had a time lasting two weeks or more when nearly every day they felt sad, blue, depressed, or when they lost interest in most things like work, hobbies, or things they usually liked to do for fun?
#z_hu003re	R6 Were Participant's experiences with depression always caused by alcohol, drugs, medications, or physical illness?

#R7
#z_q1u002re	R7 Since last interview has Participant had a time lasting two weeks or more when nearly every day they felt sad, blue, depressed, or when they lost interest in most things like work, hobbies, or things they usually liked to do for fun?
#z_q1u003re	R7 Were Participant's experiences with depression always caused by alcohol, drugs, medications, or physical illness?

colnames_depressed <- read.csv("data/depressed_symptoms.txt", header=FALSE)
colnames_depressed <- colnames_depressed$V1
colnames_depressed <- c(colnames_depressed,
                        c("pgs_dep_gwas", "pgs_dep_mtag", "death_status",
                          "time", "death_status", "age"))

df_only_depression <- df[,colnames_depressed]

df_only_depression$depression_status <- 0

#R4
df_only_depression[!is.na(df_only_depression$z_ru002re) & (df_only_depression$z_ru002re == 1) & 
                     !is.na(df_only_depression$z_ru003re) & (df_only_depression$z_ru003re == 3), ]$depression_status <- 1
#R5
df_only_depression[!is.na(df_only_depression$z_gu002re) & (df_only_depression$z_gu002re == 1) & 
                     !is.na(df_only_depression$z_gu003re) & (df_only_depression$z_gu003re == 3), ]$depression_status <- 1
#R6
df_only_depression[!is.na(df_only_depression$z_hu002re) & (df_only_depression$z_hu002re == 1) & 
                     !is.na(df_only_depression$z_hu003re) & (df_only_depression$z_hu003re == 3), ]$depression_status <- 1
#R7
df_only_depression[!is.na(df_only_depression$z_q1u002re) & (df_only_depression$z_q1u002re == 1) & 
                     !is.na(df_only_depression$z_q1u003re) & (df_only_depression$z_q1u003re == 2), ]$depression_status <- 1

table(df_only_depression$depression_status)

#--------who has depression---------
answer_threshold = 5


#R4
df_only_depression[(df_only_depression$depression_status == 1) & 
                    !is.na(df_only_depression$z_ru024rec) &
                   (df_only_depression$z_ru024rec >= answer_threshold) ,]$depression_status <- 1
#R5
df_only_depression[(df_only_depression$depression_status == 1) & 
                     !is.na(df_only_depression$z_gu024re) & 
                     (df_only_depression$z_gu024re >= answer_threshold) ,]$depression_status <- 1

#R6
df_only_depression[(df_only_depression$depression_status == 1) & 
                     !is.na(df_only_depression$z_hu024re) & 
                     (df_only_depression$z_hu024re >= answer_threshold) ,]$depression_status <- 1

#R7
df_only_depression[(df_only_depression$depression_status == 1) & 
                     !is.na(df_only_depression$z_q1u024re) &
                     (df_only_depression$z_q1u024re >= answer_threshold) ,]$depression_status <- 1


table(df_only_depression$depression_status)

df_final_depression <- df_only_depression[!is.na(df_only_depression$pgs_dep_gwas),]
table(df_final_depression$to_include)

df_final_depression[df_final_depression$depression_status == 0,]$depression_status <- "no"
df_final_depression[df_final_depression$depression_status == 1,]$depression_status <- "yes"
table(df_final_depression$depression_status)

ggplot(df_final_depression[,c("depression_status", "pgs_dep_gwas")],
       aes(x=pgs_dep_gwas, color = as.factor(depression_status))) +
  #geom_histogram( color='#e9ecef', alpha=0.6, position='identity')
  geom_density(adjust=0.2, alpha=.0)  +
  labs(title = "density of pgs", 
       x = "pgs risk score", 
       y = "density") +
  scale_color_discrete(name = "depression status") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))

df_final_depression$depression_status <- as.factor(df_final_depression$depression_status)
plot_kaplan_factorized(df_final_depression, "depression_status", "Has depression?")
plot_density_stratified(df_final_depression, "depression_status", "pgs_dep_mtag", "Has depression?")
plot_hist_stratified(df_final_depression, "depression_status", "pgs_dep_mtag", "Has depression?")

df_final_depression$depression_status <- as.factor(df_final_depression$depression_status)
cox.mod <- coxph(formula = as.formula(paste("Surv(time, death_status) ~ ",
                                                "depression_status")),
                     data = df_final_depression)
summary(cox.mod)

table(df_final_depression$depression_status)
df_final_depression$depression_status <- as.numeric(df_final_depression$depression_status)
df_final_depression[df_final_depression$depression_status == "no",]$depression_status <- 0
df_final_depression[df_final_depression$depression_status == "yes",]$depression_status <- 1

cox.mod <- coxph(formula = as.formula(paste("Surv(time, depression_status) ~ ",
                                            "pgs_dep_mtag")),
                 data = df_final_depression)
summary(cox.mod)
df_final_depression$depression_status



df_final_depression$depression_age = -1 
df_final_depression[which(df_final_depression$z_ru004re > 0),]$depression_age <- 
  df_final_depression[which(df_final_depression$z_ru004re > 0),]$z_ru004re


df_final_depression[!is.na(df_final_depression$z_gu004re) &
                   (df_final_depression$z_gu004re > 0) &
                   (df_final_depression$depression_age == -1),]$depression_age <-
  df_final_depression[!is.na(df_final_depression$z_gu004re) &
                     (df_final_depression$z_gu004re > 0) &
                     (df_final_depression$depression_age == -1),]$z_gu004re

nrow(df_final_depression[df_final_depression$depression_age > -1,])
table(df_final_depression$depression_status)


#questions to answer for depression phenotype

#R4
#z_ru006re	R4 During this episode of depression, did the you lose weight without trying to, as much as 2 pounds a week for several weeks or as much as 10 pounds altogether?
#z_ru007re	R4 Did you have two weeks or more when nearly every night you had trouble falling asleep?
#z_ru008re	R4 Did you have two weeks or more when you lacked energy or felt tired all the time, even when you had not been working very hard?
#z_ru009re	R4 Did you have two weeks or more when you felt very bad when waking up, but felt better later in the day?
#z_ru010re	R4 Did you have two weeks or more when you lost interest in most things like work, hobbies, or things you usually liked to do for fun?
#z_ru011re	R4 Did you have two weeks or more when nearly every day you had a lot more trouble concentrating than is normal?
#z_ru012re	R4 Did you have two weeks or more when you thought a lot about death, either your own, someone else's, or death in general?


#R5
#z_gu006re	R5 During this episode of depression, did you lose weight without trying to -- as much as 2 pounds a week for several weeks or as much as 10 pounds altogether?
#z_gu007re	R5 During your worst, particularly bad, or most recent episode of depression, did you have two weeks or more when nearly every night you had trouble falling asleep?
#z_gu008re	R5 During your worst, particularly bad, or most recent episode of depression, did you have two weeks or more when you lacked energy or felt tired all the time, even when you had not been working very hard?
#z_gu009re	R5 During your worst, particularly bad, or most recent episode of depression, did you have two weeks or more when you felt very bad when waking up, but felt better later in the day?
#z_gu010re	R5 During your worst, particularly bad, or most recent episode of depression, did you have two weeks or more when you lost interest in most things like work, hobbies, or things you usually liked to do for fun?
#z_gu011re	R5 During your worst, particularly bad, or most recent episode of depression, did you have two weeks or more when nearly every day you had a lot more trouble concentrating than is normal?
#z_gu012re	R5 During your worst, particularly bad, or most recent episode of depression, did you have two weeks or more when you thought a lot about death - either your own, someone else's, or death in general?

#R6
#z_hu006re	R6 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant lose weight without trying to - as much as 2 pounds a week for several weeks or as much as 10 pounds altogether?
#z_hu007re	R6 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when nearly every night they had trouble falling asleep?
#z_hu008re	R6 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when they lacked energy or felt tired all the time, even when they had not been working very hard?
#z_hu009re	R6 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when they felt very bad when waking up, but felt better later in the day?
#z_hu010re	R6 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when they lost interest in most things like work, hobbies, or things they usually liked to do for fun?
#z_hu011re	R6 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when nearly every day they had a lot more trouble concentrating than is normal?
#z_hu012re	R6 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when they thought a lot about death - either their own, someone else, or death in general?

#R7
#z_q1u006re	R7 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant lose weight without trying to - as much as 2 pounds a week for several weeks or as much as 10 pounds altogether?
#z_q1u007re	R7 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when nearly every night they had trouble falling asleep?
#z_q1u008re	R7 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when they lacked energy or felt tired all the time, even when they had not been working very hard?
#z_q1u009re	R7 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when they felt very bad when waking up, but felt better later in the day?
#z_q1u010re	R7 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when they lost interest in most things like work, hobbies, or things they usually liked to do for fun?
#z_q1u011re	R7 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when nearly every day they had a lot more trouble concentrating than is normal?
#z_q1u012re	R7 Since last interview during Participant's worst, particularly bad, or most recent episode of depression, did Participant have two weeks or more when they thought a lot about death - either their own, someone else, or death in general?


#-------------------------------------------------------------------------------
#R4 z_mu001rec
#z_iu001rec	R5 Summary score for psychological distress/depression, Modified CES-D.
#z_ju001rec	R6 Summary score for psychological distress/depression, Modified CES-D.

#z_ru020re	R4 What age were you the first time the you had a period of two weeks or more when you felt sad, blue or depressed?
#z_gu020re	R5 What age were you the first time the you had a period of two weeks or more when you felt sad, blue or depressed?

#z_ru004re	R4 What age were you when you experienced the worst, particularly bad, or the most recent period of depression?
#z_gu004re	R5 What age were you when you experienced the worst, particularly bad, or the most recent period of depression?
#z_hu004re	R6 Since last interview what age was Participant when they experienced the worst, particularly bad, or the most recent period of depression?


my_df_depression <- df[,c("z_mu001rec", "z_iu001rec", "z_ju001rec", 
                          "z_mu002re", "z_iu002re", "z_ju002re",
                          "z_ru004re", "z_gu004re", "z_hu004re",
                          "z_ru020re", "z_gu020re", 
                          "death_status", "z_brdxdy",
                          "time", "pgs_dep_gwas", "pgs_dep_mtag", "age")]
my_df_depression$depression_status <- -1

#exclude individuals who has not answered all questions
#my_df_depression = my_df_depression[!is.na(my_df_depression$z_mu002re) &
#                                      (my_df_depression$z_mu002re == 20), ]
#my_df_depression = my_df_depression[!is.na(my_df_depression$z_iu002re) &
#                                      (my_df_depression$z_iu002re == 20), ]
#my_df_depression = my_df_depression[!is.na(my_df_depression$z_ju002re) &
#                                      (my_df_depression$z_ju002re == 20), ]

depression_threshold_cesd <- 16

table(my_df_depression[!is.na(my_df_depression$z_mu001rec) &
                         (my_df_depression$z_mu001rec >= 0) &
                         (my_df_depression$z_mu001rec < depression_threshold_cesd), ]$z_mu001rec)

my_df_depression[!is.na(my_df_depression$z_mu001rec) &
                   (my_df_depression$z_mu001rec >= 0) &
                   (my_df_depression$z_mu001rec < depression_threshold_cesd), ]$depression_status <- 0
my_df_depression[!is.na(my_df_depression$z_iu001rec) &
                   (my_df_depression$z_iu001rec >= 0) &
                   (my_df_depression$z_iu001rec < depression_threshold_cesd), ]$depression_status <- 0
my_df_depression[!is.na(my_df_depression$z_ju001rec) &
                   (my_df_depression$z_ju001rec >= 0) &
                   (my_df_depression$z_ju001rec < depression_threshold_cesd), ]$depression_status <- 0

my_df_depression[!is.na(my_df_depression$z_mu001rec) &
                   (my_df_depression$z_mu001rec >= depression_threshold_cesd), ]$depression_status <- 1
my_df_depression[!is.na(my_df_depression$z_iu001rec) &
                   (my_df_depression$z_iu001rec >= depression_threshold_cesd), ]$depression_status <- 1
my_df_depression[!is.na(my_df_depression$z_ju001rec) &
                   (my_df_depression$z_ju001rec >= depression_threshold_cesd), ]$depression_status <- 1

my_df_depression <- my_df_depression[!is.na(my_df_depression$pgs_dep_mtag),]
my_df_depression <- my_df_depression[my_df_depression$depression_status != -1,]

my_df_depression[my_df_depression$depression_status == 0,]$depression_status <- "no"
my_df_depression[my_df_depression$depression_status == 1,]$depression_status <- "yes"

table(my_df_depression$depression_status)


my_df_depression$depression_status <- as.factor(my_df_depression$depression_status)
#levels(my_df_depression$depression_status)
plot_kaplan_factorized(my_df_depression, "depression_status", "Has depression?")
plot_density_stratified(my_df_depression, "depression_status", "pgs_dep_mtag", "Has depression?")
plot_hist_stratified(my_df_depression, "depression_status", "pgs_dep_mtag", "Has depression?")


my_df_depression$depression_status <- as.factor(my_df_depression$depression_status)

cox.mod_2 <- coxph(formula = as.formula(paste("Surv(time, death_status) ~ ",
                                            "depression_status")),
                 data = my_df_depression)
summary(cox.mod_2)

cc = coxph(formula = as.formula(paste("Surv(time, depression_status) ~ ",
                                      "pgs_dep_mtag")),
           data = my_df_depression)
summary(cc)

#age of depression




my_df_depression$depression_age = -1 
my_df_depression[which(my_df_depression$z_ru020re > 0),]$depression_age <- 
  my_df_depression[which(my_df_depression$z_ru020re > 0),]$z_ru020re
nrow(my_df_depression[my_df_depression$depression_age > -1,])

my_df_depression[!is.na(my_df_depression$z_gu020re) &
                   (my_df_depression$z_gu020re > 0) &
                   (my_df_depression$depression_age == -1),]$depression_age <-
  my_df_depression[!is.na(my_df_depression$z_gu020re) &
                     (my_df_depression$z_gu020re > 0) &
                     (my_df_depression$depression_age == -1),]$z_gu020re
nrow(my_df_depression[my_df_depression$depression_age > -1,])

table(my_df_depression$depression_status)

#by three columns from recent episodes
my_df_depression[which(my_df_depression$z_ru004re > 0),]$depression_age <- 
  my_df_depression[which(my_df_depression$z_ru004re > 0),]$z_ru004re


my_df_depression[!is.na(my_df_depression$z_gu004re) &
                       (my_df_depression$z_gu004re > 0) &
                        (my_df_depression$depression_age == -1),]$depression_age <-
  my_df_depression[!is.na(my_df_depression$z_gu004re) &
                     (my_df_depression$z_gu004re > 0) &
                     (my_df_depression$depression_age == -1),]$z_gu004re

nrow(my_df_depression[my_df_depression$depression_age > -1,])
table(my_df_depression$depression_status)

nrow(my_df_depression[which(my_df_depression$z_ha003re > 0),])
my_df_depression[ !is.na(my_df_depression$z_ha003re) &
                    (my_df_depression$z_ha003re > 0) &
                    (my_df_depression$depression_status == 0),]$z_ha003re <- -1

my_df_depression[ !is.na(my_df_depression$z_ha003re) &
                    (my_df_depression$z_ha003re > 0) &
                    (my_df_depression$depression_age == -1),]$depression_age <-
  my_df_depression[ !is.na(my_df_depression$z_ha003re) &
                    (my_df_depression$z_ha003re > 0) &
                    (my_df_depression$depression_age == -1),]$z_ha003re 

nrow(my_df_depression[my_df_depression$depression_age >-1,])
table(my_df_depression$depression_status)

#------------------------------
df_pgs_sorted[(df_pgs_sorted$depression_status == 1),]$depression_age

min(df_pgs_sorted[(df_pgs_sorted$depression_status == "yes") &
                    (df_pgs_sorted$depression_age != -1) &
                    (df_pgs_sorted$high_pgs_dep == "high"),]$depression_age)
min(df_pgs_sorted[(df_pgs_sorted$depression_status == "yes") &
                    (df_pgs_sorted$depression_age != -1) &
                    (df_pgs_sorted$high_pgs_dep == "low"),]$depression_age)


nrow(df_pgs_sorted[(df_pgs_sorted$depression_status == "yes") &
                (df_pgs_sorted$depression_age != -1) &
                (df_pgs_sorted$high_pgs_dep == "high") &
                (df_pgs_sorted$depression_age < 40),])
nrow(df_pgs_sorted[(df_pgs_sorted$depression_status == "yes") &
                     (df_pgs_sorted$depression_age != -1) &
                     (df_pgs_sorted$high_pgs_dep == "low") &
                     (df_pgs_sorted$depression_age < 40),])

df_pgs_sorted = my_df_depression[order(my_df_depression$pgs_dep_gwas),]
min_index = floor(nrow(df_pgs_sorted) * 0.025)
max_index = floor(nrow(df_pgs_sorted) * 0.975)
df_pgs_sorted$high_pgs_dep <- -1
df_pgs_sorted[1:min_index,]$high_pgs_dep <- "low" 
df_pgs_sorted[min_index:max_index,]$high_pgs_dep <- "average"
df_pgs_sorted[max_index:nrow(df_pgs_sorted),]$high_pgs_dep <- "high"
table(df_pgs_sorted$high_pgs_dep)
#df_pgs_sorted$high_pgs_dep <- as.factor(df_pgs_sorted$high_pgs_dep)
#plot_kaplan_factorized(df_pgs_sorted, "high_pgs_dep", "PRS")

table(df_pgs_sorted[df_pgs_sorted$high_pgs_dep == "high",]$depression_status)
table(df_pgs_sorted[df_pgs_sorted$high_pgs_dep == "low",]$depression_status)
table(df_pgs_sorted[df_pgs_sorted$high_pgs_dep == "average",]$depression_status)



collect_incidence_count <- function(dataset, col_pgs_strat, strat_level)
{
  global_min <- min(dataset$depression_age)
  global_max <- max(dataset$depression_age)
  
  age_min <- min(dataset[dataset[,col_pgs_strat] == strat_level,]$depression_age)
  age_max <- max(dataset[dataset[,col_pgs_strat] == strat_level,]$depression_age)
  
  total_incidence_count = nrow(dataset[(dataset[,col_pgs_strat] == strat_level),])
  
  tmp = dataset[(dataset[,col_pgs_strat] == strat_level) &
                  (dataset$depression_status == "yes"),]
  incidence_count = c()
  for (age_i in c(global_min:global_max))
  {
    positive_incidence_count = nrow(tmp[(tmp$depression_age != -1) &
                                        (tmp$depression_age <= age_i),])
    incidence_count = c(incidence_count, positive_incidence_count/total_incidence_count)
  }
  
  return(incidence_count)
}

incidence_count_high <- collect_incidence_count(df_pgs_sorted, "high_pgs_dep", "high")
incidence_count_average <- collect_incidence_count(df_pgs_sorted, "high_pgs_dep", "average")
incidence_count_low <- collect_incidence_count(df_pgs_sorted, "high_pgs_dep", "low")

age_dep_min = min(df_pgs_sorted$depression_age)
age_dep_max = max(df_pgs_sorted$depression_age)
count = max(df_pgs_sorted$depression_age) - min(df_pgs_sorted$depression_age) + 1

data_incidence = data.frame(
  age = c(age_dep_min:age_dep_max, age_dep_min:age_dep_max, age_dep_min:age_dep_max),
  incidence_rate = c(incidence_count_high, incidence_count_average, incidence_count_low),
  PRS = c(rep("high", count), rep("average", count), rep("low", count))
)

ggplot(data_incidence, aes(x=age, y=incidence_rate)) +
  geom_line(aes(color = PRS)) +
  scale_color_manual(values = c("red", "steelblue", "green")) +
  labs(title = "", 
       x = "Age", 
       y = "Cumulative depression rate") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))





#----------------PGS strat article approach ratio------------------------


min(df_pgs_sorted[(df_pgs_sorted$depression_status == "yes") &
                    (df_pgs_sorted$depression_age != -1) &
                    (df_pgs_sorted$high_pgs_dep == "high"),]$depression_age)
min(df_pgs_sorted[(df_pgs_sorted$depression_status == "yes") &
                    (df_pgs_sorted$depression_age != -1) &
                    (df_pgs_sorted$high_pgs_dep == "low"),]$depression_age)


nrow(df_pgs_sorted[(df_pgs_sorted$depression_status == "yes") &
                     (df_pgs_sorted$depression_age != -1) &
                     (df_pgs_sorted$high_pgs_dep == "high") &
                     (df_pgs_sorted$depression_age < 40),])
nrow(df_pgs_sorted[(df_pgs_sorted$depression_status == "yes") &
                     (df_pgs_sorted$depression_age != -1) &
                     (df_pgs_sorted$high_pgs_dep == "low") &
                     (df_pgs_sorted$depression_age < 40),])

df_pgs_sorted = df_final_depression[order(df_final_depression$pgs_dep_gwas),]
min_index = floor(nrow(df_pgs_sorted) * 0.05)
max_index = floor(nrow(df_pgs_sorted) * 0.95)
df_pgs_sorted$high_pgs_dep <- -1
df_pgs_sorted[1:min_index,]$high_pgs_dep <- "low" 
df_pgs_sorted[min_index:max_index,]$high_pgs_dep <- "average"
df_pgs_sorted[max_index:nrow(df_pgs_sorted),]$high_pgs_dep <- "high"
table(df_pgs_sorted$high_pgs_dep)
#df_pgs_sorted$high_pgs_dep <- as.factor(df_pgs_sorted$high_pgs_dep)
#plot_kaplan_factorized(df_pgs_sorted, "high_pgs_dep", "PRS")

table(df_pgs_sorted[df_pgs_sorted$high_pgs_dep == "high",]$depression_status)
table(df_pgs_sorted[df_pgs_sorted$high_pgs_dep == "low",]$depression_status)
table(df_pgs_sorted[df_pgs_sorted$high_pgs_dep == "average",]$depression_status)

df_pgs_sorted[df_pgs_sorted$high_pgs_dep == "low",]$pgs_dep_mtag

collect_incidence_count <- function(dataset, col_pgs_strat, strat_level)
{
  global_min <- min(dataset$depression_age)
  global_max <- max(dataset$depression_age)
  
  age_min <- min(dataset[dataset[,col_pgs_strat] == strat_level,]$depression_age)
  age_max <- max(dataset[dataset[,col_pgs_strat] == strat_level,]$depression_age)
  
  total_incidence_count = nrow(dataset[(dataset[,col_pgs_strat] == strat_level),])
  
  tmp = dataset[(dataset[,col_pgs_strat] == strat_level) &
                  (dataset$depression_status == "yes"),]
  incidence_count = c()
  for (age_i in c(global_min:global_max))
  {
    positive_incidence_count = nrow(tmp[(tmp$depression_age != -1) &
                                          (tmp$depression_age <= age_i),])
    incidence_count = c(incidence_count, positive_incidence_count/total_incidence_count)
  }
  
  return(incidence_count)
}

incidence_count_high <- collect_incidence_count(df_pgs_sorted, "high_pgs_dep", "high")
incidence_count_average <- collect_incidence_count(df_pgs_sorted, "high_pgs_dep", "average")
incidence_count_low <- collect_incidence_count(df_pgs_sorted, "high_pgs_dep", "low")

age_dep_min = min(df_pgs_sorted$depression_age)
age_dep_max = max(df_pgs_sorted$depression_age)
count = max(df_pgs_sorted$depression_age) - min(df_pgs_sorted$depression_age) + 1

data_incidence = data.frame(
  age = c(age_dep_min:age_dep_max, age_dep_min:age_dep_max, age_dep_min:age_dep_max),
  incidence_rate = c(incidence_count_high, incidence_count_average, incidence_count_low),
  PRS = c(rep("high", count), rep("average", count), rep("low", count))
)

ggplot(data_incidence, aes(x=age, y=incidence_rate)) +
  geom_line(aes(color = PRS)) +
  scale_color_manual(values = c("red", "steelblue", "green")) +
  labs(title = "", 
       x = "Age", 
       y = "Cumulative depression rate") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title.x = element_text(face="bold", colour="#FF7A33", size = 14),
        axis.title.y = element_text(face="bold", colour="#FF7A33", size = 14))

