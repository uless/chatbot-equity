library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(textclean)
library(psych)
library(stargazer)
library(jtools)
library(data.table)
library(ggplot2)
library(here)
library(tidyverse)
library(magrittr)
library(stm) # version v1.3.5
library(plotrix)
library(tm)
library(xlsx)
require(NLP)
library(ggpubr)
library(ggstance)
library(quantreg)
library(caret)
library(magick)
library(performance)

##### survey file #####
df <- read_xlsx("survey-sum-Jan-14.xlsx",col_names = TRUE,sheet = 1)%>%
  select(-c(ICC_code,OCC_code,IBLM_code,OBLM_code))
df <- df[,-c(1,3:21)]

df2 <- read_xlsx("survey-sum-Jan-16.xlsx",col_names = TRUE,sheet = 1)
df2 <- df2[,-c(1,3:21)]
df2 <- df2 %>%
  filter(good == 1) %>%
  select(-c(`ICC_code...101`:`OBLM_code...104`,`ICC_code...46`,`OCC_code...53`,`IBLM_code...60`,`OBLM_code...67`))%>%
  select(-c(bad:undecided))%>%
  select(-RandomID)

df3 <- read_xlsx("survey-sum-Jan-17.xlsx",col_names = TRUE,sheet = 1)%>%
  filter(good == 1) %>%
  select(-c(`ICC_code...101`:`OBLM_code...104`,`ICC_code...46`,`OCC_code...53`,`IBLM_code...60`,`OBLM_code...67`))%>%
  select(-c(good:undecided))%>%
  select(-RandomID)
df3 <- df3[,-c(1,3:21)]

df4 <- read_xlsx("Survey-sum-Feb-02.xlsx",col_names = TRUE,sheet = 1)%>%
  filter(goodppl == 1) %>%
  select(-c(`ICC_code...101`:`OBLM_code...104`,`ICC_code...46`,`OCC_code...53`,`IBLM_code...60`,`OBLM_code...67`))
df4 <- df4[,-c(1,3:21,96,98:101)]


df5 <- read_xlsx("Survey-sum-Feb-04.xlsx",col_names = TRUE,sheet = 1)%>%
  filter(appr == 1) %>%
  select(-c(`ICC_code...101`:`OBLM_code...104`,`ICC_code...46`,`OCC_code...53`,`IBLM_code...60`,`OBLM_code...67`))
df5 <- df5[,-c(1,3:21,96,98:99)]

df6 <- read_xlsx("Survey-sum-Feb-09.xlsx",col_names = TRUE,sheet = 1)%>%
  filter(good == 1) %>%
  select(-c(`ICC_code...101`:`OBLM_code...104`,`ICC_code...46`,`OCC_code...53`,`IBLM_code...60`,`OBLM_code...67`))
df6 <- df6[,-c(1,3:21,96,98:100)]

df <- rbind(df,df2)
df <- rbind(df,df3)
df <- rbind(df,df4)
df <- rbind(df,df5)
df <- rbind(df,df6)
df$code <- gsub(' ', '', df$code)
df$code <- gsub('\\.','',df$code)



##### merge survey with chat #####
df_chat <- read.csv("chat-all.csv",
                    fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                    row.names = NULL, 
                    stringsAsFactors = FALSE)%>%
  select(-c(X,timestamp))%>%
  rename('code' = 'X_id')

df <- df%>%left_join(df_chat,by="code")

write.csv(df,"survey_chat_combined.csv")

##### descriptive #####
table(df$survey_type)
table(df$gender)
table(df$edu)
table(df$ethnic)
table(df$income)
table(df$language)

table(df$political)
#45.65% conservatives
#17.02% neutral
#37.33% liberals

##### recode & sum & cronbach #####
Sys.setlocale('LC_ALL','C')
df[] <- lapply(df, gsub, pattern = 'Strongly agree', replacement = 5, fixed = TRUE)
df[] <- lapply(df, gsub, pattern = "Somewhat agree", replacement = 4, fixed = TRUE)
df[] <- lapply(df, gsub, pattern = "Neither agree nor disagree", replacement = 3, fixed = TRUE)
df[] <- lapply(df, gsub, pattern = "Somewhat disagree", replacement = 2, fixed = TRUE)
df[] <- lapply(df, gsub, pattern = "Strongly disagree", replacement = 1, fixed = TRUE)

df[] <- lapply(df, gsub, pattern = "Very liberal", replacement = 5, fixed = TRUE)
df[] <- lapply(df, gsub, pattern = "Slightly liberal", replacement = 4, fixed = TRUE)
df[] <- lapply(df, gsub, pattern = "Neutral/ Neither conservative or liberal", replacement = 3, fixed = TRUE)
df[] <- lapply(df, gsub, pattern = "Slightly conservative", replacement = 2, fixed = TRUE)
df[] <- lapply(df, gsub, pattern = "Very conservative", replacement = 1, fixed = TRUE)


#reversed scales
df$blm_pre_3 <- mgsub(df$blm_pre_3, c(5,4,2,1), c(1,2,4,5))
df$OBLM_post_3 <- mgsub(df$OBLM_post_3, c(5,4,2,1), c(1,2,4,5))
df$IBLM_post_3 <- mgsub(df$IBLM_post_3, c(5,4,2,1), c(1,2,4,5))

#bot experience
psych::alpha(data.matrix(dplyr::select(df,c(bot_pre_1,bot_pre_2,bot_pre_4)))) #0.69

#climate change - pre
psych::alpha(data.matrix(dplyr::select(df,c(cc_pre_1:cc_pre_6)))) #0.86

#climate change - post (info)
psych::alpha(data.matrix(dplyr::select(df,c(ICC_post_1:ICC_post_6)))) #0.86

#climate change - post (opinion)
psych::alpha(data.matrix(dplyr::select(df,c(OCC_post_1:OCC_post_6)))) #0.88

#blm - pre
psych::alpha(data.matrix(dplyr::select(df,c(blm_pre_1:blm_pre_6)))) #0.87

#BLM - post (info)
psych::alpha(data.matrix(dplyr::select(df,c(IBLM_post_1:IBLM_post_6)))) #0.88

#BLM - post (opinion)
psych::alpha(data.matrix(dplyr::select(df,c(OBLM_post_1:OBLM_post_6)))) #0.88

colnames(df)

#satisfaction
psych::alpha(data.matrix(dplyr::select(df,c(satisfaction_1:satisfaction_5)))) #0.89

#rating
psych::alpha(data.matrix(dplyr::select(df,c(rate_1:rate_4)))) #0.89

#learning experience
psych::alpha(data.matrix(dplyr::select(df,c(cc_learn_1:cc_learn_2)))) #0.88
psych::alpha(data.matrix(dplyr::select(df,c(blm_learn_1:blm_learn_2)))) #0.89

#continue
psych::alpha(data.matrix(dplyr::select(df,c(cc_continue_1:cc_continue_3)))) #0.93
psych::alpha(data.matrix(dplyr::select(df,c(blm_continue_1:blm_continue_3)))) #0.94

#recommend
psych::alpha(data.matrix(dplyr::select(df,c(cc_recommend_1:cc_recommend_2)))) #0.92
psych::alpha(data.matrix(dplyr::select(df,c(blm_recommend_1:blm_recommend_2)))) #0.93

##### summarize variables #####
meansd <- function(x) {
  print(paste0(round(mean(x,na.rm=TRUE),2)," (",round(sd(x,na.rm=TRUE),2),")"))
}

df$age <- as.numeric(df$age)
meansd(df$age)

df$political <- as.numeric(df$political)
meansd(df$political)

df$expect_rounds <- as.numeric(df$expect_rounds)
#boxplot(df$expect_rounds)
expect_r <- df %>% filter(expect_rounds < 1000) %>% select(expect_rounds)
meansd(expect_r$expect_rounds)

df$response_count <- as.numeric(df$response_count)
meansd(df$response_count)

df <- type.convert(df)
sapply(df, class)

data <- df %>%
  mutate(bot_pre = rowMeans(dplyr::select(.,bot_pre_1:bot_pre_4)))%>%
  mutate(cc_pre = rowMeans(dplyr::select(.,cc_pre_1:cc_pre_6)))%>%
  mutate(blm_pre = rowMeans(dplyr::select(.,blm_pre_1:blm_pre_6)))%>%
  mutate(ICC_post = rowMeans(dplyr::select(.,ICC_post_1:ICC_post_6)))%>%
  mutate(OCC_post = rowMeans(dplyr::select(.,OCC_post_1:OCC_post_6)))%>%
  mutate(IBLM_post = rowMeans(dplyr::select(.,IBLM_post_1:IBLM_post_6)))%>%
  mutate(OBLM_post = rowMeans(dplyr::select(.,OBLM_post_1:OBLM_post_6)))%>%
  mutate(satisfaction = rowMeans(dplyr::select(.,satisfaction_1:satisfaction_5)))%>%
  mutate(rate = rowMeans(dplyr::select(.,rate_1:rate_4)))%>%
  mutate(cc_learn = rowMeans(dplyr::select(.,cc_learn_1:cc_learn_2)))%>%
  mutate(cc_continue = rowMeans(dplyr::select(.,cc_continue_1:cc_continue_3)))%>%
  mutate(cc_recommend = rowMeans(dplyr::select(.,cc_recommend_1:cc_recommend_2)))%>%
  mutate(blm_learn = rowMeans(dplyr::select(.,blm_learn_1:blm_learn_2)))%>%
  mutate(blm_continue = rowMeans(dplyr::select(.,blm_continue_1:blm_continue_3)))%>%
  mutate(blm_recommend = rowMeans(dplyr::select(.,blm_recommend_1:blm_recommend_2)))%>%
  mutate(pre = paste(cc_pre , blm_pre))%>%
  mutate(post = paste(ICC_post , OCC_post , IBLM_post , OBLM_post))%>%
  mutate(eduminor = ifelse(edu == "Highschool"|edu == "Other (please specify):","minor","major"))%>%
  mutate(raceminor = ifelse(ethnic == "White","major","minor"))%>%
  mutate(raceminor_blm = ifelse(ethnic == "White","white",
                                ifelse(ethnic == " Black or African American","black","other")))%>%
  mutate(langminor = ifelse(language == "English","major","minor"))%>%
  mutate(ideology = ifelse(political == 5 | political == 4,"liberal",
                           ifelse(political == 3,"neutral","conservative")))%>%
  select(c(EndDate:political,want:check,response_count,survey_type,prompt,bot_pre:blm_recommend,pre,post,
           eduminor,raceminor,raceminor_blm,langminor,ideology))

#data <- data %>% filter(gender == "Female" | gender == "Male")

data$post <- gsub(' ', '', data$post)
data$post <- gsub('\\NA','',data$post)
data$post <- as.numeric(data$post)
data$pre <- gsub(' ', '', data$pre)
data$pre <- gsub('\\NA','',data$pre)
data$pre <- as.numeric(data$pre)
data$change <- data$post - data$pre

data_ICC <- data %>% filter(survey_type == "info-climate")
data_OCC <- data %>% filter(survey_type == "opinion-climate")
data_IBLM <- data %>% filter(survey_type == "info-blm")
data_OBLM <- data %>% filter(survey_type == "opinion-blm")

data_cc <- data %>% filter(survey_type == "info-climate" | survey_type == "opinion-climate")
#summary(data_cc$cc_pre)
data_cc <- data_cc %>%
  mutate(ccminor = ifelse(cc_pre < 3.833,"minor","major"))
table(data_cc$ccminor)

data_blm <- data %>% filter(survey_type ==  "info-blm" | survey_type == "opinion-blm")
#summary(data_blm$blm_pre)
data_blm <- data_blm %>%
  mutate(blmminor = ifelse(blm_pre < 3.167,"minor","major"))

##### data wrangling for chat #####
df_chat <- NULL
df_chat <- data %>% select(EndDate,prompt,want,response_count,survey_type)
df_chat$prompt <- gsub('The following is a conversation with an AI assistant. The assistant is helpful, creative, clever, and very friendly.\n', '', df_chat$prompt)
df_chat <- cbind(index = rownames(df_chat), df_chat)
rownames(df_chat) <- 1:nrow(df_chat)
write.csv(df_chat,"chat_cleaned.csv")

df_chat_cc <- df_chat %>% filter(survey_type == "info-climate" | survey_type == "opinion-climate")
df_chat_blm <- df_chat %>% filter(survey_type == "info-blm" | survey_type == "opinion-blm")

speakers <- c("Human", "AI")
pattern <- paste0("(", paste0(speakers, ":", collapse = "|"), ")")

##### User differences in how they say things (LIWC) #####
blm_human_LIWC <- read.csv("LIWC-22 Results - conv_blm_human - LIWC Analysis.csv",
                           fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                           row.names = NULL, 
                           stringsAsFactors = FALSE)
cc_human_LIWC <- read.csv("LIWC-22 Results - conv_cc_human - LIWC Analysis.csv",
                          fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                          row.names = NULL, 
                          stringsAsFactors = FALSE)

cc_human_LIWC <- cc_human_LIWC %>%
  group_by(index) %>%
  summarise(across(WC:OtherP, mean))
cc_human_LIWC$index <- as.character(cc_human_LIWC$index)
cc_human_LIWC <- inner_join(cc_human_LIWC,data_cc_read,by="index")

cc_human_LIWC$WC_log <- log(cc_human_LIWC$WC + 1)
cc_human_LIWC$emo_pos_log <- log(cc_human_LIWC$emo_pos + 1)
cc_human_LIWC$emo_neg_log <- log(cc_human_LIWC$emo_neg + 1)
cc_human_LIWC$emo_anx_log <- log(cc_human_LIWC$emo_anx + 1)
cc_human_LIWC$emo_anger_log <- log(cc_human_LIWC$emo_anger + 1)
cc_human_LIWC$swear_log <- log(cc_human_LIWC$swear + 1)

blm_human_LIWC <- blm_human_LIWC %>%
  group_by(index) %>%
  summarise(across(WC:OtherP, mean))
blm_human_LIWC$index <- as.character(blm_human_LIWC$index)
blm_human_LIWC <- inner_join(blm_human_LIWC,data_blm_read,by="index")

blm_human_LIWC$WC_log <- log(blm_human_LIWC$WC + 1)
blm_human_LIWC$emo_pos_log <- log(blm_human_LIWC$emo_pos + 1)
blm_human_LIWC$emo_neg_log <- log(blm_human_LIWC$emo_neg + 1)
blm_human_LIWC$emo_anx_log <- log(blm_human_LIWC$emo_anx + 1)
blm_human_LIWC$emo_anger_log <- log(blm_human_LIWC$emo_anger + 1)
blm_human_LIWC$swear_log <- log(blm_human_LIWC$swear + 1)

#start regression

#WC_log+ emo_pos_log+ emo_neg_log+ emo_anx_log+
#Analytic+ Clout+Authentic

#gender + age + income + bot_pre + raceminor + 
#langminor + eduminor + ideology +  blmminor

cc_human_LIWC$ideology <- 
  relevel(factor(cc_human_LIWC$ideology), ref = "neutral")
blm_human_LIWC$ideology <- 
  relevel(factor(blm_human_LIWC$ideology), ref = "neutral")   

cchuman_1 <- lm(WC_log ~ gender + age + income + bot_pre + raceminor + 
                  langminor + eduminor + ideology +  ccminor,data = cc_human_LIWC)
cchuman_2 <- lm(emo_pos_log ~ gender + age + income + bot_pre + raceminor + 
                  langminor + eduminor + ideology +  ccminor,data = cc_human_LIWC)
cchuman_3 <- lm(emo_neg_log ~ gender + age + income + bot_pre + raceminor + 
                  langminor + eduminor + ideology +  ccminor,data = cc_human_LIWC)
cchuman_4 <- lm(emo_anx_log ~ gender + age + income + bot_pre + raceminor + 
                  langminor + eduminor + ideology +  ccminor,data = cc_human_LIWC)
cchuman_5 <- lm(Analytic ~ gender + age + income + bot_pre + raceminor + 
                  langminor + eduminor + ideology +  ccminor,data = cc_human_LIWC)
cchuman_6 <- lm(Clout ~ gender + age + income + bot_pre + raceminor + 
                  langminor + eduminor + ideology +  ccminor,data = cc_human_LIWC)
cchuman_7 <- lm(Authentic ~ gender + age + income + bot_pre + raceminor + 
                  langminor + eduminor + ideology +  ccminor,data = cc_human_LIWC)

blmhuman_1 <- lm(WC_log ~ gender + age + income + bot_pre + raceminor + 
                   langminor + eduminor + ideology +  blmminor,data = blm_human_LIWC)
blmhuman_2 <- lm(emo_pos_log ~ gender + age + income + bot_pre + raceminor + 
                   langminor + eduminor + ideology +  blmminor,data = blm_human_LIWC)
blmhuman_3 <- lm(emo_neg_log ~ gender + age + income + bot_pre + raceminor + 
                   langminor + eduminor + ideology +  blmminor,data = blm_human_LIWC)
blmhuman_4 <- lm(emo_anx_log ~ gender + age + income + bot_pre + raceminor + 
                   langminor + eduminor + ideology +  blmminor,data = blm_human_LIWC)
blmhuman_5 <- lm(Analytic ~ gender + age + income + bot_pre + raceminor + 
                   langminor + eduminor + ideology +  blmminor,data = blm_human_LIWC)
blmhuman_6 <- lm(Clout ~ gender + age + income + bot_pre + raceminor + 
                   langminor + eduminor + ideology +  blmminor,data = blm_human_LIWC)
blmhuman_7 <- lm(Authentic ~ gender + age + income + bot_pre + raceminor + 
                   langminor + eduminor + ideology +  blmminor,data = blm_human_LIWC)


stargazer(cchuman_1,cchuman_2,cchuman_3,cchuman_4,cchuman_5,cchuman_6,cchuman_7,
          align = TRUE,out = "humanLIWCfeature_cc.html")

stargazer(blmhuman_1,blmhuman_2,blmhuman_3,blmhuman_4,blmhuman_5,blmhuman_6,blmhuman_7,
          align = TRUE,out = "humanLIWCfeature_blm.html")


##### [Topic modeling] Modeling chatbot responses #####
data <- cbind(index = rownames(data), data)
rownames(data) <- 1:nrow(data)
#data[1] <- NULL

data_topic <- right_join(data,dat,by = "index")

all_human <- dplyr::filter(data_topic, grepl("Human",speaker))
all_bot <- dplyr::filter(data_topic, grepl("AI",speaker))


data_topic_cc <- right_join(data_cc,dat_cc,by = "index")
cc_human <- dplyr::filter(data_topic_cc, grepl("Human",speaker))
cc_bot <- dplyr::filter(data_topic_cc, grepl("AI",speaker))

data_topic_blm <- right_join(data_blm,dat_blm,by = "index")
blm_human <- dplyr::filter(data_topic_blm, grepl("Human",speaker))
blm_bot <- dplyr::filter(data_topic_blm, grepl("AI",speaker))


processedbot_cc <- textProcessor(cc_bot$text, metadata = cc_bot)
outbot_cc <- prepDocuments(processedbot_cc$documents, 
                           processedbot_cc$vocab, 
                           processedbot_cc$meta,
                           lower.thresh = 3)
bot_cc_v2 <- cc_bot[-processedbot_cc$docs.removed,]
bot_cc_v3 <- bot_cc_v2[-outbot_cc$docs.removed,]

processed <- textProcessor(bot_cc_v3$text, metadata = bot_cc_v3)
outbot_cc <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docsbot_cc <- outbot_cc$documents
vocabbot_cc <- outbot_cc$vocab
metabot_cc <-outbot_cc$meta

bot_cc <- stm(outbot_cc$documents, outbot_cc$vocab, 
              K=10, prevalence = ~ age + raceminor+ eduminor + income + 
                langminor + ideology + bot_pre + ccminor,
              max.em.its = 500,
              seed = 1000,
              data = outbot_cc$meta,
              init.type = "Spectral")


blm_bot <- blm_bot %>% 
  drop_na(blmminor)%>%
  drop_na(bot_pre)

processedbot_blm <- textProcessor(blm_bot$text, metadata = blm_bot)
outbot_blm <- prepDocuments(processedbot_blm$documents, 
                            processedbot_blm$vocab, processedbot_blm$meta,
                            lower.thresh = 3)
bot_blm_v2 <- blm_bot[-processedbot_blm$docs.removed,]
bot_blm_v3 <- bot_blm_v2[-outbot_blm$docs.removed,]

processed <- textProcessor(bot_blm_v3$text, metadata = bot_blm_v3)
outbot_blm <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docsbot_blm <- outbot_blm$documents
vocabbot_blm <- outbot_blm$vocab
metabot_blm <-outbot_blm$meta

bot_blm <- stm(outbot_blm$documents, outbot_blm$vocab, 
               K=10, prevalence = ~ age + raceminor + eduminor + income + langminor + ideology + bot_pre + blmminor,
               max.em.its = 500,
               seed = 1000,
               data = outbot_blm$meta,
               init.type = "Spectral")


##### [Regressions] User Experiences Gap #####
#Our IV will be demographic features of the participants (e.g., race, gender,edu). 
#Our CV will be users prior experiences with chatbot, users language styles. 
#This finding will demonstrate where the user experiences gap lies for conversational AI.

#Based on the session "User differences in how they say things (LIWC)"
#Based on code file "full launch analysis_errorcheck.R"

cc_human_LIWC$ideology <- 
  relevel(factor(cc_human_LIWC$ideology), ref = "neutral")
blm_human_LIWC$ideology <- 
  relevel(factor(blm_human_LIWC$ideology), ref = "neutral")   


colnames(cc_human_LIWC)
hist(cc_human_LIWC$satisfaction)

qs <- 1:3/4
qs

#cc-rating
qr_rate <- rq(rate ~ gender + age + income + bot_pre + 
                raceminor + langminor + eduminor + ideology +  
                ccminor + WC_log+ emo_pos_log+ emo_neg_log+ 
                Analytic+ Clout+Authentic,data = cc_human_LIWC, tau = qs)

summary(qr_rate,se = "iid",digits=3)

lm_rate <- lm(rate ~ gender + age + income + bot_pre + 
                raceminor + langminor + eduminor + ideology +  
                ccminor + WC_log+ emo_pos_log+ emo_neg_log+ 
                Analytic+ Clout+Authentic,data = cc_human_LIWC)

summary(lm_rate)

#cc-satisfaction
qr_satisfaction <- rq(satisfaction ~ gender + age + income + bot_pre + 
                        raceminor + langminor + eduminor + ideology +  
                        ccminor + WC_log+ emo_pos_log+ emo_neg_log+ 
                        Analytic+ Clout+Authentic,data = cc_human_LIWC, tau = qs)


summary(qr_satisfaction,se = "iid",digits=3)

lm_satisfaction <- lm(satisfaction ~ gender + age + income + bot_pre + 
                        raceminor + langminor + eduminor + ideology +  
                        ccminor + WC_log+ emo_pos_log+ emo_neg_log+ 
                        Analytic+ Clout+Authentic,data = cc_human_LIWC)

summary(lm_satisfaction)


#cc-learning
qr_learn <- rq(cc_learn ~ gender + age + income + bot_pre + 
                 raceminor + langminor + eduminor + ideology +  
                 ccminor + WC_log+ emo_pos_log+ emo_neg_log+ 
                 Analytic+ Clout+Authentic,data = cc_human_LIWC, tau = qs)

summary(qr_learn,se = "iid",digits=3)

lm_learn <- lm(cc_learn ~ gender + age + income + bot_pre + 
                 raceminor + langminor + eduminor + ideology +  
                 ccminor + WC_log+ emo_pos_log+ emo_neg_log+ 
                 Analytic+ Clout+Authentic,data = cc_human_LIWC)

summary(lm_learn)




#cc-continue
qr_continue <- rq(cc_continue ~ gender + age + income + bot_pre + 
                    raceminor + langminor + eduminor + ideology +  
                    ccminor + WC_log+ emo_pos_log+ emo_neg_log+ 
                    Analytic+ Clout+Authentic,data = cc_human_LIWC, tau = qs)

summary(qr_continue,se = "iid",digits=3)

lm_continue <- lm(cc_continue ~ gender + age + income + bot_pre + 
                    raceminor + langminor + eduminor + ideology +  
                    ccminor + WC_log+ emo_pos_log+ emo_neg_log+ 
                    Analytic+ Clout+Authentic,data = cc_human_LIWC)

summary(lm_continue)


#cc-recommend
qr_recommend <- rq(cc_recommend ~ gender + age + income + bot_pre + 
                     raceminor + langminor + eduminor + ideology +  
                     ccminor + WC_log+ emo_pos_log+ emo_neg_log+ 
                     Analytic+ Clout+Authentic,data = cc_human_LIWC, tau = qs)

summary(qr_recommend,se = "iid",digits=3)

lm_recommend <- lm(cc_recommend ~ gender + age + income + bot_pre + 
                     raceminor + langminor + eduminor + ideology +  
                     ccminor + WC_log+ emo_pos_log+ emo_neg_log+ 
                     Analytic+ Clout+Authentic,data = cc_human_LIWC)

summary(lm_recommend)

#blm-rating
qr_rate <- rq(rate ~ gender + age + income + bot_pre + 
                raceminor + eduminor + langminor + blmminor + ideology +  
                WC_log+ emo_pos_log+ emo_neg_log+ 
                Analytic+ Clout+Authentic,data = blm_human_LIWC, tau = qs)

summary(qr_rate,se = "iid",digits=3)

lm_rate <- lm(rate ~ gender + age + income + bot_pre + 
                raceminor + eduminor + langminor + blmminor + ideology +  
                WC_log+ emo_pos_log+ emo_neg_log+ 
                Analytic+ Clout+Authentic,data = blm_human_LIWC)

summary(lm_rate)

stargazer(lm_rate,lm_satisfaction,lm_learn,lm_continue,lm_recommend,align = TRUE,
          out = "forpasting.html")

#blm-satisfaction
qr_satisfaction <- rq(satisfaction ~ gender + age + income + bot_pre + 
                        raceminor + eduminor + langminor + blmminor + ideology +  
                        WC_log+ emo_pos_log+ emo_neg_log+ 
                        Analytic+ Clout+Authentic,data = blm_human_LIWC, tau = qs)

summary(qr_satisfaction,se = "iid",digits=3)

lm_satisfaction <- lm(satisfaction ~ gender + age + income + bot_pre + 
                        raceminor + eduminor + langminor + blmminor + ideology +  
                        WC_log+ emo_pos_log+ emo_neg_log+ 
                        Analytic+ Clout+Authentic,data = blm_human_LIWC)

summary(lm_satisfaction)


#blm-learning
qr_learn <- rq(blm_learn ~ gender + age + income + bot_pre + 
                 raceminor + eduminor + langminor + blmminor + ideology +  
                 WC_log+ emo_pos_log+ emo_neg_log+ 
                 Analytic+ Clout+Authentic,data = blm_human_LIWC, tau = qs)

summary(qr_learn,se = "iid",digits=3)

lm_learn <- lm(blm_learn ~ gender + age + income + bot_pre + 
                 raceminor + eduminor + langminor + blmminor + ideology +  
                 WC_log+ emo_pos_log+ emo_neg_log+ 
                 Analytic+ Clout+Authentic,data = blm_human_LIWC)

summary(lm_learn)


#blm-continue
qr_continue <- rq(blm_continue ~ gender + age + income + bot_pre + 
                    raceminor + eduminor + langminor + blmminor +ideology +  
                    WC_log+ emo_pos_log+ emo_neg_log+ 
                    Analytic+ Clout+Authentic,data = blm_human_LIWC, tau = qs)

summary(qr_continue,se = "iid",digits=3)

lm_continue <- lm(blm_continue ~ gender + age + income + bot_pre + 
                    raceminor + eduminor + langminor + blmminor +ideology +  
                    WC_log+ emo_pos_log+ emo_neg_log+ 
                    Analytic+ Clout+Authentic,data = blm_human_LIWC)

summary(lm_continue)


#blm-recommend
qr_recommend <- rq(blm_recommend ~ gender + age + income + bot_pre + 
                     raceminor + eduminor + langminor + blmminor + ideology +  
                     WC_log+ emo_pos_log+ emo_neg_log+ 
                     Analytic+ Clout+Authentic,data = blm_human_LIWC, tau = qs)

summary(qr_recommend,se = "iid",digits=3)

lm_recommend <- lm(blm_recommend ~ gender + age + income + bot_pre + 
                     raceminor + eduminor + langminor + blmminor + ideology +  
                     WC_log+ emo_pos_log+ emo_neg_log+ 
                     Analytic+ Clout+Authentic,data = blm_human_LIWC)

summary(lm_recommend)


##### [Regressions] User Attitude Change Gap #####
qr_change <- rq(change ~ gender + age + income + bot_pre + 
                  raceminor + eduminor + langminor + ccminor + ideology +  
                  WC_log+ emo_pos_log+ emo_neg_log+ 
                  Analytic+ Clout+Authentic,data = cc_human_LIWC, tau = qs)


lm_change <- lm(change ~ gender + age + income + bot_pre + 
                  raceminor + eduminor + langminor + ccminor + ideology +  
                  WC_log+ emo_pos_log+ emo_neg_log+ 
                  Analytic+ Clout+Authentic,data = cc_human_LIWC)

summary(lm_change)


lm_change <- lm(change ~ gender + age + income + bot_pre + 
                  raceminor + eduminor + langminor + blmminor + ideology +  
                  WC_log+ emo_pos_log+ emo_neg_log+ 
                  Analytic+ Clout+Authentic,data = blm_human_LIWC)

summary(lm_change)

#make a plot showing changes
major <- cc_human_LIWC %>% filter(ccminor == "major")
major_pre <- mean(major$pre)
major_post <- mean(major$post)

minor <- cc_human_LIWC %>% filter(ccminor == "minor")
minor_pre <- mean(minor$pre)
minor_post <- mean(minor$post)

df_change_cc <- data.frame(time = c("Pre-chat","Post-chat","Pre-chat","Post-chat"),
                           group1 = c("Opinion majority","Opinion majority",
                                      "Opinion minority","Opinion minority"),
                           attitude = c(major_pre,major_post,minor_pre,minor_post))
df_change_cc$time <- factor(df_change_cc$time, levels=c("Pre-chat","Post-chat"))

df_change_blm <- data.frame(time = c("Pre-chat","Post-chat","Pre-chat","Post-chat"),
                            group1 = c("Opinion majority","Opinion majority",
                                       "Opinion minority","Opinion minority"),
                            attitude = c(major_pre,major_post,minor_pre,minor_post))
df_change_blm$time <- factor(df_change_blm$time, levels=c("Pre-chat","Post-chat"))

g1 <- ggplot(df_change_cc, aes(x = time, y = attitude, color = group1, group = group1)) +
  geom_line() + theme_bw() + theme(panel.grid=element_blank(),legend.position="none") +
  xlab("")+ylab("Attitude score - climate change")+
  ylim(2,5)


g2 <- ggplot(df_change_blm, aes(x = time, y = attitude, color = group1, group = group1)) +
  geom_line() + theme_bw() + theme(panel.grid=element_blank(),
                                   legend.position=c(0.77, 0.85),
                                   legend.title = element_blank()) +
  xlab("")+ylab("Attitude score - BLM")+
  ylim(2,5)

ggarrange(g1,g2,ncol = 2)

major <- blm_human_LIWC %>% filter(eduminor == "major")
major_pre <- mean(major$pre)
major_post <- mean(major$post)

minor <- blm_human_LIWC %>% filter(eduminor == "minor")
minor_pre <- mean(minor$pre)
minor_post <- mean(minor$post)

df_change_cc <- data.frame(time = c("Pre-chat","Post-chat","Pre-chat","Post-chat"),
                           group1 = c("Education majority","Education majority",
                                      "Education minority","Education minority"),
                           attitude = c(major_pre,major_post,minor_pre,minor_post))
df_change_cc$time <- factor(df_change_cc$time, levels=c("Pre-chat","Post-chat"))

df_change_blm <- data.frame(time = c("Pre-chat","Post-chat","Pre-chat","Post-chat"),
                            group1 = c("Education majority","Education majority",
                                       "Education minority","Education minority"),
                            attitude = c(major_pre,major_post,minor_pre,minor_post))
df_change_blm$time <- factor(df_change_blm$time, levels=c("Pre-chat","Post-chat"))

g1 <- ggplot(df_change_cc, aes(x = time, y = attitude, color = group1, group = group1)) +
  geom_line() + theme_bw() + theme(panel.grid=element_blank(),legend.position="none") +
  xlab("")+ylab("Attitude score - climate change")+
  ylim(2,5)


g2 <- ggplot(df_change_blm, aes(x = time, y = attitude, color = group1, group = group1)) +
  geom_line() + theme_bw() + theme(panel.grid=element_blank(),
                                   legend.position=c(0.77, 0.85),
                                   legend.title = element_blank()) +
  xlab("")+ylab("Attitude score - BLM")+
  ylim(2,5)

ggarrange(g1,g2,ncol = 2)


##### [Regressions] Chatbot LIWC differences #####
LIWC_cc <- read.csv("LIWC-22 Results - conv_cc_bot - LIWC Analysis.csv",
                    fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                    row.names = NULL, 
                    stringsAsFactors = FALSE)
#LIWC_cc <- type.convert(LIWC_cc)
LIWC_cc <- LIWC_cc %>%
  group_by(index) %>%
  summarise(across(WC:OtherP, mean))
LIWC_cc$index <- as.character(LIWC_cc$index)
LIWC_cc <- inner_join(LIWC_cc,data_cc_read,by="index")

LIWC_cc <- LIWC_cc %>% select(c(index, WC, emo_pos, emo_neg, 
                                Analytic, Clout,Authentic, 
                                gender , age ,  income , bot_pre , raceminor , 
                                langminor , eduminor , ideology , ccminor))

LIWC_cc$WC_log <- log(LIWC_cc$WC + 1)
LIWC_cc$emo_pos_log <- log(LIWC_cc$emo_pos + 1)
LIWC_cc$emo_neg_log <- log(LIWC_cc$emo_neg + 1)

LIWC_cc <- LIWC_cc %>% select(c(index, WC_log, emo_pos_log, emo_neg_log,
                                Analytic, Clout,Authentic, 
                                gender , age ,  income , bot_pre , raceminor , 
                                langminor , eduminor , ideology , ccminor))


LIWC_cc$ideology <- 
  relevel(factor(LIWC_cc$ideology), ref = "neutral")

out_LIWC_cc <- lapply(2:7, function(x) 
  lm(as.numeric(unlist(LIWC_cc[,x])) ~ gender + age +  income + bot_pre + raceminor + 
       langminor + eduminor + ideology +  ccminor, LIWC_cc))

stargazer(out_LIWC_cc,align = TRUE, 
          out = "LIWC_cc_chatbot.html")


LIWC_blm <- read.csv("LIWC-22 Results - conv_blm_bot - LIWC Analysis.csv",
                     fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                     row.names = NULL, 
                     stringsAsFactors = FALSE)
#LIWC_blm <- type.convert(LIWC_blm)
LIWC_blm <- LIWC_blm %>%
  group_by(index) %>%
  summarise(across(WC:OtherP, mean))
LIWC_blm$index <- as.character(LIWC_blm$index)
LIWC_blm <- inner_join(LIWC_blm,data_blm_read,by="index")

LIWC_blm <- LIWC_blm %>% select(c(index, WC, emo_pos, emo_neg, 
                                  Analytic, Clout,Authentic, 
                                  gender , age ,  income , bot_pre , raceminor , 
                                  langminor , eduminor , ideology , blmminor))

LIWC_blm$WC_log <- log(LIWC_blm$WC + 1)
LIWC_blm$emo_pos_log <- log(LIWC_blm$emo_pos + 1)
LIWC_blm$emo_neg_log <- log(LIWC_blm$emo_neg + 1)

LIWC_blm <- LIWC_blm %>% select(c(index, WC_log, emo_pos_log, emo_neg_log,
                                  Analytic, Clout,Authentic, 
                                  gender , age ,  income , bot_pre , raceminor , 
                                  langminor , eduminor , ideology , blmminor))


LIWC_blm$ideology <- 
  relevel(factor(LIWC_blm$ideology), ref = "neutral")

out_LIWC_blm <- lapply(2:7, function(x) 
  lm(as.numeric(unlist(LIWC_blm[,x])) ~ gender + age +  income + bot_pre + raceminor + 
       langminor + eduminor + ideology +  blmminor, LIWC_blm))

stargazer(out_LIWC_blm,align = TRUE, 
          out = "LIWC_blm_chatbot.html")

out_LIWC_cc <- lapply(2:7, function(x) 
  lm(as.numeric(unlist(LIWC_cc[,x])) ~ gender + age +  income + bot_pre + raceminor + 
       langminor + eduminor + ideology +  ccminor, LIWC_cc))

LIWC_blm$Analytic_1 <- LIWC_blm$Analytic/50
LIWC_blm$Clout_1 <- LIWC_blm$Clout/50
LIWC_blm$Authentic_1 <- LIWC_blm$Authentic/50


lm1 <- lm(WC_log ~ gender + age +  income + bot_pre + raceminor + 
            langminor + eduminor + ideology +  blmminor,LIWC_blm)
lm2 <- lm(emo_pos_log ~ gender + age +  income + bot_pre + raceminor + 
            langminor + eduminor + ideology +  blmminor,LIWC_blm)
lm3 <- lm(emo_neg_log ~ gender + age +  income + bot_pre + raceminor + 
            langminor + eduminor + ideology +  blmminor,LIWC_blm)
lm4 <- lm(Analytic_1 ~ gender + age +  income + bot_pre + raceminor + 
            langminor + eduminor + ideology +  blmminor,LIWC_blm)
lm5 <- lm(Clout_1 ~ gender + age +  income + bot_pre + raceminor + 
            langminor + eduminor + ideology +  blmminor,LIWC_blm)
lm6 <- lm(Authentic_1 ~ gender + age +  income + bot_pre + raceminor + 
            langminor + eduminor + ideology +  blmminor,LIWC_blm)

plot_summs(lm1,lm2,lm3,lm4,lm5,lm6, 
           model.names = c("Word count (log)","Positive words (log)",
                           "Negative words (log)",
                           "Analytic/50","Clout/50","Authentic/50"),
           coefs = c("Race/Ethnicity minority" = "raceminorminor",
                     "Language minority" = "langminorminor",
                     "Education minority" = "eduminorminor",
                     "Opinion minority" = "blmminorminor"),
           legend.title = "LIWC features in \nchatbot's responses",
           scale = TRUE,colors = c("grey30","orange","seagreen",
                                   "magenta","blue","goldenrod")) + 
  labs(x = "\nLess expressed --- More expressed                              \n ", y = NULL)



##### [Regressions] User Experience with the bot response gap #####
LIWC_cc <- read.csv("LIWC-22 Results - conv_cc_bot - LIWC Analysis.csv",
                    fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                    row.names = NULL, 
                    stringsAsFactors = FALSE)
LIWC_cc <- LIWC_cc %>%
  group_by(index) %>%
  summarise(across(WC:OtherP, mean))
LIWC_cc$index <- as.character(LIWC_cc$index)
LIWC_cc <- inner_join(LIWC_cc,data_cc_read,by="index")

LIWC_cc$WC_log <- log(LIWC_cc$WC + 1)
LIWC_cc$emo_pos_log <- log(LIWC_cc$emo_pos + 1)
LIWC_cc$emo_neg_log <- log(LIWC_cc$emo_neg + 1)
LIWC_cc$emo_anx_log <- log(LIWC_cc$emo_anx + 1)
LIWC_cc$emo_anger_log <- log(LIWC_cc$emo_anger + 1)
LIWC_cc$swear_log <- log(LIWC_cc$swear + 1)

LIWC_blm <- read.csv("LIWC-22 Results - conv_blm_bot - LIWC Analysis.csv",
                     fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                     row.names = NULL, 
                     stringsAsFactors = FALSE)
LIWC_blm <- LIWC_blm %>%
  group_by(index) %>%
  summarise(across(WC:OtherP, mean))
LIWC_blm$index <- as.character(LIWC_blm$index)
LIWC_blm <- inner_join(LIWC_blm,data_blm_read,by="index")

LIWC_blm$WC_log <- log(LIWC_blm$WC + 1)
LIWC_blm$emo_pos_log <- log(LIWC_blm$emo_pos + 1)
LIWC_blm$emo_neg_log <- log(LIWC_blm$emo_neg + 1)
LIWC_blm$emo_anx_log <- log(LIWC_blm$emo_anx + 1)
LIWC_blm$emo_anger_log <- log(LIWC_blm$emo_anger + 1)
LIWC_blm$swear_log <- log(LIWC_blm$swear + 1)

LIWC_cc$ideology <- 
  relevel(factor(LIWC_cc$ideology), ref = "neutral")
LIWC_blm$ideology <- 
  relevel(factor(LIWC_blm$ideology), ref = "neutral")   

#check correlations first
library("Hmisc")
cc_mat <- as.matrix(LIWC_cc %>% select(rate,satisfaction,cc_learn,
                                       cc_continue,cc_recommend))
rcorr(cc_mat)

blm_mat <- as.matrix(LIWC_blm %>% select(rate,satisfaction,blm_learn,
                                         blm_continue,blm_recommend))
rcorr(blm_mat)



#cc-rating
qr_rate <- rq(rate ~  raceminor + 
                langminor + eduminor + ccminor + ideology +
                WC_log+ emo_pos_log+ emo_neg_log +
                Analytic+ Clout+ Authentic +
                gender + age + income + bot_pre,
              LIWC_cc, tau = qs)

lm_rate <- lm(rate ~ raceminor + 
                langminor + eduminor + ccminor + ideology +
                WC_log+ emo_pos_log+ emo_neg_log +
                Analytic+ Clout+ Authentic +
                gender + age + income + bot_pre,
              LIWC_cc)

#cc-satisfaction
qr_satisfaction <- rq(satisfaction ~raceminor + 
                        langminor + eduminor + ccminor + ideology +
                        WC_log+ emo_pos_log+ emo_neg_log +
                        Analytic+ Clout+ Authentic +
                        gender + age + income + bot_pre,
                      LIWC_cc, tau = qs)

lm_satisfaction <- lm(satisfaction ~ raceminor + 
                        langminor + eduminor + ccminor + ideology +
                        WC_log+ emo_pos_log+ emo_neg_log +
                        Analytic+ Clout+ Authentic +
                        gender + age + income + bot_pre,
                      LIWC_cc)



#cc-learning
qr_learn <- rq(cc_learn ~ raceminor + 
                 langminor + eduminor + ccminor + ideology +
                 WC_log+ emo_pos_log+ emo_neg_log +
                 Analytic+ Clout+ Authentic +
                 gender + age + income + bot_pre,
               LIWC_cc, tau = qs)

lm_learn <- lm(cc_learn ~ raceminor + 
                 langminor + eduminor + ccminor + ideology +
                 WC_log+ emo_pos_log+ emo_neg_log +
                 Analytic+ Clout+ Authentic +
                 gender + age + income + bot_pre,LIWC_cc)



#cc-continue
qr_continue <- rq(cc_continue ~ raceminor + 
                    langminor + eduminor + ccminor + ideology +
                    WC_log+ emo_pos_log+ emo_neg_log +
                    Analytic+ Clout+ Authentic +
                    gender + age + income + bot_pre,
                  LIWC_cc, tau = qs)



lm_continue <- lm(cc_continue ~ raceminor + 
                    langminor + eduminor + ccminor + ideology +
                    WC_log+ emo_pos_log+ emo_neg_log +
                    Analytic+ Clout+ Authentic +
                    gender + age + income + bot_pre,
                  LIWC_cc)



#cc-recommend
qr_recommend <- rq(cc_recommend ~ raceminor + 
                     langminor + eduminor + ccminor + ideology +
                     WC_log+ emo_pos_log+ emo_neg_log +
                     Analytic+ Clout+ Authentic +
                     gender + age + income + bot_pre,
                   LIWC_cc, tau = qs)


lm_recommend <- lm(cc_recommend ~ raceminor + 
                     langminor + eduminor + ccminor + ideology +
                     WC_log+ emo_pos_log+ emo_neg_log +
                     Analytic+ Clout+ Authentic +
                     gender + age + income + bot_pre,
                   LIWC_cc)

summary(qr_rate,se = "iid",digits=3)
summary(lm_rate)
summary(qr_satisfaction,se = "iid",digits=3)
summary(lm_satisfaction)
summary(qr_learn,se = "iid",digits=3)
summary(lm_learn)
summary(qr_continue,se = "iid",digits=3)
summary(lm_continue)
summary(qr_recommend,se = "iid",digits=3)
summary(lm_recommend)

printpr2 <- function(t){
  fit0 <- rq(satisfaction ~ 1, tau = t,data = LIWC_blm)
  fit1 <- rq(satisfaction ~ gender + age + income + bot_pre + 
               raceminor + langminor + eduminor + ideology +  
               blmminor + WC_log+ emo_pos_log+ emo_neg_log+ 
               Analytic+ Clout+Authentic,tau = t,data = LIWC_blm)
  rho <- function(u,tau=.5)u*(tau - (u < 0))
  V <- 1 - fit1$rho/fit0$rho
  print(round(V,2))
}
for (i in c(0.25,0.5,0.75)) {printpr2(i)}


#blm-rating
qr_rate <- rq(rate ~  raceminor + 
                langminor + eduminor + blmminor + ideology +
                WC_log+ emo_pos_log+ emo_neg_log +
                Analytic+ Clout+ Authentic +
                gender + age + income + bot_pre,
              LIWC_blm, tau = qs)

lm_rate <- lm(rate ~ raceminor + 
                langminor + eduminor + blmminor + ideology +
                WC_log+ emo_pos_log+ emo_neg_log +
                Analytic+ Clout+ Authentic +
                gender + age + income + bot_pre,
              LIWC_blm)

#blm-satisfaction
qr_satisfaction <- rq(satisfaction ~raceminor + 
                        langminor + eduminor + blmminor + ideology +
                        WC_log+ emo_pos_log+ emo_neg_log +
                        Analytic+ Clout+ Authentic +
                        gender + age + income + bot_pre,
                      LIWC_blm, tau = qs)

lm_satisfaction <- lm(satisfaction ~ raceminor + 
                        langminor + eduminor + blmminor + ideology +
                        WC_log+ emo_pos_log+ emo_neg_log +
                        Analytic+ Clout+ Authentic +
                        gender + age + income + bot_pre,
                      LIWC_blm)



#blm-learning
qr_learn <- rq(blm_learn ~ raceminor + 
                 langminor + eduminor + blmminor + ideology +
                 WC_log+ emo_pos_log+ emo_neg_log +
                 Analytic+ Clout+ Authentic +
                 gender + age + income + bot_pre,
               LIWC_blm, tau = qs)

lm_learn <- lm(blm_learn ~ raceminor + 
                 langminor + eduminor + blmminor + ideology +
                 WC_log+ emo_pos_log+ emo_neg_log +
                 Analytic+ Clout+ Authentic +
                 gender + age + income + bot_pre,LIWC_blm)



#blm-continue
qr_continue <- rq(blm_continue ~ raceminor + 
                    langminor + eduminor + blmminor + ideology +
                    WC_log+ emo_pos_log+ emo_neg_log +
                    Analytic+ Clout+ Authentic +
                    gender + age + income + bot_pre,
                  LIWC_blm, tau = qs)



lm_continue <- lm(blm_continue ~ raceminor + 
                    langminor + eduminor + blmminor + ideology +
                    WC_log+ emo_pos_log+ emo_neg_log +
                    Analytic+ Clout+ Authentic +
                    gender + age + income + bot_pre,
                  LIWC_blm)



#blm-recommend
qr_recommend <- rq(blm_recommend ~ raceminor + 
                     langminor + eduminor + blmminor + ideology +
                     WC_log+ emo_pos_log+ emo_neg_log +
                     Analytic+ Clout+ Authentic +
                     gender + age + income + bot_pre,
                   LIWC_blm, tau = qs)


lm_recommend <- lm(blm_recommend ~ raceminor + 
                     langminor + eduminor + blmminor + ideology +
                     WC_log+ emo_pos_log+ emo_neg_log +
                     Analytic+ Clout+ Authentic +
                     gender + age + income + bot_pre,
                   LIWC_blm)

summary(qr_rate,se = "iid",digits=3)
summary(lm_rate)
summary(qr_satisfaction,se = "iid",digits=3)
summary(lm_satisfaction)
summary(qr_learn,se = "iid",digits=3)
summary(lm_learn)
summary(qr_continue,se = "iid",digits=3)
summary(lm_continue)
summary(qr_recommend,se = "iid",digits=3)
summary(lm_recommend)


##### [Stance detection] Stance changes overtime #####
library(lme4)
cc_stance_human <- read.csv("cc_human_machinecode.csv",
                            fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                            row.names = NULL, 
                            stringsAsFactors = FALSE) %>%
  select(-c(X,`Unnamed..0`,mod))%>%
  rename(text_human = text)%>%
  rename(stance_human = stance)%>%
  rename(p_ns_human = `Not.support`)%>%
  rename(p_s_human = Support)

cc_stance_bot <- read.csv("cc_bot_machinecode.csv",
                          fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                          row.names = NULL, 
                          stringsAsFactors = FALSE)%>%
  mutate(stance_bot = stance + 1)%>%
  select(-c(X,`Unnamed..0`,stance))%>%
  rename(text_bot = text)%>%
  rename(p_ns_bot = `Not.support`)%>%
  rename(p_s_bot = Support)%>%
  mutate()


cc_stance <- merge(cc_stance_human,cc_stance_bot,by=c("index","round"))
cc_stance <- merge(data_cc_read,cc_stance,by="index")

t <- tail(cc_stance)

sapply(cc_stance, class)

colnames(data_cc_read)

cc_stance$ideology <- 
  relevel(factor(cc_stance$ideology), ref = "neutral")

ccstance_model <- lmer(stance_human ~ pre + round + stance_bot +
                         gender + age + income + bot_pre + 
                         raceminor + eduminor + langminor + ccminor + ideology + 
                         (1|index), data=cc_stance,REML = F)

summary(ccstance_model)

library(nlme)
ccstance_model <- lme(p_s_human ~ round*p_s_bot + 
                        pre + gender + age + income + bot_pre + 
                        raceminor + eduminor + langminor + 
                        ccminor + ideology, data=cc_stance, 
                      random= ~round|index)


ccstance_model <- glmer(stance_human ~ pre + round*stance_bot +
                          gender + age + income + bot_pre + ideology + 
                          raceminor + eduminor + langminor + ccminor + 
                          (round|index), data=cc_stance,
                        family = binomial, verbose = TRUE,
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl=list(maxfun=2e5)))

ccstance_model <- glmer(stance_human ~ pre + round*stance_bot +
                          gender + age + income + bot_pre + ideology + 
                          raceminor + eduminor + langminor + ccminor + 
                          (round|index), data=cc_stance,
                        family = binomial, verbose = TRUE,
                        control = glmerControl(optimizer = "Nelder_Mead"))


table(cc_stance$raceminor)
cc_stance$stance_bot <- as.factor(cc_stance$stance_bot)
cc_stance$stance_human <- as.factor(cc_stance$stance_human)
#cc_stance$round <- as.factor(cc_stance$round)
#cc_stance$index <- as.factor(cc_stance$index)

cc_stance_std <- cc_stance %>% 
  mutate(across(where(is.numeric), scale))

ccstance_model <- glmer(stance_human ~ pre + round*stance_bot +
                          gender + age + income + bot_pre + ideology + 
                          raceminor + eduminor + langminor + ccminor + 
                          (round|index), data=cc_stance_std,
                        family = binomial, verbose = TRUE,
                        control = glmerControl(optimizer = "Nelder_Mead"))

tt <- getME(ccstance_model,"theta")
ll <- getME(ccstance_model,"lower")
min(tt[ll==0])

stargazer(ccstance_model,out = "cc_stance.html")

ccstance_model_simp <- glmer(stance_human ~ round*stance_bot +
                               gender + age + income + bot_pre + ideology + 
                               raceminor + eduminor + langminor + ccminor +
                               (round|index), data=cc_stance_std,
                             family = binomial, verbose = TRUE,
                             control = glmerControl(optimizer = "Nelder_Mead"))

summary(ccstance_model_simp)
stargazer(ccstance_model_simp,out = "cc_stance.html")

#blm
blm_stance_human <- read.csv("blm_human_machinecode.csv",
                             fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                             row.names = NULL, 
                             stringsAsFactors = FALSE) %>%
  select(-c(X,`Unnamed..0`))%>%
  rename(text_human = text)%>%
  rename(stance_human = stance)%>%
  rename(p_ns_human = `Not.support`)%>%
  rename(p_s_human = Support)

blm_stance_bot <- read.csv("blm_bot_machinecode.csv",
                           fill=TRUE, header=TRUE, sep=",", encoding="UTF-8",
                           row.names = NULL, 
                           stringsAsFactors = FALSE)%>%
  rename(stance_bot = stance)%>%
  select(-c(X,`Unnamed..0`))%>%
  rename(text_bot = text)%>%
  rename(p_ns_bot = `Not.support`)%>%
  rename(p_s_bot = Support)

colnames(blm_stance_bot)


blm_stance <- merge(blm_stance_human,blm_stance_bot,by=c("index","round"))
blm_stance <- merge(data_blm_read,blm_stance,by="index")

blm_stance$stance_bot <- as.factor(blm_stance$stance_bot)
blm_stance$stance_human <- as.factor(blm_stance$stance_human)
#blm_stance$round <- as.factor(blm_stance$round)
#blm_stance$index <- as.factor(blm_stance$index)

blm_stance_std <- blm_stance %>% 
  mutate(across(where(is.numeric), scale))

blmstance_model_simp <- glmer(stance_human ~ round*stance_bot +
                                gender + age + income + bot_pre + ideology + 
                                raceminor + eduminor + langminor + blmminor +
                                (round|index), data=blm_stance_std,
                              family = binomial, verbose = TRUE,
                              control = glmerControl(optimizer = "Nelder_Mead"))

summary(blmstance_model_simp)

stargazer(ccstance_model_simp,blmstance_model_simp,
          align = TRUE, 
          out = "stancemodels.html")

write.csv(cc_stance,"cc_stance_and_survey.csv")
write.csv(blm_stance,"blm_stance_and_survey.csv")
