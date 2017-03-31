library(tidyr)
library(dplyr)
library(lubridate)
library(ISLR)
library(ggplot2)


file.choose()
voter <- read.csv("C:\\Users\\tdounias\\Desktop\\Reed College\\Spring 2017\\MATH 241\\Repositories\\theodore_dounias\\hw_5_logit\\data\\voter_logit.csv")

set.seed(251981)
voter_sample <- sample_n(voter_large, 100000, replace = FALSE)
write.csv(voter_sample, file = "voter_sample.csv")

rm(voter_large)



voter$BIRTH_DATE.x <- mdy(voter$BIRTH_DATE.x)

voter <- voter %>%
  select(2, 6:7, 16) %>%
  filter(X11.08.2016 == "NO"| X11.08.2016 == "YES" ) %>%
  mutate(AGE = 2016 - year(BIRTH_DATE.x)) %>%
  select(-1)

voter$X11.08.2016 <- ifelse(voter$X11.08.2016 == "YES", 1, 0)

colnames(voter) <- c("PARTY_CODE", "COUNTY", "VOTED", "AGE")
  
voter$PARTY_CODE <- ifelse(voter$PARTY_CODE == "DEM", "DEMOCRAT", 
                           ifelse(voter$PARTY_CODE == "REP", "REPUBLICAN", "OTHER"))

write.csv(voter, file = "voter_logit.csv")

voter <- read.csv("C:\\Users\\tdounias\\Desktop\\Reed College\\Spring 2017\\MATH 241\\Repositories\\theodore_dounias\\hw_5_logit\\data\\voter_logit.csv")

set.seed(251981)
voter_indices <- sample(1:nrow(voter), size = nrow(voter)/2, replace = FALSE)
voter_sample <- slice(voter, voter_indices)
voter_test  <- slice(voter, -voter_indices)

m_age <- glm(VOTED ~ AGE, data = voter_sample, family = binomial)

m_party <- glm(VOTED ~ AGE + PARTY_CODE, data = voter_sample, family = binomial)

m_county <- glm(VOTED ~ AGE + PARTY_CODE + COUNTY, data = voter_sample, family = binomial)

AGE2 <- voter_sample$AGE^2

m_age2 <- glm(VOTED ~ AGE + AGE2, data = voter_sample, family = binomial)

y1 <- predict(m_age, newdata = voter_test, type = "response")
y2 <- predict(m_party, newdata = voter_test, type = "response")
y3 <- predict(m_county, newdata = voter_test, type = "response")
y4 <- predict(m_age2, newdata = voter_test, type = "response")
mcr <- c(0, 0, 0, 0)

voter_test1 <- voter_test %>%
  mutate(p_hat        = y1,
         pred_vote = p_hat > .5)

confusion_mat1 <- voter_test1 %>%
  group_by(VOTED, pred_vote) %>%
  tally()

mcr[1] <- confusion_mat1[1,3]/nrow(voter_test)

voter_test2 <- voter_test %>%
  mutate(p_hat        = y2,
         pred_vote = p_hat > .5)

confusion_mat2 <- voter_test2 %>%
  group_by(VOTED, pred_vote) %>%
  tally()

mcr[2] <- (confusion_mat2[2,3] + confusion_mat2[3,3])/nrow(voter_test)

voter_test3 <- voter_test %>%
  mutate(p_hat        = y3,
         pred_vote = p_hat > .5)

confusion_mat3 <- voter_test3 %>%
  group_by(VOTED, pred_vote) %>%
  tally()

mcr[3] <- (confusion_mat3[2,3] + confusion_mat3[3,3])/nrow(voter_test)

voter_test4 <- voter_test %>%
  mutate(p_hat        = y4,
         pred_vote = p_hat > .5)

confusion_mat4 <- voter_test4 %>%
  group_by(VOTED, pred_vote) %>%
  tally()

mcr[4] <- (confusion_mat4[2,3] + confusion_mat4[3,3])/nrow(voter_test)

mcr

