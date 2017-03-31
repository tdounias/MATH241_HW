hw\_5\_logit
================
Theodore Dounias
March 29, 2017

Tidying the Data:
-----------------

After reading everything in, I took a sample. I proceeded to tidy the data into the workable form we use, and then wrote that into a csv. In this exercise I will prefer to read from that CSV instead of doint the tidy steo again in rmd. I attach the code with eval = FALSE.

``` r
#Read File
voter_large <- read.csv(".....")

#Take sample
set.seed(251981)
voter_sample <- sample_n(voter_large, 100000, replace = FALSE)
write.csv(voter_sample, file = "voter_sample.csv")

#Tidy your RAM!
rm(voter_large)

#Lubridate
voter$BIRTH_DATE.x <- mdy(voter$BIRTH_DATE.x)

#Select off useful variables and create AGE variable
voter <- voter %>%
  select(2, 6:7, 16) %>%
  filter(X11.08.2016 == "NO"| X11.08.2016 == "YES" ) %>%
  mutate(AGE = 2016 - year(BIRTH_DATE.x)) %>%
  select(-1)

#Create VOTED variable
voter$X11.08.2016 <- ifelse(voter$X11.08.2016 == "YES", 1, 0)

colnames(voter) <- c("PARTY_CODE", "COUNTY", "VOTED", "AGE")

#Create usefull PARTY variable  
voter$PARTY_CODE <- ifelse(voter$PARTY_CODE == "DEM", "DEMOCRAT", 
                           ifelse(voter$PARTY_CODE == "REP", "REPUBLICAN", "OTHER"))

#Write to folder
write.csv(voter, file = "voter_logit.csv")
```

Fitting and Assesing Models
---------------------------

I will provide all the code here, and in the next section include any written assesment:

``` r
#Read File
voter <- read.csv("C:\\Users\\tdounias\\Desktop\\Reed College\\Spring 2017\\MATH 241\\Repositories\\theodore_dounias\\hw_5_logit\\data\\voter_logit.csv")

#Split the sample
set.seed(251981)
voter_indices <- sample(1:nrow(voter), size = nrow(voter)/2, replace = FALSE)
voter_train <- slice(voter, voter_indices)
voter_test  <- slice(voter, -voter_indices)

#Fit the models
m_age <- glm(VOTED ~ AGE, data = voter_train, family = binomial)

m_party <- glm(VOTED ~ AGE + PARTY_CODE, data = voter_train, family = binomial)

m_county <- glm(VOTED ~ AGE + PARTY_CODE + COUNTY, data = voter_train, family = binomial)

AGE2 <- voter_train$AGE^2

m_age2 <- glm(VOTED ~ AGE + I(AGE^2), data = voter_train, family = binomial)

#Make predictions for voter_test
y1 <- predict(m_age, newdata = voter_test, type = "response")
y2 <- predict(m_party, newdata = voter_test, type = "response")
y3 <- predict(m_county, newdata = voter_test, type = "response")
y4 <- predict(m_age2, newdata = voter_test, type = "response")
mcr <- c(0, 0, 0, 0)

#Generate mcr's
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

#Print!
mcr
```

    ## [[1]]
    ## [1] 0.1970483
    ## 
    ## [[2]]
    ## [1] 0.1913392
    ## 
    ## [[3]]
    ## [1] 0.1883543
    ## 
    ## [[4]]
    ## [1] NA

Written Assesment of Results:
-----------------------------

**Coefficients**

To judge the models we need to examine them:

``` r
summary(m_age)
```

    ## 
    ## Call:
    ## glm(formula = VOTED ~ AGE, family = binomial, data = voter_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7307   0.3614   0.5137   0.7187   1.0254  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.3294263  0.0343360  -9.594   <2e-16 ***
    ## AGE          0.0387832  0.0007644  50.735   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 41443  on 42212  degrees of freedom
    ## Residual deviance: 38501  on 42211  degrees of freedom
    ## AIC: 38505
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
summary(m_party)
```

    ## 
    ## Call:
    ## glm(formula = VOTED ~ AGE + PARTY_CODE, family = binomial, data = voter_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7906   0.3130   0.4411   0.6570   1.2462  
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.512152   0.042207  12.134   <2e-16 ***
    ## AGE                   0.032316   0.000798  40.496   <2e-16 ***
    ## PARTY_CODEOTHER      -1.254042   0.030875 -40.617   <2e-16 ***
    ## PARTY_CODEREPUBLICAN -0.008490   0.039385  -0.216    0.829    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 41443  on 42212  degrees of freedom
    ## Residual deviance: 36258  on 42209  degrees of freedom
    ## AIC: 36266
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
summary(m_county)
```

    ## 
    ## Call:
    ## glm(formula = VOTED ~ AGE + PARTY_CODE + COUNTY, family = binomial, 
    ##     data = voter_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9676   0.3041   0.4433   0.6500   1.4638  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          -0.0514524  0.2071464  -0.248 0.803835    
    ## AGE                   0.0332801  0.0008082  41.180  < 2e-16 ***
    ## PARTY_CODEOTHER      -1.2173704  0.0313055 -38.887  < 2e-16 ***
    ## PARTY_CODEREPUBLICAN  0.0434255  0.0403532   1.076 0.281867    
    ## COUNTYBENTON          0.9881616  0.2229851   4.432 9.36e-06 ***
    ## COUNTYCLACKAMAS       0.5670213  0.2056352   2.757 0.005826 ** 
    ## COUNTYCLATSOP         0.0994824  0.2378681   0.418 0.675783    
    ## COUNTYCOLUMBIA        0.6033864  0.2313382   2.608 0.009101 ** 
    ## COUNTYCOOS            0.2161365  0.2256273   0.958 0.338095    
    ## COUNTYCROOK           0.4963659  0.2642677   1.878 0.060344 .  
    ## COUNTYCURRY           0.4998419  0.2727378   1.833 0.066850 .  
    ## COUNTYDESCHUTES       0.7583444  0.2111688   3.591 0.000329 ***
    ## COUNTYDOUGLAS         0.1077574  0.2144536   0.502 0.615334    
    ## COUNTYGILLIAM         2.1358765  1.0792081   1.979 0.047803 *  
    ## COUNTYGRANT           0.9718846  0.3900545   2.492 0.012715 *  
    ## COUNTYHARNEY          0.4167575  0.3701320   1.126 0.260178    
    ## COUNTYHOOD RIVER      0.5701428  0.2667181   2.138 0.032547 *  
    ## COUNTYJACKSON         0.4229236  0.2089356   2.024 0.042951 *  
    ## COUNTYJEFFERSON       0.0177130  0.2601388   0.068 0.945714    
    ## COUNTYJOSEPHINE       0.1632154  0.2166550   0.753 0.451244    
    ## COUNTYKLAMATH         0.3584585  0.2280973   1.572 0.116063    
    ## COUNTYLAKE            0.9718145  0.4397422   2.210 0.027108 *  
    ## COUNTYLANE            0.5171769  0.2060620   2.510 0.012080 *  
    ## COUNTYLINCOLN         0.1801754  0.2311731   0.779 0.435747    
    ## COUNTYLINN            0.2376244  0.2131340   1.115 0.264891    
    ## COUNTYMALHEUR         0.1339936  0.2555319   0.524 0.600020    
    ## COUNTYMARION          0.3467232  0.2092331   1.657 0.097496 .  
    ## COUNTYMORROW          0.2140217  0.3453327   0.620 0.535419    
    ## COUNTYMULTNOMAH       0.5707165  0.2038719   2.799 0.005120 ** 
    ## COUNTYPOLK            0.4211904  0.2220228   1.897 0.057820 .  
    ## COUNTYSHERMAN         1.9008015  1.0676439   1.780 0.075015 .  
    ## COUNTYTILLAMOOK       0.4156717  0.2529999   1.643 0.100389    
    ## COUNTYUMATILLA        0.1453237  0.2237794   0.649 0.516076    
    ## COUNTYUNION           0.4311970  0.2617832   1.647 0.099527 .  
    ## COUNTYWALLOWA         0.6633484  0.3845167   1.725 0.084501 .  
    ## COUNTYWASCO           0.1542270  0.2587251   0.596 0.551106    
    ## COUNTYWASHINGTON      0.6479295  0.2048408   3.163 0.001561 ** 
    ## COUNTYWHEELER         1.5422270  1.0597503   1.455 0.145593    
    ## COUNTYYAMHILL         0.4292760  0.2176997   1.972 0.048624 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 41443  on 42212  degrees of freedom
    ## Residual deviance: 36046  on 42174  degrees of freedom
    ## AIC: 36124
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
summary(m_age2)
```

    ## 
    ## Call:
    ## glm(formula = VOTED ~ AGE + I(AGE^2), family = binomial, data = voter_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1905   0.4384   0.4859   0.6652   1.1609  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.525e+00  8.116e-02  -18.79   <2e-16 ***
    ## AGE          9.820e-02  3.725e-03   26.37   <2e-16 ***
    ## I(AGE^2)    -6.297e-04  3.804e-05  -16.55   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 41443  on 42212  degrees of freedom
    ## Residual deviance: 38248  on 42210  degrees of freedom
    ## AIC: 38254
    ## 
    ## Number of Fisher Scoring iterations: 4

Based on this examination, we get the following results:

-   The simple age model shows an increase of the probability to vote by 3.9%

-   The age model checked for party shows an increase of 3.2%

-   The age model checked for county shows an increase of 3.3%

-   The final age model shows an increase of close to 9.8%, which diminishes at higher ages.

**Misclassification**

The first three models show the same misclassification rate, which is about the same as if our model was that everyone voted (close to .19). The final model, interestingly and probably incorrectly, shows a misclassification rate of .28, meaning that it is actually a *worse* model than the first. This should probably not happen, given that adding a quadratic part should normaly restrict the bias in the model.

**Summary**

There are two points of interest in analyzing probability to vote. First, whether we can see a trend; if, for some reason, one group or another votes more consistently, so we can later examine why this happens. Second, so that we can build a system of predicting whether or not an individual will vote, based on the information we have about them. On the first question, when examining age and party in relation to voting in Oregon, there are two clear trends. First, the probability of an individual voting increases steadily with age; in all our models the older the voter, the more likely to participate in the electoral process. Second, "mainstream" party supporters are also more likely to turn out to vote than non-affiliated or third party voters. This makes intuitive sense as well; less marginalized political groups tend to be more likely, by definition, to participate in an election. On the first question, our data shows that neither age nor party can help us build a model to predict turnout. More specifically, a model is a good predictor when it can, with less fault than if we just assume that everyone voted, give a good estimate of the umber of people that will vote. Our models with age and party do not fulfill this criterium, and therefore cannot be considered worthwhile.
