library(scorecard)
library(dplyr)
data<-read.csv("credit.csv")
data %>% glimpse()

# BINNING ----

# IV (important variables) ---
iv <- 
  iv(data,y = "creditability") %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

# Exclude not important variables ---
ivars <- iv %>% 
  filter(info_value>0.02) %>% 
  select(variable) %>% 
  .[[1]] 


data.iv <- data %>% 
  select(ivars,creditability)

data.iv %>% dim()


# breaking dt into train and test ---
dt_list <- split_df(data.iv, "creditability", ratio = 0.8, seed=123)
train <- dt_list$train 
test <- dt_list$test


# woe binning ---
bins <- data.iv %>% woebin("creditability")

bins

# converting train and test into woe values
train_woe <- train %>% woebin_ply(bins) 
test_woe <- test %>% woebin_ply(bins)

library(inspectdf)
names <- train_woe %>% names()                     
names <- gsub("_woe","",names) 
names(train_woe) <- names 


names(test_woe) <- names                         


# Logistic Linear Regression Diagnostics ----
outcome <- 'creditability'
features <- train_woe %>% select(-creditability) %>% names()


#Transforming train set creditability variable to (0,1)

train_woe$creditability<-as.character(train_woe$creditability)                                                                              

train_woe$creditability[train_woe$creditability=="bad"]=0
train_woe$creditability[train_woe$creditability=="good"]=1

train_woe$creditability<-as.numeric(train_woe$creditability)

train_woe %>% glimpse()

#Transforming test set creditability variable to (0,1)

test_woe$creditability<-as.character(teste$creditability)                                                                              

test_woe$creditability[test$creditability=="bad"]=0
test_woe$creditability[test$creditability=="good"]=1

test_woe$creditability<-as.numeric(test_woe$creditability)

test_woe %>% glimpse()

#Building initial model


f <- as.formula(paste(outcome, paste(features, collapse = " + "), sep = " ~ "))
f
glm <- glm(f, data = train_woe,na.action=na.exclude)
glm %>% summary()


# Select a formula-based model by AIC
step <- glm %>% step()
step$call # copy past

glm2 <-glm(formula = creditability ~ status.of.existing.checking.account + 
             duration.in.month + credit.history + age.in.years + savings.account.and.bonds + 
             purpose + property + present.employment.since + other.installment.plans + 
             credit.amount + installment.rate.in.percentage.of.disposable.income, 
           data = train_woe, na.action = na.exclude)
glm2 %>% summary()

glm2 %>% 
  coefficients() %>% 
  as.data.frame() %>%
  rownames() %>% 
  .[-1] %>% 
  as.factor() -> all.vars
all.vars %>% length()
all.vars

# Multicollinrarity
library(highcharter)
hchart(cor(
  train_woe %>% 
    select(creditability,all.vars) %>% 
    mutate_if(is.factor,as.numeric)) %>%
    round(.,2),label = T)

# VIF - glm2
glm2 %>% vif() %>% arrange(desc(gvif)) %>% 
  filter(gvif<10) %>% 
  pull(variable) -> selected

----------------------------------------------------------------------------------------------------

# Modeling with h2o
library(h2o)
h2o.init()
train_h2o <- as.h2o(train_woe %>% select(creditability,selected)) 
test_h2o <- as.h2o(test_woe %>% select(creditability,selected))

features <- train_woe %>% select(selected) %>% 
  names()

model <- h2o.glm(
  x = features,
  y = outcome,
  training_frame = train_h2o,
  family = "binomial", 
  seed = 123,
  nfolds = 10, #Number of folds for K-fold cross-validation
  remove_collinear_columns = T, #Collinear columns can cause problems during model fitting. This option can only be used with the 'IRLSM' solver
  #balance_classes = T, 
  max_runtime_secs = 180
)

model %>% h2o.auc() %>% round(2)
#model %>% h2o.giniCoef() %>% round(2)

model %>% h2o.performance(newdata = test_h2o) %>% h2o.auc() %>% round(2)
#model %>% h2o.performance(newdata = test_h2o) %>% h2o.giniCoef() %>% round(2)


model %>% h2o.std_coef_plot()


model@model$coefficients %>% as.data.frame() %>% 
  mutate(names = rownames(model@model$coefficients %>% as.data.frame())) %>%
  `colnames<-`(c('coefficients','names')) %>% 
  select(names,coefficients) %>%
  filter(coefficients != 0) %>%
  arrange(desc(coefficients))

h2o.varimp(model) %>% as.data.frame() %>% 
  pull(percentage) %>% sum()

h2o.varimp(model) %>% as.data.frame() %>% .[.$percentage>0,] %>%
  pull(variable) -> imp.vars
imp.vars %>% length()

h2o.varimp(model) %>% as.data.frame() %>% .[.$percentage != 0,] %>%
  select(variable, percentage) %>%
  hchart("pie", hcaes(x = variable, y = percentage)) %>%
  hc_colors(colors = 'orange') %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)

model %>% h2o.performance(newdata = test_h2o) %>% 
  h2o.find_threshold_by_max_metric('f1')
pred <- model %>% h2o.predict(newdata = test_h2o) %>% as.data.frame()
pred %>% select(predict) %>% table()


# scorecard
card <- bins %>% scorecard(model@model)


# credit score, only_total_score = TRUE
train_score <- train %>% scorecard_ply(card)
test_score <- test %>% scorecard_ply(card)



# psi
psi <- perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train$creditability, test = test$creditability)
)
psi$psi  
#psi$pic  


# only_total_score = FALSE
train_score2 <- train %>% scorecard_ply(card, only_total_score=FALSE)
test_score2 <- test %>% scorecard_ply(card, only_total_score=FALSE)

# psi
psi2 <- perf_psi(
  score = list(train = train_score2, test = test_score2),
  label = list(train = train$creditability, test = test$creditability)
)
psi2$psi  
#psi2$pic  
