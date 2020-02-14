library(tidyverse)
ggplot2::theme_set(ggplot2::theme_bw())
df <- read.csv("/Users/josephmathews/Desktop/Case Studies/CaseStudy3/Case-Study-3-team-422/Data/data_93_97_99.csv")
# Function to convert Drug_ecstasy into 1's
f <- function(x) {ifelse(x == 0,1,x)}
# Response variable
D <- df %>% mutate_at(vars("Drug_ecstasy"),f) %>% select(-Drug_marijuana) %>%
  select(starts_with("Drug_")) %>% mutate_at(vars(starts_with("Drug_")), function(x){x-1}) %>% mutate(sumVar = rowSums(select(., contains("Drug_")))) %>%
  mutate(D = ifelse(sumVar > 0,1,0)) %>% select(D)

# Add response variable and drop Drug variables
df <- bind_cols(df,D) %>% select(-starts_with("Drug_"))
  

# This is how you create the CFA model
M <- ' HS_Drinking  =~ HS_drinks_5. + HS_n_drinks + HS_times_drank
       Parents_Drinkinf  =~ AlchUse_father + AlchUse_mother + AlchUse_famfeel
       DP =~ DP_argue_friends + DP_behind_schoolwork + DP_damage_property + DP_forget_where + DP_hangover +
       DP_hurt + DP_later_regret + DP_missed_class + DP_overdose + DP_trouble_police + DP_unpl_sex '

# This is how you fit CFA model
fit <- cfa(M, data=df,ordered = c("HS_drinks_5.","HS_n_drinks","HS_times_drank","AlchUse_father","AlchUse_mother","AlchUse_famfeel"))

# Creates cool plot
semPaths(fit,"std")
# Predictions
predicts <- lavPredict(fit)
# Measures
fitmeasures(fit)

df %>% ggplot(aes(x=HS_drinks_5.,fill=as.factor(D))) + geom_bar() + facet_wrap(~year)
df %>% ggplot(aes(x=HS_times_drank,fill=as.factor(D))) + geom_bar() + facet_wrap(~year)
df %>% ggplot(aes(x=HS_n_drinks,fill=as.factor(D))) + geom_bar() + facet_wrap(~year)


switch_college_alcohol <- function(variable) {
  ifelse(is.na(variable),1,variable)
}
switch_dp <- function(variable) {
  ifelse(is.na(variable),1,ifelse(variable == 3,2,variable))
}
df %>% mutate_at(vars(starts_with("DP_")),switch_dp)

df <- df %>% mutate_at(vars(X30_days_alchol,X30_days_drink_usually,X30_days_drunk),switch_college_alcohol)

which(df$X30_days_alcohol)


library(rstanarm)

model.fit <- stan_glm(D ~., family=binomial(link="logit"), data = df_logistic,
                      prior = cauchy(0,1), QR=TRUE, chains=1,iter=2000)

model.mediator <- stan_glm()
  





