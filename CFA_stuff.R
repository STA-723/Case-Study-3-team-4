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
  

df <- read.csv("/Users/josephmathews/Desktop/Case Studies/CaseStudy3/Case-Study-3-team-422/Data/final_data.csv")
f1 <- function(var) {
  ifelse((var == 3  | var == 5 | var == 6 | var == 7),1,0)
}

df <- df[,-1]
df <- df %>% mutate_at(vars(AlchUse_father,AlchUse_mother),f1)
# 1. 30 days.. questions
# 2. AA_meeting
# 3. age
# 4. Alcohol Use
# 5. Family Variables
# 6. Race
# 7. Binge
# 8. DP
# 9. Education satisfaction
# 10. Father/mother schooling
# 11. Gender
# 12. GPA
# 13. Greek member
# 14/15/16 HS drinking variable
# 17. Live alone, etc.
# 18. Marital Status
# 19. n close friends
# 20. religion
# 21. Year
# 22. Year in school
#


df <- df %>% mutate_at(vars(X30_days_alchol,X30_days_drink_usually,X30_days_drunk,AlchUse_famfeel,
                            "binge_4.","binge_5.",starts_with("DP"),Educ_satisfaction,
                            HS_drinks_5.,HS_n_drinks,HS_times_drank),as.ordered)
df <- df %>% mutate_at(vars(AA_meeting,AlchUse_father,AlchUse_mother,asian,father_schooling,
                            gender,greek_member,starts_with("live_"),Marital_status,mother_schooling,
                            native_american,other,religion,Spanish,white,year,year_in_school),as.factor)

X <- df %>% select(-c("HS_drinks_5","last_drink",starts_with("DAYE1"),"D","survey_weight")) 
y <- df %>% select("D") %>% as.matrix() %>% .[,1]

y <- ifelse(y == 0, -1,1) 
da <- list("X"=X,"y"=y)
groups <- c(1,1,1,2,3,4,5,5,5,6,7,7,6,8,8,8,8,8,8,8,8,8,8,8,8,
            9,10,11,12,13,14,15,16,17,17,17,17,18,10,19,6,6,20,6,6,21,
            22)
library(gglasso)
model_lasso <- gglasso(x=da$X,y=da$y,group=groups,loss="logit")





library(tidyverse)
df %>% rename(other = other_race)
































