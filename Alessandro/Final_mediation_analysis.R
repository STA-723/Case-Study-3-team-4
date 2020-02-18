library(tidyverse)
library(lavaan)
library(mediation)
# Import the data
df <- read.csv("/Users/antonio/Dropbox/Duke/Courses_Spring2020/Cases_Studies/Case_3/Case_3/Data/final_data.csv")
df$HS_n_drinks[which(df$year == 1993)] <- df$HS_n_drinks[which(df$year == 1993)]-1

##################### Build the latent factor variables
df2 <- df %>% 
  dplyr::select(starts_with("HS_"),X30_days_alchol,X30_days_drink_usually,X30_days_drunk,binge_4.,binge_5.,starts_with("DP")) %>%
  mutate_at(vars(starts_with("HS_"),X30_days_alchol,X30_days_drink_usually,X30_days_drunk,binge_4.,binge_5.),as.ordered) %>%
  mutate_at(vars(starts_with("DP")),as.numeric)

M1 <- ' H_Binge  =~ HS_drinks_5. + HS_n_drinks + HS_times_drank'
M2 <- 'C_Binge  =~ X30_days_alchol + X30_days_drink_usually +  X30_days_drunk + binge_4. + binge_5.' 
M3 <- 'DP =~ DP_argue_friends + DP_behind_schoolwork + DP_damage_property + DP_forget_where + DP_hangover +        
              DP_hurt + DP_later_regret + DP_missed_class + DP_overdose + DP_trouble_police + DP_unpl_sex + 
              DP_unprot_sex'

fit1 <- cfa(M1, data=df2,ordered = c("HS_drinks_5.","HS_n_drinks","HS_times_drank"))
fit2 <- cfa(M2,data=df2,ordered=c("X30_days_alchol","X30_days_drink_usually","X30_days_drunk","binge_4.","binge_5."))
fit3 <- cfa(M3,data=df2)
H_binge <- lavPredict(fit1)
C_binge <- lavPredict(fit2)
DP <- lavPredict(fit3)

##################### Build the crude aggregations
df <- df %>% mutate_at(vars(X30_days_alchol,X30_days_drink_usually,X30_days_drunk,AlchUse_famfeel,
                            "binge_4.","binge_5.",Educ_satisfaction,
                            HS_drinks_5.,HS_n_drinks,HS_times_drank),as.ordered)
df <- df %>% mutate_at(vars(AA_meeting,AlchUse_father,AlchUse_mother,asian,father_schooling,
                            gender,greek_member,starts_with("live_"),Marital_status,mother_schooling,
                            native_american,other,religion,Spanish,white,year,year_in_school,alchol_use),as.factor) 

# Aggregations for binge in college, highschool, DP 
df_crude <- df %>% mutate(C_binge = ifelse(X30_days_alchol >= 5 | X30_days_drink_usually >= 6 | 
                                       X30_days_drunk >= 5 | binge_4. >= 5 |binge_5. >= 5 , 1, 0),
                          H_binge = ifelse(HS_times_drank >= 5 | HS_drinks_5. >= 5 | HS_n_drinks >= 6, 1, 0))
df_crude$Drinkprob = apply(df_crude %>% dplyr::select(starts_with("DP")),1, mean)

# Build the sensitivity analysis - 30 days for drugs
D30 <- df %>% dplyr::select(-DAYE1A) %>%
  dplyr::select(starts_with("DAYE")) %>% mutate(sumVar = rowSums(dplyr::select(., contains("DAYE")))) %>%
  mutate(D30 = ifelse(sumVar > 0,1,0)) %>% dplyr::select(D30)

######################################################### Mediation model with crude aggregations
df_crude <- df_crude %>% dplyr::select(-c(starts_with("DP"),last_drink,starts_with("DAYE"),X,
                              starts_with("HS_"),starts_with("X30"),starts_with("binge"), survey_weight))

# Model with ever tried  drugs
out.med = glm(C_binge~., data= df_crude%>% dplyr::select(-D), family="binomial")
out.model = glm(D ~ .,data=df_crude, family=binomial("logit"))
mediation_model_crude  =  mediate(out.med, out.model, treat ="H_binge", mediator =  "C_binge", robustSE = TRUE, sims = 500)
summary(mediation_model_crude)
plot(mediation_model_crude)

# Model with 30 days drugs
out.med_30_crude = glm(C_binge~., data=df_crude%>% dplyr::select(-D), family="binomial")
out.model_30_crude = glm(D30 ~ .,data=cbind(df_crude%>% dplyr::select(-D), D30), family=binomial("logit"))
mediation_model_crude_30  =  mediate(out.med_30_crude, out.model_30_crude, treat ="H_binge", mediator =  "C_binge", robustSE = TRUE, sims = 500)
summary(mediation_model_crude_30)
plot(mediation_model_crude_30)

######################################################### Mediation model with latent factors
df_latent <- df %>% dplyr::select(-c(X,starts_with("DP"),last_drink,starts_with("DAYE"),starts_with("HS_"),starts_with("X30"),
                              starts_with("binge"),survey_weight))

df_latent$H_binge <- H_binge
df_latent$C_binge <- C_binge
df_latent$DP <- DP

# Model with ever tried  drugs
model1 <- lm(C_binge ~ ., data=df_latent %>% dplyr::select(-D))
model2 <- glm(D ~.,family=binomial,data=df_latent)

mediation_model_latent_30 <- mediate(model1, model2, sims=500, treat="H_binge", mediator="C_binge",robustSE = TRUE)
summary(mediation_model_latent_30)
plot(mediation_model_latent_30)

# Model with 30 days drugs
out.med = glm(C_binge~., data=df1, family="binomial")
out.model = glm(D ~ .,data=df, family=binomial("logit"))
mediation_model  =  mediate(out.med, out.model, treat ="H_binge", mediator =  "C_binge", robustSE = TRUE, sims = 500)
summary(mediation_model)
plot(mediation_model)