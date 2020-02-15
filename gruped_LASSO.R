library(tidyverse)
library(gglasso)
df <- read.csv("/Users/antonio/Dropbox/Duke/Courses_Spring2020/Cases_Studies/Case_3/Case_3/Data/final_data.csv")
f1 <- function(var) {
  ifelse((var == 3  | var == 5 | var == 6 | var == 7),1,0)
}

df <- df[,-1]
df <- df %>% mutate_at(vars(AlchUse_father,AlchUse_mother, alchol_use),f1)
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
                            native_american,other,religion,Spanish,white,year,year_in_school,alchol_use),as.factor) %>%
  rename(other_race = other ,age_individ= age)

X <- df %>% dplyr::select(-c("HS_drinks_5","last_drink",starts_with("DAYE1"),"D","survey_weight")) 

# Create y and recode it
y <- df %>% dplyr::select("D")# %>% as.matrix() %>% .[,1]
y <- ifelse(y == 0, -1,1) 
# Model matrix
M = model.matrix(D~., data=cbind(X,y))[,-1]
#da <- list("X"=X,"y"=y)
groups <- c(1,1,1,2,3,4,5,5,5,6,7,7,6,8,8,8,8,8,8,8,8,8,8,8,8,
            9,10,11,12,13,14,15,16,17,17,17,17,18,10,19,6,6,20,6,6,21,
            22)
columns_contrast =colnames(M)
varnames = colnames(X)
groups_contrast = rep(0, ncol(M))
for(n in 1:length(varnames)){
  for(m in 1:length(columns_contrast)){
    if(grepl(varnames[n], columns_contrast[m], fixed=TRUE)){
      print(paste(varnames[n], columns_contrast[m], sep = " - "))
      groups_contrast[m] = groups[n]
    }
  }
}

da <- list("X"=M,"y"=y)
model_lasso <- gglasso(x=da$X,y=da$y,group=groups_contrast,loss="logit")