setwd("/Users/josephmathews/Desktop/Case Studies/CaseStudy3/Case-Study-3-team-42/Data")
library(tidyverse)
df <- read_csv("data93.csv")
df <- df[,-1]


df2 <- df %>% select(A2,A3,A8,C2,C4,C6,C9,starts_with("C17_"),C20,E12A,starts_with("E1_"),
              F3,F5,G1,G2,G3,G4,G8,G9,G10,G11,G12,G13,G14,G15) %>%
              mutate_at(vars(starts_with("C17_")), function(x){x - 1}) %>%
              mutate_at(vars(starts_with("E1_")),function(x){x - 1}) %>%
              mutate(E12A_G = ifelse(E12A > 0, 1, 0),
                     G14_G = ifelse(G14 == 3 | G14 == 4, 1,0),
                     G15_G = ifelse(G15 == 3 | G15 == 4, 1,0),
                     C6_G = ifelse(C6 <= 3,0,1),
                     C17G = rowMeans(.[, 8:19]),
                     D_sum = rowMeans(.[, 23:31]),
                     Drug_Use = ifelse(D_sum > 0, 1, 0)) %>%
              select(-starts_with("E1_"),-E12A,-G14,-G15,-C6,-starts_with("C17_"),-D_sum) %>%
              as.data.frame()
              

names <- c("Gender","Year_in_school","Frat_or_soro","Binge_drink_5+","Binge_drink_4+",
           "Times_drunk","Describe_habits","Number_of_close_friends","GPA","Marital_status",
           "Hispanic","Race","Religion_raised","HS_drink_freq","HS_drink_usual_quantity",
           "HS_binge","Father_alcohol","Mother_alcohol","Family_attitude_alcohol","Attend_AA",
           "Father_education","Mother_education","Last_drink_when","Drink_problem_mean","Drug_use")
           
                              
 
colnames(df2) <- names

df_clean <- df2[complete.cases(df2),]
ggplot2::theme_set(ggplot2::theme_bw())
df_clean %>% ggplot(aes(x=Describe_habits,fill=as.factor(Drug_use))) + geom_bar()+
  facet_grid(~"Drug use over binge behavior (hich school)")+
  theme(legend.position = "bottom")+
  ylab("Frequency") +
  xlab("Binge level in high school")






























