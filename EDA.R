df <- read.csv("Data/final_data.csv")
ggplot2::theme_set(ggplot2::theme_bw())
df <- df %>% select(-survey_weight)

df <- df %>% mutate_at(vars(X30_days_alchol,X30_days_drink_usually,X30_days_drunk,AlchUse_famfeel,
                            "binge_4.","binge_5.",Educ_satisfaction,
                            HS_drinks_5.,HS_n_drinks,HS_times_drank),as.ordered)
df <- df %>% mutate_at(vars(AA_meeting,AlchUse_father,AlchUse_mother,asian,father_schooling,
                            gender,greek_member,starts_with("live_"),Marital_status,mother_schooling,
                            native_american,other,religion,Spanish,white,year,year_in_school,alchol_use,starts_with("DP")),as.factor) 


model <- glm(D ~. , data=df) 

create_proportion_plot <- function(tibble,variable,variable_level_length,label_name) {
  new_df <- tibble %>% 
    select(!!sym(variable),!!sym("D"))%>% 
    group_by(!!sym(variable),!!sym("D")) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    slice(seq(2,variable_level_length*2,2)) 
  names <- names(new_df)
  new_df %>% ggplot(aes(x=as.numeric(!!sym(variable)),y=freq)) + 
    geom_point(shape=17,fill="blue",size=2) + 
    geom_smooth(method='lm',se=FALSE,linetype=3,col="red") +
    xlab(sym(label_name)) +
    ylab("Proportion") +
    ggtitle(str_c(str_c("Proportion of Drug Use for", sym(label_name), sep =" "), "Variable", sep= " ")) +
    theme(plot.title = element_text(hjust = 0.5))
}

create_proportion_plot(df,"HS_n_drinks",length(levels(df$HS_n_drinks)),"Drinks in High School")
create_proportion_plot(df,"HS_drinks_5.",length(levels(df$HS_drinks_5.)),"5 or More Drinks Variable")
create_proportion_plot(df,"HS_times_drank",length(levels(df$HS_times_drank)),"Number of Times Drank")
create_proportion_plot(df,"Educ_satisfaction",length(levels(df$Educ_satisfaction)),"Education Satisfcaction")
create_proportion_plot(df,"X30_days_drunk",length(levels(df$X30_days_drunk)),"Days Drunk Within 30 Days")
create_proportion_plot(df,"X30_days_alchol",length(levels(df$X30_days_alchol)),"Occasions Consumed Alcohol")
create_proportion_plot(df,"X30_days_drink_usually",length(levels(df$X30_days_drink_usually)),"Usual Drinks Per Occasion")
create_proportion_plot(df,"AlchUse_famfeel",length(levels(df$AlchUse_famfeel)),"Family Feeling Towards Alcohol")
create_proportion_plot(df,"N_close_friends",length(levels(as.factor(df$N_close_friends))),"Number of Close Friends")


create_proportion_plot_categorical <- function(tibble,variable,label_name) {
  tibble %>% select(!!sym(variable),!!sym("D")) %>% 
    group_by(!!sym("D"),!!sym(variable)) %>% 
    summarise(n = n()) %>% 
    mutate(freq= n / sum(n)) %>% ungroup() %>%
    ggplot(aes(x=!!sym("D"),y=freq,color=!!sym(variable))) + 
    geom_point() + geom_line(aes(group = !!sym(variable))) + geom_point() +
    xlab("Drug Use") + ylab("Frequency") + 
    scale_color_discrete(sym(label_name)) +
    ggtitle(str_c(str_c("Proportion of Drug Use for", sym(label_name), sep =" "), "Variable", sep= " ")) +
    theme(plot.title = element_text(hjust = 0.5))
}

levels(df$gender) = c("Boy","Girl")
create_proportion_plot_categorical(df,"gender","Gender")

levels(df$AA_meeting) = c("No","Yes")
create_proportion_plot_categorical(df,"AA_meeting","Attend")

levels(df$year_in_school) = c("Freshman","Sophomore","Junior","Senior","5+")
create_proportion_plot_categorical(df,"year_in_school","Class")

levels(df$greek_member) = c("Yes","No")
create_proportion_plot_categorical(df,"greek_member","Frat or Soro")





coefficients <- model %>% coef() %>% as.data.frame() 

HS_Terms <- str_detect(rownames(coefficients),c("HS")) & 
  (str_detect(rownames(coefficients),c(".L$")) | str_detect(rownames(coefficients),c(".Q$")))
Form <- rep(c("Linear","Quadratic"),3)
Variable_names <- c("Five+ Drinks", "Five+ Drinks",
                        "Drinks Per Occasion","Drinks Per Occasion",
                        "Drinking Occasions", "Drinking Occasions")
Effect_sizes <- coefficients[HS_Terms,]
tibble(Variable_names,Effect_sizes,Form) %>% ggplot(aes(x=Variable_names,y=Effect_sizes,fill=Form)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip()  +
  ylab("Effect Size") + xlab("") +
  ggtitle("Effect Size High School Drinking Variables") +
  theme(plot.title = element_text(hjust = 0.5))





