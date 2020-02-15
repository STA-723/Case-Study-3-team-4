##MAIN
##Evan Knox, Joe Mathews, and Alessandro Zito

#Unziping data files
#unzip("Harvard_CAS_1993.zip")
#unzip("Harvard_CAS_1997.zip")
#unzip("Harvard_CAS_1999.zip")
#unzip("Harvard_CAS_2001.zip")

#Reading in column names and widths from record layout files
datalabels93=read.table("Harvard_CAS_1993/DS0001/06577-0001-Record_layout.txt",skip=10)
datalabels93$width=(datalabels93$V3-datalabels93$V2)+1

#97 still isn't working for some reason -- mentions line 427, but
#no apparent problem on that line of the file.  Try nrows=400 or something
#and look for errors in formatting, I guess.
datalabels97=read.table("Harvard_CAS_1997/DS0001/03163-0001-Record_layout.txt",skip=10,nrows=426)
datalabels97$width=(datalabels97$V3-datalabels97$V2)+1
widths97=c(datalabels97$width,11,11,3,4)
labels97=c(as.character(datalabels97$V1),"DATERECD","DATESCAN","FORMCODE","IPEDS")


datalabels99=read.table("Harvard_CAS_1999/DS0001/03818-0001-Record_layout.txt",skip=10)
datalabels99$width=(datalabels99$V3-datalabels99$V2)+1

datalabels01=read.table("Harvard_CAS_2001/DS0001/04291-0001-Record_layout.txt",skip=9)
datalabels01$width=(datalabels01$V3-datalabels01$V2)+1

#Loading data (just one takes forever, apparently.  Consider making these into
#files and commiting/pushing them.)

#I think it works!  At least for this one.
data93=read.csv("Data/data93.csv")
data97=read.csv("Data/data97.csv")
data99=read.csv("Data/data99.csv")
data01=read.csv("Data/data01.csv")

#data cleaning for 97
#Need line in here to remove na's
data97=data97[complete.cases(data97),]
gender = data97$A2
year=data97$A3
housemates=as.factor(data97$A8)
greek=data97$A9
binge5=data97$C1
binge4=data97$C2
currentalc=data97$C5
lastdrink=data97$C10
lastdrink = lastdrink[(lastdrink>2)]
drinkwithin30 = data97$C13
df97=data97[,103:114]
df97[is.na(df97)]=1
df97=as.matrix(df97)

drinkingprob=vector(length=length(df97[,1]),mode="numeric")
for(i in 1:length(df97[,1])){
  drinkingprob[i]=mean(df97[i,1:12])-1
}
AA=data97$E18_A
closefriends=data97$F2
GPA=data97$F4
maritalstatus=data97$G1
latinx=data97$G2
ethnicity=data97$G3
childhoodreligion=data97$G4
hsdrinktimes=data97$G9
hsdrinknumbers=data97$G10
hsbinges=data97$G11
daddrink=data97$G14
momdrink=data97$G15
familydrinking=data97$G16
momcollege=vector(length=length(data97$COLL_ID),mode="numeric")
dadcollege=vector(length=length(data97$COLL_ID),mode="numeric")
for(i in 1:length(data97$COLL_ID)){
  if(data97$G17[i]==3 | data97$G17[i] == 4){
    momcollege[i]=1
  }
  else{
    momcollege[i]=0
  }
  if(data97$G17[i]==2 | data97$G17[i]==4){
    dadcollege[i]=1
  }
  else{
    dadcollege[i]=0
  }
}
sum((is.na(data97$G17)))

clean97=cbind(gender, year, housemates, greek, binge5, binge4, currentalc,lastdrink, drinkwithin30,drinkingprob,AA, closefriends,GPA, maritalstatus,childhoodreligion,hsdrinktimes,hsdrinknumbers,hsbinges,daddrink,momdrink,familydrinking,dadcollege,momcollege)
str(clean97)



#data cleaning for 01
# Select the following variables
subset01 = data01 %>%
  dplyr::select(
    ################ General features
    A1, # Age
    A2, # Gender
    A3, # Year in school - 1 = Freshman, 2= Sophmore, 3 = Junior, 4 = Senior, 5= 5th year, 6= Grad
    A5, # Greek member
    A7A, # live alone
    A7B, # roommate
    A7C, # live with spouse
    A7D, # live with parents
    
    ################ Alchool related variables
    C1, # Binge 5 drinks in last two weeks
    C2, # Binge 4 drinks in last two weeks
    C7, # Current alcohol use
    C10, # When last drink - need to recode it
    C11, # Alchool in last 30 days
    C12, # 30 days, how many drinks usually
    C13, # 30 days, how many times drunk
    C17A:C17L, # drinking prob: hangover - to - drinking prob: overdose - need to aggregate them 

    ################ Related Drug variables
    E17, #Attended AA meetings
    E1A:E1L, #Variables of interests - heavy drugs
    #need to combine e and f
    
    ################ Education related variables
    F1, # Education satisfaction
    F3, # Number of close friends
    F5, # GPA
    
    ################ Background variables
    G1, # Current marital status
    G2, # Spanish/Hispanic
    G4, # Religion raised
    G9:G11, # Alchol in high school
    G14:G16, # Family and alchool
    G17, # father schooling
    G18, # mother schooling
    G3A:G3E, # etnicity
    ################ Created variables
    DAYE1A:DAYE1K  ,
    
    # Weighs
    STWGT_99
  )%>%
  mutate(year = 1999)



# Merge general features

# Age gender and year in school
colnames(subset93)[c(1:3)]= c("age", "gender", "year_in_school")
colnames(subset99)[c(1:3)] = c("age", "gender", "year_in_school")
# Greek life
colnames(subset93)[5] = c("greek_member")
colnames(subset99)[4] = c("greek_member")
# Who do you live with
colnames(subset99)[5:8] = c("live_alone","live_roommate","live_spouse","live_parents")
subset93$live_alone = (subset93$A6 == 1)*1
subset93$live_roommate = (subset93$A6 %in% c(2,13,14,15, 17, 18))*1 
subset93$live_spouse = (subset93$A6 %in% c(3,5,7,8, 9, 13, 17,18,20))*1 
subset93$live_parents = (subset93$A6 %in% c(4,8,9,10, 12, 16))*1 
# Drink variables
colnames(subset93)[c(6:7)]= c("binge_5+", "binge_4+")
colnames(subset99)[c(9:10)] = c("binge_5+", "binge_4+")
# Self reported alchol use
colnames(subset93)[colnames(subset93) == "C20"] = "alchol_use"
colnames(subset99)[colnames(subset99) == "C6"]  = "alchol_use"
colnames(subset93)[colnames(subset93) == "C6"] = "last_drink"
colnames(subset99)[colnames(subset99) == "C8"]  = "last_drink"
colnames(subset93)[colnames(subset93) == "C9"] = "30_days_drunk"
colnames(subset99)[colnames(subset99) == "C11"]  = "30_days_drunk"
colnames(subset93)[colnames(subset93) == "C8"] = "30_days_drink_usually"
colnames(subset99)[colnames(subset99) == "C10"]  = "30_days_drink_usually"
colnames(subset93)[colnames(subset93) == "C7"] = "30_days_alchol"
colnames(subset99)[colnames(subset99) == "C9"]  = "30_days_alchol"

# Drinking problems
drinkprobs= c("DP_hangover", "DP_missed_class", "DP_behind_schoolwork",
              "DP_later_regret", "DP_forget_where", "DP_argue_friends", 
              "DP_unpl_sex", "DP_unprot_sex", "DP_damage_property",
              "DP_trouble_police", "DP_hurt", "DP_overdose")

colnames(subset93)[startsWith(colnames(subset93),"C17_") ] = drinkprobs
colnames(subset97)[startsWith(colnames(subset97),"C16_") ] = drinkprobs
colnames(subset99)[startsWith(colnames(subset99),"C17")] = drinkprobs

################ Related Drug variables
colnames(subset99)[colnames(subset99) == "E17"] = "AA_meeting"
colnames(subset93)[colnames(subset93) == "E12A"] = "AA_meeting"
drugs = c("Drug_marijuana", "Drug_crack", "Drug_cocaine", "Drug_barbiturates",
          "Drug_amphetamines", "Drug_tranquilizers", "Drug_heroin", 
          "Drug_other_opiates", "Drug_LSD","Drug_PCP", "Drug_ecstasy")
colnames(subset99)[startsWith(colnames(subset99),"E1")] = drugs
colnames(subset97)[startsWith(colnames(subset97),"E1_")] = drugs
colnames(subset93)[startsWith(colnames(subset93),"E1_")] = drugs[-11]
subset93$Drug_ecstasy = 0

################ Education related variables
colnames(subset93)[colnames(subset93) == "F2"] = "Educ_satisfaction"
colnames(subset99)[colnames(subset99) == "F1"] = "Educ_satisfaction"

colnames(subset93)[colnames(subset93) == "F3"] = "N_close_friends"
colnames(subset99)[colnames(subset99) == "F2"] = "N_close_friends"

colnames(subset93)[colnames(subset93) == "F5"] = "GPA"
colnames(subset99)[colnames(subset99) == "F4"] = "GPA"

################ Background variables
colnames(subset93)[colnames(subset93) == "G1"] = "Marital_status"
colnames(subset99)[colnames(subset99) == "G1"] = "Marital_status"

colnames(subset93)[colnames(subset93) == "G2"] = "Spanish"
colnames(subset99)[colnames(subset99) == "G2"] = "Spanish"

colnames(subset93)[colnames(subset93) == "G4"] = "religion"
colnames(subset99)[colnames(subset99) == "G4"] = "religion"

hghschool = c("HS_times_drank", "HS_n_drinks", "HS_drinks_5+")
colnames(subset93)[colnames(subset93) %in% c("G8","G9","G10")] = hghschool
colnames(subset99)[colnames(subset99) %in% c("G10","G11","G12")] =hghschool

colnames(subset93)[colnames(subset93) == "G14"] = "father_schooling"
colnames(subset99)[colnames(subset99) == "G18"] = "father_schooling"

colnames(subset93)[colnames(subset93) == "G15"] = "mother_schooling"
colnames(subset99)[colnames(subset99) == "G19"] = "mother_schooling"

fam_alchool = c("AlchUse_father", "AlchUse_mother", "AlchUse_famfeel")
colnames(subset93)[colnames(subset93) %in% c("G11", "G12","G13")] = fam_alchool
colnames(subset99)[colnames(subset99) %in% c("G15","G16","G17")] =fam_alchool

race = c("white", "black", "asian", "native_american", "other")
colnames(subset99)[colnames(subset99) %in% c("G3A", "G3B", "G3C", "G3D", "G3E")] = race
subset93$white = (subset93$G3==1)*1
subset93$black = (subset93$G3==2)*1
subset93$asian = (subset93$G3==3)*1
subset93$native_american = (subset93$G3==4)*1
subset93$other = (subset93$G3==5)*1

subset97$white = (subset97$G3==1)*1
subset97$black = (subset97$G3==2)*1
subset97$asian = (subset97$G3==3)*1
subset97$native_american = (subset97$G3==4)*1
subset97$other = (subset97$G3==5)*1

subset93$DAYE1K = 0

# Weights
colnames(subset93)[colnames(subset93) == "STWGT_93"] = "survey_weight"
colnames(subset99)[colnames(subset99) == "STWGT_99"] = "survey_weight"

# Merge the datasets, keep only the same columns
common_cols = Reduce(intersect, list(colnames(subset93),colnames(subset97), colnames(subset99)))
subset93 = subset93[, colnames(subset93)%in% common_cols]
subset97 = subset97[, colnames(subset97)%in% common_cols]
subset99 = subset99[, colnames(subset99)%in% common_cols]

df = rbind(subset93[,sort(names(subset93))],  subset99[,sort(names(subset99))], subset97[,sort(names(subset97))])

# Do some fixtures
#df$Spanish[df$year==1999] = df$Spanish[df$year==1999] + 1
df$gender[df$year==1999] = df$gender[df$year==1999] + 1
df$`30_days_alchol`[df$year==1993 & is.na(df$`30_days_alchol`)] = 0
df$`30_days_alchol`[df$year==1993] = df$`30_days_alchol`[df$year==1993]  + 1
df$`30_days_alchol`[df$`30_days_alchol`==1] = NA
df$`30_days_alchol` = df$`30_days_alchol`-1

df$`30_days_drink_usually`[df$year==1993 & is.na(df$`30_days_drink_usually`)] = 0
df$`30_days_drink_usually`[df$year==1993] = df$`30_days_drink_usually`[df$year==1993]  + 1
df$`30_days_drink_usually`[df$`30_days_drink_usually`==1] = NA
df$`30_days_drink_usually` = df$`30_days_drink_usually`-1
df$`30_days_drink_usually`[df$`30_days_drink_usually`==-1] = NA

df$age[df$age==26] = 25

df$AA_meeting[df$year==1997 & df$AA_meeting==2] =  0
df$AA_meeting[df$year==1993 & df$AA_meeting==2] =  1
df$AA_meeting[df$year==1993 & df$AA_meeting==3] =  1

df$AA_meeting[df$year==1993 & df$AA_meeting==3] =  1
df$white[is.na(df$white) & df$year ==1999] = 0 
df$black[is.na(df$black) & df$year ==1999] = 0 
df$native_american[is.na(df$native_american) & df$year ==1999] = 0 
df$asian[is.na(df$asian) & df$year ==1999] = 0 
df$other[is.na(df$other) & df$year ==1999] = 0 

df$Spanish[df$Spanish == 2 ] = 0

df$DP_argue_friends[df$DP_argue_friends > 3] = 3
df$DP_behind_schoolwork[df$DP_behind_schoolwork > 3] = 3
df$DP_damage_property[df$DP_damage_property > 3] = 3
df$DP_forget_where[df$DP_forget_where > 3] = 3
df$DP_hangover[df$DP_hangover > 3] = 3
df$DP_hurt[df$DP_hurt > 3] = 3
df$DP_later_regret[df$DP_later_regret > 3] = 3
df$DP_missed_class[df$DP_missed_class > 3] = 3
df$DP_overdose[df$DP_overdose > 3] = 3
df$DP_trouble_police[df$DP_trouble_police > 3] = 3
df$DP_unpl_sex[df$DP_unpl_sex > 3] = 3
df$DP_unprot_sex[df$DP_unprot_sex > 3] = 3

df$greek_member[df$greek_member==2] = 0

df$mother_schooling[df$mother_schooling!=4 & df$year!=1997]=0 
df$mother_schooling[df$mother_schooling==4]=1

df$father_schooling[df$father_schooling!=4 & df$year!=1997]=0 
df$father_schooling[df$father_schooling==4]=1

df$live_spouse[is.na(df$live_spouse)] = 0
df$live_alone[is.na(df$live_alone)] = 0
df$live_parents[is.na(df$live_parents)] = 0
df$live_roommate[is.na(df$live_roommate)] = 0

df$N_close_friends[df$year!=1993]= df$N_close_friends[df$year!=1993] +1

df$year_in_school[df$year_in_school==6]=5 
write.csv(df, "~/Dropbox/Duke/Courses_Spring2020/Cases_Studies/Case_3/Case_3/Data/data_93_97_99.csv")
