# Import the data
data01 =  read_csv(paste(data_dir, "data97.csv", sep = ""))[,-1]
subset01 = data01 %>%
  dplyr::select(
    ################ General features
    age = A1, # Age
    gender= A2, # Gender
    year_in_school = A3, # Year in school - 1 = Freshman, 2= Sophmore, 3 = Junior, 4 = Senior, 5= 5th year, 6= Grad
    live_alone = A7A,
    live_roommate = A7B,
    live_spouse = A7C,
    live_parents = A7D,
    greek_member = A5, # Greek member
    ################ Alchool related variables
    "binge_5+" = C1, # Binge 5 drinks in last two weeks
    "binge_4+" = C2, # Binge 4 drinks in last two weeks
    last_drink = C10, # When last drink - need to recode it
    C16_A:C16_L,
    alchol_use = C7, # Current alcohol use
    "30_days_drunk" = C13,
    "30_days_drink_usually" = C12,
    "30_days_alchol" = C11,
    ################ Related Drug variables
    AA_meeting = E18, #Attended AA meetings
    E1_A:E1_L, #Variables of interests - heavy drugs
    
    
    ################ Education related variables
    Educ_satisfaction = F1, # Education satisfaction ????
    N_close_friends = F3, # Number of close friends
    GPA = F5, # GPA
    
    ################ Background variables
    Marital_status = G1, # Current marital status
    Spanish = G2, 
    G3, # etnicity - need to fix it
    religion = G4, # Religion raised
    "HS_times_drank" = G9,
    "HS_n_drinks" = G10,
    "HS_drinks_5+" = G11,
    "AlchUse_father" = G14,
    "AlchUse_mother" = G15,
    "AlchUse_famfeel" = G16,
    "fatherschooling" = G17,
    "motherschooling" = G18,
    
    
    ################ Created variables
    DAYE1A:DAYE1L, 
    
    # Weights
    survey_weight = STWGT_01
  ) %>%
  mutate(year = 2001)

subset01$opioid = max(subset01$E1E, subset01$E1F)
subset01$DAYEopioid = max(subset01$DAYE1E,subset01$DAYE1F)