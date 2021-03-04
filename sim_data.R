df <- NULL

#Baseline variables
   df$pt_id <- c(1:n)
   df$group <- var_rand(n,2)
   df$gender <- var_binom(n,0.5)
   df$age <- var_beta(n,13,18,"right")

#Feasibility outcomes
   df$f_completed_questionaires <- var_binom(n,0.8)
   df$f_compliance[df$group == "A"] <- var_binom(n/2,0.8)

#Exploratory primary clinical outcome

   #DSHI-Y
   df$p_dshiy_0 <- var_sample(n,10,500)
   df$p_dshiy_1 <- var_sample(n,10,500)
   df$p_dshiy_2 <- var_sample(n,0,500)
   df$p_dshiy_3 <- var_sample(n,0,500)
   df$p_dshiy_4 <- var_sample(n,10,500)
   df$p_dshiy_5 <- var_sample(n,10,500)
   df$p_dshiy_6 <- var_sample(n,0,500)
   df$p_dshiy_7 <- var_sample(n,0,500)
   df$p_dshiy_8 <- var_sample(n,10,500)
   df$p_dshiy_9 <- var_sample(n,10,500)
   df$p_dshiy_10 <- var_sample(n,0,500)
   df$p_dshiy_11 <- var_sample(n,0,500)
   df$p_dshiy_12 <- var_sample(n,0,500)

#Exploratory secondary clinical outcome

   #Kidscreen-10
   df$s_kidscreen10_0 <- var_beta(n,10,50,"right")
   df$s_kidscreen10_12 <- var_beta(n,10,50,"left")

   #DASS-21
   df$s_dass21_d_0 <- var_sample(n, 0, 21)
   df$s_dass21_a_0 <- var_sample(n, 0, 21)
   df$s_dass21_s_0 <- var_sample(n, 0, 21)
   df$s_dass21_d_12 <- var_sample(n, 0, 21)
   df$s_dass21_a_12 <- var_sample(n, 0, 21)
   df$s_dass21_s_12 <- var_sample(n, 0, 21)

   #Self-injury
   df$s_selfinjury_12 <- var_binom(n,0.8)
   
   #Sick days   
   df$s_sick_days_12 <- var_sample(n,0,15)

#Further explorative clinical outcome
   
   #DERS-16
   df$e_ders16_0 <- var_sample(n, 16, 80)
   df$e_ders16_1 <- var_sample(n, 16, 80)
   df$e_ders16_2 <- var_sample(n, 16, 80)
   df$e_ders16_3 <- var_sample(n, 16, 80)
   df$e_ders16_4 <- var_sample(n, 16, 80)
   df$e_ders16_5 <- var_sample(n, 16, 80)
   df$e_ders16_6 <- var_sample(n, 16, 80)
   df$e_ders16_7 <- var_sample(n, 16, 80)
   df$e_ders16_8 <- var_sample(n, 16, 80)
   df$e_ders16_9 <- var_sample(n, 16, 80)
   df$e_ders16_10 <- var_sample(n, 16, 80)
   df$e_ders16_11 <- var_sample(n, 16, 80)
   df$e_ders16_12 <- var_sample(n, 16, 80)
   
   #BSL-supplement
   df$e_bsl_0 <- var_sample(n, 0, 40)
   df$e_bsl_12 <- var_sample(n, 0, 40)
   
   #C-SSRS
   df$e_cssrs_0 <- var_sample(n, 0, 4)
   df$e_cssrs_12 <- var_sample(n, 0, 4)
   
   #Columbia
   df$e_columbia_0 <- var_sample(n, 0, 5)
   df$e_columbia_1 <- var_sample(n, 0, 5)
   df$e_columbia_2 <- var_sample(n, 0, 5)
   df$e_columbia_3 <- var_sample(n, 0, 5)
   df$e_columbia_4 <- var_sample(n, 0, 5)
   df$e_columbia_5 <- var_sample(n, 0, 5)
   df$e_columbia_6 <- var_sample(n, 0, 5)
   df$e_columbia_7 <- var_sample(n, 0, 5)
   df$e_columbia_8 <- var_sample(n, 0, 5)
   df$e_columbia_9 <- var_sample(n, 0, 5)
   df$e_columbia_10 <- var_sample(n, 0, 5)
   df$e_columbia_11 <- var_sample(n, 0, 5)
   df$e_columbia_12 <- var_sample(n, 0, 5)
   
   #CCNES-APP (only follow-up)
   df$e_ccnesapp_distress_12 <- var_norm(n, 3.5, 1,dig=1)
   df$e_ccnesapp_punitive_12 <- var_norm(n, 3.5, 1,dig=1)
   df$e_ccnesapp_expressive_12 <- var_norm(n, 3.5, 1,dig=1)
   df$e_ccnesapp_emotion_12 <- var_norm(n, 3.5, 1,dig=1)
   df$e_ccnesapp_problem_12 <- var_norm(n, 3.5, 1,dig=1)
   df$e_ccnesapp_minimization_12 <- var_norm(n, 3.5, 1,dig=1)
   
   #CCNES-A (only follow-up)
   df$e_ccnesa_distress_12 <- var_norm(n, 3.5, 1,dig=1)
   df$e_ccnesa_punitive_12 <- var_norm(n, 3.5, 1,dig=1)
   df$e_ccnesa_expressive_12 <- var_norm(n, 3.5, 1,dig=1)
   df$e_ccnesa_emotion_12 <- var_norm(n, 3.5, 1,dig=1)
   df$e_ccnesa_problem_12 <- var_norm(n, 3.5, 1,dig=1)
   df$e_ccnesa_minimization_12 <- var_norm(n, 3.5, 1,dig=1)
   
   #NEQ
   
   #WAI-SR (week 4 and 8)
   df$e_waisr_4 <- var_sample(n, 12, 84)
   df$e_waisr_8 <- var_sample(n, 12, 84)
   
   #SDQ (only baseline)
   df$e_sdq_emotion_0 <- var_sample(n, 0, 10)
   df$e_sdq_adhd_0 <- var_sample(n, 0, 10)
   df$e_sdq_kamrat_0 <- var_sample(n, 0, 10)
   df$e_sdq_cd_0 <- var_sample(n, 0, 10)
   df$e_sdq_prosocial_0 <- var_sample(n, 0, 10)
   df$e_sdq_externalising_0 <- df$e_sdq_cd_0+df$e_sdq_cd_0
   df$e_sdq_interlising_0 <- df$e_sdq_emotion_0+df$e_sdq_kamrat_0
   df$e_sdq_0 <- df$e_sdq_emotion_0+df$e_sdq_adhd_0+df$e_sdq_kamrat_0+df$e_sdq_cd_0
   
   #ECR-RC (only baseline)
   df$e_ecrrc_0 <- var_sample(n, 12, 84)
   
   #Kidscreen-10 additional (only baseline)
   df$e_kidscreen10a_0 <- var_sample(n, 1, 5)
   df$e_kidscreen10a_12 <- var_sample(n, 1, 5)
   
#Creating the df
df <- as.data.frame(df)
