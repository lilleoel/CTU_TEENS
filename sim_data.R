df <- NULL
set.seed(730)

#Baseline variables
   df$pt_id <- c(1:n)
   df$group <- var_rand(n,2)
   df$dem_gender <- var_categories(n,c("Male","Female","Transgender","Other"))
   df$dem_age <- var_beta(n,13,17,"right")
   df$dem_nationality <- var_categories(n,c("Danish","Western","Other"))
   df$dem_school <- var_categories(n,c("Middle-school","High-school","Boarding school","No school","Other"))
   df$dem_parents <- var_categories(n,c("Married","Divorced","Other"))
   
   
#Feasibility outcomes
   df$f_completed_questionaires <- var_binom(n,0.8)
   df$f_compliance[df$group == "A"] <- var_binom(n/2,0.8)

#Exploratory primary clinical outcome

   #DSHI-Y
   df$p_dshiy_0 <- var_sample(n,10,500)
   # df$p_dshiy_1[df$group == "A"] <- var_sample(n/2,10,500)
   # df$p_dshiy_2[df$group == "A"] <- var_sample(n/2,0,500)
   # df$p_dshiy_3[df$group == "A"] <- var_sample(n/2,0,500)
   # df$p_dshiy_4[df$group == "A"] <- var_sample(n/2,10,500)
   # df$p_dshiy_5[df$group == "A"] <- var_sample(n/2,10,500)
   # df$p_dshiy_6[df$group == "A"] <- var_sample(n/2,0,500)
   # df$p_dshiy_7[df$group == "A"] <- var_sample(n/2,0,500)
   # df$p_dshiy_8[df$group == "A"] <- var_sample(n/2,10,500)
   # df$p_dshiy_9[df$group == "A"] <- var_sample(n/2,10,500)
   # df$p_dshiy_10[df$group == "A"] <- var_sample(n/2,0,500)
   # df$p_dshiy_11[df$group == "A"] <- var_sample(n/2,0,500)
   df$p_dshiy_12 <- var_sample(n,0,500)

#Exploratory secondary clinical outcome

   #Kidscreen-10
   df$s_kidscreen10_HRQoL_0 <- var_beta(n,-4.327,4.703,"right",digs=2)
   df$s_kidscreen10_HRQoL_12 <- var_beta(n,-4.327,4.703,"left",digs=2)
   
   df$s_kidscreen10_HRQoLt_0 <- 
      (((df$s_kidscreen10_HRQoL_0 - 1.2078) / 1.03377) * 10 + 50)
   df$s_kidscreen10_HRQoLt_12 <- 
      (((df$s_kidscreen10_HRQoL_12 - 1.2078) / 1.03377) * 10 + 50)

   #DASS-21
   df$s_dass21_depression_0 <- var_sample(n, 0, 21)
   df$s_dass21_anxiety_0 <- var_sample(n, 0, 21)
   df$s_dass21_stress_0 <- var_sample(n, 0, 21)
   df$s_dass21_depression_12 <- var_sample(n, 0, 21)
   df$s_dass21_anxiety_12 <- var_sample(n, 0, 21)
   df$s_dass21_stress_12 <- var_sample(n, 0, 21)

   #Self-injury
   df$s_selfinjury_12 <- var_binom(n,0.8)
   
   #Sick days   
   df$s_sick_days_0<- var_sample(n,0,15)
   df$s_sick_days_12 <- var_sample(n,0,15)

#Further explorative clinical outcome
   
   #DERS-16
   df$e_ders16_0 <- var_sample(n, 16, 80)
   # df$e_ders16_1[df$group == "A"] <- var_sample(n/2, 16, 80)
   # df$e_ders16_2[df$group == "A"] <- var_sample(n/2, 16, 80)
   # df$e_ders16_3[df$group == "A"] <- var_sample(n/2, 16, 80)
   # df$e_ders16_4[df$group == "A"] <- var_sample(n/2, 16, 80)
   # df$e_ders16_5[df$group == "A"] <- var_sample(n/2, 16, 80)
   # df$e_ders16_6[df$group == "A"] <- var_sample(n/2, 16, 80)
   # df$e_ders16_7[df$group == "A"] <- var_sample(n/2, 16, 80)
   # df$e_ders16_8[df$group == "A"] <- var_sample(n/2, 16, 80)
   # df$e_ders16_9[df$group == "A"] <- var_sample(n/2, 16, 80)
   # df$e_ders16_10[df$group == "A"] <- var_sample(n/2, 16, 80)
   # df$e_ders16_11[df$group == "A"] <- var_sample(n/2, 16, 80)
   df$e_ders16_12 <- var_sample(n, 16, 80)
   
   #BSL-supplement
   df$e_bsl_0 <- var_sample(n, 0, 44)
   df$e_bsl_12 <- var_sample(n, 0, 44)
   
   #C-SSRS
   df$e_cssrs_0 <- var_sample(n, 0, 4)
   df$e_cssrs_12 <- var_sample(n, 0, 4)
   
   #Columbia
   df$e_columbia_0 <- var_norm(n, 2.5, 0.5,dig=1)
   # df$e_columbia_1 <- var_sample(n, 0, 5)
   # df$e_columbia_2 <- var_sample(n, 0, 5)
   # df$e_columbia_3 <- var_sample(n, 0, 5)
   # df$e_columbia_4 <- var_sample(n, 0, 5)
   # df$e_columbia_5 <- var_sample(n, 0, 5)
   # df$e_columbia_6 <- var_sample(n, 0, 5)
   # df$e_columbia_7 <- var_sample(n, 0, 5)
   # df$e_columbia_8 <- var_sample(n, 0, 5)
   # df$e_columbia_9 <- var_sample(n, 0, 5)
   # df$e_columbia_10 <- var_sample(n, 0, 5)
   # df$e_columbia_11 <- var_sample(n, 0, 5)
   df$e_columbia_12 <- var_norm(n, 2.5, 0.5,dig=1)
   
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
   df$e_waisr_0 <- var_sample(n, 12, 84)
   # df$e_waisr_1[df$group == "A"] <- var_sample(n/2, 12, 84)
   # df$e_waisr_2[df$group == "A"] <- var_sample(n/2, 12, 84)
   # df$e_waisr_3[df$group == "A"] <- var_sample(n/2, 12, 84)
   # df$e_waisr_4[df$group == "A"] <- var_sample(n/2, 12, 84)
   # df$e_waisr_5[df$group == "A"] <- var_sample(n/2, 12, 84)
   # df$e_waisr_6[df$group == "A"] <- var_sample(n/2, 12, 84)
   # df$e_waisr_7[df$group == "A"] <- var_sample(n/2, 12, 84)
   # df$e_waisr_8[df$group == "A"] <- var_sample(n/2, 12, 84)
   # df$e_waisr_9[df$group == "A"] <- var_sample(n/2, 12, 84)
   # df$e_waisr_10[df$group == "A"] <- var_sample(n/2, 12, 84)
   # df$e_waisr_11[df$group == "A"] <- var_sample(n/2, 12, 84)
   df$e_waisr_12 <- var_sample(n, 12, 84)
   
   #SDQ (only baseline)
   df$e_sdq_emotion_0 <- var_sample(n, 0, 10)
   df$e_sdq_adhd_0 <- var_sample(n, 0, 10)
   df$e_sdq_companion_0 <- var_sample(n, 0, 10)
   df$e_sdq_cd_0 <- var_sample(n, 0, 10)
   df$e_sdq_prosocial_0 <- var_sample(n, 0, 10)
   df$e_sdq_externalising_0 <- df$e_sdq_cd_0+df$e_sdq_cd_0
   df$e_sdq_interalising_0 <- df$e_sdq_emotion_0+df$e_sdq_companion_0
   df$e_sdq_sum_0 <- df$e_sdq_emotion_0+df$e_sdq_adhd_0+df$e_sdq_companion_0+df$e_sdq_cd_0
   
   #ECR-RC (only baseline)
   df$e_ecrrc_anxiety_0 <- var_norm(n, 3.5, 1,dig=1)
   df$e_ecrrc_avoidance_0 <- var_norm(n, 3.5, 1,dig=1)
   
   #Kidscreen-10 additional (only baseline)
   df$e_kidscreen10a_0 <- var_sample(n, 1, 5)
   df$e_kidscreen10a_12 <- var_sample(n, 1, 5)
   
#Creating the df
df <- as.data.frame(df)
