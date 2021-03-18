df <- NULL
set.seed(730)

df$pt_id <- c(1:n)
df$group <- var_rand(n,2)

#Baseline variables ----
   df$`Gender` <- var_categories(n,c("Male","Female","Transgender","Other"))
   df$`Age` <- var_beta(n,13,17,"right")
   df$`Nationality` <- var_categories(n,c("Danish","European/American","Middle Eastern","Other"))
   df$`School` <- var_categories(n,c("Middle-school","High-school","Boarding school","No school","Other"))
   df$`Parental status` <- var_categories(n,c("Married","Divorced","Other"))
   
   
#Feasibility outcomes
   df$`Completion of follow-up` <- var_binom(n,0.8)
   df$`Compliance`[df$group == "A"] <- var_binom(n/2,0.8)

#Exploratory primary clinical outcome ----

   #DSHI-Y
   df$`DSHI-Y (baseline)` <- var_sample(n,4,400)
   df$`DSHI-Y (follow-up)` <- var_sample(n,4,400)

#Exploratory secondary clinical outcome -----

   #Kidscreen-10
   df$`Kidscreen-10 (baseline)` <- var_beta(n,-4.327,4.703,"right",digs=2)
   df$`Kidscreen-10 (follow-up)` <- var_beta(n,-4.327,4.703,"left",digs=2)
   
   df$`Kidscreen-10 T-values (baseline)` <- 
      (((df$`Kidscreen-10 (baseline)` - 1.2078) / 1.03377) * 10 + 50)
   df$`Kidscreen-10 T-values (follow-up)` <- 
      (((df$`Kidscreen-10 (follow-up)` - 1.2078) / 1.03377) * 10 + 50)

   #DASS-21
   df$`DASS-21 Depression (baseline)` <- var_sample(n, 0, 21)
   df$`DASS-21 Anxiety (baseline)` <- var_sample(n, 0, 21)
   df$`DASS-21 Stress (baseline)` <- var_sample(n, 0, 21)
   df$`DASS-21 Depression (follow-up)` <- var_sample(n, 0, 21)
   df$`DASS-21 Anxiety (follow-up)` <- var_sample(n, 0, 21)
   df$`DASS-21 Stress (follow-up)` <- var_sample(n, 0, 21)

   #Self-injury
   df$`Self-injury` <- var_categories(n,c("Yes","No"))
   
   #Sick days   
   df$`Sick days (baseline)` <- var_norm(n, 0.5, 0.25,dig=1)
   df$`Sick days (baseline)`[df$`Sick days (baseline)` < 0] <- 0
   df$`Sick days (follow-up)` <- var_norm(n, 0.5, 0.25,dig=1)
   df$`Sick days (follow-up)`[df$`Sick days (follow-up)` < 0] <- 0

#Further explorative clinical outcome ----
   
   #DERS-16
   df$`DERS-16 (baseline)` <- var_sample(n, 16, 80)
   df$`DERS-16 (follow-up)` <- var_sample(n, 16, 80)
   
   #BSL-supplement
   df$`BSL-supplement (baseline)` <- var_sample(n, 0, 44)
   df$`BSL-supplement (follow-up)` <- var_sample(n, 0, 44)
   
   #CCNES-APP (only follow-up)
   df$`CCNES-APP Distress reaction (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   df$`CCNES-APP Punitive reactions (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   df$`CCNES-APP Expressive encouragment (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   df$`CCNES-APP Emotion-focused reactions (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   df$`CCNES-APP Problem-focused reactions (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   df$`CCNES-APP Minimization reactions (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   
   #CCNES-A (only follow-up)
   df$`CCNES-A Distress reaction (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   df$`CCNES-A Punitive reactions (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   df$`CCNES-A Expressive encouragment (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   df$`CCNES-A Emotion-focused reactions (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   df$`CCNES-A Problem-focused reactions (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   df$`CCNES-A Minimization reactions (follow-up)` <- var_norm(n, 3.5, 1,dig=1)
   
   #NEQ
   df$`NEQ Freq. negative effects  ` <- var_sample(n,0,20)
   df$`NEQ Freq. negative effects from treatment` <- var_sample(n,0,20) 
   df$`NEQ Freq. negative effects from other circumstances` <- df$`NEQ Freq. negative effects  `-df$`NEQ Freq. negative effects from treatment`
   df$`NEQ Freq. Fac. 1 (Symptoms) from treatment` <- var_sample(n,0,8)
   df$`NEQ Freq. Fac. 2 (Quality) from treatment` <- var_sample(n,0,6)
   df$`NEQ Freq. Fac. 3 (Dependency) from treatment` <- var_sample(n,0,1)
   df$`NEQ Freq. Fac. 4 (Stigma) from treatment`  <- var_sample(n,0,2)
   df$`NEQ Freq. Fac. 5 (Hopelessness) from treatment`  <- var_sample(n,0,3)
   df$`NEQ Neg. impact from treatment` <- var_sample(n,0,80) 
   df$`NEQ Neg. impact Fac. 1 (Symptoms) from treatment` <- var_sample(n,0,32)
   df$`NEQ Neg. impact Fac. 2 (Quality) from treatment`  <- var_sample(n,0,24)
   df$`NEQ Neg. impact Fac. 3 (Dependency) from treatment` <- var_sample(n,0,4)
   df$`NEQ Neg. impact Fac. 4 (Stigma) from treatment` <- var_sample(n,0,8)
   df$`NEQ Neg. impact Fac. 5 (Hopelessness) from treatment` <- var_sample(n,0,12)
   
   
   #WAI-SR (week 4 and 8)
   df$`WAI-SR (4 weeks)` <- var_sample(n, 12, 84)
   df$`WAI-SR (8 weeks)` <- var_sample(n, 12, 84)
   
   #SDQ (only baseline)
   df$`SDQ Prosocial (baseline)` <- var_sample(n, 0, 10)
   df$`SDQ Emotion (baseline)` <- var_sample(n, 0, 10)
   df$`SDQ ADHD (baseline)` <- var_sample(n, 0, 10)
   df$`SDQ CD (baseline)` <- var_sample(n, 0, 10)
   df$`SDQ Companion (baseline)` <- var_sample(n, 0, 10)
   df$`SDQ Externalising (baseline)` <- df$`SDQ CD (baseline)`+df$`SDQ ADHD (baseline)`
   df$`SDQ Internalising (baseline)` <- df$`SDQ Emotion (baseline)`+df$`SDQ Companion (baseline)`
   df$`SDQ Sum (baseline)` <- df$`SDQ Externalising (baseline)`+df$`SDQ Internalising (baseline)`
   
   #ECR-RC (only baseline)
   df$`ECR-RC Anxiety (baseline)` <- var_norm(n, 3.5, 1,dig=1)
   df$`ECR-RC Avoidance (baseline)` <- var_norm(n, 3.5, 1,dig=1)
   
#Creating the df
df <- as.data.frame(df, check.names=FALSE)
