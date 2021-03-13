#SIMULATED COHORT
library(tableone)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(nlme)
###### Simulated data ######
   var_rand <- function(n, no_groups){ # <- for grouping
      return(c(rep("A",n/no_groups),rep("B",n/no_groups))[sample(1:n, n, replace=F)])
   }
   
   var_categories <- function(n, categories){ # <- for grouping
      return(sample(categories, n, replace=T))
   }
   
   var_binom <- function(n, proportions){ #  <- for binomial variables
      return(rbinom(n, 1, proportions))
   }

   var_beta <- function(n, min, max, side,digs=0){ #  <- for skewed variables
      if(side == "right") return(round(min+(max-min)*rbeta(n,5,2),digits=digs))
      if(side == "left") return(round(min+(max-min)*rbeta(n,2,5),digits=digs))
   }

   var_norm <- function(n, mean, sd, digs=0){ #  <- for normaldistributed data
      return(round(rnorm(n, mean, sd),digits=digs))
   }

   var_sample <- function(n, min, max){ #  <- for normaldistributed data
      return(sample(min:max, n, replace=T))
   }
   
   
###### Table 1 ###### 
   
   tbl1_func <- function(df, cont_vars, cat_vars, strat_var){
      
      quiet <- function(x) { 
         sink(tempfile()) 
         on.exit(sink()) 
         invisible(force(x)) 
      } 
      
      tbl1 <- CreateTableOne(vars = c(cont_vars,cat_vars), strata = strat_var, 
                             data = df, factorVars = cat_vars, addOverall = TRUE,
                             test = FALSE, )
      temp <- quiet(print(tbl1))
      temp <- temp[,c(2,3,1)]
      
      
      tbl1$table <- temp
      tbl1$missingdata <- round(tbl1$MetaData$percentMissing[tbl1$MetaData$percentMissing > 0],digits=1)
      tbl1$missingdata <- paste(names(tbl1$missingdata),paste0(tbl1$missingdata,sep=" %"),sep=": ",collapse=";" )
      
      return(kable(tbl1$table,format = "latex", booktabs = T, linesep = "") %>% kable_styling(latex_options = "hold_position") %>% add_footnote(paste0("Missing data: ", tbl1$missingdata), notation="none"))
   }
   
###### Table ###### 
   
   tbl_cont_func <- function(df, cont_var, strat_var = "group"){
      
      cont_vars <- colnames(df)[grepl(cont_var,colnames(df))]
      
      tbl1 <- CreateTableOne(vars = c(cont_vars), strata = strat_var, 
                             data = df, addOverall = TRUE,
                             test = FALSE)
      return(tbl1)
   }

###### Feasibility outcomes ###### 
   
   feasibility_outcome <- function(numerator, demonitator, name, requirement){
   
      prop_result <- NULL
      prop_result$mean <- numerator/demonitator
      SE <- sqrt(prop_result$mean*(1-prop_result$mean)/demonitator)
      E <- qnorm(.975)*SE
      prop_result$lcl <- prop_result$mean-E
      prop_result$ucl <- prop_result$mean+E
      prop_result$ucl[prop_result$ucl>1] <- 1
      
      prop_result$fig <- ggplot() + geom_point(aes(x=1,y=prop_result$mean)) + 
         geom_errorbar(aes(x=1,ymin=prop_result$lcl,ymax=prop_result$ucl), width=0.1) +
         ggtitle(name) + xlim(0.8,1.2) +
         annotate("rect", xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = requirement-0.01, 
                  fill = "red", color=NA, alpha = 0.1) +
         annotate("rect", xmin = -Inf,xmax = Inf,ymin = requirement-0.01, ymax = Inf,
                  fill = "green",color=NA,  alpha = 0.1) +
         scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0,1)) +
         theme_classic() + theme(axis.title.y = element_blank(), axis.title.x = element_blank(), 
                                 axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
                                 axis.line = element_blank(), plot.title.position = "panel",
                                 plot.margin = unit(c(0,0,0,0),"cm")) + coord_flip() 
      
      prop_result$name <- name
      prop_result$requirement <- requirement
      prop_result$perc_mean <- paste0(round(prop_result$mean*100,digits=0),"%")
      prop_result$perc_lcl <- paste0(round(prop_result$lcl*100,digits=0),"%")
      prop_result$perc_ucl <- paste0(round(prop_result$ucl*100,digits=0),"%")
      prop_result$perc_requirement <- paste0(round(prop_result$requirement*100,digits=0),"%")
      
      return(prop_result)
   }
   
      
###### MWU ######
   
   MWU_test <- function(df,var,digs=2){
      
      test <- wilcox.test(df[df$group == "A",grepl(var,colnames(df))],df[df$group == "B",grepl(var,colnames(df))], correct=F,conf.int=T)
      
      output <- NULL
      output$test <- test
      output$estimate <- round(test$estimate[[1]],digits=digs)
      output$p <- round(test$p.value,digits=digs*2)
      output$lcl <- round(test$conf.int[1],digits=digs)
      output$ucl <- round(test$conf.int[2],digits=digs)
      
      output$est_ci <- paste0(output$estimate, " (", output$lcl, "-", output$ucl,")")
      
      return(output)
   }
   
###### Linear regression ######
   
   lin_reg <- function(df,var,time=c(0,12)){
      
      temp <- df[,grepl(paste0(var,"|group|pt_id"),colnames(df))]
      temp1 <- temp[,grepl(paste0(paste0(paste0("_",time), collapse="|"),"|group|pt_id"),colnames(temp))]
      colnames(temp1) <- c("pt_id","group","baseline","followup")
      temp1$baseline <- as.numeric(temp1$baseline)
      temp1$followup <- as.numeric(temp1$followup)
      lin_reg <- lm(followup~group+baseline, data = temp1)
      
      output <- NULL
      lin_p <- summary(lin_reg)$coefficients
      output$lin_reg <- lin_reg
      output$lin_p <- round(lin_p[grepl("group",row.names(lin_p)),ncol(lin_p)],digits=5)
      
      if(min(temp1$baseline) > 0 & min(temp1$followup) > 0){
         temp1$log_baseline <- log10(temp1$baseline)
         temp1$log_followup <- log(temp1$followup)
         lin_reg_log <- lm(log_followup~group+log_baseline, data = temp1)
         lin_log_p <- summary(lin_reg_log)$coefficients
         output$lin_reg_log <- lin_reg_log
         output$lin_log_p <- round(lin_log_p[grepl("group",row.names(lin_log_p)),ncol(lin_log_p)],digits=5)
      }else{
         output$lin_reg_log <- NA
         output$lin_log_p <- NA
      }
      
      output$df <- temp1
      
      return(output)
   }
   
   
   
   
###### Assumption-plot ###### 
   fig_lin_assumptions <- function(input, name){
      
      #Residuals QQ
      gg_resQQ <- function(LM) { # argument: a linear model  
         y <- quantile(LM$resid[!is.na(LM$resid)], c(0.25, 0.75))
         x <- qnorm(c(0.25, 0.75))
         slope <- diff(y)/diff(x)
         int <- y[1L] - slope * x[1L]
         p <- ggplot(LM, aes(sample=.resid)) +
            stat_qq(alpha = 0.5) +
            geom_abline(slope = slope, intercept = int, color="blue")
         
         return(p)
      }
      
      var <- input$df
      a1 <- gg_resQQ(input$lin_reg) + theme_minimal() + ggtitle("Residual QQ-plot") + 
               theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                     plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      
      homogein <- data.frame(fitted=fitted(input$lin_reg), residuals=residuals(input$lin_reg))
      a2 <- ggplot(data=homogein, aes(x=fitted, y=residuals)) +
            geom_point() +
            geom_smooth(method="lm",se=F) + theme_minimal() + ggtitle("Homogeneity") +
            theme( plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      
      if(!is.na(input$lin_reg_log)){
         b1 <- gg_resQQ(input$lin_reg_log) + theme_minimal() + ggtitle("Residual QQ-plot (log)") + 
                  theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                     plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      
         homogein <- data.frame(fitted=fitted(input$lin_reg_log), residuals=residuals(input$lin_reg_log))
         b2 <- ggplot(data=homogein, aes(x=fitted, y=residuals)) +
            geom_point() +
            geom_smooth(method="lm",se=F) + theme_minimal() + ggtitle("Homogeneity (log)") +
            theme( plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                   plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      }else{
         b1 <- ggplot() + theme_minimal() + ggtitle("Residual QQ-plot (log)") +
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
         b2 <- ggplot() + theme_minimal() + ggtitle("Homogeneity (log)") + 
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  plot.margin = unit(c(0,0,0,0),"cm"), legend.position = "none")
      }
      
     return(grid.arrange(a1,a2,b1,b2,ncol=4, top=paste0("Assumptions for linear regression | ",name)))
   }
   
   #LINEAR REGRESSION
   
   
   


###### Continuous figure ###### 
   fig_num_cont <- function(df, var, x_axis, y_axis,type){
      
      temp <- df[,grepl(paste0(var,"|group|pt_id"),colnames(df))]
      temp1 <- NULL
      for(i in c(3:ncol(temp))){
         time <- strsplit(colnames(temp)[i],"_")[[1]][length(strsplit(colnames(temp)[i],"_")[[1]])]
         temp1 <- data.frame(rbind(temp1,cbind(temp$pt_id,temp$group,time,temp[[i]])))
      }
      colnames(temp1) <- c("pt_id","group","time","result")
      temp1$time <- as.numeric(temp1$time)
      temp1$result <- as.numeric(temp1$result)
      
      median_IQR <- function(x) {
         data.frame(y = median(x), # Median
                    ymin = quantile(x)[2], # 1st quartile
                    ymax = quantile(x)[4])  # 3rd quartile
      }
      
      mean_CI <- function(x) {
         data.frame(y = mean(x), # Median
                    ymin = t.test(x)$conf.int[1], # 1st quartile
                    ymax = t.test(x)$conf.int[2])  # 3rd quartile
      }
      
      if(type == "median"){
         g1 <- ggplot(data=temp1, aes(x=time, y=result, color=group)) + 
            stat_summary(fun=median, geom="point", position=position_dodge(width=0.5)) +
            stat_summary(fun=median, geom="line", position=position_dodge(width=0.5)) +
            stat_summary(fun.data=median_IQR, geom="errorbar", width=0.0, position=position_dodge(width=0.5)) +
            theme_minimal() + labs(x=x_axis,y=y_axis,title="Median (IQR)") +
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  legend.title = element_blank(), plot.margin = unit(c(0,0,0,0),"cm"),
                  legend.margin = unit(c(0,0,0,0),"cm"))
      }else{
         g1 <- ggplot(data=temp1, aes(x=time, y=result, color=group)) + 
            stat_summary(fun=mean, geom="point", position=position_dodge(width=0.5)) +
            stat_summary(fun=mean, geom="line", position=position_dodge(width=0.5)) +
            stat_summary(fun.data=mean_CI, geom="errorbar", width=0.2, position=position_dodge(width=0.5)) +
            theme_minimal() + labs(x=x_axis,y=y_axis, title="Mean (95%CI)") +
            theme(plot.title = element_text(hjust=0.5,face="bold",size=10), plot.title.position = "panel",
                  legend.title = element_blank(), plot.margin = unit(c(0,0,0,0),"cm"),
                  legend.margin = unit(c(0,0,0,0),"cm"), legend.spacing = unit(0, 'cm'))
      }
      return(g1)
   }
   

   
   
   bootstraps_mann_whitney <- function(bootstraps_number, sample_1, sample_2) {
      differences <- numeric()
      for (i in sample_1) {
         for (j in sample_2) {
            differences[1 + length(differences)] <- i - j
         }
      }
      bootstraps_statistic <- numeric(bootstraps_number)
      hodges_lehmann_estimator <- median(differences)
      sample_1_delta <- sample_1 - hodges_lehmann_estimator
      for (i in 1:bootstraps_number) {
         sample_1_bootstraps <- sample(sample_1_delta, length(sample_1_delta), 
                                       replace = TRUE)
         sample_2_bootstraps <- sample(sample_2, length(sample_2), 
                                       replace = TRUE)
         mann_whitney_test <- wilcox.test(sample_1_bootstraps, 
                                          sample_2_bootstraps,conf.int = TRUE)
         bootstraps_statistic$estimate <- mann_whitney_test$estimate
         bootstraps_statistic$lcl[i] <- mann_whitney_test$conf.int[1]
         bootstraps_statistic$ucl[i] <- mann_whitney_test$conf.int[2]
      }
      return(data.frame(bootstraps_statistic))
   }
   
   #bootstraps_number <- 100
   #sample_1 <- c(1,3,5,2,4,2,1,4,1,4,6)
   #sample_2 <- c(5,4,5,3,4,3,5,4,5,6,4)
   
   #wilcox.test(df$p_dshiy_12[df$group == "A"], df$p_dshiy_12[df$group == "B"], conf.int=T)
   #wilcox.test(df$p_dshiy_12[df$group == "A"],df$p_dshiy_12[df$group == "B"], conf.int = TRUE)
   
   #bootstrap_test_statistic <- bootstraps_mann_whitney(5000, df$p_dshiy_12[df$group == "A"], df$p_dshiy_12[df$group == "B"])
   
   #mean(test_statistic >= bootstrap_test_statistic)
   #2 * min(tail, 1 - tail)
   