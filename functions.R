#SIMULATED COHORT
library(tableone)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(nlme)
#Functions for generating data ----
   var_rand <- function(n, no_groups){ # <- for grouping
      return(c(rep("A",n/no_groups),rep("B",n/no_groups))[sample(1:n, n, replace=F)])
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
   
#Table 1 ----
   tbl1_func <- function(df, cont_vars, cat_vars,strat_var){
   
      quiet <- function(x) { 
         sink(tempfile()) 
         on.exit(sink()) 
         invisible(force(x)) 
      } 
         
      tbl1_1 <- CreateTableOne(vars = c(cont_vars,cat_vars), strata = strat_var, 
                             data = df, factorVars = cat_vars, addOverall = TRUE,
                             test = FALSE)
      quiet(tbl1_1 <- as.data.frame(print(tbl1_1, missing=TRUE)))
      tbl1_1 <- cbind(rownames(tbl1_1),tbl1_1)
      colnames(tbl1_1)[1] <- " "
      
      tbl1_2 <- CreateTableOne(vars = c(cont_vars,cat_vars), strata = strat_var, 
                             data = df, factorVars = cat_vars, addOverall = TRUE,
                             test = FALSE)
      quiet(tbl1_2 <- as.data.frame(print(tbl1_2, nonnormal = cont_vars, missing=TRUE)))
      tbl1_2 <- cbind(rownames(tbl1_2),tbl1_2)
      colnames(tbl1_2)[1] <- " "
      tbl1_2 <- tbl1_2[tbl1_2$` ` != "n",]
      tbl1_2 <- tbl1_2[!grepl(paste(c("1","2"), collapse="|"),tbl1_2$` `),]
      tbl1 <- rbind(tbl1_1,tbl1_2)
      
      
      rownames(tbl1) <- c(1:nrow(tbl1))
      
      tbl1_temp <- tbl1[tbl1$` ` == "n",]
      tbl1 <- tbl1[tbl1$` ` != "n",]
      tbl1 <- tbl1[order(tbl1$` `),]
      tbl1 <- rbind(tbl1_temp,tbl1)
            
      return(tbl1)
   }
   
#QQ-plot / Hist - in one ----
fig_qqhist <- function(df, var){
   
   g1 <- ggplot(data=df, aes_string(x=var)) + geom_histogram() + 
      theme_minimal() + ggtitle(var)
   g2 <- ggplot(data=df, aes_string(sample=var)) + geom_qq() + geom_qq_line() +
      theme_minimal() + ggtitle("QQ-plot")
   df$log_var <- log10(df[[var]])
   g3 <- ggplot(data=df, aes_string(sample="log_var")) + geom_qq() + geom_qq_line() +
      theme_minimal() + ggtitle("Log-transformed")
   
   return(grid.arrange(g1,g2, g3, ncol=3))
}

#Mixed effects model ----

lin_mixed <- function(df,var){
   

   temp <- df[,grepl(paste0(var,"|group|pt_id"),colnames(df))]
   temp1 <- NULL
   for(i in c(3:ncol(temp))){
      time <- strsplit(colnames(temp)[i],"_")[[1]][3]
      temp1 <- data.frame(rbind(temp1,cbind(temp$pt_id,temp$group,time,temp[[i]])))
   }
   colnames(temp1) <- c("pt_id","group","time","result")
   temp1$time <- as.numeric(temp1$time)
   temp1$result <- as.numeric(temp1$result)
    
   mixed_model <- lme(fixed=result~time+group+time:group, random= ~1|pt_id, na.action=na.exclude, method="REML", data = temp1)
   temp1$pred <- predict(mixed_model)
   
   output <- NULL
   output$temp1 <- temp1
   output$mixed_model <- mixed_model
   return(output)
}
