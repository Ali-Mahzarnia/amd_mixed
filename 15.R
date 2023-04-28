options(java.parameters = "-Xmx2048m")  

library(xlsx)
library(lme4)
library(lmerTest)
library(multcomp)
library(rstatix)


ROIS_name = "ctx-rh-pericalcarine_right_to_ctx-lh-lingual_left"
Lencond =FALSE


state ='Initial'
#state ='2Year'



bundle_match_path = '/Users/ali/Desktop/Mar23/amd_mixed/bundle_analysis/new_cond_minmm_strmlines/Statistics_bundlesizelim_distance_15_affinerigid_non_inclusive_symmetric/'
ID_matcher = read.csv( paste0(bundle_match_path,state,'_', ROIS_name, '_all_bundle_order.csv' ) )




data_path = paste0(bundle_match_path ,'bundle_stats_withid/')

msr_data_amd  = read.csv( paste0(data_path,state, '_AMD_', ROIS_name  ,'_all_bundle_stats.csv' ) )
msr_data_amd$AMD = 1 

msr_data_ctrl  = read.csv( paste0(data_path,state, '_Control_', ROIS_name  ,'_all_bundle_stats.csv' ) )
msr_data_ctrl$AMD = 0











msr_data = rbind(msr_data_amd, msr_data_ctrl)




pvals= matrix(NA, dim(ID_matcher)[1], 1)
cohen= pvals*0
Fvalue= pvals*0
mean_AMD = pvals*0
mean_CTRL =  pvals*0
SE_AMD = pvals*0
SE_CTRL = pvals*0


for (bdl_num in 1:dim(ID_matcher)[1]) {
  





#bdl_num =10
#amd_bdl = ID_matcher[bdl_num,]$Paired.2.YR.AMD
#ctrl_bdl = ID_matcher[bdl_num,]$Paired.2.YR.Control

  if (state == 'Initial'){
 amd_bdl = ID_matcher[bdl_num,]$Paired.Initial.AMD
 ctrl_bdl = ID_matcher[bdl_num,]$Paired.Initial.Control
}

  if (state == '2Year'){
    amd_bdl = ID_matcher[bdl_num,]$Paired.2.YR.AMD
    ctrl_bdl = ID_matcher[bdl_num,]$Paired.2.YR.Control
  }


msr_data_amd_bdl = msr_data_amd[ msr_data_amd $Bundle.ID == amd_bdl, ]
msr_data_ctrl_bdl = msr_data_ctrl[ msr_data_ctrl $Bundle.ID == ctrl_bdl, ]
# 
# table_amd = table(msr_data_amd_bdl$Subject)/50
# table_amd = as.data.frame(t(table_amd))
# table_amd$AMD = 1
# table_ctrl = table(msr_data_ctrl_bdl$Subject)/50
# 

# table_ctrl$AMD = 0
# table = rbind(table_amd,table_ctrl )
# 
# anova(lm( Freq~AMD     ,data=table, REML = TRUE))


msr_data_bdl = rbind(msr_data_amd_bdl, msr_data_ctrl_bdl)
dim(msr_data_bdl)/50
table(msr_data_bdl$Subject )/50
#removing based on number of streamline
# table = as.data.frame(table(msr_data_bdl$Subject )/50)
# index_left_1s_qtr = table$Freq > quantile(table$Freq, 0.25)
# left_1s_qtr_subj = table$Var1[index_left_1s_qtr]
# msr_data_bdl = msr_data_bdl[msr_data_bdl$Subject %in% left_1s_qtr_subj,]
####


if (Lencond ==TRUE) {
##### lenght analysis
msr_data_bdl2 = msr_data_bdl %>%group_by(Streamlines.ID)%>% 
  dplyr::summarise(
    AMD = max(AMD),
    Len = min(Length),
    ID = min(Subject)
  )
lm <- lmer( Len~AMD+ (1|ID)     ,data=msr_data_bdl2, REML = TRUE)
}
##### lenght analysis

if (Lencond ==FALSE) { lm <- lmer( md~AMD+ (1|Subject)  + Point.ID   ,data=msr_data_bdl, REML = TRUE)}


emm = emmeans::emmeans(lm , "AMD"  ) 
emmmeans = emmeans::eff_size(emm, sigma = sigma(lm), edf = df.residual(lm))

summ_cohen = summary(emmmeans)
effectsz = summ_cohen$effect.size



a=anova(lm)
a
a$`Pr(>F)`
a$`F value`
# if ( a$`Pr(>F)`[1]<=0.05 ) cat("Bundle Top" , bdl_num   , "th is significant with pval", a$`Pr(>F)`[1]  , " \n" )
pvals[bdl_num] = a$`Pr(>F)`[1]
Fvalue[bdl_num] = a$`F value`[1]
cohen[bdl_num] = effectsz

emm = summary(emm)
mean_AMD[bdl_num] = emm[2,2]
mean_CTRL[bdl_num] = emm[1,2]
SE_AMD [bdl_num]=emm[2,3]
SE_CTRL [bdl_num]= emm[1,3]

}




pval_adj_bnf = p.adjust(pvals, method = "bonferroni")
pval_adj_fdr = p.adjust(pvals, method = "fdr")


for (i in 1:dim(pvals)[1]) {
  
  if (!is.na(pval_adj_bnf[i]) ){
    if ( pval_adj_bnf[i]<=0.05 ) 
    {cat("Bundle Top" , i   , "th is bnfrni significant with pval", pval_adj_bnf[i] , " \n" );
      cat("Means of AMD and Control with their SE are",mean_AMD[i], "(",SE_AMD[i] ,")"," and " ,mean_CTRL[i], "(",SE_CTRL[i] ,")"  , " \n" ) 
      cat("F and Cohen are",  Fvalue[i] , "  " , cohen[i] , " \n" ) ;    }
    }
  if (!is.na(pval_adj_fdr[i]) ){
    if ( pval_adj_fdr[i]<=0.05 )
    { cat("Bundle Top" , i   , "th is fdr significant with pval",  pval_adj_fdr[i]  , " \n" ); 
      cat("Means of AMD and Control with their SE are",mean_AMD[i], "(",SE_AMD[i] ,")"," and " ,mean_CTRL[i], "(",SE_CTRL[i] ,")"  , " \n" ) 
      cat("F and Cohen are",  Fvalue[i] , "  " , cohen[i] , " \n" ) ;  }
       
  }
}
cat( ROIS_name, " AT ", state, ":", "\n")




#state ='Initial'
state ='2Year'



bundle_match_path = '/Users/ali/Desktop/Mar23/amd_mixed/bundle_analysis/new_cond_minmm_strmlines/Statistics_bundlesizelim_distance_15_affinerigid_non_inclusive_symmetric/'
ID_matcher = read.csv( paste0(bundle_match_path,state,'_', ROIS_name, '_all_bundle_order.csv' ) )




data_path = paste0(bundle_match_path ,'bundle_stats_withid/')

msr_data_amd  = read.csv( paste0(data_path,state, '_AMD_', ROIS_name  ,'_all_bundle_stats.csv' ) )
msr_data_amd$AMD = 1 

msr_data_ctrl  = read.csv( paste0(data_path,state, '_Control_', ROIS_name  ,'_all_bundle_stats.csv' ) )
msr_data_ctrl$AMD = 0











msr_data = rbind(msr_data_amd, msr_data_ctrl)



pvals= matrix(NA, dim(ID_matcher)[1], 1)
cohen= pvals*0
Fvalue= pvals*0
mean_AMD = pvals*0
mean_CTRL =  pvals*0
SE_AMD = pvals*0
SE_CTRL = pvals*0

for (bdl_num in 1:dim(ID_matcher)[1]) {
  
  
  
  
  
  
  #bdl_num =10
  #amd_bdl = ID_matcher[bdl_num,]$Paired.2.YR.AMD
  #ctrl_bdl = ID_matcher[bdl_num,]$Paired.2.YR.Control
  
  if (state == 'Initial'){
    amd_bdl = ID_matcher[bdl_num,]$Paired.Initial.AMD
    ctrl_bdl = ID_matcher[bdl_num,]$Paired.Initial.Control
  }
  
  if (state == '2Year'){
    amd_bdl = ID_matcher[bdl_num,]$Paired.2.YR.AMD
    ctrl_bdl = ID_matcher[bdl_num,]$Paired.2.YR.Control
  }
  
  
  msr_data_amd_bdl = msr_data_amd[ msr_data_amd $Bundle.ID == amd_bdl, ]
  msr_data_ctrl_bdl = msr_data_ctrl[ msr_data_ctrl $Bundle.ID == ctrl_bdl, ]
  # 
  # table_amd = table(msr_data_amd_bdl$Subject)/50
  # table_amd = as.data.frame(t(table_amd))
  # table_amd$AMD = 1
  # table_ctrl = table(msr_data_ctrl_bdl$Subject)/50
  # 
  
  # table_ctrl$AMD = 0
  # table = rbind(table_amd,table_ctrl )
  # 
  # anova(lm( Freq~AMD     ,data=table, REML = TRUE))
  
  
  msr_data_bdl = rbind(msr_data_amd_bdl, msr_data_ctrl_bdl)
  dim(msr_data_bdl)/50
  table(msr_data_bdl$Subject )/50
  #removing based on number of streamline
  # table = as.data.frame(table(msr_data_bdl$Subject )/50)
  # index_left_1s_qtr = table$Freq > quantile(table$Freq, 0.25)
  # left_1s_qtr_subj = table$Var1[index_left_1s_qtr]
  # msr_data_bdl = msr_data_bdl[msr_data_bdl$Subject %in% left_1s_qtr_subj,]
  ####
  
  if (Lencond ==TRUE) {
    ##### lenght analysis
    msr_data_bdl2 = msr_data_bdl %>%group_by(Streamlines.ID)%>% 
      dplyr::summarise(
        AMD = max(AMD),
        Len = min(Length),
        ID = min(Subject)
      )
    lm <- lmer( Len~AMD+ (1|ID)     ,data=msr_data_bdl2, REML = TRUE)
  }
  ##### lenght analysis
  
  if (Lencond ==FALSE) { lm <- lmer( md~AMD+ (1|Subject)  + Point.ID   ,data=msr_data_bdl, REML = TRUE)}
  
  emm = emmeans::emmeans(lm , "AMD"  ) 
  emmmeans = emmeans::eff_size(emm, sigma = sigma(lm), edf = df.residual(lm))
  
  summ_cohen = summary(emmmeans)
  effectsz = summ_cohen$effect.size
  
  
  
  a=anova(lm)
  a
  a$`Pr(>F)`
  a$`F value`
  # if ( a$`Pr(>F)`[1]<=0.05 ) cat("Bundle Top" , bdl_num   , "th is significant with pval", a$`Pr(>F)`[1]  , " \n" )
  pvals[bdl_num] = a$`Pr(>F)`[1]
  Fvalue[bdl_num] = a$`F value`[1]
  cohen[bdl_num] = effectsz
  
  emm = summary(emm)
  mean_AMD[bdl_num] = emm[2,2]
  mean_CTRL[bdl_num] = emm[1,2]
  SE_AMD [bdl_num]=emm[2,3]
  SE_CTRL [bdl_num]= emm[1,3]
  
}




pval_adj_bnf = p.adjust(pvals, method = "bonferroni")
pval_adj_fdr = p.adjust(pvals, method = "fdr")


for (i in 1:dim(pvals)[1]) {
  if (!is.na(pval_adj_bnf[i]) ){
  if ( pval_adj_bnf[i]<=0.05 ) 
  {cat("Bundle Top" , i   , "th is bnfrni significant with pval", pval_adj_bnf[i] , " \n" );
    cat("Means of AMD and Control with their SE are",mean_AMD[i], "(",SE_AMD[i] ,")"," and " ,mean_CTRL[i], "(",SE_CTRL[i] ,")"  , " \n" ); 
    cat("F and Cohen are",  Fvalue[i] , "  " , cohen[i] , " \n" )   }
  }
  if (!is.na(pval_adj_fdr[i]) ){
    if ( pval_adj_fdr[i]<=0.05 ) 
    {cat("Bundle Top" , i   , "th is fdr significant with pval",  pval_adj_fdr[i]  , " \n" ); 
      
      cat("Means of AMD and Control with their SE are",mean_AMD[i], "(",SE_AMD[i] ,")"," and " ,mean_CTRL[i], "(",SE_CTRL[i] ,")"  , " \n" ) ;
      
      cat("F and Cohen are",  Fvalue[i] , "  " , cohen[i] , " \n" ) ;   }
 
  }
  
}
cat( ROIS_name, " AT ", state, ":", "\n")








