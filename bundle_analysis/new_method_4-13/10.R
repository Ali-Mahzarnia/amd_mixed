options(java.parameters = "-Xmx2048m")  

library(xlsx)
library(lme4)
library(lmerTest)
library(multcomp)
library(rstatix)


data_path = '/Users/ali/Desktop/Mar23/amd_mixed/bundle_analysis/new_method_4-13/6/Statistics_bundlesizelim_distance_6_affinerigid_non_inclusive_symmetric/bundle_stats_withid/'

msr_data_amd  = read.csv( paste0(data_path,'2Year_AMD_ctx-rh-lingual_right_to_ctx-lh-lingual_left_all_bundle_stats.csv' ) )
msr_data_amd$AMD = 1 

msr_data_ctrl  = read.csv( paste0(data_path,'2Year_Control_ctx-rh-lingual_right_to_ctx-lh-lingual_left_all_bundle_stats.csv' ) )
msr_data_ctrl$AMD = 0





# table_amd = table(msr_data_amd$Subject)/50
# table_amd = as.data.frame(t(table_amd))
# table_amd$AMD = 1
# table_ctrl = table(msr_data_ctrl$Subject)/50
# table_ctrl = as.data.frame(t(table_ctrl))
# table_ctrl$AMD = 0
# table = rbind(table_amd,table_ctrl )
# 
# anova(lm( Freq~AMD     ,data=table, REML = TRUE))
# 
# 
# 





msr_data = rbind(msr_data_amd, msr_data_ctrl)

bundle_match_path = '/Users/ali/Desktop/Mar23/amd_mixed/bundle_analysis/new_method_4-13/6/Statistics_bundlesizelim_distance_6_affinerigid_non_inclusive_symmetric/'
ID_matcher = read.csv( paste0(bundle_match_path,'2Year_ctx-rh-lingual_right_to_ctx-lh-lingual_left_all_bundle_order.csv' ) )

bdl_num =2
amd_bdl = ID_matcher[bdl_num,]$Paired.2.YR.AMD
ctrl_bdl = ID_matcher[bdl_num,]$Paired.2.YR.Control

# amd_bdl = ID_matcher[bdl_num,]$Paired.Initial.AMD
# ctrl_bdl = ID_matcher[bdl_num,]$Paired.Initial.Control


msr_data_amd_bdl = msr_data_amd[ msr_data_amd $Bundle.ID == amd_bdl, ]
msr_data_ctrl_bdl = msr_data_ctrl[ msr_data_ctrl $Bundle.ID == ctrl_bdl, ]
# 
# table_amd = table(msr_data_amd_bdl$Subject)/50
# table_amd = as.data.frame(t(table_amd))
# table_amd$AMD = 1
# table_ctrl = table(msr_data_ctrl_bdl$Subject)/50
# table_ctrl = as.data.frame(t(table_ctrl))
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



lm <- lmer( fa~AMD+ (1|Subject)  + Point.ID   ,data=msr_data_bdl, REML = TRUE)

a=anova(lm)
a
a$`Pr(>F)`
#c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Stage)))
d=eta_squared(lm)
e=effectsize::cohens_f(lm, alternative='two.sided')



