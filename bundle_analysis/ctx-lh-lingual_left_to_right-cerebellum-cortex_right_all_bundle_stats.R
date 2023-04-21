options(java.parameters = "-Xmx2048m")  

library(xlsx)
library(lme4)
library(lmerTest)
library(multcomp)
library(rstatix)


data_path = '/Users/ali/Desktop/Mar23/amd_mixed/bundle_analysis/data/'

msr_data_amd  = read.csv( paste0(data_path,'2Year_AMD_ctx-lh-lingual_left_to_right-cerebellum-cortex_right_all_bundle_stats.csv' ) )
msr_data_amd$AMD = 1 
msr_data_ctrl  = read.csv( paste0(data_path,'2Year_Control_ctx-lh-lingual_left_to_right-cerebellum-cortex_right_all_bundle_stats.csv' ) )
msr_data_ctrl$AMD = 0

msr_data = rbind(msr_data_amd, msr_data_ctrl)

bundle_match_path = '/Users/ali/Desktop/Mar23/amd_mixed/bundle_analysis/id_matchers/'
ID_matcher = read.csv( paste0(bundle_match_path,'2Year_ctx-lh-lingual_left_to_right-cerebellum-cortex_right_all_bundle_order.csv' ) )

bdl_num = 4
amd_bdl = ID_matcher[bdl_num,]$Paired.2.YR.AMD
ctrl_bdl = ID_matcher[bdl_num,]$Paired.2.YR.Control


msr_data_amd_bdl = msr_data_amd[ msr_data_amd $Bundle.ID == amd_bdl, ]
msr_data_ctrl_bdl = msr_data_ctrl[ msr_data_ctrl $Bundle.ID == ctrl_bdl, ]

msr_data_bdl = rbind(msr_data_amd_bdl, msr_data_ctrl_bdl)


lm <- lmer(fa ~AMD + Point.ID  + (1|Subject) +  (1|Bundle.ID)    ,data=msr_data, REML = TRUE)

a=anova(lm)
a
a$`Pr(>F)`
#c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Stage)))
d=eta_squared(lm)
e=effectsize::cohens_f(lm, alternative='two.sided')



