options(java.parameters = "-Xmx2048m")  

library(xlsx)
library(lme4)
library(lmerTest)
library(multcomp)
library(rstatix)


results = matrix(NA, 4,10)
colnames(results) = c("CTRL-mean", "CTRL-SE", "AMD-mean", "AMD-SE", "F-value", "Cohen", "Pvalue", "CI-AMD_0-1", "CI-AMD_0-1", "Name")
rownames(results) = c("Initial-FA", "Initial-Len", "2Year-FA" , "2Year-Len")
ROIS_names = c('ctx-rh-lingual_right_to_ctx-lh-lingual_left', 'ctx-rh-inferiortemporal_right_to_ctx-lh-superiortemporal_left', 'ctx-lh-lingual_left_to_right-cerebellum-cortex_right', 'ctx-rh-lingual_right_to_left-cerebellum-cortex_left')
#ROIS_name = ROIS_names[1]
resultold = results
for (ROIS_name in ROIS_names){

#resultold = results


for (state in c('Initial','2Year')) {
  

#state ='Initial'
#state ='2Year'




bundle_match_path = '/Users/ali/Desktop/Mar23/amd_mixed/bundle_analysis/new_cond_minmm_strmlines/Statistics_bundlesizelim_distance_15_affinerigid_non_inclusive_symmetric/'
ID_matcher = read.csv( paste0(bundle_match_path,state,'_', ROIS_name, '_all_bundle_order.csv' ) )

data_path = paste0(bundle_match_path ,'bundle_stats_withid/')

msr_data_ctrl  = read.csv( paste0(data_path,state, '_Control_', ROIS_name  ,'_all_bundle_stats.csv' ) )
msr_data_ctrl$AMD = 0
msr_data_ctrl$Bundle_mtch = msr_data_ctrl$Bundle.ID


msr_data_amd  = read.csv( paste0(data_path,state, '_AMD_', ROIS_name  ,'_all_bundle_stats.csv' ) )
msr_data_amd$AMD = 1 

Bundle_mtch = msr_data_amd$Bundle.ID

for (i in 1:dim(ID_matcher)[1]) {
  Bundle_mtch[msr_data_amd$Bundle.ID==ID_matcher[i,3]] = ID_matcher[i,2]
}

msr_data_amd$Bundle_mtch=Bundle_mtch

msr_data = rbind(msr_data_amd, msr_data_ctrl)



lm <- lmer( fa~AMD+ (1|Subject)  + Point.ID +Bundle_mtch  ,data=msr_data, REML = TRUE)

emm = emmeans::emmeans(lm , "AMD"  ) 
emmmeans = emmeans::eff_size(emm, sigma = sigma(lm), edf = df.residual(lm))

summ_cohen = summary(emmmeans)
effectsz = summ_cohen$effect.size



a=anova(lm)
a
a$`Pr(>F)`
a$`F value`


emm = summary(emm)
if(state == "Initial"){
results[1,1] = emm[1,2]
results[1,2] = emm[1,3]
results[1,3] = emm[2,2]
results[1,4] = emm[2,3]
results[1,5] = a$`F value`[1]
results[1,6] = effectsz
results[1,7] = a$`Pr(>F)`[1]
results[1,8] = summ_cohen$asymp.LCL
results[1,9] = summ_cohen$asymp.UCL
results[1,10] = ROIS_name

}

if(state == "2Year"){
  results[3,1] = emm[1,2]
  results[3,2] = emm[1,3]
  results[3,3] = emm[2,2]
  results[3,4] = emm[2,3]
  results[3,5] = a$`F value`[1]
  results[3,6] = effectsz
  results[3,7] = a$`Pr(>F)`[1]
  results[3,8] = summ_cohen$asymp.LCL
  results[3,9] = summ_cohen$asymp.UCL
  results[3,10] = ROIS_name
  
}



##### lenght analysis
msr_data_bdl2 = msr_data %>%group_by(Streamlines.ID)%>% 
  dplyr::summarise(
    AMD = max(AMD),
    Len = min(Length),
    Bundle_mtch = min(Bundle_mtch),
    ID = min(Subject)
  )
lm <- lmer( Len~AMD+ (1|ID) + Bundle_mtch    ,data=msr_data_bdl2, REML = TRUE)
##### lenght analysis


emm = emmeans::emmeans(lm , "AMD"  ) 
emmmeans = emmeans::eff_size(emm, sigma = sigma(lm), edf = df.residual(lm))

summ_cohen = summary(emmmeans)
effectsz = summ_cohen$effect.size

a=anova(lm)
a
a$`Pr(>F)`
a$`F value`

emm = summary(emm)

if(state == "Initial"){
  results[2,1] = emm[1,2]
  results[2,2] = emm[1,3]
  results[2,3] = emm[2,2]
  results[2,4] = emm[2,3]
  results[2,5] = a$`F value`[1]
  results[2,6] = effectsz
  results[2,7] = a$`Pr(>F)`[1]
  results[2,8] = as.numeric(summ_cohen[5])
  results[2,9] = as.numeric(summ_cohen[6])
  results[2,10] = ROIS_name
  
  
}

if(state == "2Year"){
  results[4,1] = emm[1,2]
  results[4,2] = emm[1,3]
  results[4,3] = emm[2,2]
  results[4,4] = emm[2,3]
  results[4,5] = a$`F value`[1]
  results[4,6] = effectsz
  results[4,7] = a$`Pr(>F)`[1]
  results[4,8] = as.numeric(summ_cohen[5])
  results[4,9] = as.numeric(summ_cohen[6])
  results[4,10] = ROIS_name
  
}

}


  
resultold = rbind(resultold ,  results)

}
results = resultold
results = results[-c(1,2,3,4),]

mypath = '/Users/ali/Desktop/Mar23/amd_mixed/bundle_analysis/new_cond_minmm_strmlines/'

write.xlsx2(as.data.frame(results), paste0(mypath,'table3.xlsx') ,append = TRUE)
