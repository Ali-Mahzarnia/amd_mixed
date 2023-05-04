options(java.parameters = "-Xmx2048m")  

library(xlsx)
library(lme4)
library(lmerTest)
library(multcomp)
library(rstatix)



bundle_match_path = '/Users/ali/Desktop/Mar23/amd_mixed/bundle_analysis/new_cond_minmm_strmlines/Statistics_bundlesizelim_distance_15_affinerigid_non_inclusive_symmetric/'

ROIS_names = c('ctx-rh-lingual_right_to_ctx-lh-lingual_left', 'ctx-rh-inferiortemporal_right_to_ctx-lh-superiortemporal_left', 'ctx-lh-lingual_left_to_right-cerebellum-cortex_right', 'ctx-rh-lingual_right_to_left-cerebellum-cortex_left')
#ROIS_name =ROIS_names[1]


results = matrix(NA, 4,10)
colnames(results) = c( "Name", "CTRL-mean", "CTRL-SE", "AMD-mean", "AMD-SE", "P_value", "F_value" , "Cohen", "winner bundle ", "alll sig bundles")
rownames(results) = c("Initial-FA", "Initial-Len", "2Year-FA" , "2Year-Len")

#state ='Initial'
#state ='2Year'
resultold = results

for (ROIS_name in ROIS_names){


  for (state in c('Initial','2Year')) {

    for (lencon in c(FALSE, TRUE)) {
      
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


 msr_data_bdl = rbind(msr_data_amd_bdl, msr_data_ctrl_bdl)
# dim(msr_data_bdl)/50
# table(msr_data_bdl$Subject )/50


if (lencon ==TRUE) {
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

if (lencon ==FALSE) { lm <- lmer( md~AMD+ (1|Subject)  + Point.ID   ,data=msr_data_bdl, REML = TRUE)}


emm = emmeans::emmeans(lm , "AMD"  ) 
emmmeans = emmeans::eff_size(emm, sigma = sigma(lm), edf = df.residual(lm))

summ_cohen = summary(emmmeans)
effectsz = summ_cohen$effect.size



a=anova(lm)
a
a$`Pr(>F)`
a$`F value`

pvals[bdl_num] = a$`Pr(>F)`[1]
Fvalue[bdl_num] = a$`F value`[1]
cohen[bdl_num] = effectsz

emm = summary(emm)
mean_AMD[bdl_num] = emm[2,2]
mean_CTRL[bdl_num] = emm[1,2]
SE_AMD [bdl_num]=emm[2,3]
SE_CTRL [bdl_num]= emm[1,3]

}




#pval_adj_bnf = p.adjust(pvals, method = "bonferroni")
pval_adj_fdr = p.adjust(pvals, method = "fdr")
winner_ind = which(pval_adj_fdr == min(pval_adj_fdr) )
winner_ind = winner_ind[1]
all_sig_ind = which(pval_adj_fdr < 0.05)

which_Row = NA

if (lencon ==FALSE & state == 'Initial') {which_Row =1}
if (lencon ==TRUE & state == 'Initial') {which_Row =2}
if (lencon ==FALSE & state == '2Year') {which_Row =3}
if (lencon ==TRUE & state == '2Year') {which_Row =4}

#"Name", "CTRL-mean", "CTRL-SE", "AMD-mean", "AMD-SE", "P_value", "F_value" , "Cohen", "winner bundle ", "alll sig bundles"
results[which_Row, 1] = ROIS_name
results[which_Row, 2] = mean_CTRL[winner_ind]
results[which_Row, 3] = SE_CTRL[winner_ind]
results[which_Row, 4] = mean_AMD[winner_ind]
results[which_Row, 5] = SE_AMD[winner_ind]
results[which_Row, 6] = pval_adj_fdr[winner_ind]
results[which_Row, 7] = Fvalue[winner_ind]
results[which_Row, 8] = cohen[winner_ind]
results[which_Row, 9] = winner_ind
results[which_Row, 10] = paste(all_sig_ind,collapse=', ')




    }
    
  }
  
  resultold = rbind(resultold ,  results)
  
}

results = resultold
results = results[-c(1,2,3,4),]

