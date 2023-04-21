options(java.parameters = "-Xmx2048m")  

library(xlsx)
library(lme4)
library(lmerTest)
library(multcomp)
library(rstatix)


root_path = '/Users/ali/Desktop/Mar23/amd_mixed/'

amd_sheet = read.xlsx( paste0(root_path,'Whitson_info_all.xlsx' ) , sheetIndex = 1) 
amd_sheet = na.omit(select(amd_sheet, ID, AMD.status , Visit))
#amd_sheet = na.omit(amd_sheet[,1:4])




amd_sheet_v1 = filter(amd_sheet , Visit == 'initial' )
amd_sheet_v2 = filter(amd_sheet , Visit %in% c('2-year', '2-year ', '2 year') )

amd_sheet_amd = filter(amd_sheet , AMD.status == 'AMD' )
amd_sheet_ctrl = filter(amd_sheet , AMD.status == 'Control' )











msr_data  = read.xlsx( paste0(root_path,'ctx-lh-lingual_left_to_right-cerebellum-cortex_right_allsubj.xlsx' ) , sheetIndex = 1)
colnames(msr_data) = c('ID', colnames(msr_data)[2:dim(msr_data)[2]])



msr_data_amd = filter( msr_data , msr_data$ID %in% amd_sheet_amd$ID)
msr_data_ctrl = filter( msr_data , msr_data$ID %in% amd_sheet_ctrl$ID)



temp_table = as.data.frame( table(msr_data_v1$ID)  ) # table of number of streamlines
##adding AMD feature to the msr_data
msr_data$AMD=NA
#msr_data$age=NA
msr_data$num_str = NA
for (i in 1:dim(msr_data)[1] ) {
  
  temp_ID= msr_data$ID[i]
  if (sum(temp_ID==amd_sheet$ID ) >0){
  temp_amd = amd_sheet$AMD.status[temp_ID==amd_sheet$ID ]
  if (temp_amd == 'AMD') {msr_data$AMD[i]=1} else {;msr_data$AMD[i]=0}
  temp_num = temp_table [ temp_table$Var1 ==temp_ID,2  ] 
  msr_data$num_str[i] = temp_num
  #temp_age = amd_sheet$age[temp_ID==amd_sheet$ID ]
  #msr_data$age[i]=temp_age
  }
}
#msr_data = na.omit(msr_data)

msr_data_v1 = filter( msr_data , msr_data$ID %in% amd_sheet_v1$ID)
msr_data_v2 = filter( msr_data , msr_data$ID %in% amd_sheet_v2$ID)

msr_data_v1 =as.data.frame(msr_data_v1)
msr_data_v2 = as.data.frame(msr_data_v2)

#table(msr_data_v1$ID)

#AMD_sheet_num_str = amd_sheet_v1 

#index_temp = match (temp_table$Var1 ,AMD_sheet_num_str$ID )
#AMD_sheet_num_str$num_str = NA
#AMD_sheet_num_str$num_str[index_temp] = temp_table$Freq

#aggregate(fa~ID, data=msr_data_v1, FUN=function(x) c(mean=mean(x), count=length(x)))
#lm <- lm(num_str ~AMD.status    ,data=AMD_sheet_num_str)
#aggregate(num_str~AMD.status, data=AMD_sheet_num_str[-c(35,39),], FUN=function(x) c(mean=mean(x), count=length(x)))


aggregate(fa~AMD, data=msr_data_v1, FUN=function(x) c(mean=mean(x), count=length(x)))



lm <- lmer(num ~AMD   +(1|ID)   ,data=msr_data_v1, REML = TRUE)
#lm <- lmer(fa ~AMD +(1|ID) + (0+AMD|ID)   ,data=msr_data_v1, REML = TRUE)
#lm <- lmer(fa ~AMD +(1+AMD|ID)   , data=msr_data_v1, REML = TRUE)

a=anova(lm)
a
a$`Pr(>F)`[3]
#c=summary(glht(lm_winding_trials, emm(pairwise ~ agecat|Stage)))
d=eta_squared(lm)
e=effectsize::cohens_f(lm, alternative='two.sided')



