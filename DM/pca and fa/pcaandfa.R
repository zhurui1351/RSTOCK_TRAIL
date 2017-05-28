require('psych')
#fa.parallel(USJudgeRatings[,-1],fa='PC',n.iter = 100,show.legend = F,main='R')
#fa.parallel(Harman23.cor$cov,fa='PC',n.iter = 100,show.legend = F,main='R')
pc = principal(USJudgeRatings[,-1],nfactors = 2,score=T)
pc

train_scale = scale_data(train_xx[,1:10])

pc_train = principal(train_xx[,1:10],nfactors = 3,rotate = 'none',score=T)
pc_train

fa(train_xx[,1:10],nfactors = 3,rotate = 'none')

pc = principal(Harman23.cor$cov,nfactors = 2,rotate = 'varimax',score=T)
pc

rc = principal(Harman23.cor$cov,nfactors = 2,rotate = 'varimax',score=T)
rc
