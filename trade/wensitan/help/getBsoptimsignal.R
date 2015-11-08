getBsoptimsignal = function(pricedata,analysedata,starttrain,endtrain,updatestart,updateend)
{
  
  updateperiod = paste(updatestart,updateend,sep='/')
  analysedata_train = analysedata[updateperiod]
  
  
  #添加最优指标信号
  smap = optimSMA(pricedata,analysedata,starttrain,endtrain,longpara =c(5,10,15,20,30),shortpara = c(3,5,10))
  if(!is.null(nrow(smap)))
  {
    analysedata_train$smasignal = getSMAsignal(pricedata,smap$short,smap$long,updateperiod)
   # analysedata_train$smasignal = getSMAsignal(pricedata,3,10,updateperiod)
  }
  
  ccip = optimCCI(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30),uppara = seq(80,120,by=10),downpara= seq(-80 , -120,by=-10))
  if(!is.null(nrow(ccip)))
  {
    analysedata_train$ccisignal = getCCIsignal(pricedata,ccip$n,ccip$up,ccip$down,updateperiod)
   # analysedata_train$ccisignal = getCCIsignal(pricedata,5,100,-100,updateperiod)
  }
  
  rsip = optimRSI(pricedata,analysedata,starttrain,endtrain, npara = 3 : 15,uppara = seq(60,90,by=10),downpara= seq(50 , 10,by=-10))
  if(!is.null(nrow(rsip))) 
  {
    analysedata_train$rsisignal = getRSIsignal(pricedata,rsip$n,rsip$up,rsip$down,updateperiod)
    
  }
  
  macdp = optimMACD(pricedata,analysedata,starttrain,endtrain,nfastpara =seq(3,16,by=2),nslowpara = seq(15,30,by=2),nsigpara=seq(5,12,by=2),seppara=seq(-20,20,5))
  if(!is.null(nrow(macdp))) 
  {
    analysedata_train$macdsignal = getMACDsignal(pricedata,macdp$nfast,macdp$nslow,macdp$nsig,macdp$sep,updateperiod)
  }  
  
  adxp = optimADX(pricedata,analysedata,starttrain,endtrain,npara = seq(3,20,by=2))
  if(!is.null(nrow(adxp))) 
  {
    analysedata_train$adxsignal = getADXsignal(pricedata,adxp$n,updateperiod)
  } 
  
  mfip = optimMFI(pricedata,analysedata,starttrain,endtrain,npara = seq(3,20,by=2),uppara = seq(30,0,by=-10),downpara = seq(60,100,by=10))
  if(!is.null(nrow(mfip)))
  {
    analysedata_train$mfisignal = getMFIsignal(pricedata,mfip$n,mfip$up,mfip$down,updateperiod)
  }   
  
  bbandp =optimBBANDS(pricedata,analysedata,starttrain,endtrain,n = c(3,5,10,15,20))
  if(!is.null(nrow(bbandp)))
  {
    analysedata_train$bbandssignal = getBBANDSsignal(pricedata,bbandp$n,updateperiod)
  }  
  
  rocp = optimROC(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20),seppara = seq(-20,20,10))
  if(!is.null(nrow(rocp)))
  {
    analysedata_train$rocsignal = getROCsignal(pricedata,rocp$n,rocp$sep,updateperiod)
  }  
  
  sarp = optimSAR(pricedata,analysedata,starttrain,endtrain,a1para = seq(0.01,0.09,0.01),a2para = seq(0.1,0.5,0.1))
  if(!is.null(nrow(sarp)))
  {
    analysedata_train$sarsignal = getSARsignal(pricedata,sarp$ac1,sarp$ac2,updateperiod)
  }
  
  wprp = optimWPR(pricedata,analysedata,starttrain,endtrain,npara = seq(3,15,2),uppara = seq(0.6,0.9,0.1),downpara = seq(0.4,0.1,-0.1))
  if(!is.null(nrow(wprp))) 
  {
    analysedata_train$wprsignal = getWPRsignal(pricedata,wprp$n,wprp$up,wprp$down,updateperiod)
  }
  
  kdjp = optimKDJ(pricedata,analysedata,starttrain,endtrain,nfkpara = seq(5,20,2),nfdpara=seq(3,11,2),nsdpara = seq(3,11,2))
  if(!is.null(nrow(kdjp))) 
  {
    analysedata_train$kdjsignal = getKDJsignal(pricedata,kdjp$nfk,kdjp$nfd,kdjp$nsd,updateperiod)
  }
  
  tdip = optimTDI(pricedata,analysedata,starttrain,endtrain,npara = seq(3,20,2),seppara =seq(-10,10,5))
  if(!is.null(nrow(tdip))) 
  {
    analysedata_train$tdisignal = getTDIsignal(pricedata,tdip$n,tdip$sep,updateperiod)
  }
  
  kstp = optimKST(pricedata,analysedata,starttrain,endtrain,npara = seq(3,20,2))
  if(!is.null(nrow(kstp)))
  {
    analysedata_train$kstsignal = getKSTsignal(pricedata,kstp$n,updateperiod)
  }
  
  chkADp = optimChaikinAD(pricedata,analysedata,starttrain,endtrain)
  if(!is.null(nrow(chkADp)))
  {
    analysedata_train$chkADsignal = getChaikinsignal(pricedata,updateperiod)
  }
  
  obvp = optimOBV(pricedata,analysedata,starttrain,endtrain, seppara = seq(10,-10,-5))
  if(!is.null(nrow(obvp)))
  {
    analysedata_train$obvsignal = getOBVsignal(pricedata,obvp$sep,updateperiod)
  }
  
  cmop = optimCMO(pricedata,analysedata,starttrain,endtrain,npara = seq(3,20,2),n1para = seq(3,20,2))
  if(!is.null(nrow(cmop))) 
  {
    analysedata_train$cmosignal = getCMOsignal(pricedata,cmop$n,cmop$n1,updateperiod)
  }
   
  cmfp =optimCMF(pricedata,analysedata,starttrain,endtrain,npara = seq(3,21,2),seppara = seq(10,-10,-5))
  if(!is.null(nrow(cmfp)))
  {
    analysedata_train$cmfsignal = getCMFsignal(pricedata,cmfp$n,cmfp$sep,updateperiod)
  }

  #emvp = optimEMV(pricedata,analysedata,starttrain,endtrain,npara = seq(3,21,2),seppara = seq(10,-10,-5))
  #if(!is.null(nrow(emvp))) 
  #{
   # analysedata_train$emvsignal = getEMVsignal(pricedata,emvp$n,emvp$sep,updateperiod)
  #}
  
  trixp = optimTRIX(pricedata,analysedata,starttrain,endtrain,npara = seq(3,21,2),sigpara = seq(3,10,2))
  if(!is.null(nrow(trixp)))
  {
    analysedata_train$trixsignal = getTRIXsignal(pricedata,trixp$n,trix$sig,updateperiod)
  }
  
  willimadp = optimWilliamsAD(pricedata,analysedata,starttrain,endtrain,seppara = seq(10,-10,-5))
  if(!is.null(nrow(willimadp)))
  {
    analysedata_train$willimadsignal = getWilliamADsignal(pricedata, willimadp$sep,updateperiod)
  }
    
  
  return(analysedata_train)
}