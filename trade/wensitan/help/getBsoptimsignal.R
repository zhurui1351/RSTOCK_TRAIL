getBsoptimsignal = function(pricedata,analysedata,starttrain,endtrain,updatestart,updateend)
{
  
  updateperiod = paste(updatestart,updateend,sep='/')
  analysedata_train = analysedata[updateperiod]
  
  shortstatus =tryCatch(getClosestatus(pricedata,3,updateperiod),
                      error = function(e){print('shortstatusstatusError');return(NULL)})
  if(!is.null(nrow(shortstatus)))
  {
    analysedata_train$shortstatus = shortstatus
  }
  
  longstatus =tryCatch(getClosestatus(pricedata,30,updateperiod),
                        error = function(e){print('longstatusstatusError');return(NULL)})
  if(!is.null(nrow(longstatus)))
  {
    analysedata_train$longstatus = longstatus
  }
  
  #添加最优指标信号
  smap =tryCatch(optimSMA(pricedata,analysedata,starttrain,endtrain,longpara =c(5,10,15,20,30),shortpara = c(3,5,10)),
                 error = function(e){print('smvError');return(NULL)})
  if(!is.null(nrow(smap)))
  {
    analysedata_train$smasignal = getSMAsignal(pricedata,smap$short,smap$long,updateperiod)
   # analysedata_train$smasignal = getSMAsignal(pricedata,3,10,updateperiod)
  }
  
  smastatus =tryCatch(getSMAstatus(pricedata,3,5,updateperiod),
                 error = function(e){print('smvstatusError');return(NULL)})
  if(!is.null(nrow(smastatus)))
  {
    analysedata_train$smastatus = smastatus
  }
  
  ccip = tryCatch(optimCCI(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30),uppara = seq(80,120,by=10),downpara= seq(-80 , -120,by=-10)),
                  error = function(e){print('cciError');return(NULL)})
  if(!is.null(nrow(ccip)))
  {
    analysedata_train$ccisignal = getCCIsignal(pricedata,ccip$n,ccip$up,ccip$down,updateperiod)
   # analysedata_train$ccisignal = getCCIsignal(pricedata,5,100,-100,updateperiod)
  }
  
  ccistatus = tryCatch(getCCIstatus(pricedata,5,80,-80,updateperiod),
                  error = function(e){print('ccistatusError');return(NULL)})
  if(!is.null(nrow(ccistatus)))
  {
    analysedata_train$ccistatus = ccistatus
  }
  
  rsip = tryCatch(optimRSI(pricedata,analysedata,starttrain,endtrain, npara = c(3,5,10,15,20),uppara = seq(60,90,by=10),downpara= seq(50 , 10,by=-10)),
                  error = function(e){print('rsiError');return(NULL)})
  if(!is.null(nrow(rsip))) 
  {
    analysedata_train$rsisignal = getRSIsignal(pricedata,rsip$n,rsip$up,rsip$down,updateperiod)
    
  }
  rsistatus = tryCatch(getRSIstatus(pricedata,5,80,20,updateperiod),
                  error = function(e){print('rsistatusError');return(NULL)})
  if(!is.null(nrow(rsistatus))) 
  {
    analysedata_train$rsistatus = rsistatus
    
  }
  macdp = tryCatch(optimMACD(pricedata,analysedata,starttrain,endtrain,nfastpara =c(3,5,10,15),nslowpara =c(10,15,20,25,30),nsigpara=c(5,8,10,12),seppara=seq(-20,20,5)),
                   error = function(e){print('macdError');return(NULL)})
  if(!is.null(nrow(macdp))) 
  {
    analysedata_train$macdsignal = getMACDsignal(pricedata,macdp$nfast,macdp$nslow,macdp$nsig,macdp$sep,updateperiod)
  }  
  
  
  macdstatus = tryCatch(getMACDstatus(pricedata,3,10,5,10,updateperiod),
                   error = function(e){print('macdstatusError');return(NULL)})
  if(!is.null(nrow(macdstatus))) 
  {
    analysedata_train$macdstatus = macdstatus
  }  
  
  adxp = tryCatch(optimADX(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30)),
                  error = function(e){print('adxError');return(NULL)})
  if(!is.null(nrow(adxp))) 
  {
    analysedata_train$adxsignal = getADXsignal(pricedata,adxp$n,updateperiod)
  } 
  
  adxstatus = tryCatch(getADXstatus(pricedata,10,updateperiod),
                  error = function(e){print('adxstatusError');return(NULL)})
  if(!is.null(nrow(adxstatus))) 
  {
    analysedata_train$adxstatus = adxstatus
  } 
  
  
  mfip =tryCatch(optimMFI(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30),uppara = seq(30,0,by=-10),downpara = seq(60,100,by=10)),
                 error = function(e){print('mfipError');return(NULL)})
  if(!is.null(nrow(mfip)))
  {
    analysedata_train$mfisignal = getMFIsignal(pricedata,mfip$n,mfip$up,mfip$down,updateperiod)
  }   
  
  mfistatus =tryCatch(getMFIstatus(pricedata,5,20,70,updateperiod),
                 error = function(e){print('mfistatusError');return(NULL)})
  if(!is.null(nrow(mfistatus)))
  {
    analysedata_train$mfistatus = mfistatus
  }   
  
  bbandp =tryCatch(optimBBANDS(pricedata,analysedata,starttrain,endtrain,n = c(3,5,10,15,20,30)),
                   error = function(e){print('bbandError');return(NULL)})
  if(!is.null(nrow(bbandp)))
  {
    analysedata_train$bbandssignal = getBBANDSsignal(pricedata,bbandp$n,updateperiod)
  }  
  
  bbandstatus =tryCatch(getBBANDSstatus(pricedata,10,updateperiod),
                   error = function(e){print('bbandstatusError');return(NULL)})
  if(!is.null(nrow(bbandstatus)))
  {
    analysedata_train$bbandstatus = bbandstatus
  }  
  
  rocp = tryCatch(optimROC(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30),seppara = seq(-20,20,10)),
                  error = function(e){print('rocError');return(NULL)})
  if(!is.null(nrow(rocp)))
  {
    analysedata_train$rocsignal = getROCsignal(pricedata,rocp$n,rocp$sep,updateperiod)
  }  
  
  rocstatus = tryCatch(getROCstatus(pricedata,10,10,updateperiod),
                  error = function(e){print('rocstatusError');return(NULL)})
  if(!is.null(nrow(rocstatus)))
  {
    analysedata_train$rocstatus = rocstatus
  }  
  
  sarp = tryCatch(optimSAR(pricedata,analysedata,starttrain,endtrain,a1para = seq(0.01,0.09,0.01),a2para = seq(0.1,0.5,0.1)),
                  error = function(e){print('sarError');return(NULL)})
  if(!is.null(nrow(sarp)))
  {
    analysedata_train$sarsignal = getSARsignal(pricedata,sarp$ac1,sarp$ac2,updateperiod)
  }
  
  sarstatus = tryCatch(getSARstatus(pricedata,0.05,0.3,updateperiod),
                  error = function(e){print('sarstatusError');return(NULL)})
  if(!is.null(nrow(sarstatus)))
  {
    analysedata_train$sarstatus = sarstatus
  }
  
  wprp = tryCatch(optimWPR(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20),uppara = seq(0.6,0.9,0.1),downpara = seq(0.4,0.1,-0.1)),
                  error = function(e){print('wprError');return(NULL)})
  if(!is.null(nrow(wprp))) 
  {
    analysedata_train$wprsignal = getWPRsignal(pricedata,wprp$n,wprp$up,wprp$down,updateperiod)
  }
  
  wprstatus = tryCatch(getWPRstatus(pricedata,10,0.8,0.2,updateperiod),
                  error = function(e){print('wprstatusError');return(NULL)})
  if(!is.null(nrow(wprstatus))) 
  {
    analysedata_train$wprstatus = wprstatus
  }
  
  kdjp = tryCatch(optimKDJ(pricedata,analysedata,starttrain,endtrain,nfkpara = c(5,10,15,20),nfdpara=c(3,5,10),nsdpara = c(3,5,10)),
                  error = function(e){print('kdjError');return(NULL)})
  if(!is.null(nrow(kdjp))) 
  {
    analysedata_train$kdjsignal = getKDJsignal(pricedata,kdjp$nfk,kdjp$nfd,kdjp$nsd,updateperiod)
  }
 
  kdjstatus = tryCatch(getKDJstatus(pricedata,15,5,5,updateperiod),
                  error = function(e){print('kdjstatusError');return(NULL)})
  if(!is.null(nrow(kdjstatus))) 
  {
    analysedata_train$kdjstatus = kdjstatus
  }
  
  tdip = tryCatch(optimTDI(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30),seppara =seq(-10,10,5)),
                  error = function(e){print('tdiError');return(NULL)})
  if(!is.null(nrow(tdip))) 
  {
    analysedata_train$tdisignal = getTDIsignal(pricedata,tdip$n,tdip$sep,updateperiod)
  }
  
  tdistatus = tryCatch(getTDIsignal(pricedata,10,10,updateperiod),
                  error = function(e){print('tdistatusError');return(NULL)})
  if(!is.null(nrow(tdistatus))) 
  {
    analysedata_train$tdistatus = tdistatus
  }
  
  kstp = tryCatch(optimKST(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30)),
                  error = function(e){print('kstError');return(NULL)})
  if(!is.null(nrow(kstp)))
  {
    analysedata_train$kstsignal = getKSTsignal(pricedata,kstp$n,updateperiod)
  }
  
  kststatus = tryCatch(getKSTstatus(pricedata,10,updateperiod),
                  error = function(e){print('kststatusError');return(NULL)})
  if(!is.null(nrow(kstp)))
  {
    analysedata_train$kststatus = kststatus
  }
  
  chkADp = tryCatch(optimChaikinAD(pricedata,analysedata,starttrain,endtrain),
                    error = function(e){print('chkADError');return(NULL)})
  if(!is.null(nrow(chkADp)))
  {
    analysedata_train$chkADsignal = getChaikinsignal(pricedata,updateperiod)
  }
  
  chkVostatus = tryCatch(getChaikinVostatus(pricedata,5,updateperiod),
                    error = function(e){print('chkvostatusError');return(NULL)})
  if(!is.null(nrow(chkVostatus)))
  {
    analysedata_train$chkVostatus = chkVostatus
  }
  
  
  obvp = tryCatch(optimOBV(pricedata,analysedata,starttrain,endtrain, seppara = seq(10,-10,-5)),
                  error = function(e){print('obvError');return(NULL)})
  if(!is.null(nrow(obvp)))
  {
    analysedata_train$obvsignal = getOBVsignal(pricedata,obvp$sep,updateperiod)
  }
  
  obvstatus = tryCatch(getOBVstatus(pricedata,5,updateperiod),
                  error = function(e){print('obvstatusError');return(NULL)})
  if(!is.null(nrow(obvstatus)))
  {
    analysedata_train$obvstatus = obvstatus
  }
  
  cmop = tryCatch(optimCMO(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30),n1para = c(3,5,10,15,20,30)),
                  error = function(e){print('cmoError');return(NULL)})
  if(!is.null(nrow(cmop))) 
  {
    analysedata_train$cmosignal = getCMOsignal(pricedata,cmop$n,cmop$n1,updateperiod)
  }
   
  cmostatus = tryCatch(getCMOstatus(pricedata,5,5,updateperiod),
                  error = function(e){print('cmostatusError');return(NULL)})
  if(!is.null(nrow(cmostatus))) 
  {
    analysedata_train$cmostatus = cmostatus
  }
  
  
  cmfp =tryCatch(optimCMF(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30),seppara = seq(10,-10,-5)),
                 error = function(e){print('cmfError');return(NULL)})
  if(!is.null(nrow(cmfp)))
  {
    analysedata_train$cmfsignal = getCMFsignal(pricedata,cmfp$n,cmfp$sep,updateperiod)
  }

  cmfstatus =tryCatch(getCMFstatus(pricedata,10,5,updateperiod),
                 error = function(e){print('cmfstatusError');return(NULL)})
  if(!is.null(nrow(cmfstatus)))
  {
    analysedata_train$cmfstatus = cmfstatus
  }
  
  emvp = tryCatch(optimEMV(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30),seppara = seq(10,-10,-5)),
                  error = function(e){print('emvError');return(NULL)})
  if(!is.null(nrow(emvp))) 
  {
   analysedata_train$emvsignal = getEMVsignal(pricedata,emvp$n,emvp$sep,updateperiod)
  }
  
  emvstatus = tryCatch(getEMVstatus(pricedata,10,5),
                  error = function(e){print('emvstatusError');return(NULL)})
  if(!is.null(nrow(emvstatus))) 
  {
    analysedata_train$emvsignal = emvstatus
  }
  
  trixp = tryCatch(optimTRIX(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30),sigpara = c(3,5,10)),
                   error = function(e){print('trixError');return(NULL)})
  if(!is.null(nrow(trixp)))
  {
    analysedata_train$trixsignal = getTRIXsignal(pricedata,trixp$n,trix$sig,updateperiod)
  }
  
  trixstatus = tryCatch(getTRIXstatis(pricedata,10,5),
                   error = function(e){print('trixstatusError');return(NULL)})
  if(!is.null(nrow(trixstatus)))
  {
    analysedata_train$trixstatus = trixstatus
  }
  
  
  willimadp = tryCatch(optimWilliamsAD(pricedata,analysedata,starttrain,endtrain,seppara = seq(10,-10,-5)),
                       error = function(e){print('williamError');return(NULL)})
  if(!is.null(nrow(willimadp)))
  {
    analysedata_train$willimadsignal = getWilliamADsignal(pricedata, willimadp$sep,updateperiod)
  }
    
  return(analysedata_train)
}





#只计算组合中的指标，且必须存在最优值,否则终止计算
getBsoptimsignalinvarset = function(pricedata,analysedata,starttrain,endtrain,updatestart,updateend,varset)
{
  
  updateperiod = paste(updatestart,updateend,sep='/')
  analysedata_train = analysedata[updateperiod]
  
  
  #添加最优指标信号
  if('smasignal' %in%  varset)
  {
    smap =tryCatch(optimSMA(pricedata,analysedata,starttrain,endtrain,longpara =c(5,10,15,20,30),shortpara = c(3,5,10)),
                   error = function(e){print('smvError');return(NULL)})    
    if(!is.null(nrow(smap)))
    {
      analysedata_train$smasignal = getSMAsignal(pricedata,smap$short,smap$long,updateperiod)
      # analysedata_train$smasignal = getSMAsignal(pricedata,3,10,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
 
  if('ccisignal' %in%  varset)
  {
    ccip = tryCatch(optimCCI(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30),uppara = seq(80,120,by=10),downpara= seq(-80 , -120,by=-10)),
                    error = function(e){print('cciError');return(NULL)})
    if(!is.null(nrow(ccip)))
    {
      analysedata_train$ccisignal = getCCIsignal(pricedata,ccip$n,ccip$up,ccip$down,updateperiod)
      # analysedata_train$ccisignal = getCCIsignal(pricedata,5,100,-100,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
 
  if('rsisignal' %in%  varset)
  {
    rsip = tryCatch(optimRSI(pricedata,analysedata,starttrain,endtrain, npara = c(3,5,10,15,20),uppara = seq(60,90,by=10),downpara= seq(50 , 10,by=-10)),
                    error = function(e){print('rsiError');return(NULL)})
    if(!is.null(nrow(rsip))) 
    {
      analysedata_train$rsisignal = getRSIsignal(pricedata,rsip$n,rsip$up,rsip$down,updateperiod)
      
    }
    else
    {
      return(NULL)
    }
  }
  
  if('macdsignal' %in%  varset)
  {
    macdp = tryCatch(optimMACD(pricedata,analysedata,starttrain,endtrain,nfastpara =c(3,5,10,15),nslowpara =c(15,20,25,30),nsigpara=c(5,8,10,12),seppara=seq(-20,20,5)),
                     error = function(e){print('macdError');return(NULL)})
    if(!is.null(nrow(macdp))) 
    {
      analysedata_train$macdsignal = getMACDsignal(pricedata,macdp$nfast,macdp$nslow,macdp$nsig,macdp$sep,updateperiod)
    }  
    else
    {
      return(NULL)
    }
  }
  if('adxsignal' %in%  varset)
  {
    adxp = tryCatch(optimADX(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30)),
                    error = function(e){print('adxError');return(NULL)})
    if(!is.null(nrow(adxp))) 
    {
      analysedata_train$adxsignal = getADXsignal(pricedata,adxp$n,updateperiod)
    } 
    else
    {
      return(NULL)
    }
  }
  if('mfisignal' %in%  varset)
  {
    mfip =tryCatch(optimMFI(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20,30),uppara = seq(30,0,by=-10),downpara = seq(60,100,by=10)),
                   error = function(e){print('mfipError');return(NULL)})
    if(!is.null(nrow(mfip)))
    {
      analysedata_train$mfisignal = getMFIsignal(pricedata,mfip$n,mfip$up,mfip$down,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
  if('bbandssignal' %in%  varset)
  {
    bbandp =optimBBANDS(pricedata,analysedata,starttrain,endtrain,n = c(3,5,10,15,20))
    if(!is.null(nrow(bbandp)))
    {
      analysedata_train$bbandssignal = getBBANDSsignal(pricedata,bbandp$n,updateperiod)
    }  
    else
    {
      return(NULL)
    }
  }
  if('rocsignal' %in%  varset)
  {
    rocp = optimROC(pricedata,analysedata,starttrain,endtrain,npara = c(3,5,10,15,20),seppara = seq(-20,20,10))
    if(!is.null(nrow(rocp)))
    {
      analysedata_train$rocsignal = getROCsignal(pricedata,rocp$n,rocp$sep,updateperiod)
    }  
    else
    {
      return(NULL)
    }
  }
  if('sarsignal' %in%  varset)
  {
    sarp = optimSAR(pricedata,analysedata,starttrain,endtrain,a1para = seq(0.01,0.09,0.01),a2para = seq(0.1,0.5,0.1))
    if(!is.null(nrow(sarp)))
    {
      analysedata_train$sarsignal = getSARsignal(pricedata,sarp$ac1,sarp$ac2,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
 
  if('wprsignal' %in%  varset)
  {
    wprp = optimWPR(pricedata,analysedata,starttrain,endtrain,npara = seq(3,15,2),uppara = seq(0.6,0.9,0.1),downpara = seq(0.4,0.1,-0.1))
    if(!is.null(nrow(wprp))) 
    {
      analysedata_train$wprsignal = getWPRsignal(pricedata,wprp$n,wprp$up,wprp$down,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
 
  if('kdjsignal' %in%  varset)
  {
    kdjp = optimKDJ(pricedata,analysedata,starttrain,endtrain,nfkpara = seq(5,20,2),nfdpara=seq(3,11,2),nsdpara = seq(3,11,2))
    if(!is.null(nrow(kdjp))) 
    {
      analysedata_train$kdjsignal = getKDJsignal(pricedata,kdjp$nfk,kdjp$nfd,kdjp$nsd,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
 
  if('tdisignal' %in%  varset)
  {
    tdip = optimTDI(pricedata,analysedata,starttrain,endtrain,npara = seq(3,20,2),seppara =seq(-10,10,5))
    if(!is.null(nrow(tdip))) 
    {
      analysedata_train$tdisignal = getTDIsignal(pricedata,tdip$n,tdip$sep,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
  if('kstsignal' %in%  varset)
  {
    kstp = optimKST(pricedata,analysedata,starttrain,endtrain,npara = seq(3,20,2))
    if(!is.null(nrow(kstp)))
    {
      analysedata_train$kstsignal = getKSTsignal(pricedata,kstp$n,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
  
  if('chkADsignal' %in%  varset)
  {
    chkADp = optimChaikinAD(pricedata,analysedata,starttrain,endtrain)
    if(!is.null(nrow(chkADp)))
    {
      analysedata_train$chkADsignal = getChaikinsignal(pricedata,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
 
  if('obvsignal' %in%  varset)
  {
    obvp = optimOBV(pricedata,analysedata,starttrain,endtrain, seppara = seq(10,-10,-5))
    if(!is.null(nrow(obvp)))
    {
      analysedata_train$obvsignal = getOBVsignal(pricedata,obvp$sep,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
 
  if('cmosignal' %in%  varset)
  {
    
    cmop = optimCMO(pricedata,analysedata,starttrain,endtrain,npara = seq(3,20,2),n1para = seq(3,20,2))
    if(!is.null(nrow(cmop))) 
    {
      analysedata_train$cmosignal = getCMOsignal(pricedata,cmop$n,cmop$n1,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
  if('cmfsignal' %in%  varset)
  {
    cmfp =optimCMF(pricedata,analysedata,starttrain,endtrain,npara = seq(3,21,2),seppara = seq(10,-10,-5))
    if(!is.null(nrow(cmfp)))
    {
      analysedata_train$cmfsignal = getCMFsignal(pricedata,cmfp$n,cmfp$sep,updateperiod)
    }
    else
    {
      return(NULL)
    }
    
  }
  if('emvsignal' %in%  varset)
  {
    emvp = optimEMV(pricedata,analysedata,starttrain,endtrain,npara = seq(3,21,2),seppara = seq(10,-10,-5))
    if(!is.null(nrow(emvp))) 
    {
      analysedata_train$emvsignal = getEMVsignal(pricedata,emvp$n,emvp$sep,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
  
  if('trixsignal' %in%  varset)
  {
    trixp = optimTRIX(pricedata,analysedata,starttrain,endtrain,npara = seq(3,21,2),sigpara = seq(3,10,2))
    if(!is.null(nrow(trixp)))
    {
      analysedata_train$trixsignal = getTRIXsignal(pricedata,trixp$n,trix$sig,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
  if('willimadsignal' %in%  varset)
  {
    willimadp = optimWilliamsAD(pricedata,analysedata,starttrain,endtrain,seppara = seq(10,-10,-5))
    if(!is.null(nrow(willimadp)))
    {
      analysedata_train$willimadsignal = getWilliamADsignal(pricedata, willimadp$sep,updateperiod)
    }
    else
    {
      return(NULL)
    }
  }
  
  return(analysedata_train)
}