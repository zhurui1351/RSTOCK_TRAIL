poLCA.postClass.C = function (prior, vp, y) 
{
  ret <- .C("postclass", as.double(t(prior)), as.double(vp$vecprobs), 
            as.integer(t(y)), as.integer(length(vp$numChoices)), 
            as.integer(dim(y)[1]), as.integer(vp$numChoices), as.integer(vp$classes), 
            posterior = double(dim(y)[1] * vp$classes))
  ret$posterior <- matrix(ret$posterior, ncol = vp$classes, 
                          byrow = TRUE)
  return(ret$posterior)
}

postclass = function()
{
  #变量数目
  citems = as.integer(length(vp$numChoices))
  #观察样本数目
  cobs = as.integer(dim(y)[1])
  #潜变量类别数目
  cclasses = as.integer(vp$classes)
  #每组变量的水平
  numChoices = as.integer(vp$numChoices)
  #每个观察值的后验概率
  posterior = double(dim(y)[1] * vp$classes)
  #每个潜类别的likelihood
  llik = numeric(500)
  
  one = 1
  
  totalChoices=0
  for (i in 1 : citems) totalChoices = totalChoices+numChoices[i]
  #为每个观察值计算潜变量的类别概率
  for (i in 1 : cobs) {
    ylik(probs,y, (int *) &one,items,numChoices,classes,llik);
    denom = 0.0;
    for (j in 1 : cclasses) denom = denom + prior[j]*llik[j];
    for (j in 1 : cclasses) {
      posterior[j]=prior[j]*llik[j]/denom;
    }
    y = y + citems; //Increment y pointer to next obs
    prior = prior + cclasses; // Increment proir pointer to next obs
    posterior = posterior + cclasses;
  }
}

ylik = function()
{
 
  #变量数目
  citems = as.integer(length(vp$numChoices))
  #观察样本数目
  cobs = as.integer(dim(y)[1])
  #潜变量类别数目
  cclasses = as.integer(vp$classes)
  #初始概率
  firstprobs = as.double(vp$vecprobs)
  
}

poLCA.probHat.C = function (rgivy, y, vp) 
{
  ret <- .C("probhat", as.integer(t(y)), as.double(t(rgivy)), 
            as.integer(length(vp$numChoices)), as.integer(dim(y)[1]), 
            as.integer(vp$numChoices), as.integer(vp$classes), ph = double(sum(vp$numChoices) * 
                                                                             vp$classes))
  return(ret$ph)
}