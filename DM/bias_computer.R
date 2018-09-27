getfee = function(contract_usage = 3000,actual_usage = 3500)
{
  
  bias = actual_usage - contract_usage
  bias_ratio = (actual_usage - contract_usage) / contract_usage
  
  uplimit = 0.01
  up_adjust_serive_fee = 0.75
  up_b = 0.1
  
  neglimit = -0.02
  neg_adjust_serive_fee = 0.75
  neg_b = 0.1
  
  weighted_average = 0.75
  
  if(bias_ratio > uplimit)
  {
    y = weighted_average * actual_usage + (actual_usage - contract_usage*(1+uplimit)) * up_adjust_serive_fee * up_b 
  }
  
  
  if(bias_ratio > neglimit && bias_ratio<uplimit)
  {
    y = weighted_average * actual_usage
  }
  
  if(bias_ratio < neglimit)
  {
    y = weighted_average * actual_usage + (contract_usage * (1+neglimit) - actual_usage) * neg_adjust_serive_fee * neg_b
    
  }
  return(y)
}

actual_usage = 0 : 6000
actual_fee = c()
contract_usage = 3000
for(a  in actual_usage)
{
  fee = getfee(contract_usage = contract_usage,actual_usage = a)
  actual_fee = c(actual_fee,fee)
}

plot(actual_usage,actual_fee)
