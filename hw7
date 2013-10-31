######run till Line 54####################
setwd("C:/Users/Lucy/Desktop/7410")
control <- read.csv("control.csv")[-25,]
highdose <- read.csv("highdose.csv")
##r##
r <- function(snt,month){
  r= sum(snt*month)
  return(r)
}
##SWT & DWT##
swt_dwt <- function(swt,dwt,month,lam){
  sum <- 0
  for (i in 1:length(swt)) {
    total <- swt[i]+dwt[i]
    if (total <= 0){
      next
    }else{
      sum <- sum + month[i]*total*exp(-month[i]*lam)/(1-exp(-month[i]*lam))
    }
  }
  swt_dwt <- sum
  return(swt_dwt)
}

##Control group r
r_control <- r(control$dnt,control$month)+r(control$snt,control$month)
##control group##
for (i in seq(.001:1,by=.00000001)) {
  left <- swt_dwt(control$swt,control$dwt,control$month,i)
  if (left< r_control){
    print(i)
    break
  }else{
    next
  }  
}

##high dose
r_highdose <- r(highdose$dnt,highdose$month)+r(highdose$snt,highdose$month)
for (i in seq(.03:.05,by=.00000001)) {
  left <- swt_dwt(highdose$swt,highdose$dwt,highdose$month,i)
  if (left< r_highdose){
    print(i)
    break
  }else{
    next
  }
}


both <- merge(control,highdose,by="month",all.x=T,all.y=T)
both[is.na(both)] =0

##combine control and highdose
both$dnt <- both$dnt.x + both$dnt.y
both$dwt <- both$dwt.x + both$dwt.y
both$snt <- both$snt.x + both$snt.y
both$swt <- both$swt.x + both$swt.y
keep <- c("month","dnt","dwt","snt","swt")
both <- both[,keep]

##both
r_both <- r(both$dnt,both$month)+r(both$snt,both$month)
for (i in seq(.01:.05,by=.00000001)) {
  left <- swt_dwt(both$swt,both$dwt,both$month,i)
  if (left< r_both){
    print(i)
    break
  }else{
    next
  }
}
