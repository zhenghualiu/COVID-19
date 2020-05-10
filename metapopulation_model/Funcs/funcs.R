library(tibble)
library(Hmisc)
get_exp_r <- function(y)
{
  y <- sort(y)
  y.diff <- diff(y)
  loc  = which(y.diff == max(y.diff))
  y <- y[1:(loc+1)]
  M <- which(y==0)
  if(length(M) != 0) {y <- y[-M]}
  N <- length(y)
  x <- seq(1,N)
  res.fit <- NULL
  if(length(y) < 8) return(list(res=c(NA,NA,NA),y=y,pre=rep(NA,length(y))))
  try(res.fit <- lm(log(y)~x))
  if(length(res.fit) ==0) {return(list(res=c(NA,NA,NA),y=y,pre=rep(NA,length(y))))}
  temp <- summary(res.fit)
  if(nrow(temp$coefficients) !=2) {return(list(res=c(NA,NA,NA),y=y,pre=rep(NA,length(y))))}else{
  return(list(res=c(res.fit$coefficients["x"],temp$coefficients[2,4],
           temp$adj.r.squared),y=y,
           pre = exp(res.fit$fitted.values)
           ) )}
}

get_exp_r1 <- function(y,x,st=10,ed=10,m=1)
{
  xy.new <- list(y=y,x=y)
  try(xy.new <- detect_mis_values(y,x,m)) 
#  xy.new <- get_xy(y,x,m)
  y <- xy.new$y
  x <- xy.new$x
  r.sq <- NULL;Coef <- NULL;p.value <- NULL
  y.pre <- list()
  if(length(y) < st+1) return(NA)
  for(i in 1:ed)
  {
    Coef.temp <- NA;r.sq.temp <- NA;p.value.temp <- NA;y.pre.temp <- NA
    if(length(y) < st+i) break;
    y.temp <- y[1:(st+i)]
    x.temp <- x[1:(st+i)]
    try(res.fit <- lm(log(y.temp)~x.temp))
    fit.temp <- summary(res.fit)
    try(Coef.temp <-fit.temp$coefficients[2,1])
    try(p.value.temp <- fit.temp$coefficients[2,4])
    try(r.sq.temp <- fit.temp$adj.r.squared)
    try(y.pre.temp <- res.fit$fitted.values)
    r.sq <- c(r.sq,r.sq.temp);Coef <- c(Coef,Coef.temp)
    p.value <- c(p.value,p.value.temp)
    y.pre[[i]] <- cbind.data.frame(Day=x.temp,
                                   Pre=y.pre.temp)
  }
  res <- cbind.data.frame(Coef=Coef,p.value=p.value,adj.rsq=r.sq)
  if(length(y.pre)!=0 ){names(y.pre) <- seq(1,nrow(res))}
  return(as_tibble(list(fit.res=res,fit.value=y.pre)))
}


get_exp_r2 <- function(y,x,st=10,ed=10,m=1)
{
  xy.new <- detect_mis_values(y,x,m) #
#  xy.new <- get_xy(y,x,m)
  y <- xy.new$y
  x <- xy.new$x
  r.sq <- NULL;Coef <- NULL;p.value <- NULL
  y.pre <- list()
  if(length(y) < st+1) return(NA)
  for(i in 1:ed)
  {
    Coef.temp <- NA;r.sq.temp <- NA;p.value.temp <- NA;y.pre.temp <- NA
    if(length(y) < st+i) break;
    y.temp <- y[1:(st+i)]
    #N <- length(y.temp)
    #x.temp <- seq(1,N)
    rs.logit <-  get_logistic_xy(y.temp)
    try(res.fit <- lm(rs.logit$y~rs.logit$x))
    fit.temp <- summary(res.fit)
    try(Coef.temp <-fit.temp$coefficients[1,1])
    try(p.value.temp <- fit.temp$coefficients[1,4])
    try(r.sq.temp <- fit.temp$adj.r.squared)
    try(y.pre.temp <- y2(res.fit$fitted.values,rs.logit$x0))
    r.sq <- c(r.sq,r.sq.temp);Coef <- c(Coef,Coef.temp)
    p.value <- c(p.value,p.value.temp)
    y.pre[[i]] <- cbind.data.frame(Day=seq(1,length(y.pre.temp)),
                                   Pre=y.pre.temp)
  }
  res <- cbind.data.frame(Coef=Coef,p.value=p.value,adj.rsq=r.sq)
  names(y.pre) <- seq(1,nrow(res))
  return(as_tibble(list(fit.res=res,fit.value=y.pre)))
}

get_exp_r3 <- function(y,x,st=10,ed=10,
                       beta=0.4,gamma=1/7, kappainv=1/20000)
{
  params = c('beta'=beta,'gamma'=gamma, 'kappainv'=kappainv)
  loc <- which(y==0)
  if(length(loc) != 0) {y  <- y[-loc];x <- x[-loc]}
  R0 <- NULL;Coef <- NULL;Beta <- NULL;Gamma <- NULL
  y.pre <- list()
  if(length(y) < st) return(NA)
  for(i in 1:ed)
  {
    Coef.temp <- NA;R0.temp <- NA;Gamma.temp <- NA;Beta.temp <- NA
    if(length(y) < st+i) break;
    y.temp <- y[1:(st+i)]
    x.temp <- x[1:(st+i)]
    times=x.temp
    data=y.temp
    res = optim(params,fn=SIRML,times=times,data=data)
    paramests=abs(res$par)
    xest = ode(x0fun(data,paramests), times, SIRode, paramests, method='ode45')
    Beta.temp = paramests['beta']
    Gamma.temp = paramests['gamma']
    R0.temp = Beta.temp/Gamma.temp
    Coef.temp = (R0.temp-1)*Beta.temp
    R0 <- c(R0,R0.temp);Coef <- c(Coef,Coef.temp)
    Gamma <- c(Gamma,Gamma.temp);Beta <- c(Beta,Beta.temp)
    xest <- ode(x0fun(data,paramests), times, SIRode, paramests, method='ode45')
    y.pre.temp <- yfun(xest,paramests)
    y.pre[[i]] <- cbind.data.frame(Day=times,
                                   Pre=y.pre.temp)
  }
  res <- cbind.data.frame(Coef=Coef,R0=R0,Beta=Beta,Gamma=Gamma)
  names(y.pre) <- seq(1,nrow(res))
  return(as_tibble(list(fit.res=res,fit.value=y.pre)))
}

get_exp_r4 <- function(y,x,fw_size=7,m=1)
{
  xy.new <- detect_mis_values(y,x,m) #
#  xy.new <- get_xy(y,x,m)
  y <- xy.new$y
  x <- xy.new$x
  r.sq <- NULL;Coef <- NULL;p.value <- NULL
  y.pre <- list()
  N <- length(y)
  if(N < fw_size) return(NA)
  for(i in 1:(N-fw_size+1))
  {
    Coef.temp <- NA;r.sq.temp <- NA;p.value.temp <- NA;y.pre.temp <- NA
    y.temp <- y[i:(i+fw_size-1)]
    x.temp <- x[i:(i+fw_size-1)]
    try(res.fit <- lm(log(y.temp)~x.temp))
    fit.temp <- summary(res.fit)
    try(Coef.temp <-fit.temp$coefficients[2,1])
    try(p.value.temp <- fit.temp$coefficients[2,4])
    try(r.sq.temp <- fit.temp$adj.r.squared)
    try(y.pre.temp <- res.fit$fitted.values)
    r.sq <- c(r.sq,r.sq.temp);Coef <- c(Coef,Coef.temp)
    p.value <- c(p.value,p.value.temp)
    y.pre[[i]] <- cbind.data.frame(Day=seq(1,length(y.pre.temp)),
                                   Pre=y.pre.temp)
  }
  res <- cbind.data.frame(Coef=Coef,p.value=p.value,adj.rsq=r.sq)
  if(length(y.pre)!=0){names(y.pre) <- seq(1,nrow(res))}
  return(as_tibble(list(fit.res=res,fit.value=y.pre)))
}


get_exp_r5 <- function(y,x,fw_size=7,m=1)
{
  xy.new <- detect_mis_values(y,x,m) #
#  xy.new <- get_xy(y,x,m)
  y <- xy.new$y
  x <- xy.new$x
  r.sq <- NULL;Coef <- NULL;p.value <- NULL
  y.pre <- list()
  N <- length(y)
  if(N < fw_size) return(NA)
  for(i in 1:(N-fw_size+1))
  {
    Coef.temp <- NA;r.sq.temp <- NA;p.value.temp <- NA;y.pre.temp <- NA
    y.temp <- y[i:(i+fw_size-1)]
    #N <- length(y.temp)
    #x.temp <- seq(1,N)
    rs.logit <-  get_logistic_xy(y.temp)
    try(res.fit <- lm(rs.logit$y~rs.logit$x))
    fit.temp <- summary(res.fit)
    try(Coef.temp <-fit.temp$coefficients[1,1])
    try(p.value.temp <- fit.temp$coefficients[1,4])
    try(r.sq.temp <- fit.temp$adj.r.squared)
    try(y.pre.temp <- y2(res.fit$fitted.values,rs.logit$x0))
    r.sq <- c(r.sq,r.sq.temp);Coef <- c(Coef,Coef.temp)
    p.value <- c(p.value,p.value.temp)
    y.pre[[i]] <- cbind.data.frame(Day=seq(1,length(y.pre.temp)),
                                   Pre=y.pre.temp)
  }
  res <- cbind.data.frame(Coef=Coef,p.value=p.value,adj.rsq=r.sq)
  names(y.pre) <- seq(1,nrow(res))
  return(as_tibble(list(fit.res=res,fit.value=y.pre)))
}

get_exp_r6 <- function(y,x,interval=5,min=50)
{
  xy.new <- detect_mis_values(y,x,m) #
#  xy.new <- get_xy(y,x,m)
  y <- xy.new$y
  x <- xy.new$x
  r.sq <- NULL;Coef <- NULL;p.value <- NULL
  y.pre <- list()
  N <- length(y)
  if(N < interval) return(NA)
  r = (log(y[interval])-log(y[1]))/(x[interval]-x[1])
  return(r)
}

get_r0 <- function(y,x,m=7.5,s=3.4,ld=5,max_R=8)
{
  xy.new <- detect_mis_values(y,x,1) #
  y <- xy.new$y
#  x <- xy.new$x
  y.diff1 <-diff(y,1)
  y.new <- c(y[1],y.diff1)
  y.new[y.new < 0] <- 0
  N <- length(y.new)
  x <- incidence(rep(seq(1,N),y.new),last_date = 5)
  res.temp <- get_R(x,si_mean=7.5,si_sd=3.4,max_R=max_R)
  res <- res.temp$R_ml
  return(res)
}


get_logistic_xy <- function(y)
{
  y.diff <- diff(y)
  y.new <- y[-1]
  x0 <- y[1]
  y.new2 <- y.diff/y.new
  x <- y.new
  return(list(x=x,y=y.new2,x0=x0))
}

y2 <- function(y.pre,x0)
{
  N <- length(y.pre)
  x.new <- x0
  for(i in 1:N)
  {
    x1 <- x.new[i]/(1-y.pre[i])
    x.new <- c(x.new,x1)
  }
  return(x.new)
}

get_r_eps <- function(l,eps=0.001)
{
  if(is.null(nrow(l))) return(c(NA,NA,NA,NA))
  Coef <- l$fit.res$Coef
  Coef.diff <- diff(Coef,1)
  loc <- which(abs(Coef.diff) <= eps)
  if(length(loc) == 0) return(c(NA,NA,NA,NA))
  coef.range <- NA
  try(coef.range <- find_1_inter(loc))
  if(is.na(coef.range)) return(c(NA,NA,NA,NA))
  Coef.eps <- Coef[coef.range]
  if(length(Coef.eps) == 1) return(c(Coef.eps,NA,NA,NA))
  tt.temp <- t.test(Coef.eps)
  Coef.sd <- sd(Coef.eps)
  res <- c(tt.temp$estimate,Coef.sd,
           tt.temp$conf.int[1],tt.temp$conf.int[2])
  return(res)
}

find_1_inter <- function(loc)
{
  N <- length(loc)
  a <- NULL
  c <- NULL
  i=2
  while(i <= N)
  {
    b = 0
    s=loc[i]-loc[i-1]
   while(s==1 & (i <= N-1))
    {
      i=i + 1
      b = b + 1 
      s=loc[i]-loc[i-1]
   }
    a <- c(a,i-1)
    c <- c(c,b)
    i=i + 1
  }
  c.u <- which(c==max(c))
  if(c.u != 1){
  c.l <- c.u - 1
  u <- loc[a[c.u]]+1;l <- loc[a[c.l]+1] 
  return(c(l:u))}else{return(c(min(loc),loc+1))}
}

get_lm_coef <- function(x,y)
{
  if(length(na.omit(x)) <=3) return(c(NA,NA))
  lm.fit <- lm(y~x,na.action = na.omit)
  coe <- lm.fit$coefficients["x"]
  lm.fit.s <- summary(lm.fit)
  coe.p <- lm.fit.s$coefficients[2,4]
  return(c(coe,coe.p))
}
get_lm_coef2 <- function(x,y)
{
  if(length(na.omit(x)) <=3) return(c(NA,NA))
  lm.fit <- lm(formula = y ~ x + I(x^2),na.action = na.omit)
  coe <- lm.fit$coefficients["I(x^2)"]
  lm.fit.s <- summary(lm.fit)
  coe.p <- lm.fit.s$coefficients[3,4]
  return(c(coe,coe.p))
}

get_coef <- function(x){if(length(x) == 2){
  if(nrow(x$fit.res) !=0){
    coef <- x$fit.res$Coef
    d <- seq(1,length(coef))
    p <- x$fit.res$p.value
    r2 <- x$fit.res$adj.rsq
  }else{coef = NA; d = NA;p=NA;r2=NA}
}else{coef = NA; d = NA;p=NA;r2=NA}
  return(data.frame(coef=coef,d=d,p=p,r2=r2))
}

#Clean
clean_df <- function(df,x)
{
  N <- nrow(df)
  res <- df
  for(i in 1:N)
  {
    temp <- df[i,]
    Na.loc <- is.na(temp)
    if(sum(Na.loc) != 0) {
      temp2 <- NULL
      for(j in which(!Na.loc)){
    strsp <- unlist(strsplit(as.character(temp[1,j]),split="[,]"))
    temp2 <- c(temp2,strsp)
      }
      res[i,] <- temp2
    }
  }
  res[,3] <- as.numeric(res[,3])
  res[,4] <- as.numeric(res[,4])
  res$target_city <- x
  return(res)
}

#Aa letter
letter_tran <- function(x)
{
  x <- tolower(x)
  temp <- unlist(strsplit(x,split=" "))
  for(i in 1:length(temp))
  {
    if(temp[i] != "of")
    {
      temp[i] <- capitalize(temp[i])
    }
  }
  return(paste(temp,collapse = " "))
}

#find cor 
find_cor <- function(y,x)
{
  N <- ncol(x)
  pearson.cor <- NULL
  p1 <- NULL
  spearman.cor <- NULL
  p2 <- NULL
  for(i in 1:N)
  {
    res.temp <- cor.test(as.numeric(y),as.numeric(x[,i]))
    pearson.cor <- c(pearson.cor,res.temp$estimate)
    p1 <- c(p1,res.temp$p.value)
    res.temp <- cor.test(as.numeric(y),as.numeric(x[,i]),method = "spearman")
    spearman.cor <- c(spearman.cor,res.temp$estimate)
    p2 <- c(p2,res.temp$p.value)
  }
  res <- cbind.data.frame(Variabls=colnames(x),
                          pearson.cor=pearson.cor,p1=p1,
                          spearman.cor=spearman.cor,p2=p2)
  return(res)
}

#selected variables by random forest
sv_rf <- function(y,x,thre=0.5)
{
  Mi <- 0;x.new <-x;y.new <- data.frame(y=y)
  while(Mi < thre ) {
    if(ncol(x.new) <= 2) break
    res.temp <- randomForest(x=x.new,y=y.new[,1])
    imp <- res.temp$importance
    Mi <- min(imp[,1])
    loc <- which(imp[,1]==Mi)
    x.new <- x.new[,-loc]
  }
  return(list(x=x.new,y=y.new,imp=imp))
}

#selected variables by rda
sv_rda <- function(y,x1,x2,p=0.1)
{
  N1 <- ncol(x1)
  N2 <- ncol(x2)
  rm.N1 <- NULL;rm.N2 <- NULL
  if(N1 >1 ){
  for(i in 1:N1)
  {
    r <- anova(rda(y,x1[,i],x2))
    if(r$`Pr(>F)`[1] >p) rm.N1 <- c(rm.N1,i)               
  }
  if(length(rm.N1) != 0) x1 <- x1[-rm.N1]}
  if(N2 >1){
  for(j in 1:N2)
  {
    r <- anova(rda(y,x1,x2[,j]))
    if(r$`Pr(>F)`[1] >p) rm.N2 <- c(rm.N2,j)               
  }
  if(length(rm.N2) != 0) x2 <- x2[-rm.N2]}
  return(list(y=y,x1=x1,x2=x2))
}


#Extract imp
extract_imp <- function(l)
{
  x <- l$x
  y <- l$y
  x.cor <- cor(x)
  x2.loc <- findCorrelation(x.cor,0.9)
  x2 <- x
  if(length(x2.loc) != 0){x2 <- x[,-x2.loc]}
  
  rf.res <- train(x=x2,y=data.frame(y=y)[,1],
                    trControl = trainControl("cv", number = 5))
  imp <- rf.res$finalModel$importance
  imp2 <- data.frame(variable = rownames(imp),imp=imp[,1])
  return(list(imp=imp2,x=x2,y=y))
}

get_explanations <- function(y,x,human,nature)
{
  v <- colnames(x)
  h <- which(v %in% human)
  n <- which(v %in% nature)
  y <- data.frame(y=y)
  x1 <- NA;x2 <- NA
  if(length(h) != 0) x1=as.data.frame(x[,h])
  if(length(n) != 0) x2=as.data.frame(x[,n])
#  sv.rda.res <- sv_rda(y,x1=as.data.frame(x[,h]),x2=as.data.frame(x[,n]))
  if(is.na(x1)){
    print(1)
    vpa.res <- varpart(y,x2)
   }else if(is.na(x2)){
     print(2)
    vpa.res <- varpart(y,x1)
   }else{
     print(3)
      vpa.res <- varpart(y,x1,x2)
  }
  res <- c(human=vpa.res$part$indfract[1,3],
          nature=vpa.res$part$indfract[3,3],
          interaction=vpa.res$part$indfract[2,3])
  return(res)
}

get_explanations2 <- function(y,x,human,nature)
{
  v <- colnames(x)
  h <- which(v %in% human)
  n <- which(v %in% nature)
  y <- data.frame(y=y)
  indi.var <- rdaenvpart(y,x)$IJ.R2
  indi.var.names <- rownames(indi.var)
  hc <- 0;nc <- 0
  if(length(h)!=0){hc <- sum(indi.var[h,1])}
  if(length(n)!=0){nc <- sum(indi.var[n,1])}
  res <- c(human=hc,
           nature=nc,
           tot=hc+nc)
  return(res)
}

get_xy <- function(y,x,m)
{
  loc <- y >= m
  y <- y[loc]
  x <- x[loc]
  x <- as.numeric(x - min(x) + 1)
  or <- order(x)
  x <- x[or]
  y <- y[or]
  return(list(x=x,y=y))
}

detect_mis_values <- function(y,x,m)
{
  loc <- y >= m
  y <- y[loc]
  x <- x[loc]
  x <- as.numeric(x - min(x) + 1)
  or <- order(x)
  x <- x[or]
  y <- y[or]
  N <- length(x)
  x2 <- seq(min(x),max(x))
  M <- length(x2)
  if( M != N)
  {
    loc <- which(!(x2 %in% x))
    for(i in loc){
    x.ad <- x2[i]
    y.ad <- y[i-1]
    y <- c(y,y.ad)
    x <- c(x,x.ad)
    or <- order(x)
    y <- y[or]
    x <- x[or]
    }
  }
  return(list(x=x,y=y))
}

st_variables <- function(l1.imp,l1.x,l2.imp,l2.x)
{
  M <- ncol(l1.x)
  N <- ncol(l2.x)
  L <- min(M,N)
  if(L==M){
    or <- order(-l2.imp$imp)
    va <- l2.imp$variable[which(or <= M)]
    l2.x.new <- l2.x[,va]
    l1.x.new <- l1.x
  }
  if(L==N){
    or <- l1.imp$imp
    va <- l1.imp$variable[which(or <= M)]
    l1.x.new <- l1.x[,va]
    l2.x.new <- l2.x
  }
  return(list(l1.imp=l1.imp,l2.imp=l2.imp,
              l1.x=l1.x.new,l2.x=l2.x.new))
}

func01 <- function(y)
{
  return( (y-min(y,na.rm = T))/(max(y,na.rm = T)-min(y,na.rm = T)) )
}
