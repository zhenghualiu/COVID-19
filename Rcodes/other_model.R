expr_efw <- list()
st = 5
j <-  1
for(m in 1:10)
{
  print(m)
  res.temp <- con.ts %>% group_by(provinceEnglishName,countryEnglishName) %>% do(fit=get_exp_r1(.$province_confirmedCount,st=st,ed=60,m=m))
  names(res.temp$fit) <- res.temp$provinceEnglishName
  res.coef <- list.rbind(lapply(res.temp$fit,get_coef))
  res.coef$region <- str_extract(rownames(res.coef),pattern = "[\\w]+")
  res.coef$d <- res.coef$d + st
  res.coef <- res.coef %>% group_by(region) %>% dplyr::mutate(N=dplyr::n()) 
  res.coef <- na.omit(res.coef)
  res.coef <- merge.data.frame(res.coef,con.ts[!duplicated(con.ts[,c(2,3)]),],by.x="region",by.y="provinceEnglishName",all.x=T)
  res.coef$sig <- res.coef$p < 0.05
  res.coef <- within(res.coef,sig <- factor(sig,levels = c("TRUE","FALSE")))
  res.coef <- res.coef %>% group_by(region) %>% 
    mutate(Max.coef = max(as.logical(sig)*coef,na.rm = T))
  expr_efw[[j]] <- as.data.frame(res.coef)
  j <- j + 1
}
names(expr_efw) <- paste("m_",seq(1,10),sep="")

expr_mw <- list()
m=1
j <-  1
for(fw_size in 7:14)
{
  print(fw_size)
  res.temp <- con.ts %>% group_by(provinceEnglishName,countryEnglishName) %>% do(fit=get_exp_r4(.$province_confirmedCount,fw_size=fw_size,m=m))
  names(res.temp$fit) <- res.temp$provinceEnglishName
  res.coef <- list.rbind(lapply(res.temp$fit,get_coef))
  res.coef$region <- str_extract(rownames(res.coef),pattern = "[\\w]+")
  res.coef$d <- res.coef$d + st
  res.coef <- res.coef %>% group_by(region) %>% dplyr::mutate(N=dplyr::n()) 
  res.coef <- na.omit(res.coef)
  res.coef <- merge.data.frame(res.coef,con.ts[!duplicated(con.ts[,c(2,3)]),],by.x="region",by.y="provinceEnglishName",all.x=T)
  res.coef$sig <- res.coef$p < 0.05
  res.coef <- within(res.coef,sig <- factor(sig,levels = c("TRUE","FALSE")))
  res.coef <- res.coef %>% group_by(region) %>% 
    mutate(Max.coef = max(as.logical(sig)*coef,na.rm = T))
  expr_mw[[j]] <- as.data.frame(res.coef)
  j <- j + 1
}
names(expr_mw) <- paste("fw_size_",7:14,sep="")

covid19_r50 <-con.ts %>% group_by(provinceEnglishName,countryEnglishName) %>% 
  do(data.frame(r50_1=get_exp_r6(.$province_confirmedCount)))

covid19_R0 <- con.ts %>% group_by(provinceEnglishName,countryEnglishName) %>% 
  do(data.frame(R0_1=get_r0(.$province_confirmedCount,ld=10)))



expr_efw.df <- list.rbind(expr_efw)
expr_mw.df <- list.rbind(expr_mw)
expr_efw.df$rn <- rownames(expr_efw.df)
expr_efw.df$rn <- list.rbind(strsplit(expr_efw.df$rn,split="[.]"))[,1]
expr_mw.df$rn <- rownames(expr_mw.df)
expr_mw.df$rn <- list.rbind(strsplit(expr_mw.df$rn,split="[.]"))[,1]
expr_efw.df.cl <- expr_efw.df[,c(1,18,19)]
expr_efw.df.cl <- expr_efw.df.cl[!duplicated((expr_efw.df.cl)),]
expr_efw.df.cl <- spread(expr_efw.df.cl,key="rn",value = "Max.coef")
expr_mw.df.cl <- expr_mw.df[,c(1,18,19)]
expr_mw.df.cl <- expr_mw.df.cl[!duplicated((expr_mw.df.cl)),]
expr_mw.df.cl <- spread(expr_mw.df.cl,key="rn",value = "Max.coef")

###
sig.exp.coef <- exp.coef %>% group_by(region) %>% 
  mutate(Sig.coef = as.logical(sig)*coef,na.rm = T)
sig.exp.coef <- sig.exp.coef %>%  group_by(region) %>% 
  mutate(ther.coef = -sort(-Sig.coef)[10],na.rm = T) %>% 
  filter(Sig.coef >= ther.coef & N >= 10) %>% 
  mutate(rank = rank(Sig.coef),na.rm = T) %>%
  filter(rank <= 10)
sig.exp.coef$method = paste("rank_",sig.exp.coef$rank,sep="")
sig.exp.coef <- sig.exp.coef %>% filter(!region %in% c("Bolivia","Honduras","Burkina")) %>%
  select(region=region,coef=Sig.coef,method=method)
sig.exp.coef <- spread(sig.exp.coef,key=method,value = coef) 


human <- c("GDP.pc","GDP","Pop","Pop.density","Urban.pop","Commuting","PM2.5","PM10",
           "All.BMI","Female.BMI","Male.BMI","All.Obesity","Female.Obesity","Male.Obesity","All.Underweight",   
           "Female.Underweight","Male.Underweight","Impro.sanit","Impro.water",
           "Energy.pc", "Energy.pGDP","FDI","Paved.roads","Physicians","Mortality.under5","CO2.pc",
           "N2O.pc","CH4.pc"
)
nature <- c("Lat","Elev","Avg.Tem.Jan","Avg.Tem.Feb","Avg.Tem.Mar",
            "Avg.Pre.Jan", "Avg.Pre.Feb","Avg.Pre.Mar","Avg.HT.Jan",
            "Avg.HT.Feb","Avg.HT.Mar","Avg.LT.Jan","Avg.LT.Feb","Avg.LT.Mar","UV","Extreme.Climate"
)

data1_v <- c("Lat","Elev","Avg.Tem.Jan","Avg.Tem.Feb","Avg.Tem.Mar",
             "Avg.HT.Jan","Avg.HT.Feb","Avg.HT.Mar","Avg.LT.Jan","Avg.LT.Feb","Avg.LT.Mar",
             "Avg.Pre.Jan","Avg.Pre.Feb","Avg.Pre.Mar",
             "GDP.pc", "GDP","Pop","Pop.density","Commuting","PM2.5","PM10")
data2_v <- 
  c("Lat","Elev","Avg.Tem.Jan","Avg.Tem.Feb","Avg.Tem.Mar",
    "Avg.HT.Jan","Avg.HT.Feb","Avg.HT.Mar","Avg.LT.Jan","Avg.LT.Feb","Avg.LT.Mar",
    "Avg.Pre.Jan","Avg.Pre.Feb","Avg.Pre.Mar","UV","Extreme.Climate",
    "GDP.pc", "GDP","Pop","Pop.density","Urban.pop","Commuting","PM2.5","PM10",
    "All.BMI","Female.BMI","Male.BMI","All.Obesity","Female.Obesity","Male.Obesity",      
    "All.Underweight","Female.Underweight","Male.Underweight","Impro.sanit","Impro.water",
    "Energy.pc","Energy.pGDP","FDI","Paved.roads","Physicians","Mortality.under5",
    "CO2.pc","CH4.pc")

c.train.sf1 <- merge.data.frame(c.train.sf,sig.exp.coef,by="region")
c.train.sf1 <- merge.data.frame(c.train.sf1,expr_mw.df.cl,by="region")
nc.train.meta_all.part1 <- 
  merge.data.frame(nc.train.meta_all.part,sig.exp.coef,by="region")
nc.train.meta_all.part1 <- 
  merge.data.frame(nc.train.meta_all.part1,expr_mw.df.cl,by="region")

c.train.sf1 <- merge.data.frame(c.train.sf1,covid19_R0[,c(1,3)],by.x="region",by.y="provinceEnglishName")
c.train.sf1 <- merge.data.frame(c.train.sf1,covid19_r50[,c(1,3)],by.x="region",by.y="provinceEnglishName")
c.train.sf1$mr_up4 <- (c.train.sf1$rank_1+c.train.sf1$rank_2+
                         c.train.sf1$rank_3+c.train.sf1$rank_4)/4
nc.train.meta_all.part1 <- merge.data.frame(nc.train.meta_all.part1,covid19_R0[,c(1,3)],by.x="region",by.y="provinceEnglishName")
nc.train.meta_all.part1 <- merge.data.frame(nc.train.meta_all.part1,covid19_r50[,c(1,3)],by.x="region",by.y="provinceEnglishName")
nc.train.meta_all.part1$mr_up4 <- (nc.train.meta_all.part1$rank_1+nc.train.meta_all.part1$rank_2+
                                     nc.train.meta_all.part1$rank_3+nc.train.meta_all.part1$rank_4)/4

c.train.sf1 <- na.omit(c.train.sf1)
nc.train.meta_all.part1 <- na.omit(nc.train.meta_all.part1)


data1.train.facs <- c.train.sf1[,-c(1,6,7,8,17,24:27,28,29:49)]
data2.train.facs <- nc.train.meta_all.part1[,-c(1,5:6,16,23:26,50,51:71)]
data1.train.facs$GDP <- log(data1.train.facs$GDP)
data1.train.facs$GDP.pc <- log(data1.train.facs$GDP.pc)
data1.train.facs <- as.data.frame(scale(data1.train.facs))

data2.train.facs$GDP <- log(data2.train.facs$GDP)
data2.train.facs$GDP.pc <- log(data2.train.facs$GDP.pc)
data2.train.facs <- as.data.frame(scale(data2.train.facs))

data1.train.y <- c.train.sf1[,c(29:49)]
data2.train.y <- nc.train.meta_all.part1[,c(51:71)]
data1.train.y <- as.data.frame(scale(data1.train.y))
data2.train.y <- as.data.frame(scale(data2.train.y))

#### RF importance ####
N1 <- ncol(data1.train.y)
data1.rf.imp <- list()
data1.rf.x <- list()
set.seed(1)
for( i in 11:N1)
{
  print(i)
  temp <- sv_rf(x=data1.train.facs,y=data1.train.y[,i])
  temp <- extract_imp(temp)
  data1.rf.imp[[i]] <- temp$imp
  data1.rf.x[[i]] <- temp$x
}

N2 <- ncol(data2.train.y)
data2.rf.imp <- list()
data2.rf.x <- list()
for( i in 1:N2)
{
   print(i)
   temp <- sv_rf(x=data2.train.facs,y=data2.train.y[,i])
   temp <- extract_imp(temp)
   data2.rf.imp[[i]] <- temp$imp
   data2.rf.x[[i]] <- temp$x
}
##
names(data1.rf.imp) <- colnames(data1.train.y)
names(data2.rf.imp) <- colnames(data2.train.y)

data1.rf.imp.df <- list.rbind(data1.rf.imp)
data2.rf.imp.df <- list.rbind(data2.rf.imp)
data1.rf.imp.df$group <- "Province"
data2.rf.imp.df$group <- "State"
data.rf.imp.df <- rbind.data.frame(data1.rf.imp.df,data2.rf.imp.df)

data.rf.imp.df$method <- rownames(data.rf.imp.df)
data.rf.imp.df$method <- list.rbind(str_split(data.rf.imp.df$method,"\\.",n=2))[,1]
#data.rf.imp.df$method[which(data.rf.imp.df$method == "m_m_up4")] <- "mm_up4"
data.rf.imp.df$m <- str_extract(data.rf.imp.df$method,"[:alpha:]+")
data.rf.imp.df$par <- as.numeric(str_extract(data.rf.imp.df$method,"[\\d]+"))

data.rf.imp.df <- within(data.rf.imp.df,method <- factor(method,
  levels =c("rank_1","rank_2","rank_3","rank_4","mr_up4",
            "rank_5","rank_6","rank_7","rank_8","rank_9","rank_10",
            "fw_size_7","fw_size_8","fw_size_9","fw_size_10","fw_size_11",
            "fw_size_12","fw_size_13","fw_size_14","R0_1","r50_1"
            )
))


pdf("./fig/imp.all.pdf",width = 12,height = 6)
data.rf.imp.df %>% filter(!method %in% c("rank_5",
  "rank_6","rank_7","rank_8","rank_9","rank_10","r50_1"))  %>%
  ggplot(aes(x=method,y=variable,fill=imp))+
  geom_raster() + theme_bw() +facet_wrap(~group,scale="free_y")+
  theme(axis.line = element_blank(),panel.grid = element_blank(),
        axis.text = element_text(color="black"),
        strip.background = element_blank(),axis.text.x = element_text(angle=90))+
  scale_x_discrete(label=c("r.efw.1","r.efw.2","r.efw.3","r.efw.4","r.efw.mean",
                           "r.mfw.7","r.mfw.8","r.mfw.9","r.mfw.10","r.mfw.11",
                           "r.mfw.12","r.mfw.13","r.mfw.14","R0"),
                   expand = c(0,0,0,0))+
  scale_y_discrete(expand = c(0,0,0,0))+guides(fill=guide_colorbar(title=""))+
  scale_fill_gradient2(high = "red",low="grey")+
  labs(x="",y="")
dev.off()

pdf("./fig/imp.exp.rank.pdf",width = 6,height = 5)
data.rf.imp.df %>% filter(m=="rank") %>% ggplot(aes(x=par,y=variable,fill=imp))+
  geom_raster() + theme_bw() +facet_wrap(~group,scale="free_y")+
  theme(axis.line = element_blank(),panel.grid = element_blank(),
        axis.text = element_text(color="black"),strip.background = element_blank()) +
  scale_x_continuous(breaks = seq(1,10),expand = c(0,0,0,0))+
  scale_y_discrete(expand = c(0,0,0,0))+guides(fill=guide_colorbar(title=""))+
  scale_fill_gradient2(high = "red",low="grey")+
  labs(x="Rank",y="")
dev.off()
##
data1.condi <- list()
data2.condi <- list()

for( i in 1:N1)
{
  data1.condi[[i]] <- get_explanations(data1.train.y[,i],data1.rf.x[[i]],human,nature)
}
for( i in 1:N2)
{
  data2.condi[[i]] <- get_explanations(data2.train.y[,i],data2.rf.x[[i]],human,nature)
}
names(data1.condi) <- colnames(data1.train.y)
names(data2.condi) <- colnames(data2.train.y)
data1.condi.df <- as.data.frame(list.rbind(data1.condi))
data2.condi.df <- as.data.frame(list.rbind(data2.condi))
data1.condi.df$group <- "Province"
data2.condi.df$group <- "State"
data1.condi.df$method <- rownames(data1.condi.df)
data2.condi.df$method <- rownames(data2.condi.df)

data.condi <- rbind.data.frame(data1.condi.df,data2.condi.df)
data.condi[data.condi < 0] <- 0
data.condi$tot <- data.condi$human+data.condi$nature+data.condi$interaction
data.condi$m <- str_extract(data.condi$method,"[:alpha:]+")
data.condi$par <- as.numeric(str_extract(data.condi$method,"[\\d]+"))

colnames(data.condi)[c(1:3,6)] <- c("Human","Nature","Interaction","Tot") 
data.condi.plot <- 
  melt.data.frame(data=data.condi,
                  measure.vars = c("Tot","Human","Nature","Interaction"))

data.condi.plot <- within(data.condi.plot,method <- factor(method,
     levels =c("rank_1","rank_2","rank_3","rank_4","mr_up4",
     "rank_5","rank_6","rank_7","rank_8","rank_9","rank_10",
     "fw_size_7","fw_size_8","fw_size_9","fw_size_10","fw_size_11",
      "fw_size_12","fw_size_13","fw_size_14","R0_1","r50_1")
))

pdf("./fig/explanations.all.pdf",width = 6,height = 5)
data.condi.plot %>% filter(!method %in% c("rank_5",
                                         "rank_6","rank_7","rank_8","rank_9","rank_10","r50_1")) %>%  
  ggplot(aes(x=method,y=value*100))+
  geom_bar(aes(fill=group),color="black",stat="identity",position=position_dodge())+
  theme_bw() + labs(x="",y="Explanation %") +
  scale_x_discrete(label=c("r.efw.1","r.efw.2","r.efw.3","r.efw.4","r.efw.mean",
                           "r.mfw.7","r.mfw.8","r.mfw.9","r.mfw.10","r.mfw.11",
                           "r.mfw.12","r.mfw.13","r.mfw.14","R0"),
                   expand = c(0.05,0.05,0.05,0.1)) + 
  scale_y_continuous(expand = c(0,.01,.01,.01),limits = c(0,100))+
  theme(axis.text = element_text(color="black"),
        strip.background = element_blank()
        ,panel.grid = element_blank(),
        axis.text.x = element_text(angle=90)) +
  scale_fill_manual(values=c("white","black"))+ guides(fill=F)+
  facet_wrap(~variable)
dev.off()

pdf("./fig/explanations.exp.rank.pdf",width = 6,height = 5)
data.condi.plot %>% filter(m=="rank") %>%  ggplot(aes(x=par,y=value*100))+
  geom_bar(aes(fill=group),color="black",stat="identity",position=position_dodge())+
  theme_bw() + labs(x="Rank",y="Explanation %") +
  scale_x_continuous(breaks = seq(1,10)) + scale_y_continuous(expand = c(0,.01,.01,.01),limits = c(0,100))+
  theme(axis.text = element_text(color="black"),
        strip.background = element_blank(),panel.grid = element_blank()) +
  scale_fill_manual(values=c("white","black"))+ guides(fill=F)+
  facet_wrap(~variable)
dev.off()
