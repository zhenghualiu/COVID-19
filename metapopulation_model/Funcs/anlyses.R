setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#=====
library(rjson)
library(stringr)
library(rlist)
library(reshape)
library(dplyr)
library(igraph)
library(network)
library(intergraph)
source("./json2df.R")
source("./funcs.R")
#=====
N_path <- "./sim.res/N50/"
N_files <- dir(N_path)
q.level <- lapply(strsplit(N_files,split="_"),function(x){
  as.numeric(str_extract(x[6],"[\\d]+.[\\d]+"))
})
q.level <- as.vector(list.cbind(q.level))  

coef.N <- list()
for(i in 1:length(N_files))
{
  temp <- fromJSON(file=paste(N_path,N_files[i],sep=""))
  coef.N[[i]] <- json2df(temp,q.level = q.level[i])
}
coef.N.df <- list.rbind(coef.N)



st=5;m=1
coef.fit <- coef.N.df %>% filter(day >= 21 & value != 1e5) %>% 
  group_by(q.level,variable,n.nodes,rep) %>% 
  do(fit=get_exp_r1(100000-.$value,.$day,st = st,ed=60,m=m))

coef.fit.1 <- coef.fit %>% group_by(q.level,variable,n.nodes,rep) %>% 
  mutate(max.coef = get_max_coef(fit)) %>% 
  select(q.level=q.level,variable=variable,n.nodes=n.nodes,rep=rep,coef=max.coef)

#coef.fit.1.N10 <- coef.fit.1
#coef.fit.1.N30 <- coef.fit.1
#coef.fit.1.N50 <- coef.fit.1

#coef.fit.2 <- coef.fit.1 %>% group_by(q.level,variable,n.nodes) %>%
#  do(data.frame(coef=mean(.$max.coef,na.rm = T)))

net <- read.graph("./network/N-50_p1-0.3_p2-0.15_pop-1e+05.graphml",format = "graphml")
net_node_attr <- get_node_attr_df(net)

meta_data <- merge.data.frame(coef.fit.1,net_node_attr,by.x="variable",by.y="id")
#meta_data.N10 <- meta_data %>% filter(abs(coef)!= Inf) %>% na.omit()
#meta_data.N30 <- meta_data %>% filter(abs(coef)!= Inf) %>% na.omit()
meta_data.N50 <- meta_data %>% filter(abs(coef)!= Inf) %>% na.omit()

save(meta_data.N10,meta_data.N30,meta_data.N50,file = "meta_data.rData")

meta_data <- rbind.data.frame(meta_data.N10,meta_data.N30)
meta_data <-rbind.data.frame(meta_data,meta_data.N50)

tot_explanations <- meta_data %>% group_by(q.level,n.nodes,rep) %>% 
  do(data.frame(
    tot = get_tot_explanation(.$coef,data.frame(x1=.$Climate,x2=.$Socio.economic)))) %>% na.omit()
tot_explanations2 <- tot_explanations %>% group_by(q.level,n.nodes) %>%
  do(data.frame(M = mean(.$tot,na.rm=T),sd=sd(.$tot,na.rm=T)))

fig.model.1 <- tot_explanations2 %>% 
  ggplot(aes(x=1-q.level,y=M*100,colour=as.factor(n.nodes)))+
  geom_point()+ geom_line() + theme_bw()+
  labs(x="Quarantine level",y="Total explanation %")+
  geom_errorbar(aes(ymin=(M-sd)*100, ymax=(M+sd)*100), width=.05)+
  theme(axis.text = element_text(color="black"),panel.grid = element_blank(),
        legend.position = c(0.9,0.2))+
  guides(colour=guide_legend(title=""))

pdf("../Figs/fig.tot_explanation.pdf", width = 6,height = 4)
print(fig.model.1)
dev.off()

meta_data2 <- meta_data %>% group_by(variable,q.level,n.nodes) %>%
  do(data.frame(Coef=mean(.$coef),sd=sd(.$coef),
                growth_rate=mean(.$growth_rate)))
fig.model.2 <- meta_data2 %>% ggplot(aes(x=growth_rate,y=Coef))+geom_point()+
  facet_grid(n.nodes~(1-q.level)) + theme_bw()+
  labs(y="Estimated value",x="Theoritical value")+
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(color="black"))+
  geom_smooth(method="lm",se=F,size=0.5)+
  scale_x_continuous(limits = c(0.2,0.4),
                     breaks = seq(0.25,0.45,0.1),
                     labels = c("0.25","0.35","0.45"))+
  scale_y_continuous(limits = c(0.2,0.4),breaks = seq(0.25,0.45,0.1),
                     labels = c("0.25","0.35","0.45"))
pdf("../Figs/fig.growth_rate.pdf", width = 6,height = 4)
print(fig.model.2)
dev.off()
