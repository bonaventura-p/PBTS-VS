
###################################
###########PCA analysis ########### 
##################################

#functions
#####################
#### K-fold custom ##
#####################

kfold_custom <- function(n, k=NULL){
  #k
  if(is.null(k)){ k <- n} # if undefined, assume leave-one-out
  res <- vector(mode="list", k)
  n.remain <- seq(n)
  for(i in seq(k)){
    samp <- sample(seq(length(n.remain)), ceiling(length(n.remain)/(k-i+1)))
    res[[i]] <- n.remain[samp]
    n.remain <- n.remain[-samp]
  }
  return(res)
}


pca_kcv <- function(X, ks=2, npc.max=ncol(X)){
  kgroups <- kfold_custom(n = nrow(X), k = ks)
  error <- matrix(0, nrow=dim(X)[1], ncol=min(dim(X)[2],npc.max))
  
  for(n in seq(kgroups)){
    
    Xtrain = X[-kgroups[[n]],]
    Xtrain = scale(Xtrain, center=TRUE, scale=FALSE)
    V = svd(Xtrain)$v
    Xtest = X[kgroups[[n]],,drop = FALSE]
    Xtest = scale(Xtest, center=attr(Xtrain, "scaled:center"), scale=FALSE)
    
    for(j in 1:min(dim(V)[2],npc.max)){
      P = V[,1:j] %*% t(V[,1:j])
      err <- Xtest %*% (diag(length(diag(P))) - P + diag(diag(P)))
      error[kgroups[[n]],j] <- error[kgroups[[n]],j] + rowSums(sqrt(err^2))
      print(paste("n =", n, "; j =", j))
    }
  }
  res <- list(
    error=error
  )
  return(res)
}


#load data
# "V:/Pacileo_B/NOBACKUP/PISA PM/PFS/COL_pca1118.csv" %>%
#   read.csv(.,header=T) %>%
#   dplyr::select(.,dplyr::matches("^AP|age")) %>%
#   {.}->pca_data

#load data
"V:/Pacileo_B/NOBACKUP/PISA PM/PFS/AND_pca.csv" %>%
  read.csv(.,header=T) %>%
  dplyr::select(.,dplyr::matches("^AP|age")) %>%
  {.}->pca_data

#PCA Analysis
#normal sample
set.seed(1992) #replicable results

PCA_allAND<-prcomp(pca_data,retx = TRUE, center = TRUE,scale. = TRUE) 

PCA_allAND$x[,cumsum(PCA_allAND$sdev^2)/sum(PCA_allAND$sdev^2)<0.95] %>% base::NCOL() ->npc_max   ##only .5 variance (81 components)

PCA_allAND$x[,cumsum(PCA_allAND$sdev^2)/sum(PCA_allAND$sdev^2)<0.8] %>% base::NCOL() ->npc_oecd   ##only .5 variance (81 components)


expl.var <- data.frame(PC=seq(1:length(PCA_allAND$sdev)), Value=cumsum(PCA_allAND$sdev^2)/sum(PCA_allAND$sdev^2))

expl.var %>%
  ggplot(.,aes(x=PC,y=Value)) + 
  geom_line()+
  geom_point(col=pbts_cols("lightblue")) +
  geom_hline(yintercept=0.95, linetype="dashed", color = "red")+
  geom_hline(yintercept=0.8, linetype="dashed", color = pbts_cols("oecdblue"))+
  theme(legend.position="bottom") +
  labs(title="Percentage of variance explained by PCA components",x="Number of PCs", y = "% Variance") +
  scale_x_continuous(breaks=seq(0,500,50)) +  
  scale_y_continuous(limits=c(0, 1),breaks=c(0,.25,.5,.75,.95),labels=c("0","25","50","75","95")) 


#full sample
ResList<-list()

for(i in c(2,5)) {
  
  res <- pca_kcv(X=pca_data, ks=i)
  res<-lapply(res,colSums)
  
  ResList[[length(ResList)+1]] = res
  
}


ResList %>% 
  data.frame(.) %>% 
  cbind(.,seq(523)) %>% 
  dplyr::rename(.,fold2=error,fold10=error.1,PC=`seq(523)`) %>%
  tidyr::gather(.,key=PC, value=Value) ->ColombiaRES

names(ColombiaRES)[2] <- "Folds"

ColombiaRES %>%
  ggplot(.,aes(x=PC,y=Value,color=factor(Folds))) + 
  geom_line()+
  geom_point() +
  #geom_vline(xintercept = 203,linetype="dashed", color = pbts_cols("oecdblue"))+
  #geom_vline(xintercept = 309,linetype="dashed", color = pbts_cols("red"))+
  scale_color_pbts(palette="RdGn", reverse=TRUE, labels=c("10-folds", "2-folds"))+
  theme(legend.position="bottom") +
  labs(title="K-fold Cross Validation",x="Number of PCs", y = "PRESS") +
  scale_y_continuous(breaks=seq(300000, 500000,25000)) +
  scale_x_continuous(limits=c(0,400), breaks=seq(0, 400,50)) +
  theme_gray()+
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.background = element_rect(fill="#EBEBEB"),
        plot.subtitle=element_text(face="italic"))


ColombiaRES %>%
  dplyr::filter(., ColombiaRES$Value>0 )->Col2
  
  
#PC that minimise PRESS
npc_press<-Col2$PC[Col2$Value==min(Col2$Value) & Col2$Folds=="fold10"] #PC256
PCA_allAND$Cum<-cumsum(PCA_allAND$sdev^2)/sum(PCA_allAND$sdev^2)




# define aux data frame
aux <- dplyr::select(gold.data,dplyr::matches("^W_FSTUWT$|^W_FSTR\\d+$|^stidsch$|^stidstd$"))


ResultsList<-list()

for(i in seq(0.45,0.95,0.1)) {
  
  meani<-paste("Mean",i,sep="_")
  sei<-paste("SE",i,sep="_")
  
  
  #  get direct regressors
  direct.regs <- DirectRegs(stu.data=gold.data, raw.data=raw.data) 
  
  # do pca
  pca.res  <-  PcaComp(pca.data=pca.data,pctvar=i) 
  
  
  # join pca direct regressors
  direct.regs %<>% cbind(.,pca.res)
  
  
  # get item parameters
  IntlPars("math","diff") %>%
    AnchorValues("math", score.data = score.data, item.data = ., irtpar = "diff") %>%
    data.matrix(.) -> xsi.fixed
  
  IntlPars("math","slope") %>%
    AnchorValues(domn="math", score.data = score.data, item.data = ., irtpar = "slope") %>%
    data.matrix(.) -> B
  
  dim(B)[3]<-1
  
  # do scaling
  res <- PFSscale(domn="math", resp=score.data, Y=direct.regs, xsi.fixed=xsi.fixed, B=B, aux=aux) 
  
  resMean<-intsvy::pisa.mean.pv(pvlabel="MATH", by = "stidsch",data =res) 
  
  #creating dataset
  #mean
  resMean %<>%
    dplyr::select(.,stidsch,Mean,dplyr::matches("s.e.")) %>%
    data.table::setnames(.,c("stidsch",paste("Mean",i,sep="_"),paste("SE",i,sep="_"))) %>%
    {.}->SchoolMath
  
  ResultsList[[length(ResultsList)+1]] = SchoolMath
  
  
}

for(i in 1:6) {
  assign(paste0("Math_PCA", i), data.frame(ResultsList[i]))
}

Math_PCA1 %>%
  dplyr::full_join(.,Math_PCA2,by="stidsch") %>%
  dplyr::full_join(.,Math_PCA3,by="stidsch") %>%
  dplyr::full_join(.,Math_PCA4,by="stidsch") %>%
  dplyr::full_join(.,Math_PCA5,by="stidsch") %>%
  dplyr::full_join(.,Math_PCA6,by="stidsch") ->Math_fullpca


Math_fullpca %>%
  reshape(., idvar="stidsch", varying=2:13, direction="long", sep="_") %>%
  ggplot(.,aes(x=stidsch,y=Mean,color=factor(time))) + 
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, alpha=0.1,
                position=position_dodge(0.05))+
  geom_point()+
  scale_color_pbts(palette="Main", labels=c("45%","55%","65%","75%","85%","95%"))+
  labs(y="Mean score in mathematics", 
       title ="Mean score in mathematics with by % variation explained by PCA components")+
  theme_gray()+
  theme(axis.text.x = element_text(angle = 90), 
        axis.title.x = element_blank(),
        legend.position="top",
        legend.title=element_blank(),
        legend.background = element_rect(fill="#EBEBEB"),
        plot.subtitle=element_text(face="italic"))



