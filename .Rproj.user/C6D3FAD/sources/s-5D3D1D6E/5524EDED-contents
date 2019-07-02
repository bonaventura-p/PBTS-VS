
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

#colombia
 raw.data <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Data/Colombia/Round3/raw.data.txt", header=T, sep="\t")
 score.data <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Data/Colombia/Round3/score.data.txt", header=T, sep="\t")
 pca.data <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Data/Colombia/Round3/pca.data.txt", header=T, sep="\t")
 gold.data <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Data/Colombia/Round3/gold.data.txt", header=T, sep="\t")
# 


# #andorra
# raw.data <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Validation Study/PBTS-VS/data/Andorra/raw.data.txt", header=T, sep="\t")
# score.data <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Validation Study/PBTS-VS/data/Andorra/score.data.txt", header=T, sep="\t")
# pca.data <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Validation Study/PBTS-VS/data/Andorra/pca.data.txt", header=T, sep="\t")
# gold.data <- read.table("V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Validation Study/PBTS-VS/data/Andorra/gold.data.txt", header=T, sep="\t")



pca.data %<>% dplyr::select(.,dplyr::matches("^AP|age"))

# get item parameters
IntlPars("math","diff") %>%
  AnchorValues("math", score.data = score.data, item.data = ., irtpar = "diff") %>%
  data.matrix(.) -> xsi.fixed

IntlPars("math","slope") %>%
  AnchorValues(domn="math", score.data = score.data, item.data = ., irtpar = "slope") %>%
  data.matrix(.) -> B

dim(B)[3]<-1

pca.res  <-  PcaComp(pca.data=pca.data) 


#  get direct regressors




resdf <- data.frame() 

for(i in  1:length(pca.res)) {
  
  direct.regs <- DirectRegs(stu.data=gold.data, raw.data=raw.data) 
  
  # join pca direct regressors
  direct.regs %<>% cbind(.,pca.res[,1:i])
  
  score.data %>%
    dplyr::select(.,dplyr::matches("^PM\\d{4}Q\\d{2}.?$")) %>%
    TAM::tam.mml(., xsi.fixed = xsi.fixed, B = B, irtmodel = "2PL", Y = direct.regs, control=list(maxiter = 500)) -> res
  
  res["person"] %>%
    #math specific transformations
    as.data.frame(.) %>%
    dplyr::mutate(., person.EAP=PisaRescale(person.EAP,"math"), person.SD.EAP= person.SD.EAP/1.28^2*100)  %>%
    dplyr::summarize_at(.,dplyr::vars(person.EAP,person.SD.EAP),c(mean,sd)) %>%
    dplyr::rename(Mean.EAP=`person.EAP_function (x, ...) ...`, 
                  Mean.SD.EAP=`person.SD.EAP_function (x, ...) ...`,
                  SD.EAP=`person.EAP_function (x, na.rm = FALSE) ...`,
                  SD.SD.EAP=`person.SD.EAP_function (x, na.rm = FALSE) ...`) %>%
    dplyr::mutate(., PC=i) %>%
    dplyr::select(., PC, Mean.EAP, SD.EAP, Mean.SD.EAP, SD.SD.EAP)-> EAP_res
  
  resdf <- rbind(resdf, data.frame(EAP_res,row.names=NULL))
  
}


# plot results
resdf %>%
  ggplot(., aes(y=Mean.EAP,x=PC)) +
  geom_line(col=pbts_cols("oecdblue"),size=1)+
  theme(legend.position="bottom") +
  labs(title="Mean EAP by number of PCA components",x="Number of PCs") +
  scale_y_continuous(limits=c(min(resdf$Mean.EAP)-25,max(resdf$Mean.EAP)+25)) -> MeanEAP

# plot results
resdf %>%
  ggplot(., aes(y=SD.EAP,x=PC)) +
  geom_line(col=pbts_cols("oecdblue"),size=1)+
  theme(legend.position="bottom") +
  labs(title="SD(EAP) by number of PCA components",x="Number of PCs") +
  scale_y_continuous(limits=c(min(resdf$SD.EAP)-15,max(resdf$SD.EAP)+15)) -> SDEAP


# plot results
resdf %>%
  ggplot(., aes(y=Mean.SD.EAP,x=PC)) +
  geom_line(col=pbts_cols("oecdblue"),size=1)+
  theme(legend.position="bottom") +
  labs(title="Mean of SD(EAP) by number of PCA components",x="Number of PCs") +
  scale_y_continuous(limits=c(min(resdf$Mean.SD.EAP)-15,max(resdf$Mean.SD.EAP)+15))  ->M.SDEAP


# plot results
resdf %>%
  ggplot(., aes(y=SD.SD.EAP,x=PC)) +
  geom_line(col=pbts_cols("oecdblue"),size=1)+
  theme(legend.position="bottom") +
  labs(title="Standard deviation of SD(EAP) by number of PCA components",x="Number of PCs") +
  scale_y_continuous(limits=c(min(resdf$SD.SD.EAP)-10,max(resdf$SD.SD.EAP)+10)) -> SD.SDEAP


gridExtra::grid.arrange( MeanEAP , SDEAP, M.SDEAP, SD.SDEAP,   ncol=2, nrow=2, widths = c(1, 0.6))


# 
# 
# #load data
#  "V:/Pacileo_B/NOBACKUP/PISA PM/PFS/COL_pca1118.csv" %>%
#    read.csv(.,header=T) %>%
#    dplyr::select(.,dplyr::matches("^AP|age")) %>%
#    {.}->pca_data

#load data
# "V:/Pacileo_B/NOBACKUP/PISA PM/PFS/AND_pca.csv" %>%
#   read.csv(.,header=T) %>%
#   dplyr::select(.,dplyr::matches("^AP|age")) %>%
#   {.}->pca_data

#PCA Analysis
#normal sample
set.seed(1992) #replicable results

PCA_allAND<-prcomp(pca.data,retx = TRUE, center = TRUE,scale. = TRUE) 

PCA_allAND$x[,cumsum(PCA_allAND$sdev^2)/sum(PCA_allAND$sdev^2)<0.95] %>% base::NCOL() ->npc_max   ##only .5 variance (81 components)

PCA_allAND$x[,cumsum(PCA_allAND$sdev^2)/sum(PCA_allAND$sdev^2)<0.8] %>% base::NCOL() ->npc_oecd   ##only .5 variance (81 components)


expl.var <- data.frame(PC=seq(1:length(PCA_allAND$sdev)), Value=cumsum(PCA_allAND$sdev^2)/sum(PCA_allAND$sdev^2))

expl.var %>%
  ggplot(.,aes(x=PC,y=Value)) + 
  geom_line()+
  geom_point(col=pbts_cols("lightblue")) +
  geom_hline(yintercept=0.95, linetype="dashed", color = "red")+
  geom_hline(yintercept=0.8, linetype="dashed", color = pbts_cols("oecdblue"))+
  labs(title="Variance by PCA",x="Number of PCs", y = "% Variance") +
  scale_x_continuous(limits=c(0,400)) +   #200 for andorra
  scale_y_continuous(limits=c(0, 1),breaks=c(0,.25,.5,.75,.95),labels=c("0","25","50","75","95")) +
  theme_gray()+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.background = element_rect(fill="#EBEBEB")) ->expl.fig


#full sample
ResList<-list()

for(i in c(2,5)) {
  
  res <- pca_kcv(X=pca.data, ks=i)
  res<-lapply(res,colSums)
  
  ResList[[length(ResList)+1]] = res
  
}



ResList %>% 
  data.frame(.) %>% 
  cbind(.,seq(length(pca.data))) %>% 
  dplyr::rename(.,fold2=error,fold5=error.1,PC=`seq(length(pca.data))`) %>%
  tidyr::gather(.,key=PC, value=Value) -> KCVres

names(KCVres)[2] <- "Folds"


##PC that minimise PRESS
KCVres %>%
  dplyr::filter(., KCVres$Value>0 )->Col2


npc_press<-Col2$PC[Col2$Value==min(Col2$Value) & Col2$Folds=="fold5"] #PC256

#plot
KCVres %>%
  #dplyr::filter(.,Folds == "fold5") %>%
  ggplot(.,aes(x=PC,y=Value,color=factor(Folds))) + 
  geom_line()+
  geom_point() +
  geom_vline(xintercept = npc_oecd,linetype="dashed", color = pbts_cols("oecdblue"))+
  geom_vline(xintercept = npc_max,linetype="dashed", color = pbts_cols("red"))+
#  geom_vline(xintercept = npc_press,linetype="dashed", color = pbts_cols("turquoise"))+
  scale_color_pbts(palette="RdGn", reverse=TRUE)+
  theme(legend.position="bottom") +
  labs(title="K-fold Cross Validation",x="Number of PCs", y = "PRESS") +
  scale_y_continuous(limits=c(350000, 500000)) + #30,000 to 40,000 fpr andorra
  scale_x_continuous(limits=c(0,400)) + #200 for andorra
  theme_gray()+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.background = element_rect(fill="#EBEBEB"),
        plot.subtitle=element_text(face="italic"))  ->KCVfig



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
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1, alpha=0.3,
                position=position_dodge(0.05))+
  geom_point()+
  scale_color_pbts(palette="Main", labels=c("45%","55%","65%","75%","85%","95%"))+
  labs(y="Mean score in mathematics", 
       title ="Mean score in mathematics with by PCA components")+
  theme_gray()+
  theme(axis.text.x = element_text(angle = 90), 
        axis.title.x = element_blank(),
        legend.position="top",
        legend.title=element_blank(),
        legend.background = element_rect(fill="#EBEBEB"),
        plot.subtitle=element_text(face="italic")) -> MeanPCA

gridExtra::grid.arrange(KCVfig, expl.fig, ncol=2, nrow=1, widths=c(1,0.7))



