
#table creation



##############################################
#Table 1. Students’ sample: grade gender
##############################################

#please add

#################################################################
#Table 2. Correlation between score frequencies 0,1,2 pilot 
####################################################################

Correl<-function(ctt.data,intl.ctt) {
  
  midx <- rbind( c(4,1), c(5,2), c(6,3))  
  
  ctt.data %>%
    dplyr::select(., ctt.output.item, ctt.output.Categ, ctt.output.RelFreq) %>%
    dplyr::mutate(., ctt.output.RelFreq = ctt.output.RelFreq*100, 
                  ctt.output.item= as.character(ctt.output.item)) %>%
    tidyr::spread(., key = ctt.output.Categ, value = ctt.output.RelFreq) %>% 
    dplyr::mutate_at(., dplyr::vars("2"), dplyr::funs(replace(., is.na(.), 0))) %>%
    dplyr::rename(., item=ctt.output.item) %>%
    dplyr::full_join(., intl.ctt, by="item") %>%
    dplyr::select(., -item)  %>%
    stats::cor(., method = "pearson") -> table2.output
  
  
  data.frame(item.freq = c("0","1","2"), 
             corr.value = table2.output[midx])  %>%
    return(.)
  
}

#Figure 2
CorrFig<-function(ctt.data,intl.ctt) {
  
  ctt.data %>%
    dplyr::select(., ctt.output.item, ctt.output.Categ, ctt.output.RelFreq) %>%
    dplyr::mutate(., ctt.output.RelFreq = ctt.output.RelFreq*100, 
                  ctt.output.item= as.character(ctt.output.item)) %>%
    tidyr::spread(., key = ctt.output.Categ, value = ctt.output.RelFreq) %>% 
    dplyr::mutate_at(., dplyr::vars("2"), dplyr::funs(replace(., is.na(.), 0))) %>%
    dplyr::rename(., item=ctt.output.item) %>%
    dplyr::full_join(., intl.ctt, by="item") -> figure2.output
  
  
  figure2.output  %>%
    return(.)
  
}



######################################################################
#Table 3 Dodgy items based on biserial-point values - criteria #1
####################################################################


#Criteria #1: Categories 0 must have a discrimination biserial-point index negative.
#Criteria #2: The discrimination biserial-point for a partial credit item must be ordered
#Criteria #3: Discrimination of the correct answer must be greater than 0,2. 

PtBis <- function(ctt.data) {
  
  ctt.data %>%
    dplyr::select(., ctt.output.item, ctt.output.Categ, ctt.output.rpb.PV) %>%
    tidyr::spread(., key = ctt.output.Categ, value = ctt.output.rpb.PV) %>%
    dplyr::mutate_at(.,dplyr::vars("2"),dplyr::funs(replace(.,is.na(.),1)))-> table3.input 
  
  
  table3.input %>%
    dplyr::mutate(., crit.one = ifelse(`0` < 0, 0, 1),
                  crit.two = ifelse(`0` < `1` && `1` < `2`, 0, 1),
                  crit.three = ifelse(`1` > 0.2, 0, 1)) %>%
    dplyr::select(., ctt.output.item, `1`, dplyr::matches("^crit")) %>%
    dplyr::rename(., discr=`1`, item = ctt.output.item) -> table3.output
  
  table3.output %>%
    return(.)
}


#####################################
# Table 4. Dodgy items based on difficulty differences
###################################

DiffItem<-function(tam.mod,intl.tam) {
  
  tam.mod %>%
    dplyr::mutate(., ztam.value = (tam.output.tam.value-mean(tam.output.tam.value))/sd(tam.output.tam.value),
                  zdif = dplyr::if_else(
                    ztam.value - intl.tam$ztam.value >= qnorm(0.975), 1,
                    dplyr::if_else(ztam.value - intl.tam$ztam.value <= -qnorm(0.975), -1,0))) %>%
    dplyr::rename(., item = tam.output.item, tam.value=tam.output.tam.value) %>%
    dplyr::full_join(., intl.tam, by="item") %>%
    dplyr::select(.,item, tam.value.x,tam.value.y,zdif) -> table4.output
  
  table4.output %>%
    return(.)
  
}


#####################################################
#Table 5. Dodgy items based on coefficient MNSQ:
####################################################

Infit<- function(tam.fit) {
  
  tam.fit %>%
    dplyr::select(., fit.output.parameter, fit.output.Infit, fit.output.Infit_t) %>%
    dplyr::mutate(., w.mnsq = dplyr::if_else(fit.output.Infit > 1.2, 1, dplyr::if_else(fit.output.Infit < 0.8, -1, 0))) %>%
    dplyr::rename(., item=fit.output.parameter,	Infit = fit.output.Infit, Infit_t =	 fit.output.Infit_t)-> table5.output
  
  table5.output %>%
    return(.)
}

############################################
#Table 6. Dodgy items based on DIF criteria
############################################

DIFitem<-function(DIF.mod) {
  
  #DIF.length <- dim(DIF.mod)
  DIF.mod %>%  
    dplyr::select(., dif.output.item, dif.output.xsi.item, dif.output.intxsi.item) %>%
    dplyr::mutate(., gender=substr(dif.output.item,17,18),dif.output.item=substr(dif.output.item,1,9)) %>%
    spread_n(., key = gender, value = c(dif.output.xsi.item,dif.output.intxsi.item)) %>%
    dplyr::mutate(., gdif = dplyr::if_else(`2_dif.output.intxsi.item` > 0.25, 1, dplyr::if_else(`2_dif.output.intxsi.item` < -0.25, -1, 0))) %>%
    dplyr::rename(., item = dif.output.item) %>%
    dplyr::select(., item, `1_dif.output.xsi.item`, `2_dif.output.xsi.item`, gdif)-> table6.output #difference as boys-girls
  
  table6.output %>%
    return(.)
}

############################
#Figure 7 wright map
############################

#"resp","nitems","maxK","AXsi","xsi","A","B","ndim","person"

WrightmapElms<-function(wright.mod) {
  
  
  wright.mod %>%
    TAM::tam.threshold(., prob.lvl = 0.5) %>% 
    as.data.frame(.) %>%
    dplyr::mutate(., item=rownames(.)) %>%
    tidyr::gather(., key=Cat, value=tam.value, Cat1:Cat2, factor_key=TRUE) %>%
    dplyr::mutate(., item = paste(item,Cat,sep="_")) -> item.est
  
  wright.mod %>%
    WrightMap.sim.PV(.,ndim=1) %>% #check when changing to 2PL, better to use wright.mod$ndim
    as.data.frame(.) %>%
    dplyr::rename(., theta=`.`) -> pers.est
  
  
  lim.x.min <- min(c(pers.est$theta, item.est$tam.value),na.rm=TRUE) +0.3
  lim.x.max <- max(c(pers.est$theta, item.est$tam.value),na.rm=TRUE) + 0.3
  
  # final wrap-up creates list
  list("item.est" = item.est, "pers.est" = pers.est,
       "lim.x.min"= lim.x.min, "lim.x.max" = lim.x.max) -> WMoutput 
  # Theoretically, when candidates and items are opposite each other on the map, the difficulty of the item.est
  # and the ability of the candidate are comparable, so the candidate has approximately a 50% probability
  # of answering the item correctly.  
  WMoutput %>% 
    return(.)
  
}
