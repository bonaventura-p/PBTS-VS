# general analysis
VSscale <-function(domn, resp) {
  # PFS scale computes pl
  # Args: domn=PISA domain
  #       
  # Returns: dataframe with s
  
  # subset scored data
  resp %>%
    select(.,matches(ifelse(domn == "read",
                            "^PR\\d{4}Q\\d{2}[A-Z]*",
                            ifelse(domn == "math",
                                   "^PM\\d{4}Q\\d{2}[A-Z]*",
                                    ifelse(domn=="scie","^PS\\d{4}Q\\d{2}[A-Z]*",
                                           ""))))) -> score.input
  # 1PL TAM model
  score.input %>%
    tam(.) -> tam.input
  
  #wright map
  wright<-c("resp","nitems","maxK","AXsi","xsi","A","B","ndim","person")
  tam.input[wright] -> wright.output
  
  
  #compute sig. diff stat
  data.frame(item = rownames(tam.input$xsi), 
             tam.value = tam.input$xsi$xsi) -> tam.output
  
  # plausible values 
  tam.input %>% 
    tam.pv(.,nplausible=5) -> pv.input
  
  # rel freq, pt biserial correlation
  score.input %>% 
    tam.ctt(., pvscores = pv.input$pv,  group=NULL , progress=TRUE) -> ctt.output
  
  #MNSQ/INFIT
  tam.input %>%
    tam.fit(.)->fit.data
  
  fit.data[[1]] %>% as.data.frame(.) -> fit.output
  
 
  
  
  # final wrap-up creates list
  list("tam.output" = tam.output, "ctt.output" = ctt.output,
       "fit.output"= fit.output,  "wright.output" = wright.output) -> VSoutput 
   
  VSoutput %>% 
   return(.)
  
} 


VSDIFscale <- function(domn, resp, gender.data, gender.name) {
  
  # subset scored data
  resp %>%
    select(.,matches(ifelse(domn == "read",
                            "^PR\\d{4}Q\\d{2}[A-Z]*",
                            ifelse(domn == "math",
                                   "^PM\\d{4}Q\\d{2}[A-Z]*",
                                   ifelse(domn=="scie","^PS\\d{4}Q\\d{2}[A-Z]*",
                                          ""))))) -> score.input
  
  #DIF gender
  formulaA <- ~item+item:step+item*gender
  
  #gender variable as values
  gender<-gender.data[,gender.name] #1 female, 2 male
  
  #Set up facet variable
  facets <- as.data.frame(gender)
  
  score.input %>%
    tam.mml.mfr( ., facets= facets , formulaA = formulaA, control=list(maxiter = 500) ) ->dif.data
  
  dif.data[[5]] %>%
    as.data.frame(.) -> dif.output
  
  dif.output %>%
    return(.)
}


#wrapper #,gender.data,gender.name
VSscaleloop<-function(domains=domains,resp,dodif=FALSE,gender.data=NULL, gender.name=NULL) {
 
  ResList<-list()
  
for (domn in domains) {  
  

  VSscale(domn=domn, resp=resp)->resdomn
  
  #if true do the dif analysis
  if(dodif) { 
    
    VSDIFscale(domn,resp=resp, gender.data=gender.data, gender.name=gender.name)->dif.output
    
    resdomn[[5]]<-dif.output
    names(resdomn)[[5]]<-"dif.output"
  } 
  
  
  ResList[[length(ResList)+1]] <-resdomn
  
  names(ResList)[[length(ResList)]] <- domn
  
}
  ResList %>%
    return(.)
  
}

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
    select(., ctt.output.item, ctt.output.Categ, ctt.output.RelFreq) %>%
    mutate(., ctt.output.RelFreq = ctt.output.RelFreq*100, 
           ctt.output.item= as.character(ctt.output.item)) %>%
    spread(., key = ctt.output.Categ, value = ctt.output.RelFreq) %>% 
    mutate_at(., vars("2"), funs(replace(., is.na(.), 0))) %>%
    rename(., item=ctt.output.item) %>%
  full_join(., intl.ctt, by="item") %>%
    select(., -item)  %>%
    cor(., method = "pearson") -> table2.output
  
  
  data.frame(item.freq = c("0","1","2"), 
             corr.value = table2.output[midx])  %>%
     return(.)
  
}

#Figure 2
CorrFig<-function(ctt.data,intl.ctt) {
  
  ctt.data %>%
    select(., ctt.output.item, ctt.output.Categ, ctt.output.RelFreq) %>%
    mutate(., ctt.output.RelFreq = ctt.output.RelFreq*100, 
           ctt.output.item= as.character(ctt.output.item)) %>%
    spread(., key = ctt.output.Categ, value = ctt.output.RelFreq) %>% 
    mutate_at(., vars("2"), funs(replace(., is.na(.), 0))) %>%
    rename(., item=ctt.output.item) %>%
    full_join(., intl.ctt, by="item") -> figure2.output
  
  
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
    select(., ctt.output.item, ctt.output.Categ, ctt.output.rpb.PV) %>%
    spread(., key = ctt.output.Categ, value = ctt.output.rpb.PV) %>%
    mutate_at(.,vars("2"),funs(replace(.,is.na(.),1)))-> table3.input 
  
  
  table3.input %>%
    mutate(., crit.one = ifelse(`0` < 0, 0, 1),
           crit.two = ifelse(`0` < `1` && `1` < `2`, 0, 1),
           crit.three = ifelse(`1` > 0.2, 0, 1)) %>%
    select(., ctt.output.item, `1`, matches("^crit")) %>%
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
                  zdif = if_else(
                    ztam.value - intl.tam$ztam.value >= qnorm(0.975), 1,
                    if_else(ztam.value - intl.tam$ztam.value <= -qnorm(0.975), -1,0))) %>%
    rename(., item = tam.output.item, tam.value=tam.output.tam.value) %>%
    full_join(., intl.tam, by="item") %>%
    select(.,item, tam.value.x,tam.value.y,zdif) -> table4.output
  
  table4.output %>%
    return(.)
  
}


#####################################################
#Table 5. Dodgy items based on coefficient MNSQ:
####################################################

Infit<- function(tam.fit) {
  
  tam.fit %>%
    select(., fit.output.parameter, fit.output.Infit, fit.output.Infit_t) %>%
    mutate(., w.mnsq = if_else(fit.output.Infit > 1.2, 1, if_else(fit.output.Infit < 0.8, -1, 0))) %>%
    rename(., item=fit.output.parameter,	Infit = fit.output.Infit, Infit_t =	 fit.output.Infit_t)-> table5.output
  
  table5.output %>%
    return(.)
}

############################################
#Table 6. Dodgy items based on DIF criteria
############################################

DIFitem<-function(DIF.mod) {
  
  #DIF.length <- dim(DIF.mod)
  
  DIF.mod %>%  
    select(., dif.output.item, dif.output.xsi.item) %>%
    mutate(., gender=substr(dif.output.item,17,18),dif.output.item=substr(dif.output.item,1,9)) %>%
    spread(., key = gender, value = dif.output.xsi.item) %>%
    mutate(., gdif = if_else(`2` - `1` > 0.25, 1, if_else(`2` - `1` < -0.25, -1, 0))) %>%
    rename(., item = dif.output.item) -> table6.output #difference as boys-girls
  
  table6.output %>%
    return(.)
}

############################
#wright map
############################

#"resp","nitems","maxK","AXsi","xsi","A","B","ndim","person"

WrightmapElms<-function(wright.mod) {
  

  wright.mod %>%
    tam.threshold(., prob.lvl = 0.5) %>% 
    as.data.frame(.) %>%
    mutate(., item=rownames(.)) %>%
    gather(., key=Cat, value=tam.value, Cat1:Cat2, factor_key=TRUE) %>%
    mutate(., item = paste(item,Cat,sep="_")) -> item.est
  
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

################################################
#Technical functions
################################################

###########
#intl freq
########

IntlFreq<-function(domn) {
  #international data sets
  "intltam.data.xlsx" %>%
    read.xlsx(., colNames=TRUE,sheet="Freq") %>%
    as.data.frame(.) -> intl.freq 
  
  if(domn=="math") {
    intl.freq %>%
      filter(.,grepl("^PM\\d{4}Q\\d{2}.?$", item)) -> freq.output
  } 
  
  else if(domn=="scie") {
    intl.freq %>%
      filter(.,grepl("^PS\\d{4}Q\\d{2}.?$", item)) -> freq.output
    
  } 
  
  else  if(domn=="read") {
    intl.freq %>%
      filter(.,grepl("^PR\\d{4}Q\\d{2}.?$", item)) -> freq.output
  }
  
  freq.output %>%
    return(.)
  
}


###########
#intl diff
########

IntlDiff<-function(domn) {
  #international data sets
  "intltam.data.xlsx" %>%
    read.xlsx(., colNames=TRUE,sheet="Diff") %>%
    as.data.frame(.) -> intl.diff

  
  if(domn=="math") {
    intl.diff %>%
      filter(.,grepl("^PM\\d{4}Q\\d{2}.*", item)) -> diff.output
  } 
  
  else if(domn=="scie") {
    intl.diff %>%
      filter(.,grepl("^PS\\d{4}Q\\d{2}.*", item)) -> diff.output
    
  } 
  
  else  if(domn=="read") {
    intl.diff %>%
      filter(.,grepl("^PR\\d{4}Q\\d{2}.*", item)) -> diff.output
  }
  
  diff.output %>%
    return(.)
  
}


##############################
# Primary analysis functions
##################################


PFSscaleloop<-function(domains,resp,stu.data,raw.data,pca.data,kill.item) {
  
  # drop dodgy items in scored data
  resp %<>%  .[,!(names(.) %in% kill.item)]
  
  #keeping the items in raw dataset
  
  # define aux data frame
  aux <- select(stu.data,matches("^W_FSTUWT$|^W_FSTR\\d+$|^stidsch$|^stidstd$"))
  
  #  get direct regressors
  direct.regs <- DirectRegs(stu.data=stu.data, raw.data=raw.data) 
  
  # do pca
  pca.res  <-  PcaComp(pca.data=pca.data) 
  
  # join pca direct regressors
  direct.regs %<>% cbind(.,pca.res)
  
  
  Reslist <- list()
  
  for(domn in domains) {

    # get item parameters
    intl.tam <- IntlDiff(domn) %>% rename(., item.name=item)
    
    xsi.fixed <- AnchorValues(domn=domn, score.data=resp, item.data=intl.tam) 

    # do scaling
    res <- PFSscale(domn=domn, resp=resp, Y=direct.regs, xsi.fixed=xsi.fixed, aux=aux) 
    
    Reslist[[length(Reslist)+1]] <- res
    names(Reslist)[[length(Reslist)]] <- domn
  }
  
  Reslist %>%
    return(.)
}


######################
## scaling functions###



#multiple columns spread
spread_n <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  
  # break value vector into quotes/value is c("","",.)
  valueq <- rlang::enquo(value)
  
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

#color palette
pbts_colors <- c(
  `red`         = "#d04432",
  `orange`      = "#e5b921",
  `lightgreen`  = "#c6da50",
  `turquoise`   = "#00ae88",
  `lightblue`   = "#0076a0",
  `oecdblue`    = "#005581",
  `dustygrey`   = "#999999",
  `tundora`     = "#4D4D4D")

pbts_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (pbts_colors)
  
  pbts_colors[cols]
}

pbts_palettes <- list(
  
  `Main`  = pbts_cols("red", "orange", "lightgreen","turquoise","lightblue","oecdblue"),
  
  `RdGn`  = pbts_cols("red", "orange", "lightgreen","turquoise"),
  
  `GnBe`   = pbts_cols("turquoise","lightblue", "oecdblue"),
  
  `GyBe`  = pbts_cols("dustygrey", "oecdblue"),
  
  `Grey`  = pbts_cols("dustygrey", "tundora")
)

pbts_pal <- function(palette = "main", reverse = FALSE, ...) {
  # This returned function will interpolate the palette colors for a certain number of levels, 
  # making it possible to create shades between our original colors. 
  pal <- pbts_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_color_pbts <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- pbts_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("pbts_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_pbts <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- pbts_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("pbts_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#please add documentation
WrightMap.sim.PV <- function( object, ndim ){
  person <- object$person
  N <- nrow(person)
  if (ndim==1){
    pers.est <- stats::rnorm( N, mean=person$EAP, sd=person$SD.EAP )
  }
  if (ndim>1){
    pers.est <- matrix( 0, nrow=N, ncol=ndim)
    for (dd in 1:ndim){
      pers.est[,dd] <- stats::rnorm( N, mean=person[,paste0("EAP.Dim",dd)],
                                     sd=person[,paste0("SD.EAP.Dim",dd)] )
    }
  }
  return(pers.est)
}



#Functions
PisaRescale <- function(x,domn) {
  # PisaRescale convers logit scale from TAM x into PISA scale for selected domn
  # Args: x= data frame from TAM logit scale with pvs
  #     domn= domain as "read,domn,scie"
  # Returns: a data fr
  if (domn=="math") {
    ((x+0.1344)/1.2838)*100+500 %>% 
      return(.)
  }
  
  else if(domn=="scie") {
    ((x-0.1797)/1.0724)*100+500 %>%
      return(.)
  }
  
  else if(domn=="read") {
    ((0.883*(x)-0.4837)/1.1002)*100+500 %>%
      return(.)
  }
}

#mode function
FunMode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

AnchorValues<-function(domn,score.data, item.data) {
  # Anchor Values creates a data.frame with item position and parameter of the correct size given domn/country
  # Args: domn=domain in "read","math","scie" 
  #   score.data =scored dataset, 
  #   item.data= tam parameters file, names item.name and tam.value
  #  Returns: a dataframe with item position and item parameter
  
  
  item.data %>%
    mutate(.,item.code = stringr::str_extract(string = item.name,
                                               pattern =  ifelse(domn == "math", "^PM\\d{4}Q\\d{2}[A-Z]*", 
                                                          ifelse(domn=="scie","^PS\\d{4}Q\\d{2}[A-Z]*",
                                                          ifelse(domn=="read","^PR\\d{4}Q\\d{2}[A-Z]*",""))))) %>%
    mutate(., id = match(item.code, names(score.data))) %>%
    arrange(., id) %>%
    filter(., !(is.na(id))) %>%
    mutate(., id = 1:nrow(.)) %>% 
    dplyr::rename(., tam=tam.value) %>%
    select(., id, tam) %>%
    return(.)
  
  
}

PcaComp  <-  function(pca.data,pctvar=0.95) {
  # PcaComp computes PCA components for a given input file and stores pctvar% components
  # Args: pca.data= data frame with the input variables, in case of PFS all dummified stud items+age
  #       pctvar= the threshold of % of explained variance required 
  # Returns: a data frame with the PC components up to pctvar%
  
  set.seed(1992) #replicable results
  
  pca.data %>%
    select(.,matches("^age|^AP")) %>%
    prcomp(., retx = TRUE, center = TRUE, scale. = TRUE)-> pca.res
  
  pca.res$x[, cumsum(pca.res$sdev^2)/sum(pca.res$sdev^2) < pctvar] %>%
    as.data.frame(.) %>%
    return(.)
}

DirectRegs <- function(stu.data, raw.data) {
  # DirectRegs creates a dataset for the direct regressors of the IRT model
  # Args: stu.data=student questionnaire data, e.g. gold_data
  #       raw.data=raw scores dataset, for computing % not reached
  # Returns: a data frame with direct regressors: Grade,Gender,HISEI, Booklet dummies and school dummies
  
  matrix(ncol=0,nrow=raster::nrow(raw.data)) %>%  as.data.frame(.) -> direct.regs
  
  # booklet structure defined and joined to main data frame 
  cbind(c(1,2,3,4,5,6,7),c(57,61,55,63,63,65,59)) %>% 
    as.data.frame() %>% 
    dplyr::rename(., bookid= V1, nitems= V2 ) %>% 
    plyr::join(raw.data, ., by='bookid', type = "left", match = "all") -> raw.data
  
  # rowsums of notreached divided by total number of items in the booklet
  (rowSums(select(raw.data,matches("^P[RSM]\\d{4}Q\\d{2}.?$"))=="r" |
             select(raw.data,matches("^P[RSM]\\d{4}Q\\d{2}.?$"))=="rr" |
             select(raw.data,matches("^P[RSM]\\d{4}Q\\d{2}.?$"))=="rrr" |
             select(raw.data,matches("^P[RSM]\\d{4}Q\\d{2}.?$"))=="rrrr" |
             select(raw.data,matches("^P[RSM]\\d{4}Q\\d{2}.?$"))=="rrrrr")
  )/raw.data$nitems -> direct.regs$not.reached
  
  # getting grade gender (recoded as 0 1), hisei and bookid  and renaming them
  stu.data %>%
    select(.,matches("ST001Q01_15|ST004Q01_15|HISEI|bookid|stidsch")) %>%
    dplyr::rename(., grade=ST001Q01_15, gender=ST004Q01_15,hisei=HISEI) %>%
    mutate(., gender=gender-1) %>%
    cbind(., direct.regs) -> direct.regs
  
  # creating deviation contrast coded dummies for booklets 
  for (i in 1:6) {
    newi <- paste("B", i, sep="")
    
    direct.regs$booki=case_when(
      direct.regs$bookid != i~ 0,
      direct.regs$bookid== i ~ 1)
    
    # replacing to -1 if booklet 7
    direct.regs[direct.regs$bookid==7, ] %<>% mutate_at(., vars(matches('^booki$')), ~-1)
    
    names(direct.regs)[names(direct.regs)=='booki'] <- newi
    rm(newi)
  }
  
  # creating school dummies with -1 for the largest one
  direct.regs %<>% fastDummies::dummy_cols(., select_columns = "stidsch")
  
  # replace to -1
  direct.regs[direct.regs$stidsch ==FunMode(direct.regs[, 'stidsch']),] %<>% mutate_at(., vars(matches('^stidsch_')), ~-1)
  
  # drop dummy of the largest one, in principle dummy cols has remove_most_frequent_dummy option
  direct.regs %<>% .[, !(colnames(.)==paste('stidsch_', FunMode(.[, 'stidsch']), sep=''))]
  
  # final wrap up
  direct.regs %>%
    .[!(names(.) %in% c("bookid", "stidsch"))] %>%
    sapply(., as.numeric) %>%
    as.data.frame(.) %>%
    return(.)
}

PFSscale <-function(domn, resp, Y, xsi.fixed, aux) {
  # PFS scale computes plausible values for given domain using PBTS model 
  # and merges aux variables(weights etc) for secondary analysis
  # Args: domn=PISA domain
  #       resp=scored data
  #       Y= direct regressors
  #       xsi.fixed= item parameters
  #       aux= auxiliary variables DataFrame
  # Returns: dataframe with pv for domain and aux variables
  
  # domn must be a string e.g. domn<-"read"

  set.seed(1992)
  
  # marginal model for 1PL
  resp %>%
    select(.,matches(ifelse(domn=="read","^PR\\d{4}Q\\d{2}.?$",
                            ifelse(domn=="math","^PM\\d{4}Q\\d{2}.?$",
                                   ifelse(domn=="scie","^PS\\d{4}Q\\d{2}.?$",""))))) %>%
    tam(., xsi.fixed = xsi.fixed, Y = Y, pid=NULL) -> marginal.model

  # computing plausible values
  plausible.values <- tam.pv(marginal.model, nplausible = 5)
  
  # rename plausible values
  pv.names <- paste('PV', 1:5, toupper(domn), sep='') 
  
  # final wrap-up
  cbind(PisaRescale(x=plausible.values$pv[2:6],domn=domn), aux) %>%
    setnames(., old = c('PV1.Dim1','PV2.Dim1', 'PV3.Dim1', 'PV4.Dim1', 'PV5.Dim1'), new = pv.names) %>%
    as.data.frame(.) %>%
    return(.)
  
} 

