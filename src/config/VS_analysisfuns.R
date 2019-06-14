# VS_analysisfuns.R
# Bonaventura Pacileo
# This program defines the functions that perform all the analysis

######################
## WrightMap.sim.PV##
##########################
WrightMap.sim.PV <- function( object, ndim ){
  # WrightMap.sim.PV creates a data.frame with PV person estimates from a distribution of mean equal to EAP
  # Args: object= TAM object with "resp","nitems","maxK","AXsi","xsi","A","B","ndim","person" 
  #  ndim = 1 for 1PL
  #  Returns: a dataframe with PV person estimates
  
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

######################
## AnchorValues ##
##########################

AnchorValues<-function(domn,score.data, item.data) {
  # Anchor Values creates a data.frame with item position and parameter of the correct size given domn/country
  # Args: domn=domain in "read","math","scie" 
  #   score.data =scored dataset, 
  #   item.data= tam parameters file, names item.name and tam.value
  #  Returns: a dataframe with item position and item parameter
  
  
  item.data %>%
    dplyr::mutate(.,item.code = stringr::str_extract(string = item.name,
                                               pattern =  ifelse(domn == "math", "^PM\\d{4}Q\\d{2}[A-Z]*", 
                                                          ifelse(domn=="scie","^PS\\d{4}Q\\d{2}[A-Z]*",
                                                          ifelse(domn=="read","^PR\\d{4}Q\\d{2}[A-Z]*",""))))) %>%
    dplyr::mutate(., id = match(item.code, names(score.data))) %>%
    dplyr::arrange(., id) %>%
    dplyr::filter(., !(is.na(id))) %>%
    dplyr::mutate(., id = 1:nrow(.)) %>% 
    dplyr::rename(., tam=tam.value) %>%
    dplyr::select(., id, tam) %>%
    return(.)
  
  
}

######################
## PcaComp ##
##########################
PcaComp  <-  function(pca.data,pctvar=0.95) {
  # PcaComp computes PCA components for a given input file and stores pctvar% components
  # Args: pca.data= data frame with the input variables, in case of PFS all dummified stud items+age
  #       pctvar= the threshold of % of explained variance required 
  # Returns: a data frame with the PC components up to pctvar%
  
  set.seed(1992) #replicable results
  
  pca.data %>%
    dplyr::select(.,dplyr::matches("^age|^AP")) %>%
    stats::prcomp(., retx = TRUE, center = TRUE, scale. = TRUE)-> pca.res
  
  pca.res$x[, cumsum(pca.res$sdev^2)/sum(pca.res$sdev^2) < pctvar] %>%
    as.data.frame(.) %>%
    return(.)
}

######################
## DirectRegs ##
##########################
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
  (rowSums(dplyr::select(raw.data,dplyr::matches("^P[RSM]\\d{4}Q\\d{2}.?$"))=="r" |
             dplyr::select(raw.data,dplyr::matches("^P[RSM]\\d{4}Q\\d{2}.?$"))=="rr" |
             dplyr::select(raw.data,dplyr::matches("^P[RSM]\\d{4}Q\\d{2}.?$"))=="rrr" |
             dplyr::select(raw.data,dplyr::matches("^P[RSM]\\d{4}Q\\d{2}.?$"))=="rrrr" |
             dplyr::select(raw.data,dplyr::matches("^P[RSM]\\d{4}Q\\d{2}.?$"))=="rrrrr")
  )/raw.data$nitems -> direct.regs$not.reached
  
  # getting grade gender (recoded as 0 1), hisei and bookid  and renaming them
  stu.data %>%
    dplyr::select(.,dplyr::matches("ST001Q01_15|ST004Q01_15|HISEI|bookid|stidsch")) %>%
    dplyr::rename(., grade=ST001Q01_15, gender=ST004Q01_15,hisei=HISEI) %>%
    dplyr::mutate(., gender=gender-1) %>%
    cbind(., direct.regs) -> direct.regs
  
  # creating deviation contrast coded dummies for booklets 
  for (i in 1:6) {
    newi <- paste("B", i, sep="")
    
    direct.regs$booki=dplyr::case_when(
      direct.regs$bookid != i~ 0,
      direct.regs$bookid== i ~ 1)
    
    # replacing to -1 if booklet 7
    direct.regs[direct.regs$bookid==7, ] %<>% dplyr::mutate_at(., dplyr::vars(dplyr::matches('^booki$')), ~-1)
    
    names(direct.regs)[names(direct.regs)=='booki'] <- newi
    rm(newi)
  }
  
  # creating school dummies with -1 for the largest one
  direct.regs %<>% fastDummies::dummy_cols(., select_columns = "stidsch")
  
  # replace to -1
  direct.regs[direct.regs$stidsch ==FunMode(direct.regs[, 'stidsch']),] %<>% dplyr::mutate_at(., dplyr::vars(dplyr::matches('^stidsch_')), ~-1)
  
  # drop dummy of the largest one, in principle dummy cols has remove_most_frequent_dummy option
  direct.regs %<>% .[, !(colnames(.)==paste('stidsch_', FunMode(.[, 'stidsch']), sep=''))]
  
  # final wrap up
  direct.regs %>%
    .[!(names(.) %in% c("bookid", "stidsch"))] %>%
    sapply(., as.numeric) %>%
    as.data.frame(.) %>%
    return(.)
}

######################
## PFSscale ##
##########################
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
    dplyr::select(.,dplyr::matches(ifelse(domn=="read","^PR\\d{4}Q\\d{2}.?$",
                            ifelse(domn=="math","^PM\\d{4}Q\\d{2}.?$",
                                   ifelse(domn=="scie","^PS\\d{4}Q\\d{2}.?$",""))))) %>%
    TAM::tam(., xsi.fixed = xsi.fixed, Y = Y, pid=NULL) -> marginal.model

  # computing plausible values
  plausible.values <- TAM::tam.pv(marginal.model, nplausible = 5)
  
  # rename plausible values
  pv.names <- paste('PV', 1:5, toupper(domn), sep='') 
  
  # final wrap-up
  cbind(PisaRescale(x=plausible.values$pv[2:6],domn=domn), aux) %>%
    data.table::setnames(., old = c('PV1.Dim1','PV2.Dim1', 'PV3.Dim1', 'PV4.Dim1', 'PV5.Dim1'), new = pv.names) %>%
    as.data.frame(.) %>%
    return(.)
  
} 


######################
## VSscale ##
##########################
VSscale <-function(domn, resp) {
  # VSscale computes xsi, fit, wright,  pv and pt bis correlation  returns them as a list 
  # Args: domn=PISA domain
  #       resp=scored data
  # Returns: list with xsi estimates, fit, wright,  pv and pt bis correlation 
  
  # subset scored data
  resp %>%
    dplyr::select(.,dplyr::matches(ifelse(domn == "read",
                                          "^PR\\d{4}Q\\d{2}[A-Z]*",
                                          ifelse(domn == "math",
                                                 "^PM\\d{4}Q\\d{2}[A-Z]*",
                                                 ifelse(domn=="scie","^PS\\d{4}Q\\d{2}[A-Z]*",
                                                        ""))))) -> score.input
  # 1PL TAM model
  score.input %>%
    TAM::tam(.) -> tam.input
  
  #wright map
  wright<-c("resp","nitems","maxK","AXsi","xsi","A","B","ndim","person")
  tam.input[wright] -> wright.output
  
  
  #compute sig. diff stat
  data.frame(item = rownames(tam.input$xsi), 
             tam.value = tam.input$xsi$xsi) -> tam.output
  
  # plausible values 
  tam.input %>% 
    TAM::tam.pv(.,nplausible=5) -> pv.input
  
  # rel freq, pt biserial correlation
  score.input %>% 
    TAM::tam.ctt(., pvscores = pv.input$pv,  group=NULL , progress=TRUE) -> ctt.output
  
  #MNSQ/INFIT
  tam.input %>%
    TAM::tam.fit(.)->fit.data
  
  fit.data[[1]] %>% as.data.frame(.) -> fit.output
  
  
  
  
  # final wrap-up creates list
  list("tam.output" = tam.output, "ctt.output" = ctt.output,
       "fit.output"= fit.output,  "wright.output" = wright.output) -> VSoutput 
  
  VSoutput %>% 
    return(.)
  
} 

######################
## VSDIFscale ##
##########################
VSDIFscale <- function(domn, resp, gender.data, gender.name) {
  # VSDIFscale computes gender DIF
  # Args: domn=PISA domain
  #       resp=scored data
  #       gender.data= data frame with gender variable
  #       gender.name= variable name for gender
  # Returns: DIF parameter estimates (i.e. interaction term)
  
  # subset scored data
  resp %>%
    dplyr::select(.,dplyr::matches(ifelse(domn == "read",
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
    TAM::tam.mml.mfr( ., facets= facets , formulaA = formulaA, control=list(maxiter = 500) ) ->dif.data
  
  dif.data[[5]] %>%
    as.data.frame(.) -> dif.output
  
  dif.output %>%
    return(.)
}

