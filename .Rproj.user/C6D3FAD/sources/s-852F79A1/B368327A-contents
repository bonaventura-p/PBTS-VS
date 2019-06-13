


################################################
#Technical functions
################################################


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



##############################
# Primary analysis functions
##################################


PFSscaleloop<-function(domains,resp,stu.data,raw.data,pca.data,kill.item) {
  
  # drop dodgy items in scored data
  resp %<>%  .[,!(names(.) %in% kill.item)]
  
  #keeping the items in raw dataset
  
  # define aux data frame
  aux <- dplyr::select(stu.data,dplyr::matches("^W_FSTUWT$|^W_FSTR\\d+$|^stidsch$|^stidstd$"))
  
  #  get direct regressors
  direct.regs <- DirectRegs(stu.data=stu.data, raw.data=raw.data) 
  
  # do pca
  pca.res  <-  PcaComp(pca.data=pca.data) 
  
  # join pca direct regressors
  direct.regs %<>% cbind(.,pca.res)
  
  
  Reslist <- list()
  
  for(domn in domains) {
    
    # get item parameters
    intl.tam <- IntlDiff(domn) %>% dplyr::rename(., item.name=item)
    
    xsi.fixed <- AnchorValues(domn=domn, score.data=resp, item.data=intl.tam) 
    
    # do scaling
    res <- PFSscale(domn=domn, resp=resp, Y=direct.regs, xsi.fixed=xsi.fixed, aux=aux) 
    
    Reslist[[length(Reslist)+1]] <- res
    names(Reslist)[[length(Reslist)]] <- domn
  }
  
  Reslist %>%
    return(.)
}













###########
#intl freq
########

IntlFreq<-function(domn) {
  #international data sets
  "intltam.data.xlsx" %>%
    openxlsx::read.xlsx(., colNames=TRUE,sheet="Freq") %>%
    as.data.frame(.) -> intl.freq 
  
  if(domn=="math") {
    intl.freq %>%
      dplyr::filter(.,grepl("^PM\\d{4}Q\\d{2}.?$", item)) -> freq.output
  } 
  
  else if(domn=="scie") {
    intl.freq %>%
      dplyr::filter(.,grepl("^PS\\d{4}Q\\d{2}.?$", item)) -> freq.output
    
  } 
  
  else  if(domn=="read") {
    intl.freq %>%
      dplyr::filter(.,grepl("^PR\\d{4}Q\\d{2}.?$", item)) -> freq.output
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
    openxlsx::read.xlsx(., colNames=TRUE,sheet="Diff") %>%
    as.data.frame(.) -> intl.diff
  
  
  if(domn=="math") {
    intl.diff %>%
      dplyr::filter(.,grepl("^PM\\d{4}Q\\d{2}.*", item)) -> diff.output
  } 
  
  else if(domn=="scie") {
    intl.diff %>%
      dplyr::filter(.,grepl("^PS\\d{4}Q\\d{2}.*", item)) -> diff.output
    
  } 
  
  else  if(domn=="read") {
    intl.diff %>%
      dplyr::filter(.,grepl("^PR\\d{4}Q\\d{2}.*", item)) -> diff.output
  }
  
  diff.output %>%
    return(.)
  
}



#multiple columns spread
spread_n <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  
  # break value vector into quotes/value is c("","",.)
  valueq <- rlang::enquo(value)
  
  s <- rlang::quos(!!valueq)
  df %>% tidyr::gather(variable, value, !!!s) %>%
    tidyr::unite(temp, !!keyq, variable) %>%
    tidyr::spread(temp, value)
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

