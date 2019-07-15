# PBTS-VS_techfuns.R
# Bonaventura Pacileo
# This program defines the functions auxiliary to the analysis and to the dashboard


################################################
#Technical functions
################################################

PrItem <- function(xsi,theta) {
  
  1 / (1 + exp(-1 * (theta - xsi)))
  
} 


PrTest <- function(xsi, theta) {
  J <- length(xsi)
  for (j in 1:J) {
    P <- 1 / (1 + exp(-1*(theta-xsi[j])))
  }
  P
}

######################
## VSscaleloop##
##########################
VSscaleloop<-function(domains=domains,resp,dodif=FALSE,gender.data=NULL, gender.name=NULL,kill.item=NULL) {
  # VSscaleloop is a loop function to run VSscale and VSDIFscale across domains
  # Args: domains= PISA domains
  #         resp = scored.data
  #         dodif= boolean for DIF
  #   gender.data= data frame with gender data
  #   gender.name= variable name for gender
  #   kill.item= item name to drop from resp
  #  Returns: a list with 2 levels, first domain, second analysis output 

  #  , kill.item
  
  # drop dodgy items in scored data
  resp %<>%  .[,!(names(.) %in% kill.item)]
  
  ResList<-list()
  
  for (domn in domains) {  
    
    
    VSscale(domn=domn, resp=resp)->resdomn
    
    #if true do the dif analysis
    if(dodif) { 
      
      VSDIFscale(domn,resp=resp, gender.data=gender.data, gender.name=gender.name)->dif.output
      
      resdomn[[length(resdomn)+1]]<-dif.output
      names(resdomn)[[length(resdomn)]]<-"dif.output"
    } 
    
    
    ResList[[length(ResList)+1]] <-resdomn
    
    names(ResList)[[length(ResList)]] <- domn
    
  }
  ResList %>%
    return(.)
  
}



######################
## PFSscaleloop##
##########################
PFSscaleloop<-function(domains,resp,stu.data,raw.data,pca.data) {
  # PFSscaleloop is a loop and wrapper function to compute cognitive scales across domains
  # Args: domains= PISA domains
  #         resp = scored.data
  #         stu.data= questionnaire data
  #   raw.data= data frame with not reached variable
  #   pca.data= data frame with PCA dummy input
  #  Returns: a list with results by domains 
  

  #keeping the items in raw dataset
  
  # define aux data frame
  aux <- dplyr::select(stu.data,dplyr::matches("^W_FSTUWT$|^W_FSTR\\d+$|^stidsch$|^stidstd$|ST004Q01_15"))
  
  #  get direct regressors
  direct.regs <- DirectRegs(stu.data=stu.data, raw.data=raw.data) 
  
  # do pca
  pca.res  <-  PcaComp(pca.data=pca.data) 
  
  # join pca direct regressors
  direct.regs %<>% cbind(.,pca.res)
  
  
  Reslist <- list()
  
  for(domn in domains) {
    
    # get item parameters
    #getting international parameters (for now slopes Are all fixed to 1)
    IntlPars(domn,"diff") %>%
      AnchorValues(domn=domn, score.data = resp, item.data = ., irtpar = "diff") %>%
      data.matrix(.) -> xsi.fixed
    
    IntlPars(domn,"slope") %>%
      AnchorValues(domn=domn, score.data = resp, item.data = ., irtpar = "slope") %>%
      data.matrix(.) -> B
    
    #dim input for tam.mml
    dim(B)[3] <- 1
    
    # do scaling
    res <- PFSscale(domn=domn, resp=resp, Y=direct.regs, xsi.fixed=xsi.fixed, B=B, aux=aux) 
    
    Reslist[[length(Reslist)+1]] <- res
    names(Reslist)[[length(Reslist)]] <- domn
  }
  
  Reslist %>%
    return(.)
}



######################
## PisaRescale##
##########################
PisaRescale <- function(x,domn,trans="mean") {
  # PisaRescale convers logit scale from TAM x into PISA scale for selected domn
  # Args: x= data frame from TAM logit scale with pvs
  #     domn= domain as "read,domn,scie"
  #       trans= transforming mean or sd
  # Returns: a data frame with TAM estimates on a PISA scale
 

         if (domn=="math" & trans == "mean") {
          ((x+0.1344)/1.2838)*100+500 %>% 
            return(.)
        }
        
        else if(domn=="scie" & trans == "mean") {
          ((x-0.1797)/1.0724)*100+500 %>%
            return(.)
        }
        
        else if(domn=="read" & trans == "mean") {
          ((0.883*(x)-0.4837)/1.1002)*100+500 %>%
            return(.)
          }
    
  else if (domn=="math" & trans == "sd") {
      x*77.89 %>% 
        return(.)
    }
    
    else if(domn=="scie" & trans == "sd") {
      x*93.25 %>%
        return(.)
    }
    
    else if(domn=="read" & trans == "sd") {
      x*80.26 %>%
        return(.)
    }
  
  
  
}

######################
## IntlPars##
##########################

IntlPars<-function(domn, pars) {
  # IntlPars loads international item parameters by domain
  # Args:  domn= domain as "read,domn,scie"
  #         pars= freq, diff or slope
  # Returns: a data frame with international item frequencies or IRT parameters
  
  
  #international data sets
  intlpars.path <- paste(wd, "data", "intltam.data.xlsx", sep="/") 
  
  intlpars.path %>%
    openxlsx::read.xlsx(., colNames=TRUE,sheet=pars) %>%
    as.data.frame(.) -> intl.pars
  
  if(domn=="math") {
    intl.pars %>%
      dplyr::filter(.,grepl("^PM\\d{4}Q\\d{2}.*", item)) -> intl.output
  } 
  
  else if(domn=="scie") {
    intl.pars %>%
      dplyr::filter(.,grepl("^PS\\d{4}Q\\d{2}.*", item)) -> intl.output
    
  } 
  
  else  if(domn=="read") {
    intl.pars %>%
      dplyr::filter(.,grepl("^PR\\d{4}Q\\d{2}.*", item)) -> intl.output
  }
  
  intl.output %>%
    return(.)
  
}




######################
## spread_n##
##########################
# spread_n is a STATA wide reshape over "values"
# Args:  domn= domain as "read,domn,scie"
#         pars= freq or diff  
# Returns: a data frame with international item frequencies or IRT parameters
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

######################
## FunMode##
##########################
# FunMode computes the mode of a given input
# Args:  x= vector
#         pars= freq or diff  
# Returns: the mode value
FunMode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}


######################
## color palette##
##########################
pbts_colors <- c(
  #vector of color codes in HEX format for PISA for Schools
  `red`         = "#d04432",
  `orange`      = "#e5b921",
  `lightgreen`  = "#c6da50",
  `turquoise`   = "#00ae88",
  `lightblue`   = "#0076a0",
  `oecdblue`    = "#005581",
  `dustygrey`   = "#999999",
  `tundora`     = "#4D4D4D")


######################
## pbts_cols##
##########################
pbts_cols <- function(...) {
  # pbts_cols extract pbts colors as hex codes
  # Args:  ...= Character names of pbts_colors 
  # Returns: desired color as hex code
    cols <- c(...)
  
  if (is.null(cols))
    return (pbts_colors)
  
  pbts_colors[cols]
}

######################
## pbts_palettes##
##########################
pbts_palettes <- list(
  #pbts_palettes is a list of color palettes based on pbts_colors
  
  `Main`  = pbts_cols("red", "orange", "lightgreen","turquoise","lightblue","oecdblue"),
  
  `RdGn`  = pbts_cols("red", "orange", "lightgreen","turquoise"),
  
  `RdGn2`  = pbts_cols("red", "turquoise", "red"), #this is defined for 3 categories dummy (-1,0,1) where both -1 and 1 have negative connotation
  
  `GnBe`   = pbts_cols("turquoise","lightblue", "oecdblue"),
  
  `GyBe`  = pbts_cols("dustygrey", "oecdblue"),
  
  `Grey`  = pbts_cols("dustygrey", "tundora")
)

######################
## pbts_pal##
##########################
pbts_pal <- function(palette = "main", reverse = FALSE, ...) {
  # pbts_pal interpolates the palette colors for a certain number of levels so to create shades
  # Args:  palette= element of pbts_palettes 
  #         reverse= boolean for color ordering
  # Returns: interpolated palette colors
  
  pal <- pbts_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

######################
## scale_color_pbts##
##########################
scale_color_pbts <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  # scale_color_pbts constructs Color scales from pbts_palettes
  # Args:  palette= element of pbts_palettes 
  #         discrete= boolean for color aesthetic
  #         reverse= boolean for color ordering
  # Returns: color scale
  pal <- pbts_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("pbts_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...) #from ggplot
  }
}

######################
## scale_fill_pbts##
##########################
scale_fill_pbts <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  # scale_fill_pbts constructs fill scales from pbts_palettes
  # Args:  palette= element of pbts_palettes 
  #         discrete= boolean for color aesthetic
  #         reverse= boolean for color ordering
  # Returns: fill scale
  pal <- pbts_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("pbts_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}





