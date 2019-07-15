


server <- function(input,output){
  
  # download report


  
  #################
  #Load data
  ##################
  
  
  # load national data sets
  folderInput <- shiny::reactive({
    switch(input$folder,
           "Andorra" = paste(wd, "data", "Andorra", sep="/"),
           "Japan" = paste(wd, "data", "Japan", sep="/"))
  })
  
  score.data <- shiny::reactive({
    read.table( paste(folderInput(), "score.data.txt", sep="/"), header=T, sep="\t")
  }) 
  
  gold.data <- shiny::reactive({
    read.table( paste(folderInput(), "gold.data.txt", sep="/"), header=T, sep="\t")
  })   
  
  raw.data <- shiny::reactive({
    read.table( paste(folderInput(), "raw.data.txt", sep="/"), header=T, sep="\t")
  })   
  
  pca.data <- shiny::reactive({
    read.table( paste(folderInput(), "pca.data.txt", sep="/"), header=T, sep="\t")
  })   
  
  #####################
  #run all analysis
  #####################
  
  domains<-c("math","read","scie") 
  
  #Validation study analysis
  results<-shiny::reactive({
    VSscaleloop(domains=domains, resp=score.data(), dodif=input$dodif, gender.data=gold.data(), gender.name="ST004Q01_15")
  })
  #,kill.item=input$kill
  #Primary analysis
  primary<-shiny::reactive({
    PFSscaleloop(domains=domains, resp = score.data(), stu.data = gold.data(), 
                 raw.data = raw.data(), pca.data = pca.data())
  })
  
}