
source('//main.oecd.org/ASgenEDU/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Validation Study/PBTS-VS/VS_init.R')



ui2 <-navbarPage("PISA-Based Test for Schools Validation Study",
  
  tabPanel("Control room",
    selectInput("folder", h5("Choose a country:"), choices = c("Andorra")),        
    HTML('</br>'),
    checkboxInput("dodif", "Do DIF analysis", value = F), #and change choice of country with no default
    HTML('</br>'),
    radioButtons('format', h5('Document format'), c('PDF', 'HTML', 'Word'), inline = TRUE),
    downloadButton('downloadReport'),
    HTML('</br> </br>'),
    HTML('<a href="http://www.oecd.org/pisa/aboutpisa/PfS_TechReport_CRC_final.pdf">PISA for Schools Technical Report</a> '),
    HTML('<a href="http://www.oecd.org/pisa/data/2015-technical-report/">PISA 2015 Technical Report</a> ')
  ),
  

     
  
    tabPanel("Summary of exploratory analysis",
             HTML('<p> <b> Objectives </b> </br> The main purpose of the validation study
of the PISA-Based Test for Schools assessment is to assess quality, reliability and 
comparability of the assessment instruments translated into PLACE language to international
assessment instruments in English. The validation study consists of items psychometric
properties analyses in PLACE language and further international comparison of items
characteristics. The decisions regarding item treatment for results generation and for
possible future PfS implementation in PLACE can also be made. </br>
This technical report provides the results of the national item analysis in PLACE
 with the purpose of validation of the assessment instrument in PLACE
 </p> </br>'),
             HTML('<p> <b> Methodology </b> </br> The PLACE data was calibrated in order
                  to determine whether the items function similarly to the international pilot. </br>
                  A total of 140 items were analyzed: 46 reading items (43 dichotomously scored), 
                  40 mathematics items (34 dichotomously scored) and 54 science (49 dichotomously scored). </br>
                  The discrimination coefficient, the coefficient MNSQ (Infit), and the item parameter estimates have been computed. 
                  The Differential item functioning (DIF) analysis has also been conducted in order to detect potential bias
                  between girls and boys responses.</p> </br>'),
             numericInput("sum", label = h5("Number of items to view"), 15),
             HTML('</br> <b> Mathematics items:</b>'),
             tableOutput('summary_math'),
             HTML('</br> <b>Reading items </b>'),
             tableOutput('summary_read'),
             HTML('</br> <b>Science items </b>'),
             tableOutput('summary_scie'),
             #add DIF
             textInput("text_model", label = "Conclusions", value = "Enter text...")
    ),

  
  navbarMenu("Exploratory analysis",  
  
    tabPanel("Correlation with international score frequencies",
             HTML('<p> <b> Correlation with international score frequencies </b> </br> The frequency of students scores distribution has been estimated
for three response codes: 0 - incorrect; 1 - correct for dichotomy items or partially correct for partial credit items; 2 - totally correct for partial credit items. </p>  </br> <p>
The correlation between score frequencies in PLACE and in the international pilot has been estimated. An high correlation among all response codes in all domains suggests that there are high similarities 
of the response distributions between students in PLACE and in the international pilot. </p> </br>'),
            sidebarLayout(
              sidebarPanel(
                selectInput("domain2", h5("Choose a domain:"), choices = c("math","read","scie")), 
                width=2
              ),
              mainPanel(
                plotOutput("plot2"),
                width=10
              )
            ),
            
             HTML('</br> <b> Mathematics items </b>'),
              tableOutput('table2_math'),
               HTML('</br> <b> Reading items </b>'),
               tableOutput('table2_read'),
               HTML('</br> <b> Science items </b>'),
               tableOutput('table2_scie')
       ),
    
    tabPanel("Point-biserial correlation",
             HTML("<b> Point-biserial correlation </b> </br>  <p>  The point-biserial correlation 
is the correlation between a response category coded as a dummy variable
(a score of 1 for students that responded with the correct code and a score of 0 for students 
in other response categories) and the total domain score. For dichotomous items the point-biserial
is equal to the adjusted correlation. Correct responses should have positive correlations 
with the total score, incorrect responses negative correlations. </p> </br> <p> There are three criteria to take into consideration: </br>
Criterion 1: Categories 0 must have a discrimination biserial-point index negative; </br> 
Criterion 2: The discrimination biserial-point for a partial credit item must be ordered; </br> 
                  Criterion 3: Discrimination of the correct answer must be greater than 0,2. </p> </br> </br>"),
             numericInput("obs3", label = h5("Number of items to view"), 10),
             HTML('</br> <b> Mathematics items </b>'),
             tableOutput('table3_math'),
             HTML('</br> <b> Reading items </b>'),
             tableOutput('table3_read'),
             HTML('</br> <b> Science items </b>'),
             tableOutput('table3_scie')
    ),
    
    tabPanel("IRT item difficulty",
       HTML("<b> IRT item difficulty </b> </br> <p>  The national scaling provides nationally
            specific item parameter estimates. If the test measured the same latent trait
            per domain in all countries, then items should have the same relative difficulty. </p> </br> </br>"),      
             sidebarLayout(
               sidebarPanel(
                 selectInput("domain4", h5("Choose a domain:"), choices = c("math","read","scie")), 
                 width=2
               ),
               mainPanel(
                 plotOutput("plot4"),
                 width=10
               )
             ),
             HTML('</br>'),
       HTML("</br>  <p> If the difference is statistically significant, the item is flagged as dodgy. </p> </br>"),      
             numericInput("obs4", label = h5("Number of items to view"), 10),
             HTML('</br> <b> Mathematics items </b>'),
             tableOutput('table4_math'),
             HTML('</br> <b> Reading items </b>'),
             tableOutput('table4_read'),
             HTML('</br> <b> Science items </b>'),
             tableOutput('table4_scie')   
    ),
    
    tabPanel("Model fit (Infit)",
             HTML("<b> Model fit (Infit) </b> </br>  <p> For each item parameter, the fit MNSQ (infit) index was 
                  used to provide an indication of the compatibility of the model and the data. For each student,
                  the model describes the probability of obtaining the different item scores. It is therefore 
                  possible to compare the model prediction and what has been observed for one item across students.
                  Accumulating comparisons across students gives an item-fit statistic.  </br> </br>
                  A weighted MNSQ greater than one is associated with a low discrimination index, meaning the data 
                  exhibits more variability than expected by the model. </br> </br>
                  PISA for Schools accepts small variations of MNSQ around one, however, values larger than 1.2 indicate
                  that the item discrimination is lower than assumed by the model, and values below 0.8 show that the item
                  discrimination is higher than assumed </p> </br> </br> "),
             numericInput("obs5", label = h5("Number of items to view"), 10),
             HTML('</br> <b> Mathematics items </b>'),
             tableOutput('table5_math'),
             HTML('</br> <b> Reading items </b>'),
             tableOutput('table5_read'),
             HTML('</br> <b> Science items </b>'),
             tableOutput('table5_scie')   
    ),
    
     tabPanel("Gender DIF",
              HTML("<b> Differential item functioning </b> </br>  <p>   The DIF analysis was performed using the multi-facet model
                   of TAM (Wu et al., 2007) through the difference in parameters of item characteristic curves (ICCs) 
                   of girls and boys groups. The DIF value for each item is computed as the difference between the two
                   relative difficulty estimates (boys versus girls). An item is flagged as having substantial DIF if
                   this difference is greater than 0.25  </p> </br> </br> "), 
              sidebarLayout(
                sidebarPanel(
                  selectInput("domain6", h5("Choose a domain:"), choices = c("math","read","scie")), 
                  width=2
                ),
                mainPanel(
                  plotOutput("plot6"),
                  width=10
                )
              ),
              HTML('</br>'),
              numericInput("obs6", label = h5("Number of items to view"), 10),
              HTML('</br> <b> Mathematics items </b>'),
              tableOutput('table6_math'),
              HTML('</br> <b> Reading items </b>'),
              tableOutput('table6_read'),
              HTML('</br> <b> Science items </b>'),
              tableOutput('table6_scie')   
    ),
  
     tabPanel("Wright Map",
              sidebarLayout(
                sidebarPanel(
                  selectInput("domain7", h5("Choose a domain:"), choices = c("math","read","scie")), 
                  width=2
                ),
                mainPanel(
                  HTML('<b> Wright Map </b> </br> <p>  Theoretically, when candidates and items are opposite each other on the map, the difficulty of the item 
                        and the ability of the candidate are comparable, so the candidate has approximately a 50% probability
                        of answering the item correctly. </p> </br>   </br>'),
                  HTML('</br>'),
                  plotOutput("plot7"),
                  width=10
                )
              )
      )
  ),
  
  navbarMenu("Dodgy items review",
             tabPanel("TCC/Cronbach")
             
  ),
  
  navbarMenu("Results preview",
   
  tabPanel("Primary analysis",
           
           HTML('<b> Primary analysis </b> </br> </br> </br>'),
           
           sidebarLayout(
             sidebarPanel(
               selectInput("domain8", h5("Choose a domain:"), choices = c("math","read","scie")), 
               width=2
             ),
             mainPanel(
               plotOutput("plot8"),
               width=10
             )
           ),
           HTML('</br>  </br>'),
           HTML('<b> Dodgy items </b> </br> </br> </br>'),
           uiOutput('killitem.ui')
           
  )
  
), 
    
navbarMenu("More",
 tabPanel("Sub-Component A"),
 tabPanel("Sub-Component B"))
) 
    
  







########################
########## SERVER ###############
###########################





server2 <- function(input,output){
  
  
  # download report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')
      
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    })

  #################
  #Load data
  ##################


  # load national data sets
  folderInput <- reactive({
    switch(input$folder,
           "Andorra" = "V:/PISA/BACKUP/PISA/PISA for Schools/11. Item Parameters/Data/Andorra/")
  })

  score.data <- reactive({
    read.table( paste0(folderInput(), "score.data.txt"), header=T, sep="\t")
  }) 

  gold.data <- reactive({
    read.table( paste0(folderInput(), "gold.data.txt"), header=T, sep="\t")
  })   
  
  raw.data <- reactive({
    read.table( paste0(folderInput(), "raw.data.txt"), header=T, sep="\t")
  })   
  
  pca.data <- reactive({
    read.table( paste0(folderInput(), "pca.data.txt"), header=T, sep="\t")
  })   
  
 #####################
 #run all analysis
 #####################
  
  domains<-c("math","read","scie") 
  
  #Validation study analysis
  results<-reactive({
    VSscaleloop(domains=domains, resp=score.data(), dodif=input$dodif, gender.data=gold.data(), gender.name="ST004Q01_15")
  })
  
  #Primary analysis
  primary<-reactive({
    PFSscaleloop(domains=domains, resp = score.data(), stu.data = gold.data(), 
                 raw.data = raw.data(), pca.data = pca.data(),kill.item=input$kill)
  })

#############
# Create tables
#############
  
   lapply(domains, function(domn) {
     
  
     
     #Table 2. Correlation between score frequencies 0,1,2 pilot 
     intl.freq <- IntlFreq(domn)

      corr.output<- reactive({ 
         results()[[domn]]["ctt.output"] %>% 
        as.data.frame(.) %>%  
        Correl(ctt.data=.,intl.ctt = intl.freq)
        })
     
     output[[paste('table2',domn,sep ="_")]] <- renderTable({
       
       corr.output() %>% 
            rename(., "Item category"=item.freq, Correlation=corr.value) 
       })  
  
  #Table 3 Dodgy items based on biserial-point values 
      ptbis.output<- reactive({ 
        results()[[domn]]["ctt.output"] %>% 
          as.data.frame(.) %>%
          PtBis(ctt.data = .) 
      })
    
  
      output[[paste('table3',domn,sep ="_")]]<-renderTable({
        
        ptbis.output() %>%
          filter(.,!(crit.one ==0 & crit.two==0 & crit.three == 0))  %>%
          mutate_at(., vars("crit.one"), funs(ifelse(.== 1, "Non-negative Category 0","0"))) %>%
          mutate_at(., vars("crit.two"),funs(ifelse(.== 1, "Not ordered pt-biserial correlation","0"))) %>%
          mutate_at(., vars("crit.three"),funs(ifelse(.== 1, "Low Discrimination","0"))) %>%
          rename(., "Discrimination index" = discr, 'Criterion 1'= crit.one,
                 'Criterion 2'= crit.two,
                 'Criterion 3'= crit.three) %>%
        head(., n=input$obs3)
      })
  
  
      # Table 4. Dodgy items based on difficulty differences
      intl.diff <- IntlDiff(domn)
      
      diff.output<- reactive({ 
        results()[[domn]]["tam.output"] %>% 
        as.data.frame(.) %>%
        DiffItem(tam.mod = .,intl.tam = intl.diff) 
      })
      
      
      output[[paste('table4',domn,sep ="_")]]<-renderTable({
        
        diff.output() %>%
          filter(.,!(zdif==0)) %>%
          select(.,-zdif) %>%
          rename(., Item=item, 'National beta' = tam.value.x, "International beta" = tam.value.y) %>%
          head(., n=input$obs4)
      })
      
      #Table 5. Dodgy items based on coefficient MNSQ
      fit.output<- reactive({ 
        results()[[domn]]["fit.output"] %>%
          as.data.frame(.) %>%  
          Infit(tam.fit = .)
      })
      
      output[[paste('table5',domn,sep ="_")]]<-renderTable({ 
  
      fit.output() %>%  
        filter(., !(w.mnsq==0)) %>%
        select(.,-w.mnsq) %>%
      head(., n=input$obs5)
      })
  
    #Table 6. Dodgy items based on DIF criterion
      dif.output<- reactive({ 
        results()[[domn]]["dif.output"] %>% 
          as.data.frame(.) %>%
          DIFitem(DIF.mod = .)
        })
      
      output[[paste('table6',domn,sep ="_")]]<-renderTable({ 
        
      dif.output() %>% 
         filter(., !(gdif==0)) %>%
         select(.,-gdif) %>%
         rename(., Girls = `1`, Boys = `2`) %>%
       head(., n=input$obs6)
      })
    
  
  #Table summary
      summary.input <- reactive({  
        
        if ( !(input$dodif) ) {
          diff.output() %>%
            full_join(.,fit.output(),by="item")%>%
            select(.,matches("item|zdif|w.mnsq")) %>%
            mutate(.,irtdif=substr(item,11,14),item=substr(item,1,9)) %>%
            spread_n(.,key = irtdif, value =c("zdif","w.mnsq")) %>%
            full_join(.,ptbis.output(),by="item") 
        } else if (input$dodif) {
           diff.output() %>%
             full_join(.,fit.output(),by="item")%>%
             select(.,matches("item|zdif|w.mnsq")) %>%
             mutate(.,irtdif=substr(item,11,14),item=substr(item,1,9)) %>%
             spread_n(.,key = irtdif, value =c("zdif","w.mnsq")) %>%
             full_join(.,ptbis.output(),by="item") %>%
             full_join(.,dif.output(),by="item")
             
         }
      })


    output[[paste('summary',domn,sep ="_")]] <- renderTable({
      
      
      if ( !(input$dodif)) {
        
       summary.input() %>%
         filter(.,!(crit.one ==0 & crit.two==0 & crit.three == 0 
                    & Cat1_zdif==0 & Cat2_zdif==0 & Cat1_w.mnsq ==0 & Cat2_w.mnsq ==0)) %>%
         mutate_at(., vars("crit.one"), funs(ifelse(.== 1, "Non-negative Category 0",NA))) %>%
         mutate_at(., vars("crit.two"),funs(ifelse(.== 1, "Not ordered pt-biserial correlation",NA))) %>%
         mutate_at(., vars("crit.three"),funs(ifelse(.== 1,"Low discrimination",NA))) %>%
         mutate_at(.,vars("Cat1_zdif"),funs(ifelse(.==1,"Higher difficulty",ifelse(.==-1,"Lower difficulty",NA)))) %>% 
         mutate_at(.,vars("Cat2_zdif"),funs(ifelse(.==1,"Higher difficulty",ifelse(.==-1,"Lower difficulty",NA)))) %>%
         mutate_at(.,vars("Cat1_w.mnsq"),funs(ifelse(.==1,"MNSQ > 1.2 ",ifelse(.==-1,"MNSQ < 0.8 ",NA)))) %>%
         mutate_at(.,vars("Cat2_w.mnsq"),funs(ifelse(.==1,"MNSQ > 1.2 ",ifelse(.==-1,"MNSQ < 0.8 ",NA)))) %>%
         select(.,item,crit.one,crit.two, crit.three, Cat1_zdif, Cat2_zdif, Cat1_w.mnsq, Cat2_w.mnsq) %>% ##reorder
         rename(., 'PtBis #1'= crit.one,
                'PtBis #2'= crit.two,'PtBis #3'= crit.three,
                Item=item, 'IRT difficulty Cat 1' = Cat1_zdif, 
                'IRT difficulty Cat 2' = Cat2_zdif,
                'Infit Cat 1' = Cat1_w.mnsq,
                'Infit Cat 2' = Cat2_w.mnsq)  %>%
        head(.,n=input$sum)
        
      } else  if(input$dodif) {
        
        summary.input() %>%
          filter(.,!(crit.one ==0 & crit.two==0 & crit.three == 0 
                     & Cat1_zdif==0 & Cat2_zdif==0 & Cat1_w.mnsq ==0 & Cat2_w.mnsq ==0 & gdif==0)) %>%
          mutate_at(., vars("crit.one"), funs(ifelse(.== 1, "Non-negative Category 0",NA))) %>%
          mutate_at(., vars("crit.two"),funs(ifelse(.== 1, "Not ordered pt-biserial correlation",NA))) %>%
          mutate_at(., vars("crit.three"),funs(ifelse(.== 1,"Low discrimination",NA))) %>%
          mutate_at(.,vars("Cat1_zdif"),funs(ifelse(.==1,"Higher difficulty",ifelse(.==-1,"Lower difficulty",NA)))) %>% 
          mutate_at(.,vars("Cat2_zdif"),funs(ifelse(.==1,"Higher difficulty",ifelse(.==-1,"Lower difficulty",NA)))) %>%
          mutate_at(.,vars("Cat1_w.mnsq"),funs(ifelse(.==1,"MNSQ > 1.2 ",ifelse(.==-1,"MNSQ < 0.8 ",NA)))) %>%
          mutate_at(.,vars("Cat2_w.mnsq"),funs(ifelse(.==1,"MNSQ > 1.2 ",ifelse(.==-1,"MNSQ < 0.8 ",NA)))) %>%
          mutate_at(.,vars("gdif"),funs(ifelse(.>0,"Boys > Girls","Girls > Boys"))) %>%
          select(.,item,crit.one,crit.two, crit.three, Cat1_zdif, Cat2_zdif, Cat1_w.mnsq, Cat2_w.mnsq,gdif) %>% ##reorder
          rename(., 'PtBis #1'= crit.one,
                 'PtBis #2'= crit.two,'PtBis #3'= crit.three,
                 "Item"=item, 'IRT difficulty Cat 1' = Cat1_zdif, 
                 'IRT difficulty Cat 2' = Cat2_zdif,
                 'Infit Cat 1' = Cat1_w.mnsq,
                 'Infit Cat 2' = Cat2_w.mnsq,
                 "Gender DIF"=gdif)  %>%
          head(.,n=input$sum)
        
      }
        
     })
    
    
   
  })
  
############### 
#Create figures
############
  

    # Figure 2: CTT Difficulty comparison
  intlfig.freq <- reactive({
    IntlFreq(input$domain2)
  })
  
  corrfig.output<- reactive({ 
    results()[[input$domain2]]["ctt.output"] %>% 
      as.data.frame(.) %>%  
      CorrFig(ctt.data=.,intl.ctt = intlfig.freq())
  })
  
  output$plot2 <- renderPlot({
    
    corrfig.output() %>% 
      ggplot(., aes(x=`1.x`, y=`1.y`,label=.$item)) +
      geom_abline(slope=1,intercept = 0,color="lightgrey",size=0.5) +
      geom_smooth(method=lm, se=FALSE, col=pbts_cols("oecdblue"), size=1) +
      geom_text_repel(size=3,col=pbts_cols("oecdblue")) + #geom_text_repel not available in plotly
      scale_x_continuous(limits = c(0, 100)) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(x="National sample",
           y="International sample",
           title ="CTT Difficulty comparison",
           subtitle = "Percentage of correct responses to the items in the national and international samples") +
      theme_gray() +
      theme(plot.subtitle=element_text(face="italic")) -> g2

    g2
    
  })
  
  
  #Figure 4: IRT Difficulty comparison
  intlfig.diff <-reactive({
    IntlDiff(input$domain4)
  }) 
  
  diff.fig.output<- reactive({ 
    results()[[input$domain4]]["tam.output"] %>% 
      as.data.frame(.) %>%
      DiffItem(tam.mod = .,intl.tam = intlfig.diff()) 
  })

  output$plot4 <- renderPlot({
    
    diff.fig.output() %>%
      mutate(.,z2dif=ifelse(zdif==0,0,1)) %>%
      ggplot(., aes(x=tam.value.x, y=tam.value.y,label=ifelse(zdif != 0,item,""),color=factor(z2dif)))+
      geom_point(aes(size=ifelse(z2dif == 1,0.2,2)))+
      scale_size_identity()+
      geom_smooth(aes( group = 0), method=lm, col=pbts_cols("turquoise"), se=FALSE)+ 
      geom_abline(slope = 1, intercept = 0, color="lightgrey", size = 0.2)+
      geom_text_repel(aes(colour = factor(z2dif)),size=2.5) +
      scale_color_pbts(palette="RdGn",reverse=TRUE, labels=c("Unproblematic items", "Dodgy items"))+
      scale_x_continuous(limits = c(-3.5, 5.5))+
      scale_y_continuous(limits = c(-3.5, 5.5))+
      labs(x="National IRT beta parameter",
           y="International IRT beta parameter", 
           title ="IRT Difficulty comparison",
           subtitle = "Estimated item difficulty in the national and international samples")+
      theme_gray()+
      theme(legend.position="top", legend.title=element_blank(),
            legend.background = element_rect(fill="#EBEBEB"),
            plot.subtitle=element_text(face="italic"))-> DiffPlot
    
    
    #plot density
    
    diff.fig.output() %>%
      select(., tam.value.x, tam.value.y) %>%
      gather(., tam.source, tam.value, factor_key=TRUE) %>%
      ggplot(., aes(tam.value, fill = tam.source)) + 
      geom_density(ggplot2::alpha=.5) + 
      scale_fill_pbts(palette="GyBe",reverse=TRUE, labels=c("National estimates", "International estimates"))+
      labs(x="IRT difficulty (beta parameter)", title ="IRT Difficulty distribution",
           subtitle = "Density distribution of estimated beta parameters in national and international samples")+
      theme_gray()+
      theme(legend.position="top", legend.title=element_blank(),
            legend.background = element_rect(fill="#EBEBEB"),
            plot.subtitle=element_text(face="italic")) -> DiffDensity
    
    #plots together
    grid.arrange( DiffPlot , DiffDensity,   ncol=2, nrow=1,widths = c(1, 0.6))
  })
  
  
  #Figure 6: Gender DIF
  dif.fig.output<- reactive({ 
    results()[[input$domain6]]["dif.output"] %>% 
      as.data.frame(.) %>%
      DIFitem(DIF.mod = .)
  })

    output$plot6 <-renderPlot({
      
      dif.fig.output() %>%
        gather(., key=gender, value=dif.value, `1`:`2`, factor_key=TRUE) %>%
        ggplot(., aes(x=item, y=dif.value, group=gender)) +
        geom_line(aes(color = gender))+
        geom_point(aes(color = gender,shape=ifelse(gdif==0,1,16)),size=3)+
        labs(y="IRT difficulty (beta parameter)", title ="Gender DIF",
             subtitle = "IRT difficulty estimates for boys and girls (significant values with filled dot)")+
        scale_color_pbts(palette="GnBe", labels=c("Girls", "Boys"))+
        scale_shape_identity()+
        theme_gray()+
        theme(axis.text.x = element_text(angle = 90), 
              axis.title.x = element_blank(),
              legend.position="top",
              legend.title=element_blank(),
              legend.background = element_rect(fill="#EBEBEB"),
              plot.subtitle=element_text(face="italic"))
    })
    
# Figure 7: Wright Map
  
  fig.wright.nested<-reactive({
    results()[[input$domain7]]["wright.output"]
  })
  
  fig.wright<-reactive({
    fig.wright.nested()[["wright.output"]] %>%
      WrightmapElms(.)
  })
    
    output$table8<-renderTable({
      fig.wright() %>%
        head(.)
    })
    
    output$plot7 <-renderPlot({

      fig.wright()[[2]] %>%
        ggplot(., aes(x = theta)) +
        geom_histogram(aes(y=..density..), binwidth = 0.3, fill = pbts_cols("dustygrey"), col="black", na.rm = TRUE) +
        geom_density(ggplot2::alpha=.2, fill=pbts_cols("lightblue")) +
        xlim(fig.wright()[[3]], fig.wright()[[4]]) +
        coord_flip() + 
        scale_y_reverse() +
        xlab("Student ability") +
        theme_gray()+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) -> theta.density
      
      
      fig.wright()[[1]] %>%
        ggplot(., aes(x=item, y=tam.value, label=item)) + 
        geom_point(color = pbts_cols("dustygrey"), shape=18, size=3)+
        scale_y_continuous(position = "right",limits=c(fig.wright()[[3]], fig.wright()[[4]]))+
        geom_text_repel(col=pbts_cols("dustygrey"),size=2.5)+
        theme_gray()+
        theme(axis.title.x = element_blank(),
              axis.text.x=element_blank(), 
              axis.ticks.x=element_blank())+
        ylab("Item difficulty")-> beta.plot
      
      #plot_grid(theta.density, beta.plot)
      grid.arrange( theta.density , beta.plot,   ncol=2, nrow=1,widths = c(1, 1),top="Wright Map")

    })
 
      #Figure 8 primary analysis 
    kill.item<-reactive({
      if(input$domain8=="math") {
        score.data() %>%
          select(., matches("^PM")) %>%
          names(.)
      } else if(input$domain8=="read") {
        score.data() %>%
          select(., matches("^PR")) %>%
          names(.)
      } else if(input$domain8=="scie") {
        score.data() %>%
          select(., matches("^PS")) %>%
          names(.)
      } 
    
    })
    
    output$killitem.ui<-renderUI({
      checkboxGroupInput("kill", "Tick to delete the item:", inline=TRUE,
                         choices = kill.item())
    })
    

    
      primary.output<- reactive({ 
        primary()[[input$domain8]] %>% 
          as.data.frame(.) %>%
          pisa.mean.pv(pvlabel=toupper(input$domain8), by = "stidsch",data =.) 
      })
      
      
      output$plot8<- renderPlot({
        
        primary.output() %>% 
          ggplot(., aes(y = Mean, x = stidsch),fill=variable) + 
          geom_bar(stat="identity",width=0.2, ggplot2::alpha=.4, fill=pbts_cols("lightblue"))+  
          labs(y="PISA scale", x="School ID", title ="Performance of students across schools",
               subtitle = "Average performance of students in each schools")+
          scale_y_continuous(limits=c(200, 600),oob = rescale_none)+
          theme_gray()+
          theme(axis.text.x = element_text(angle = 90), 
                legend.position="top",
                legend.title=element_blank(),
                legend.background = element_rect(fill="#EBEBEB"),
                plot.subtitle=element_text(face="italic"))-> g8
        
        g8
        
      })
    

}

shinyApp(ui = ui2, server = server2)




# Render a renderTable or renderDataTable within an application page. renderTable uses a standard HTML table, 
# while renderDataTable uses the DataTables Javascript library to create an interactive table with more features.
