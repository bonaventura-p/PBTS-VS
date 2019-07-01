


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





