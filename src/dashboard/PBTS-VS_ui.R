
# PBTS-VS_ui.R
# Bonaventura Pacileo
# This program defines the ui object of the shiny app

ui <-shiny::navbarPage(shiny::uiOutput("cntname"),
      

              shiny::tabPanel("Introduction",
                        shiny::h4(shiny::strong("Overview")),     
                        shiny::HTML('<p> The main purpose of the validation study
                        of the PISA-Based Test for Schools is to assess quality, reliability and comparability of the assessment instruments in the country of test.
                      The validation study consists of items psychometric properties analyses in the language of the country of test and further international comparison of items
                      characteristics. The decisions regarding item treatment for results generation and for future administrations the PBTS can also be made based on this report. </p>
                      The report is organised in three sections:  </br> 
                                    Exploratory analysis: the results of the validation of the psychometric properties of the items are presented; </br>
                                    Dodgy items review: the item and test characteristics curves and information functions are presented; </br>
                                    Results preview: diagnostic checks over the stability of the Principal Component Analysis (PCA) and average scores by schools are presented.</br> </br>'),
                        shiny::HTML('</br> </br>'),

                        shiny::sidebarLayout(
                          shiny::sidebarPanel(
                            
                            shiny::selectInput("folder", shiny::h5("Select a country: "), choices = c("Andorra","Brazil","Japan")),
                            width=4
                          ),
                          shiny::mainPanel(
                            shiny::h4("Country profile"),
                            shiny::uiOutput("cntprofile"),
                            width=8
                          ) 
                        ),
                        
                        shiny::HTML('</br> </br>'),
                        shiny::checkboxInput("dodif", "Do DIF analysis", value = F), 
                        shiny::HTML('</br> </br>'),
                        shiny::HTML('<a href="http://www.oecd.org/pisa/aboutpisa/PfS_TechReport_CRC_final.pdf">PISA for Schools Technical Report</a> '),
                        shiny::HTML('<a href="http://www.oecd.org/pisa/data/2015-technical-report/">PISA 2015 Technical Report</a> '),
                        shiny::HTML('</br>')
                        
                        #hidden download option for now
                        #shiny::HTML('</br>'),
                        #shiny::radioButtons('format', shiny::h5('Select a format for download'), c('PDF', 'HTML', 'Word'), inline = TRUE),
                        #shiny::downloadButton('downloadReport'),
 
                 ),
                 
                 
                
                 
                 
                 shiny::navbarMenu("Exploratory analysis",  
                                   
                                   
                                   shiny::tabPanel("Summary of exploratory analysis",
                                                   shiny::HTML('<p> <b> Summary of exploratory analysis </b> </br> The national data was calibrated in order
                                                               to determine whether the items behave similarly to the international pilot. </br>
                                                               A total of 140 items were analyzed: 46 reading items (43 dichotomously scored), 
                                                               40 mathematics items (34 dichotomously scored) and 54 science (49 dichotomously scored). </br>
                                                               The point-biserial correlation, the discrimination coefficient, the MNSQ (Infit) coefficient and item difficulty estimates have been computed. 
                                                               The Differential item functioning (DIF) analysis has also been conducted in order to detect potential bias
                                                               between girls and boys responses. The results are summarised below. </p> </br>'),
                                                   shiny::numericInput("sum", label = shiny::h5("Number of items to view"), 15),
                                                   #
                                                   
                                                   shiny::HTML('</br> <b> Mathematics items:</b>'),
                                                   shiny::tableOutput('summary_math'),
                                                   shiny::HTML('</br> <b>Reading items </b>'),
                                                   shiny::tableOutput('summary_read'),
                                                   shiny::HTML('</br> <b>Science items </b>'),
                                                   shiny::tableOutput('summary_scie'),
                                                   #add DIF
                                                   shiny::textInput("text_model", label = "Conclusions", value = "Enter text...")
                                                   ),
                            
                                  shiny::tabPanel("Correlation with international score frequencies",
                                    shiny::HTML('<p> <b> Correlation with international score frequencies </b> </br> The frequency of students scores distribution has been estimated
                                          for three response codes: 0 - incorrect; 1 - correct for dichotomy items or partially correct for partial credit items; 2 - totally correct for partial credit items. </p>  </br> <p>
                                          The correlation between score frequencies in the country and in the international pilot has been estimated. An high correlation among all response codes in all domains suggests that there are high similarities 
                                          of the response distributions between students in the country and in the international pilot. </p> </br>'),
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        
                                        shiny::selectInput("domain2", shiny::h5("Choose a domain:"), choices = c("math","read","scie")), 
                                         width=2
                                       ),
                                      shiny::mainPanel(
                                        shiny::plotOutput("plot2"),
                                         width=10
                                       )
                                     ),
                                     
                                    shiny::HTML('</br> <b> Mathematics items </b>'),
                                    shiny::tableOutput('table2_math'),
                                    shiny::HTML('</br> <b> Reading items </b>'),
                                    shiny::tableOutput('table2_read'),
                                    shiny::HTML('</br> <b> Science items </b>'),
                                    shiny::tableOutput('table2_scie')
                                     ),
                            
                                  shiny::tabPanel("Point-biserial correlation",
                                    shiny::HTML("<b> Point-biserial correlation </b> </br>  <p>  The point-biserial correlation 
                                          is the correlation between a response category coded as a dummy variable
                                          (a score of 1 for students that responded with the correct code and a score of 0 for students 
                                          in other response categories) and the total domain score. For dichotomous items the point-biserial
                                          is equal to the adjusted correlation. Correct responses should have positive correlations 
                                          with the total score, incorrect responses negative correlations. </p> </br> <p> There are three criteria to take into consideration: </br>
                                          Criterion 1: Categories 0 must have a discrimination biserial-point index negative; </br> 
                                          Criterion 2: The discrimination biserial-point for a partial credit item must be ordered; </br> 
                                          Criterion 3: Discrimination of the correct answer must be greater than 0.2. </p> </br> </br>"),
                                    shiny::numericInput("obs3", label = shiny::h5("Number of items to view"), 10),
                                    
                                    
                                    shiny::HTML('</br> <b> Mathematics items </b> <p> The table below presents the results for the mathematics items </p> </br>'),
                                    shiny::tableOutput('table3_math'),
                                    shiny::HTML('</br> <b> Reading items </b> <p> The table below presents the results for the reading items </p> </br>'),
                                    shiny::tableOutput('table3_read'),
                                    shiny::HTML('</br> <b> Science items </b> <p> The table below presents the results for the science items </p> </br>'),
                                    shiny::tableOutput('table3_scie')
                                     ),
                            
                                  shiny::tabPanel("IRT item difficulty",
                                  shiny::HTML("<b> IRT item difficulty </b> </br> <p>  The national scaling provides nationally
                                          specific item parameter estimates. If the test measured the same latent trait
                                          per domain in all countries, then items should have the same relative difficulty. </p> </br> </br>"),      
                                  shiny::sidebarLayout(
                                    shiny::sidebarPanel(
                                      
                                      shiny::selectInput("domain4", shiny::h5("Choose a domain:"), choices = c("math","read","scie")), 
                                         width=2
                                       ),
                                    shiny::mainPanel(
                                      shiny::plotOutput("plot4"),
                                         width=10
                                       )
                                     ),
                                  shiny::HTML('</br>'),
                                  shiny::HTML("</br>  <p> If the difference is statistically significant, the item is flagged as dodgy. </p> </br>"),      
                                  shiny::numericInput("obs4", label = shiny::h5("Number of items to view"), 10),
                                  
                                  shiny::HTML('</br> <b> Mathematics items </b> <p> The table below presents the results for the mathematics items </p> </br>'),
                                  shiny::tableOutput('table4_math'),
                                  shiny::HTML('</br> <b> Reading items </b> <p> The table below presents the results for the reading items </p> </br>'),
                                  shiny::tableOutput('table4_read'),
                                  shiny::HTML('</br> <b> Science items </b> <p> The table below presents the results for the science items </p> </br>'),
                                  shiny::tableOutput('table4_scie')   
                                     ),
                            
                                  shiny::tabPanel("Model fit (Infit)",
                                  shiny::HTML("<b> Model fit (Infit) </b> </br>  <p> For each item parameter, the fit MNSQ (infit) index was 
                                          used to provide an indication of the compatibility of the (national) model and the (national) data. For each student,
                                          the model describes the probability of obtaining the different item scores. It is therefore 
                                          possible to compare the model prediction (with national parameters) and what has been observed for one item across students.
                                          Accumulating comparisons across students gives an item-fit statistic.  </br> </br>
                                          A weighted MNSQ greater than one is associated with a low discrimination index, meaning the data 
                                          exhibits more variability than expected by the model. </br> </br>
                                          PISA for Schools accepts small variations of MNSQ around one, however, values significantly larger than 1.2 indicate
                                          that the item discrimination is lower than assumed by the model, and values significantly below 0.8 show that the item
                                          discrimination is higher than assumed. Only statistically significant values are reported. </p> </br> </br> "),
                                  shiny::numericInput("obs5", label = shiny::h5("Number of items to view"), 10),
                                  
                                  shiny::HTML('</br> <b> Mathematics items </b> <p> The table below presents the results for the mathematics items </p> </br>'),
                                  shiny::tableOutput('table5_math'),
                                  shiny::HTML('</br> <b> Reading items </b> <p> The table below presents the results for the reading items </p> </br>'),
                                  shiny::tableOutput('table5_read'),
                                  shiny::HTML('</br> <b> Science items </b> <p> The table below presents the results for the science items </p> </br>'),
                                  shiny::tableOutput('table5_scie')   
                                     ),
                            
                                  shiny::tabPanel("Gender DIF",
                                  shiny::HTML("<b> Differential item functioning </b> </br>  <p>   The DIF analysis was performed using the multi-facet model
                                          of TAM (Wu et al., 2007) through the difference in parameters of item characteristic curves (ICCs) 
                                          of girls and boys groups. The figure below shows the difficulty estimates for the two groups (boys versus girls). 
                                          An item is flagged as having substantial DIF if the interaction term (item:gender) is greater than 0.25 in absolute terms.  </p> </br> </br> "), 
                                  shiny::sidebarLayout(
                                    shiny::sidebarPanel(
                                      
                                      shiny::selectInput("domain6", shiny::h5("Choose a domain:"), choices = c("math","read","scie")), 
                                         width=2
                                       ),
                                    shiny::mainPanel(
                                      shiny::plotOutput("plot6"),
                                         width=10
                                       )
                                     ),
                                  shiny::HTML('</br>'),
                                  shiny::numericInput("obs6", label = shiny::h5("Number of items to view"), 10),
                                  
                                  shiny::HTML('</br> <b> Mathematics items </b> <p> The table below presents the results for the mathematics items </p> </br>'),
                                  shiny::tableOutput('table6_math'),
                                  shiny::HTML('</br> <b> Reading items </b> <p> The table below presents the results for the reading items </p> </br>'),
                                  shiny::tableOutput('table6_read'),
                                  shiny::HTML('</br> <b> Science items </b> <p> The table below presents the results for the science items </p> </br>'),
                                  shiny::tableOutput('table6_scie')   
                                     )
                            ),
                 
                 
                 shiny::navbarMenu("Dodgy items review",
                                   
                                   shiny::tabPanel("Item information functions and characteristic curves",
                                                   
                                                   shiny::HTML('<b> Item information and characteristic curves </b> <p> Item characteristic curve describes the relationship
                                                               between latent ability and the performance on a test item. In other words, the curve models the probability of
                                                               a correct response given the item difficulty and the individual ability level. </p>  </br> 
                                                                <p>  The item information function displays the amount of information provided by an item about the ability of the student.
                                                               The amount of this information depends on how closely the difficulty of the item matches the ability of the person.
                                                               In other words, any item is most informative for students whose ability is equal to the difficulty of the item. </p>  </br>'),
                                                   
                                                   shiny::sidebarLayout(
                                                     shiny::sidebarPanel(
                                                       
                                                       shiny::selectInput("domain9", shiny::h5("Choose a domain:"), choices = c("math","read","scie")),
                                                       shiny::uiOutput('icc.ui'),
                                                       width=3
                                                     ),
                                                     shiny::mainPanel(
                                                       shiny::plotOutput("plot9"),
                                                       shiny::HTML('</br> </br>'),
                                                       shiny::HTML('</br> </br>'),
                                                       shiny::HTML('<b> Wright Map </b> <p>  The Wright Map is organized vertically. The left hand side shows students and the right hand side
                                                          shows items. On the left, there is the distribution of the measured ability of the candidates from most able at the top to least able at the bottom. 
                                                      The items on the right hand side of the map are distributed from the most difficult at the top to the least difficult at the bottom.  
                                                    Theoretically, when candidates and items are opposite each other on the map, the difficulty of the item 
                                               and the ability of the candidate are comparable, so the candidate has approximately a 50% probability
                                                                   of answering the item correctly. </p> </br>   </br>'),
                                                       shiny::HTML('</br>'),
                                                       shiny::plotOutput("plot7"),
#                                                       shiny::tableOutput('tablex') ,  
                                                       
                                                       width=9
                                                     )
                                                   )
                                                   
                                   ),
                                   
                                   shiny::tabPanel("Test information functions and test characteristic curves",
                                                   
                                                   shiny::HTML('<b> Test information functions and test characteristic curves </b> <p> 
                                                               The test characteristic curve models the relationship between the expected score and
                                                                the ability levels. In other terms, it shows the probability of answering correctly the test items
                                                                for different ability levels.    </p> </br>
                                                               <p> Similarly, the test information function is build on the item information function. It displays the sum of all
                                                                  item information functions. Moreover, it provides a measure of the accuracy of any value of the latent ability. </p> </br> </br>'),
                                                   
                                                   shiny::sidebarLayout(
                                                     shiny::sidebarPanel(
                                                       
                                                       shiny::selectInput("domain10", shiny::h5("Choose a domain:"), choices = c("math","read","scie")),
                                                       width=2
                                                     ),
                                                     shiny::mainPanel(
                                                       shiny::plotOutput("plot10"),
                                                       width=10
                                                     )
                                                   ),
                                                   shiny::HTML('</br>  </br>'),
                                                   shiny::HTML('<b> Dodgy items </b> <p> By ticking the items below, they are dropped from the dataframe and
                                                               the updated test information function and test characteristic curve are estimated. 
                                                               If they improve, it means that dropping the item increases the accuracy of the test. </p> </br>'),
                                                   shiny::uiOutput('killitem.ui')
                                                   
                                   )
                            
                 ),
                 
                 shiny::navbarMenu("Results preview",
                                   
                        shiny::tabPanel("PCA diagnostics",
                                        shiny::HTML('<b> Stability of Principal Component Analysis (PCA) estimates </b> </br> '),
                                        shiny::HTML('</br> This is a computationally intensive analysis. It could take up to 5 minutes. </br> </br>
                                                    <p> All the categorical variables from the Student Questionnaire that are not used as direct regressors
                                                    are dummy coded. These dummy variables, the numerical variables age of school entry (09-ST06), age of
                                                    arrival in the country (15-ST21), as well as the recoded numerical variable (AGE) are
                                                    analysed in a principal component analysis.  The number of component vectors that must 
                                                    be extracted and used in the scaling model as regressors is country specific and must
                                                    explain 95% of the total variance in all the original variables. </p> </br> <p> However, the numerosity of these component vectors could negatively affect
                                                    the stability of the IRT model. The following diagnostics are analysed to ensure PCA factors are behaving properly: </p> 
                                                    <b> EAP reliability: </b> this index from Adams (2005) measures the (average) proportion of the uncertainty in the location of each student. It is defined as the ratio between the variance of the ability estimates and the true population variance; </br>
                                                    <b> EAP mean: </b> the expected a posteriori (EAP) estimate is the expected value of the posterior probability distribution of latent trait scores for a given student. The mean is computed over all students for a given number of PCA components;  </br>
                                                    <b> EAP standard deviation (SD): </b> the standard deviation of the EAP estimates;  </br>
                                                    <b> Mean of the SD(EAP): </b> the mean of the standard deviation of the EAP estimates;  </br>
                                                    <b> Standard deviation of the SD(EAP): </b> the standard deviation of the standard deviation of the EAP estimates;  </br>
                                                    <b> Model variance (sigma): </b> the variance of the prior distribution of the EAP estimate of each student determines to what extent regressors (or item responses) contribute to the estimation of ability.  </br>
                                                     </br> </br>'),
                                        
                                        shiny::sidebarLayout(
                                          shiny::sidebarPanel(
                                            
                                            shiny::selectInput("domainpca", shiny::h5("Choose a domain:"), choices = c("math","read","scie")), 
                                            shiny::sliderInput("pctvar", shiny::h5("Percentage of variance explained:"),value=0.95, min=0, max=0.95, step=0.05), 
                                            
                                            width=3
                                          ),
                                          shiny::mainPanel(
                                            shiny::plotOutput("plotpca"),
                                            width=9
                                          )
                                        ),
                                        shiny::HTML('</br> </br> </br>')
                                        
                              ),  
                            
                         shiny::tabPanel("Primary analysis",
                           
                                         shiny::HTML('<b> Primary analysis </b> <p> The figures below show the average score (on a PISA scale) for the
                                                     different domains. On the left, the average results for each school are presented. On the right,
                                                     the results are broken down by gender. </p> </br>'),
                           
                                         shiny::sidebarLayout(
                                           shiny::sidebarPanel(
                                             
                                             shiny::selectInput("domain8", shiny::h5("Choose a domain:"), choices = c("math","read","scie")), 
                                             width=2
                                           ),
                                           shiny::mainPanel(
                                             shiny::plotOutput("plot8"),
                                         width=10
                                       )
                                     )
                            )
                            
                 ),

  shiny::navbarMenu("Gold dataset generator",
                  
                  shiny::tabPanel("Control panel",
                                  shiny::HTML('<b> Description </b> <p> This panel allows to generate the PBTS gold datasets. It provides a diagnostics panel for all the required QA checks, </p> </br>'),         
                                  shiny::sidebarPanel(
                                    #add country
                                    shiny::selectInput("input666", shiny::h5("Select a country: "), choices = c("Andorra","Brazil","Japan")),
                                    width=4),
                                  
                                  shiny::mainPanel(
                                    
                                    shiny::wellPanel( shiny::HTML("<b> Checks </b> </br>"),
                                                      shiny::selectInput("input667", shiny::h5("Items to drop: "), choices = c("PM5109Q02","PM5201Q01","PM5109Q01")),
                                                      
                                                      shiny::checkboxInput("input668", "Percentage of correct responses", value = F),
                                                      
                                                      shiny::checkboxInput("input669", "Percentage of missing responses", value = F),
                                                      shiny::checkboxInput("input670", "Items with gender DIF", value = F),
                                                      shiny::sliderInput("input671", "Percentage of explained variance:",  
                                                                  min = 0, max = 100, value = 95),
                                                      shiny::actionButton("button", "Apply changes")
                                                      #submitButton(text = "Apply changes", icon = NULL, width = NULL)
                                    ),
                                    shiny::wellPanel(shiny::HTML("<b> Downloads </b> </br>"),
                                                     #shiny::selectInput("items2", shiny::h5("Items to drop: "), choices = c("Andorra","Brazil","Japan")),
                                                     
                                                     shiny::checkboxInput("input672", "Download the student level dataset", value = F),
                                                     
                                                     shiny::checkboxInput("input673", "Download the school level dataset", value = F),
                                                     shiny::HTML('</br>'),
                                                     shiny::radioButtons('format', shiny::h5('Select a format for download'), c('PDF', 'HTML', 'Word'), inline = TRUE),
                                                     shiny::downloadButton('downloadReport')
                                    ), width = 8)
                                  
                  )
)


  ) 





