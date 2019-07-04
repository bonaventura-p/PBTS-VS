


ui <-shiny::navbarPage("PISA-Based Test for Schools Validation Study",
                 
                        shiny::tabPanel("Control room",
                        shiny::selectInput("folder", shiny::h5("Choose a country:"), choices = c("Andorra","Japan")),        
                        shiny::HTML('</br>'),
                        shiny::checkboxInput("dodif", "Do DIF analysis", value = F), #and change choice of country with no default
                        shiny::HTML('</br>'),
                        shiny::radioButtons('format', shiny::h5('Document format'), c('PDF', 'HTML', 'Word'), inline = TRUE),
                        shiny::downloadButton('downloadReport'),
                        shiny::HTML('</br> </br>'),
                        shiny::HTML('<a href="http://www.oecd.org/pisa/aboutpisa/PfS_TechReport_CRC_final.pdf">PISA for Schools Technical Report</a> '),
                        shiny::HTML('<a href="http://www.oecd.org/pisa/data/2015-technical-report/">PISA 2015 Technical Report</a> ')
                 ),
                 
                 
                 
                 
                 shiny::tabPanel("Summary of exploratory analysis",
                          shiny::HTML('<p> <b> Objectives </b> </br> The main purpose of the validation study
                               of the PISA-Based Test for Schools assessment is to assess quality, reliability and 
                               comparability of the assessment instruments translated into PLACE language to international
                               assessment instruments in English. The validation study consists of items psychometric
                               properties analyses in PLACE language and further international comparison of items
                               characteristics. The decisions regarding item treatment for results generation and for
                               possible future PfS implementation in PLACE can also be made. </br>
                               This technical report provides the results of the national item analysis in PLACE
                               with the purpose of validation of the assessment instrument in PLACE
                               </p> </br>'),
                          shiny::HTML('<p> <b> Methodology </b> </br> The PLACE data was calibrated in order
                               to determine whether the items function similarly to the international pilot. </br>
                               A total of 140 items were analyzed: 46 reading items (43 dichotomously scored), 
                               40 mathematics items (34 dichotomously scored) and 54 science (49 dichotomously scored). </br>
                               The discrimination coefficient, the coefficient MNSQ (Infit), and the item parameter estimates have been computed. 
                               The Differential item functioning (DIF) analysis has also been conducted in order to detect potential bias
                               between girls and boys responses.</p> </br>'),
                          shiny::numericInput("sum", label = shiny::h5("Number of items to view"), 15),
                          shiny::HTML('</br> <b> Mathematics items:</b>'),
                          shiny::tableOutput('summary_math'),
                          shiny::HTML('</br> <b>Reading items </b>'),
                          shiny::tableOutput('summary_read'),
                          shiny::HTML('</br> <b>Science items </b>'),
                          shiny::tableOutput('summary_scie'),
                          #add DIF
                          shiny::textInput("text_model", label = "Conclusions", value = "Enter text...")
                          ),
                 
                 
                 shiny::navbarMenu("Exploratory analysis",  
                            
                                  shiny::tabPanel("Correlation with international score frequencies",
                                    shiny::HTML('<p> <b> Correlation with international score frequencies </b> </br> The frequency of students scores distribution has been estimated
                                          for three response codes: 0 - incorrect; 1 - correct for dichotomy items or partially correct for partial credit items; 2 - totally correct for partial credit items. </p>  </br> <p>
                                          The correlation between score frequencies in PLACE and in the international pilot has been estimated. An high correlation among all response codes in all domains suggests that there are high similarities 
                                          of the response distributions between students in PLACE and in the international pilot. </p> </br>'),
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
                                          Criterion 3: Discrimination of the correct answer must be greater than 0,2. </p> </br> </br>"),
                                    shiny::numericInput("obs3", label = shiny::h5("Number of items to view"), 10),
                                    shiny::HTML('</br> <b> Mathematics items </b>'),
                                    shiny::tableOutput('table3_math'),
                                    shiny::HTML('</br> <b> Reading items </b>'),
                                    shiny::tableOutput('table3_read'),
                                    shiny::HTML('</br> <b> Science items </b>'),
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
                                  shiny::HTML('</br> <b> Mathematics items </b>'),
                                  shiny::tableOutput('table4_math'),
                                  shiny::HTML('</br> <b> Reading items </b>'),
                                  shiny::tableOutput('table4_read'),
                                  shiny::HTML('</br> <b> Science items </b>'),
                                  shiny::tableOutput('table4_scie')   
                                     ),
                            
                                  shiny::tabPanel("Model fit (Infit)",
                                  shiny::HTML("<b> Model fit (Infit) </b> </br>  <p> For each item parameter, the fit MNSQ (infit) index was 
                                          used to provide an indication of the compatibility of the (international) model and the (national) data. For each student,
                                          the model describes the probability of obtaining the different item scores. It is therefore 
                                          possible to compare the model prediction (with international parameters) and what has been observed for one item across students.
                                          Accumulating comparisons across students gives an item-fit statistic.  </br> </br>
                                          A weighted MNSQ greater than one is associated with a low discrimination index, meaning the data 
                                          exhibits more variability than expected by the model. </br> </br>
                                          PISA for Schools accepts small variations of MNSQ around one, however, values larger than 1.2 indicate
                                          that the item discrimination is lower than assumed by the model, and values below 0.8 show that the item
                                          discrimination is higher than assumed. Only statistically significant values are reported. </p> </br> </br> "),
                                  shiny::numericInput("obs5", label = shiny::h5("Number of items to view"), 10),
                                  shiny::HTML('</br> <b> Mathematics items </b>'),
                                  shiny::tableOutput('table5_math'),
                                  shiny::HTML('</br> <b> Reading items </b>'),
                                  shiny::tableOutput('table5_read'),
                                  shiny::HTML('</br> <b> Science items </b>'),
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
                                  shiny::HTML('</br> <b> Mathematics items </b>'),
                                  shiny::tableOutput('table6_math'),
                                  shiny::HTML('</br> <b> Reading items </b>'),
                                  shiny::tableOutput('table6_read'),
                                  shiny::HTML('</br> <b> Science items </b>'),
                                  shiny::tableOutput('table6_scie')   
                                     )#,
                            
                            #       shiny::tabPanel("Wright Map",
                            #         shiny::sidebarLayout(
                            #           shiny::sidebarPanel(
                            #             shiny::selectInput("domain7", shiny::h5("Choose a domain:"), choices = c("math","read","scie")), 
                            #              width=2
                            #            ),
                            #           shiny::mainPanel(
                            #             shiny::HTML('<b> Wright Map </b> </br> <p>  Theoretically, when candidates and items are opposite each other on the map, the difficulty of the item 
                            #                   and the ability of the candidate are comparable, so the candidate has approximately a 50% probability
                            #                   of answering the item correctly. </p> </br>   </br>'),
                            #             shiny::HTML('</br>'),
                            #             shiny::plotOutput("plot7"),
                            #              width=10
                            #              )
                            #            )
                            # )
                            ),
                 
                 
                 shiny::navbarMenu("Dodgy items review",
                                   
                                   shiny::tabPanel("Item information and characteristic curves",
                                                   
                                                   shiny::HTML('<b> Item information and characteristic curves </b> </br> </br> </br>'),
                                                   
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
                                                       shiny::plotOutput("plot7"),
#                                                       shiny::tableOutput('tablex') ,  
                                                       
                                                       width=9
                                                     )
                                                   )
                                                   
                                   ),
                                   
                                   shiny::tabPanel("Test information and test characteristic curves",
                                                   
                                                   shiny::HTML('<b> Test information and test characteristic curves </b> </br> </br> </br>'),
                                                   
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
                                                   shiny::HTML('<b> Dodgy items </b> </br> </br> </br>'),
                                                   shiny::uiOutput('killitem.ui')
                                                   
                                   )
                            
                 ),
                 
                 shiny::navbarMenu("Results preview",
                                   
                                   
                            
                         shiny::tabPanel("Primary analysis",
                           
                                         shiny::HTML('<b> Primary analysis </b> </br> </br> </br>'),
                           
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
                 
                 shiny::navbarMenu("More",
                                   shiny::tabPanel("Sub-Component A"),
                                   shiny::tabPanel("Sub-Component B"))
                            ) 





