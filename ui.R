ui <- dashboardPage(
  dashboardHeader(title = "Executive Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Substance Use Disorders", tabName="SUD"),
      menuItem("Waivers", tabName="W"),
      menuItem("Capitation, Enrollment & MLR", tabName="CEMLR"),
      menuItem("Chronic Conditions", tabName="CC"),
      menuItem("Foster Care", tabName="FC"),
      menuItem("Nursing Home", tabName="NH"),
      menuItem("Pharmacy", tabName="Pharm")
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "grey_light"),
    tabItems(
      tabItem(tabName="SUD",
              fluidRow(
                h3("Substance Use Disorders", align="center")
              ),
              fluidRow(
                box(width=12, solidHeader=T, collapsible=T, title = "Background on SUD data and helpful tips:"
                    ,p("We can add whatever helpful text we need to in order to guide the user through the dashboard options.")
                )
              ),
              fluidRow(
                box(width=12, solidHeader = T, collapsible = T, title = "SUD Prevalence",
                    fluidRow(column(4,
                                    radioGroupButtons(
                                      "prev_var_select"
                                      ,label = p("Prevalence Calculation:")
                                      ,choiceNames = c("Total Count","Percentage","Rate Per 1000")
                                      ,choiceValues = c("Total Members","Percent","Rate Per 1000")
                                      ,selected = "Rate Per 1000"
                                      ,checkIcon = list(
                                        yes = icon("ok",
                                                   lib = "glyphicon")))),
                             column(2),
                             column(4,
                                    sliderTextInput(
                                      inputId = "time_rng_select",
                                      label = "Date Range:", 
                                      choices = dates[,Dates],
                                      selected = c(dates[,min(Dates)],dates[,max(Dates)]))),
                             column(2,
                                    actionButton(inputId = "time_rng_update",
                                                 label = "Apply Date Filter",
                                                 style="color: #fff; background-color: #42AB35; border-color: #313131"))
                    ),
                    fluidRow(
                      column(6,
                             box(width=12, solidHeader = T, collapsible = F, title = "SUD Prevalence by Drug and Payer",
                                 withLoader(plotlyOutput('SUD_prev_plot', width="auto"), type="html")),
                             box(width=12, solidHeader = F, collapsible = T, title="Interpretation",
                                 p(strong("Data:"), "Any member who received a SUD diagnosis (ICD10 F code) enters the SUD population and remains there over time. We do not remove them from the population after observing a remission diagnosis due to the high rates of relapse. In the visualizaion above, we calculated the monthly prevalence rate and then averaged that prevalence rate over the time period you've selected."),
                                 p(strong("Synopsis:"), "As you can see, Nicotine SUDs are the most prominent, which is very consistent with other states and non-Medicaid populations as well. There is an interesting spread among your payers in the Nicotine SUD rates, which could be due to the mix of their population they've inherited, and/or a more proactive SUD mitigation strategy might encourage providers to assess and diagnose more Nicotine SUDs - which would be a good thing."),
                                 p(strong("What now?:"), "You can use the lasso symbol in the visualization to select a particular SUD population. For example, you could highlight just the Opioid SUD population and see their cost distribution, time to treatment, etc. all below."))),
                      column(6,
                             box(width=12, solidHeader = T, collapsible = F, title = "SUD Prevalence by Payer Over Time",
                                 withLoader(plotlyOutput('SUD_prev_time_plot', width="auto"), type="html")))
                    ))),
              fluidRow(
                box(width=12, solidHeader=T, collapsible=T, title = "SUD Cost & Utilization",
                    fluidRow(
                      column(6,
                             box(width=12, solidHeader = T, collapsible = F, title = "Cost Distribution by Payer",
                                 withLoader(plotlyOutput('cost_ridgeplot', width="auto"), type="html"))),
                      column(6,
                             box(width=12, solidHeader = T, collapsible = F, title = "Cost Breakdown by Payer",
                                 withLoader(plotlyOutput('cost_stackBar', width="auto"), type="html")))
                    ),
                    fluidRow(
                      column(2, 
                             actionButton(inputId = "cost_rng_update",
                                          label = "Filter on selected cost range",
                                          style="color: #fff; background-color: #42AB35; border-color: #313131")),
                      column(4,
                             textOutput("cost_rng_selected"))
                    ))),
              fluidRow(
                box(width=12, solidHeader=T, collapsible=T, title = "SUD Treatment",
                    fluidRow(
                      column(6,
                             box(width=12, solidHeader=T, colapsible=F, title="Time to First SUD Treatment",
                                 withLoader(plotlyOutput('tx_time_plot', width="auto"), type="html"))),
                      column(6,
                             box(width=12, solidHeader=T, colapsible=F, title="Time to First SUD Treatment",
                                 withLoader(plotlyOutput('tx_breakdown_plot', width="auto"), type="html"))
                             
                      )
                    )
                )
              )
              
              
      ),
      tabItem(tabName="W",
              fluidRow(
                h1("Waiver visuals here")
              )),
      tabItem(tabName="CEMLR",
              fluidRow(
                h1("Capitation, Spend, MLR & Enrollment", align="center")
              ),
              fluidRow(
                box(width=12, solidHeader=T, collapsible=T, title = "Background on Capitation, Encounter, MLR, & Enrollment:"
                    ,p("We can add whatever helpful text we need to in order to guide the user through the dashboard options.")
                )
              ),
              fluidRow(
                box(width=12, solidHeader=T, collapsible=T, title = "Global Filters and Configurations",
                    fluidRow(column(1),
                             column(5,
                                    radioGroupButtons(
                                      "group_pop"
                                      ,label = p("Select Breakdown of Members:")
                                      ,choiceNames = c("Medicaid Eligibility Group (MEG)","Capitation Rate Cell","Population Group")
                                      ,choiceValues = c("kancare_meg","cap_category","pop_desc_cap")
                                      ,selected = "kancare_meg"
                                      ,checkIcon = list(
                                        yes = icon("ok",
                                                   lib = "glyphicon")))),
                             column(1),
                             column(4,
                                    sliderTextInput(
                                      inputId = "time_rng_select_t3",
                                      label = "Date Range:",
                                      choices = cost_cap_mlr[,unique(Date)],
                                      selected = c(cost_cap_mlr[,min(Date)],cost_cap_mlr[,max(Date)]))),
                             column(1)),
                    fluidRow(column(8),
                             column(3,
                                    actionButton(inputId = "time_rng_update_t3",
                                                 label = "Filter on selected date range",
                                                 style="color: #fff; background-color: #42AB35; border-color: #313131")),
                             column(2)
                    )
                )
              ),
              fluidRow(
                box(width=12, solidHeader = T, collapsible = T, title = "Medical Loss Ratio",
                    fluidRow(column(2),
                             column(3,
                                    selectizeInput(
                                      inputId = "dot_size_var"
                                      ,label=p("Circle Size Represents:")
                                      ,choices=c("Avg Monthly Member Cnt"
                                                 , "Total Encounter Spend"
                                                 , "Total Capitation Paid"
                                                 , "Avg PMPM"
                                                 , "Median PMPM")
                                      ,selected = 'Total Encounter Spend'
                                      ,multiple=FALSE)
                             ),
                             column(2),
                             column(3,
                                    numericInput('targetMLR'
                                                 ,'Target MLR(%)'
                                                 , value=93.5
                                                 , step=0.5
                                                 , min=80
                                                 , max=120)),
                             column(2)
                    ),
                    fluidRow(column(6,
                                    box(width=12, solidHeader = T, collapsible = F, title = "MLR by Payer and Member Grouping",
                                        withLoader(plotlyOutput('MLR_dotplot', width="auto", height=1000), type="html"))),
                             column(6,
                                    box(width=12, solidHeader = T, collapsible = F, title = "MLR by Payer Over Time",
                                        withLoader(plotlyOutput('MLR_ts_plot', width="auto", height=1000), type="html"))))
                )),
              fluidRow(
                box(width=12, solidHeader = T, collapsible = T, title = "Capitation Paid and Encounter Spend",
                    fluidRow(column(4),
                             column(4,
                                    radioGroupButtons(
                                      "cap_or_enc_totals"
                                      ,label = "" #p("Cost Variable:")
                                      ,choiceNames = c("Total Capitation","Total Encounter","% Capitation", "% Encounter")
                                      ,choiceValues = c("Total Capitation Paid","Total Encounter Spend","Percent of Total Capitation Paid", "Percent of Total Program Spend")
                                      ,selected = "Percent of Total Program Spend"
                                      ,checkIcon = list(
                                        yes = icon("ok",
                                                   lib = "glyphicon")))),
                             column(4)
                    ),
                    fluidRow(column(6,
                                    box(width=12, solidHeader=T, collapsible = F, title = "Capitation or Encounter - Total and Proportions by Payer",
                                        withLoader(plotlyOutput('cap_enc_totals', width='auto'), type='html'))),
                             column(6,
                                    box(width=12, solidHeader=T, collapsible=F, title="Capitation/Encounter By Payer Over Time",
                                        withLoader(plotlyOutput('cap_enc_ts_plot', width='auto')), type='html'))
                    )
                    
                )
              ),
              fluidRow(
                box(width=12, solidHeader = T, collapsible = T, title = "Per Member Per Month Spend (PMPM)",
                    fluidRow(column(4),
                             column(4,
                                    radioGroupButtons(
                                      "pmpm_calc_select"
                                      ,label = p("Select PMPM Calculation:")
                                      ,choiceNames = c("Average","Median")
                                      ,choiceValues = c("Median PMPM","Avg PMPM")
                                      ,selected = "Median PMPM"
                                      ,checkIcon = list(
                                        yes = icon("ok",
                                                   lib = "glyphicon")))),
                             column(4)
                    ),
                    fluidRow(column(6,
                                    box(width=12, solidHeader=T, collapsible = F, title = "PMPM Spend by Population Group and Payer",
                                        withLoader(plotlyOutput('pmpm_dotplot', width='auto'), type='html'))),
                             column(6,
                                    box(width=12, solidHeader=T, collapsible=F, title="Capitation/Encounter By Payer Over Time",
                                        withLoader(plotlyOutput('pmpm_ts_plot', width='auto')), type='html'))
                    )
                )
              ),
              fluidRow(
                box(width=12, solidHeader = T, collapsible = T, title = "Enrollment",
                    fluidRow(column(1),
                             column(4,
                                    radioGroupButtons(
                                      "enr.group.var"
                                      ,label = p("Enrollment Breakdown:")
                                      ,choiceNames = enr_cap_mlr[,unique(Pop_group)]
                                      ,choiceValues = enr_cap_mlr[,unique(Pop_group)]
                                      ,selected = enr_cap_mlr[,unique(Pop_group)][1]
                                      ,checkIcon = list(
                                        yes = icon("ok",
                                                   lib = "glyphicon")))),
                             column(2),
                             column(4,
                                    radioGroupButtons(
                                      "enr_calc"
                                      ,label = p("Enrollment Calculation:")
                                      ,choiceNames = c("Avg. Monthly Count","% Payer Population")
                                      ,choiceValues = c(0,1)
                                      ,selected = 0
                                      ,checkIcon = list(
                                        yes = icon("ok",
                                                   lib = "glyphicon")))),
                             
                             column(1)
                    ),
                    fluidRow(column(6,
                                    box(width=12, solidHeader=T, collapsible = F, title = "Enrollment by Payer and Group",
                                        withLoader(plotlyOutput('enr_stackbar', width='auto'), type='html'))),
                             column(6,
                                    box(width=12, solidHeader=T, collapsible=F, title="Enrollment by Payer Over Time",
                                        withLoader(plotlyOutput('enr_ts', width='auto'), type='html')))
                    )
                )
              )
      ),
      tabItem(tabName="CC",
              fluidRow(
                h1("Chronic Conditions visuals here")
              )),
      tabItem(tabName="FC",
              fluidRow(
                h1("Foster Care visuals here")
              )),
      tabItem(tabName="NH",
              fluidRow(
                h1("Nursing Home visuals here")
              )),
      tabItem(tabName="Pharm",
              fluidRow(
                h1("Pharmacy visuals here")
              ))
    )
  )
)