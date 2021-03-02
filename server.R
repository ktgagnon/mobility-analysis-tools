server <- function(input, output) {
  
  #########################################
  ######## Reactive Data & Filters ########
  #########################################
  
  
  ############## Cost Filters ###################
  cost_fltr_vals <- reactive({
    evnt_data <- event_data(event = "plotly_relayout", source="cost_ridgeplot")
    cost_fltr_vals <- c("min_cost"=round(10^(as.numeric(evnt_data$`xaxis.range[0]`)),2)
                        ,"max_cost"=round(10^(as.numeric(evnt_data$`xaxis.range[1]`)),2))
    return(cost_fltr_vals)
  })
  cost_id_fltr_react <- eventReactive(input$cost_rng_update,{
    if(length(cost_fltr_vals())==0){
      cost_id_fltr_react <- memberids[,.(member_id)]
    } else {
      cost_id_fltr_react <- cost_pmpm()[Avg_PMPM_paid >= cost_fltr_vals()[1] & Avg_PMPM_paid <= cost_fltr_vals()[2],.(member_id)]
    }
  }) 
  cost_id_fltr <- reactive({
    if(input$cost_rng_update==0){
      cost_id_fltr <- memberids[,.(member_id)]
    } else {
      cost_id_fltr <- cost_id_fltr_react()
    }
  })
  # printing cost range selected to the UI next to action button
  output$cost_rng_selected <- renderText({
    if(length(cost_fltr_vals())==0){
      cost_rng_selected = "Currently viewing the entire cost distribution"
    } else {
      cost_rng_selected = paste0("Viewing costs between ","$",cost_fltr_vals()[1]," and ","$",cost_fltr_vals()[2])
    }
  })
  ##############################################
  
  ############## Date Filter ###################
  date_id_fltr_react <- eventReactive(input$time_rng_update,{
    date_id_fltr_react <- dates[Dates >= min(input$time_rng_select) & Dates <= max(input$time_rng_select), .(date_id)]
  })
  date_id_fltr <- reactive({
    if(input$time_rng_update==0){
      date_id_fltr <- dates[,.(date_id)]
    } else {
      date_id_fltr <- date_id_fltr_react()
    }
    return(date_id_fltr)
  })
  ##############################################
  # Payer - Drug Filters
  payer_drug_fltr <- reactive({
    evnt_data <- event_data(event = "plotly_selected", source="prev_plot")
    payer_drug_fltr <- paste0(evnt_data$customdata,evnt_data$key)
    return(payer_drug_fltr)
  })
  payer_drug_id_fltr <- reactive({
    if(identical(payer_drug_fltr(), character(0))){
      payer_drug_id_fltr <- memberids[,.(member_id)]
    } else {
      # Unsure if I need to filter from prev_long_fltr() or original prev_long
      payer_drug_id_fltr <- unique(prev_long[paste0(payer_id,`Drug Category`) %in% payer_drug_fltr(),.("member_id"=member_id)])
    }
  })
  
  # Filter prev_long by date and cost filters 
  prev_long_fltr <- reactive({
     if(sum(input$time_rng_update,input$cost_rng_update)==0){
       return(prev_long)
     } else {
       return(prev_long[date_id %in% date_id_fltr()[,date_id] & member_id %in% cost_id_fltr()[,member_id],])
     }
  })
 
  
  # Filter cost_wide by date and drug-payer filters and remove cost/util rows that occur before their first sud diagnosis. This may not be the same sud filtered to by user, but it's the members first sud dx of any type.
  cost_wide_fltr <- reactive({
    if(isTruthy(input$time_rng_update) | isTruthy(event_data(event = "plotly_selected", source="prev_plot"))){
      cost_wide_fltr <- cost_wide[date_id %in% date_id_fltr()[,date_id] & member_id %in% payer_drug_id_fltr()[,member_id],]
    } else {
      return(cost_wide)
    }
  })
  
  # Aggregate enrollment counts based on any necessary filters
  enr_cnts_agg <- reactive({
    enr_cnts_agg <-  enr_cnts[# Here I can filter out demographics like zip, race, gender, age group
      ,.("Total_member_cnt"=sum(total_enrolled,na.rm=TRUE))
      , by=c("payer_id","payer_name","date_id")]
    return(enr_cnts_agg)
  })
  
  # Dataset for prevalence Dot Plot
  sud_prev_plot_data <- reactive({
    sud_prev_plot_data <- enr_cnts_agg()[ 
      prev_long_fltr()[ # Inner Join sud counts aggregated over payer, drug, and date
        ,.("SUD_member_cnt"=length(unique(member_id)))
        ,by=c("payer_id","Drug Category","date_id")]
      ,on=c("payer_id","date_id"), nomatch=0][
        ,`:=`("Percent"=round((SUD_member_cnt/Total_member_cnt)*100,2)# Create prevalence calculations
              ,"Rate Per 1000"=round(SUD_member_cnt/(Total_member_cnt/1000),1))][
                ,.("Total Members"=round(mean(SUD_member_cnt, na.rm=TRUE),0)# Average monthly prevalence calculations by payer and drug category
                   ,"Percent"=round(mean(Percent, na.rm=TRUE),2)
                   ,"Rate Per 1000"=round(mean(`Rate Per 1000`, na.rm=TRUE),1))
                ,by=c("payer_id","payer_name","Drug Category")]
  })
  
  # Dataset for prevalence time-series plot
  sud_prev_ts_data <- reactive({
    if(is.null(event_data(event = "plotly_selected", source="prev_plot"))){
      ts_data <- prev_long_fltr()
    } else {
      ts_data <- prev_long_fltr()[member_id %in% payer_drug_id_fltr()[,member_id],] #paste0(payer_id,`Drug Category`) %in% payer_drug_fltr()
    }
    sud_prev_ts_data <- enr_cnts_agg()[
                  ts_data[
                       ,.("Total Members"=length(unique(member_id)))
                       ,by=c("payer_id","date_id","Date")]
      ,on=c("payer_id","date_id"), nomatch=0][
        # Create prevalence calculations
        ,`:=`("Percent"=round((`Total Members`/Total_member_cnt)*100,2)
              ,"Rate Per 1000"=round(`Total Members`/(Total_member_cnt/1000),1))]
    setorder(sud_prev_ts_data, cols=date_id)
    return(sud_prev_ts_data)
  })
  
  # Dataset for cost ridgeplots
  non_sud_agg <- reactive({
    if(isTruthy(input$time_rng_update) | isTruthy(event_data(event = "plotly_selected", source="prev_plot"))){
      nsa_data <- non_sud_cost[date_id %in% date_id_fltr()[,date_id],]
    }else{
      nsa_data <- non_sud_cost
    }
    non_sud_agg <- nsa_data[
                                ,.("Non SUD Median PMPM"=median(pmpm_paid, na.rm=TRUE)
                                   ,"Non SUD Avg PMPM" =mean(pmpm_paid, na.rm=TRUE)
                                   ,"Non SUD Median MLR"=median(mlr, na.rm=TRUE)
                                   ,"Non SUD Avg MLR"=mean(mlr, na.rm=TRUE))
                                , by=c("payer_id","payer_name")]
  })
  
  cost_pmpm <- reactive({
    cost_wide_fltr()[
      ,.("Avg_PMPM_paid" = mean(claim_paid_total)
         ,"Avg_mlr" = mean(MLR_pmpm, na.rm=TRUE)
         ,"PMPM_paid_total" = sum(claim_paid_total)
         ,"PMPM_paid_inp"= sum(inp_stay_cost, na.rm=TRUE)
         ,"PMPM_paid_ed" = sum(total_ed_cost, na.rm=TRUE)
         ,"PMPM_paid_tx" = sum(Total_SUD_Tx_cost, na.rm=TRUE)
         ,"Alc_MAT_cnt" = sum(alcohol_sud_mat)
         ,"Nic_MAT_cnt" = sum(nicotine_sud_mat)
         ,"Op_MAT_cnt" = sum(opioid_sud_mat)
         ,"Alc_psych_cnt" = sum(alcohol_sud_psychtx)
         ,"Nic_psych_cnt" = sum(nicotine_sud_psychtx)
         ,"General_psych_cnt" = sum(general_sud_psychtx))
      , by=c("payer_name","payer_id","member_id","mos_to_any_tx","Months Observing Tx")][
        ,`:=`("Median PMPM"= median(Avg_PMPM_paid, na.rm=TRUE)
              ,"Avg PMPM"=mean(Avg_PMPM_paid, na.rm=TRUE)
              ,"Median MLR"=median(Avg_mlr, na.rm=TRUE)
              ,"Avg MLR"=mean(Avg_mlr, na.rm=TRUE))
        , by=c('payer_name',"payer_id")]
    
  })
  
  sud_cost_density <- reactive({
    density_data <-cost_pmpm()[
      ,.("PMPM Cost" = round(expm1(density(log1p(Avg_PMPM_paid)
                                           ,bw = "nrd0"
                                           ,adjust = 1
                                           ,kernel = "gaussian"
                                           ,n = 512
                                           , na.rm = FALSE)$x),2)
         , "PMPM Density" = density(log1p(Avg_PMPM_paid)
                                    ,bw = "nrd0"
                                    ,adjust = 1
                                    ,kernel = "gaussian"
                                    ,n = 512
                                    , na.rm = FALSE)$y)
      , by=c("payer_name","payer_id","Median PMPM","Avg PMPM")]
    sud_cost_density <- non_sud_agg()[density_data, on="payer_id"]
    return(sud_cost_density)
  })
  
  cost_breakdown_percent <- reactive({
    cost_pmpm()[member_id %in% cost_id_fltr()[,member_id]
                ,.("Total_Spend" = sum(PMPM_paid_total)
                   ,"Total_Inp_Spend" = sum(PMPM_paid_inp)
                   ,"Total_ED_Spend" = sum(PMPM_paid_ed)
                   ,"Total_SUDTx_Spend" = sum(PMPM_paid_tx))
                , by=c("payer_id","payer_name")][
                  ,`:=`("Total_Other_Spend" = Total_Spend - (Total_Inp_Spend + Total_ED_Spend + Total_SUDTx_Spend)
                        ,"Perc_ED_Spend" = round((Total_ED_Spend / Total_Spend)*100,1)
                        ,"Perc_Inp_Spend" = round((Total_Inp_Spend / Total_Spend)*100,1)
                        ,"Perc_SUDTx_Spend" = round((Total_SUDTx_Spend / Total_Spend)*100,1)
                        ,"Perc_Other_Spend" = round(((Total_Spend - (Total_Inp_Spend + Total_ED_Spend + Total_SUDTx_Spend))/Total_Spend)*100,2))
                ]
  })
  
  
  ### Make treatment time plot and treatment type breakdown
  tx_time_props <- reactive({
    # Count number of distinct members who have been diagnosed with whatever SUD(s) prev_long is filtered to, and who are within filtered cost range (if filtered yet)
    total_new_dx <-cost_wide_fltr()[(mos_to_any_tx > -1 | is.na(mos_to_any_tx)) & member_id %in% cost_id_fltr()[,member_id]
                                    ,.("Total_New_Dx" = length(unique(member_id)))
                                    ,by=c("payer_name")]
    # Now 
    total_treated <- cost_wide_fltr()[mos_to_any_tx > -1 & member_id %in% cost_id_fltr()[,member_id]
                                      ,.("Total_Treated" = length(unique(member_id)))
                                      ,by=c("payer_name", "mos_to_any_tx")]
    tx_time_props <- total_treated[total_new_dx, on=c("payer_name")][
      ,"Percentage Treated":= round((Total_Treated/Total_New_Dx)*100,2)
      , by=c("payer_name","mos_to_any_tx")]
    setorderv(tx_time_props, cols=c("payer_name","mos_to_any_tx"), order=1L, na.last=TRUE)
    tx_time_props <- tx_time_props[
      ,"Cumulative Percent Treated":=cumsum(`Percentage Treated`)
      ,by="payer_name"]
    
    return(tx_time_props)
  })
  
  
  tx_breakdown <- reactive({
    tx_long <- melt(cost_pmpm()[member_id %in% cost_id_fltr()[,member_id],]
                    , id.vars=c("member_id","payer_name","mos_to_any_tx","Months Observing Tx")
                    , measure.vars=c("Op_MAT_cnt","Nic_MAT_cnt","Alc_MAT_cnt","Alc_psych_cnt","Nic_psych_cnt","General_psych_cnt")
                    , variable.name="Treatment_Type"
                    , value.name="Treatment_Count")
    
    # stacked bar for just counts by sud treatment type
    tx_breakdown  <- tx_long[mos_to_any_tx>-1 & Treatment_Count > 0 #Add optional filter to only those who we've observed 12+ months on
                             ,.("Member_Cnt"=length(unique(member_id))), by=c("payer_name","Treatment_Type")]
    
  })
  
  
  ##########################################
  ########### Prevalence Plots #############
  ##########################################
  # Create Prevalence Dot Plot
  output$SUD_prev_plot <- renderPlotly({
    # Define inputs to pass to plotting function.
    data=sud_prev_plot_data()
    x="Drug Category"
    y=input$prev_var_select
    color="payer_name"
    x.title="Drug Category"
    y.title=paste("Monthly Average", input$prev_var_select)
    legend.title="Payer Name"
    source="prev_plot"
    key2.var="payer_id"
    # Make plot
    SUD_prev_plot <- multigroup_dot_plot(data=data,x=x,y=y,color=color,x.title=x.title,y.title=y.title,legend.title=legend.title,source=source,key2.var=key2.var)
    # Return plot
    return(SUD_prev_plot) 
  })
  
  # Create Prevalence Time Series Plot
  output$SUD_prev_time_plot <- renderPlotly({
    # Define some inputs for plotting function
    data=sud_prev_ts_data()
    x="Date"
    y=input$prev_var_select
    color="payer_name"
    #min.time=min(input$time_rng_select)
    #max.time=max(input$time_rng_select)
    
    # Make time series plot
    SUD_prev_time_plot <- time_series_plot(data=data,x=x,y=y,color=color,y.title=input$prev_var_select,x.title="Date")
    
    # Return plot
    return(SUD_prev_time_plot)
  })
  
  #########################################
  ############### Cost Plots ##############
  #########################################
  
  output$cost_ridgeplot <- renderPlotly({
    # Define inputs for ridgeplot
    x.title="Average PMPM Spend ($)" #this will change depending on what cost variable user selects
    y.title="<br>Density"
    legend.title="Payer Name"
    hover.text.density = "%{text}<br>Cost: %{x:$.2f}<extra></extra>"
    hover.text.ref.line1="SUD Population<br>Median Cost: %{x:$.2f}"
    hover.text.ref.line2="Non-SUD Population<br>Median Cost: %{x:$.2f}"
    
    cost_ridgeplot <- ridgeplot(data=sud_cost_density(), x="PMPM Cost", x.title=x.title, y="PMPM Density", y.title=y.title, legend.title=legend.title, subplot.grp.var="payer_name", ref.line1="Median PMPM", ref.line2="Non SUD Median PMPM", hover.text.density=hover.text.density, hover.text.ref.line1=hover.text.ref.line1, hover.text.ref.line2=hover.text.ref.line2)
  })
  output$cost_stackBar <- renderPlotly({
    data=cost_breakdown_percent()
    x="Perc_Inp_Spend"
    y="payer_name"
    stack1="Perc_ED_Spend"
    stack2="Perc_SUDTx_Spend"
    stack3="Perc_Other_Spend"
    y.title="Payers"
    x.title="Percentage of Total Encounter Cost"
    legend.title="Cost Category"
    stack0.name="Inpatient"
    stack1.name="ED"
    stack2.name="SUD Treatment"
    stack3.name="Everything Else"
    
    cost_stackBar <- stackBar_plot(data,x,y,stack1,stack2,stack3,x.title,y.title,legend.title,stack0.name,stack1.name,stack2.name,stack3.name)
  })
  
  output$tx_time_plot <- renderPlotly({
    data=tx_time_props()
    x="mos_to_any_tx" #input variable to select total vs. percent vs. cumulative percent
    y="Cumulative Percent Treated"
    group.var="payer_name"
    x.title="Months between 1st SUD Diagnosis and 1st SUD Treatment"
    y.title=y
    legend.title="Payer Name"
    hover.text.density=paste(y,": %%{y:.1f}<br>Months since 1st Diagnosis: %{x}")
    
    tx_time_plot <- cumulativePercentPlot(data, x, x.title, y, y.title,legend.title, group.var, hover.text.density)
  })
  
  output$tx_breakdown_plot <- renderPlotly({
    data=tx_breakdown()
    data %>%
      plot_ly(x = ~payer_name
              , y = ~Member_Cnt
              , color = ~Treatment_Type
              , type = "bar"
              , orientation = 'v') %>% 
      layout(barmode="stack")
    
  })
  
  ##################################################################################################################  
  ##################################################################################################################    
  ######################################## Capitation, Enrollment, MLR Tab #########################################
  ##################################################################################################################
  ##################################################################################################################
  
  
  ############## Date Filter ###################
  mlr_fltr <- reactive({
    evnt_data <- event_data(event = "plotly_selected", source="MLR_dot")
    mlr_fltr <- paste0(evnt_data$customdata,evnt_data$key)
    mlr_fltr
  })
  
  ##############################################
  enr_grp_fltr <- reactive({
    evnt_data <- event_data(event = "plotly_click", source="enr_groupbar")
    if(is.null(evnt_data)){
      enr_grp_fltr <- unique(enr_cap_mlr[,Pop_subgroup]) 
    }else{
      enr_grp_fltr <- evnt_data$y
    }
  })
  
  
  cost_agg<- reactive({
    if(input$group_pop=="cap_category"){
      grouping_vars = c("rate_cell", "cap_category")
    } else if(input$group_pop=="pop_desc_cap"){
      grouping_vars = c("pop_desc_cap")
    } else if(input$group_pop=="kancare_meg"){
      grouping_vars=c("kancare_meg")
    }
    if(identical(mlr_fltr(), character(0))){
      cost_agg <- cost_cap_mlr[Payer_Name != "FFS" & capitation_paid>0
                               ,.("Total Capitation Paid"=round(sum(capitation_paid),2)
                                  ,"Total Encounter Spend"=round(sum(encounter_cost),2)
                                  ,"MLR"=round(sum(encounter_cost)/sum(capitation_paid),2)
                                  ,"MLR_percent"=round((sum(encounter_cost)/sum(capitation_paid))*100,2)
                                  ,"Avg PMPM"=round(mean(cap_PMPM),2)
                                  ,"Median PMPM"=round(median(cap_PMPM),2)
                                  ,"Avg Monthly Member Cnt"=round(mean(member_count)))
                               ,by=c("Payer_Name", grouping_vars)][
                                 ,`:=`("Grand Total Spend"=sum(`Total Encounter Spend`)
                                       ,"Percent of Total Program Spend"=round((`Total Encounter Spend`/sum(`Total Encounter Spend`))*100,2)
                                       ,"Grand Total Cap Paid"=sum(`Total Capitation Paid`)
                                       ,"Percent of Total Capitation Paid"=round((`Total Capitation Paid`/sum(`Total Capitation Paid`))*100,2)
                                       ,"Grand Average PMPM"=round(mean(`Avg PMPM`),2)
                                       ,"Grand Median PMPM"=round(mean(`Median PMPM`),2))
                               ]  
      
    }else{
      cost_agg <- cost_cap_mlr[Payer_Name != "FFS" & paste0(Payer_Name,get(grouping_vars[1])) %in% mlr_fltr()
                               ,.("Total Capitation Paid"=round(sum(capitation_paid),2)
                                  ,"Total Encounter Spend"=round(sum(encounter_cost),2)
                                  ,"MLR"=round(sum(encounter_cost)/sum(capitation_paid),2)
                                  ,"MLR_percent"=round((sum(encounter_cost)/sum(capitation_paid))*100,2)
                                  ,"Avg PMPM"=round(mean(cap_PMPM),2)
                                  ,"Median PMPM"=round(median(cap_PMPM),2)
                                  ,"Avg Monthly Member Cnt"=round(mean(member_count)))
                               ,by=c("Payer_Name", grouping_vars)][
                                 ,`:=`("Grand Total Spend"=sum(`Total Encounter Spend`)
                                       ,"Percent of Total Program Spend"=round((`Total Encounter Spend`/sum(`Total Encounter Spend`))*100,2)
                                       ,"Grand Total Cap Paid"=sum(`Total Capitation Paid`)
                                       ,"Percent of Total Capitation Paid"=round((`Total Capitation Paid`/sum(`Total Capitation Paid`))*100,2)
                                       ,"Grand Average PMPM"=round(mean(`Avg PMPM`),2)
                                       ,"Grand Median PMPM"=round(mean(`Median PMPM`),2))
                               ]   
    }
    
  })
  
  cost_agg_ts <- reactive({
    if(input$group_pop=="cap_category"){
      grouping_vars = c("rate_cell", "cap_category")
    } else if(input$group_pop=="pop_desc_cap"){
      grouping_vars = c("pop_desc_cap")
    } else if(input$group_pop=="kancare_meg"){
      grouping_vars=c("kancare_meg")
    }
    if(identical(mlr_fltr(), character(0))){
      cost_agg_ts <- cost_cap_mlr[Payer_Name != "FFS" & capitation_paid>0
                                  ,.("Total Capitation Paid"=round(sum(capitation_paid),2)
                                     ,"Total Encounter Spend"=round(sum(encounter_cost),2)
                                     ,"MLR"=round(sum(encounter_cost)/sum(capitation_paid),2)
                                     ,"MLR_percent"=round((sum(encounter_cost)/sum(capitation_paid))*100,2)
                                     ,"Avg PMPM"=round(mean(cap_PMPM),2)
                                     ,"Median PMPM"=round(median(cap_PMPM),2)
                                     ,"Avg Monthly Member Cnt"=round(mean(member_count)))
                                  ,by=c("Payer_Name", "Date", "elig_month", "date_id")][
                                    ,`:=`("MLR_percent_smooth"=predict(loess(MLR_percent ~ date_id), se=TRUE)$fit
                                          , "MLR_percent_smooth_error"=predict(loess(MLR_percent ~ date_id), se=TRUE)$se.fit)
                                    , by =c("Payer_Name")][
                                      ,`:=`("MLR_percent_smooth_lci"=MLR_percent_smooth - 1.96*MLR_percent_smooth_error
                                            ,"MLR_percent_smooth_uci"=MLR_percent_smooth + 1.96*MLR_percent_smooth_error)][
                                              ,`:=`("Grand Total Encounter Spend"=round(sum(`Total Encounter Spend`),2)
                                                    ,"Grand Total Capitation Paid"=round(sum(`Total Capitation Paid`),2)
                                                    ,"Percent of Total Program Spend"=round(`Total Encounter Spend`/sum(`Total Encounter Spend`),2)*100
                                                    ,"Percent of Total Capitation Paid"=round(`Total Capitation Paid`/sum(`Total Capitation Paid`),2)*100)
                                              ,by = c("Date")]
    }else{
      cost_agg_ts <- cost_cap_mlr[Payer_Name != "FFS" & capitation_paid>0 & paste0(Payer_Name,get(grouping_vars[1])) %in% mlr_fltr()
                                  ,.("Total Capitation Paid"=round(sum(capitation_paid),2)
                                     ,"Total Encounter Spend"=round(sum(encounter_cost),2)
                                     ,"MLR"=round(sum(encounter_cost)/sum(capitation_paid),2)
                                     ,"MLR_percent"=round((sum(encounter_cost)/sum(capitation_paid))*100,2)
                                     ,"Avg PMPM"=round(mean(cap_PMPM),2)
                                     ,"Median PMPM"=round(median(cap_PMPM),2)
                                     ,"Avg Monthly Member Cnt"=round(mean(member_count)))
                                  ,by=c("Payer_Name", "Date", "elig_month", "date_id")][
                                    ,`:=`("MLR_percent_smooth"=predict(loess(MLR_percent ~ date_id), se=TRUE)$fit
                                          , "MLR_percent_smooth_error"=predict(loess(MLR_percent ~ date_id), se=TRUE)$se.fit)
                                    , by =c("Payer_Name")][
                                      ,`:=`("MLR_percent_smooth_lci"=MLR_percent_smooth - 1.96*MLR_percent_smooth_error
                                            ,"MLR_percent_smooth_uci"=MLR_percent_smooth + 1.96*MLR_percent_smooth_error)][
                                              ,`:=`("Grand Total Encounter Spend"=round(sum(`Total Encounter Spend`),2)
                                                    ,"Grand Total Capitation Paid"=round(sum(`Total Capitation Paid`),2)
                                                    ,"Percent of Total Program Spend"=round(`Total Encounter Spend`/sum(`Total Encounter Spend`),2)*100
                                                    ,"Percent of Total Capitation Paid"=round(`Total Capitation Paid`/sum(`Total Capitation Paid`),2)*100)
                                              ,by = c("Date")]
    }
    
    
  })
  
  enr_agg <- reactive({
    enr_agg <- enr_cap_mlr[Pop_group %in% input$enr.group.var #filter to whichever user has selected
                           ,.("avg_member_count"=round(mean(member_count))
                              ,"avg_perc_member_count"=round(mean(percent_member_count),2))
                           ,by=c("Payer_Name","Pop_subgroup")]
  })
  
  enr_agg_ts <- reactive({
    enr_agg_ts <- enr_cap_mlr[Pop_group %in% input$enr.group.var & Pop_subgroup !='' & Pop_subgroup %in% c(enr_grp_fltr())# here filter out MEGs or funding sources from enr_agg custom data
                              ,.("total_member_count"=sum(member_count)
                                 ,"percent_member_count"=sum(percent_member_count))
                              ,by=c("Payer_Name","Date","date_id")]
    
    #                         ,.("avg_member_count"=round(mean(member_count))
    #    ,"avg_perc_member_count"=round(mean(percent_member_count),2))
    # ,by=c("Payer_Name","Date","date_id")]
  })
  
  
  output$MLR_dotplot <- renderPlotly({
    
    MLR_dotplot <- multigroup_dot_plot_vert(data=cost_agg()
                                            , source="MLR_dot"
                                            , x="MLR_percent"
                                            , y=input$group_pop
                                            , y.title=""
                                            , x.title="Medical Loss Ratio (%)"
                                            , legend.title="Payer Name"
                                            , x.scale.log=TRUE
                                            , key2.var="Payer_Name"
                                            , marker.size.var=input$dot_size_var
                                            , ref.line1=input$targetMLR
                                            , ref.line1.units="%"
                                            , hover.text.ref.line1="MLR Target"
                                            , color="Payer_Name"
                                            , marker.size.text="Avg. Member Count"
                                            , hover.text.markers.units="%")
  })
  
  # Time series MLR plot (subplots with loess smoother)
  output$MLR_ts_plot <- renderPlotly({
    MLR_ts_plot <- time_series_plot_smoother_subplots(data = cost_agg_ts()
                                                      , x = "Date"
                                                      , y = "MLR_percent"
                                                      , y.axis.title = "MLR %"
                                                      , x.axis.title = "Date"
                                                      , legend.title = "Payer Name"
                                                      , y.smooth = "MLR_percent_smooth"
                                                      , y.smooth.lci = "MLR_percent_smooth_lci"
                                                      , y.smooth.uci = "MLR_percent_smooth_uci"
                                                      , subplot.grp.var = "Payer_Name"
                                                      , ref.line1 = input$targetMLR)
  })
  
  output$cap_enc_totals <- renderPlotly({
    data=cost_agg()
    x=input$cap_or_enc_totals
    y=input$group_pop
    color="Payer_Name"
    if(input$cap_or_enc_totals %in% c("Percent of Total Capitation Paid", "Percent of Total Program Spend")){
      hover.text = paste("%{x:.1f}% of total")
    } else {
      hover.text = paste("%{x:$,.0f}")
    }
    y.title=""
    x.title=x
    legend.title=color
    
    y_disp <- list(
      title = y.title,
      showticklabels=TRUE,
      dtick=1,
      tickfont = list(size=9),
      categoryorder="array",
      categoryarray=~get(x))
    x_disp <- list(
      title = x.title)
    leg_disp <- list(
      title=list(text=legend.title, size=8),
      font = list(size=10))
    title_disp <- list(
      text="",
      x=0.2,
      y=0.98)
    m <- list(
      t = 20,
      l = 20,
      b = 5
    )
    
    data %>%
      plot_ly(x = ~get(x)
              , y = ~get(y)
              , color = ~get(color)
              , type = "bar"
              , orientation = 'h'
              , hovertemplate=hover.text) %>% 
      layout(barmode="stack"
             ,title=title_disp
             ,yaxis=y_disp
             ,xaxis=x_disp
             ,legend=leg_disp
             ,margin=m)
  })
  
  output$cap_enc_ts_plot <- renderPlotly({
    data=cost_agg_ts()
    x="Date"
    y=input$cap_or_enc_totals
    color = "Payer_Name"
    
    cap_enc_ts_plot <- time_series_plot(data=data
                                        ,x=x
                                        ,y=y
                                        ,color=color
                                        ,y.title=input$cap_or_enc_totals
                                        ,x.title="Date")
    cap_enc_ts_plot
  })
  
  output$pmpm_dotplot <- renderPlotly({
    
    hover.text.ref.line1=cost_agg()[,unique(`Grand Average PMPM`)]
    pmpm_dotplot <- multigroup_dot_plot_vert(data=cost_agg()
                                             , source="pmpm_dotplot"
                                             , x=input$pmpm_calc_select
                                             , y=input$group_pop
                                             , y.title=""
                                             , x.title="Monthly PMPM ($)"
                                             , legend.title="Payer Name"
                                             , x.scale.log=TRUE
                                             , key2.var="Payer_Name"
                                             , marker.size.var="Avg Monthly Member Cnt"
                                             , ref.line1=hover.text.ref.line1
                                             , ref.line1.units="$"
                                             , hover.text.ref.line1="Avg. PMPM Across<br>Groups:"
                                             , color="Payer_Name"
                                             , marker.size.text="Avg. Member Count"
                                             , hover.text.markers.units = "$")
  })
  output$pmpm_ts_plot <- renderPlotly({
    data=cost_agg_ts()
    x="Date"
    y=input$pmpm_calc_select
    color = "Payer_Name"
    
    pmpm_ts_plot <- time_series_plot(data
                                     ,x=x
                                     ,y=y
                                     ,color=color
                                     ,y.title=input$pmpm_calc_select
                                     ,x.title="Date")
    pmpm_ts_plot
  })
  
  output$enr_stackbar <- renderPlotly({
    data=enr_agg()
    if(input$enr_calc==0){
      x="avg_member_count"
      x.title="Average Monthly Member Count"
      hover.text=paste("%{x:,.0f} Members")
    } else if (input$enr_calc==1){
      x="avg_perc_member_count"
      x.title="% of Payer's Population"
      hover.text=paste("%{x:,.1f}%")
    }
    y="Pop_subgroup"
    color="Payer_Name"
    y.title=""
    legend.title="Payer Name"
    source="enr_stackbar"
    
    enr_barplot <- barplot(data=data
                           ,x=x
                           ,y=y
                           ,color=color
                           ,barmode="group"
                           ,orientation="h"
                           ,x.title="Average Monthly Enrollment"
                           ,y.title=""
                           ,legend.title="Payer Name"
                           ,source="enr_groupbar"
                           ,hover.text=paste("%{x:,.0f} Members")
                           ,left.margin=10)
  })
  
  output$enr_ts <- renderPlotly({
    
    if(input$enr_calc==0){
      y="total_member_count"
      y.title="Total Member Count"
      hover.text=paste("%{x:,.0f} Members")
    } else if (input$enr_calc==1){
      y="percent_member_count"
      y.title="% of Payer's Population"
      hover.text=paste("%{x:,.1f}%")
    }
    enr_ts <-  time_series_plot(data=enr_agg_ts()
                                ,x="Date"
                                ,y=y
                                ,y.title=y.title
                                ,x.title="Date"
                                ,color="Payer_Name")
  })
  
}