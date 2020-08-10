shinyServer(function(input, output) {
  
 output$map = renderGvis({
   gvisGeoChart(state_stat, "state", "Chipotles",
                options=list(region="US", displayMode="regions", 
                            resolution="provinces",
                            width="auto", height="auto",
                            colors="['#ffe7e3','#a81612' , '#451400']",
                            title= "Chipotle Count By State"))
  })
 
 output$county = renderPlot({
   chipotles %>% 
     filter(., state ==input$state_select) %>% 
   ggplot(., aes(x = has_chipotle, y = .[, input$feature_select], fill = has_chipotle))+
            geom_boxplot()+
     labs(title = "Counties with/without a Chipotle vs Selection",
          x = "Has a Chipotle",
          y = input$feature_select)
 })
 output$scatter = renderPlot({
   chipotles %>% 
     filter(., store_count > 0) %>% 
   ggplot(., aes(x = store_count, y = .[, input$feature_select2]))+
     geom_point(alpha = 0.1, color = "blue")+
     labs(title = "Chipotles in County vs Selection",
          x = "Chipotle Count",
          y = input$feature_select2)
   
 })
 output$table = DT::renderDataTable({
   datatable(chipotles, rownames=FALSE) 
 })
 
 output$table2 = DT::renderDataTable({
   datatable(store_loc, rownames=FALSE) 
 })
})