# modules/single_analysis/single_analysis_server.R

single_analysis_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      df <- data()
      if (is.null(df)) return(NULL)
      updateCheckboxGroupInput(session, "factorVars",
                               choices = names(df),
                               selected = names(df)[sapply(df, is.factor)])
    })
    
    res_single <- eventReactive(input$runsingle, {
      tryCatch({
        req(data())
        if (is.null(input$factorVars) || length(input$factorVars) == 0) {
          showNotification("请至少选择一个因子变量进行分析。", type = "warning")
          return(NULL)
        }
        tbl_data <- data() %>% 
          tbl_summary(
            by = Disease,
            type = all_continuous() ~ "continuous2",
            statistic = all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})"),
            digits = all_continuous() ~ 2
          ) %>% 
          add_n() %>% 
          add_p() 
        tbl_data
      }, error = function(e) {
        showNotification(paste("单因素分析错误:", e$message), type = "error")
        NULL
      })
    })
    
    output$singleAnalysisOutput <- renderUI({
      req(res_single())
      if (is.null(res_single())) {
        HTML("<p style='color: red;'>单因素分析未能成功完成。</p>")
      } else {
        gt_table <- as_gt(res_single())
        html_output <- gt::as_raw_html(gt_table)
        HTML(html_output)
      }
    })
    
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste("table-summary", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        req(res_single())
        flextable::save_as_docx(as_flex_table(res_single()), path = file)
      }
    )
  })
}