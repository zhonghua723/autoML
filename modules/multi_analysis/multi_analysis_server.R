# modules/multi_analysis/multi_analysis_server.R

multi_analysis_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # 监听数据导入模块的数据变化，更新选择输入
    observe({
      df <- data()
      if (is.null(df)) return(NULL)
      updateSelectInput(session, "multiVars",
                        choices = names(df)[names(df) != "Disease"],
                        selected = NULL)
    })
    
    # 多因素回归分析
    results_logis <- eventReactive(input$runmulti, {
      tryCatch({
        req(data())
        selected_vars <- input$multiVars
        if (is.null(selected_vars) || length(selected_vars) == 0) {
          showNotification("请至少选择一个自变量进行多因素回归分析。", type = "warning")
          return(NULL)
        }
        data_new <- data() %>%
          dplyr::select(all_of(c("Disease", selected_vars))) %>%
          mutate(across(where(is.character), as.factor)) %>%
          mutate(Disease = as.factor(Disease))  # 确保 Disease 是因子
        
        # 确保 Disease 是二元
        if (length(levels(data_new$Disease)) != 2) {
          showNotification("Disease 变量必须是二元因子！", type = "error")
          return(NULL)
        }
        
        # 执行逻辑回归
        logis <- glm(Disease ~ ., data = data_new, family = 'binomial')
        tbl_regression(logis, exponentiate = TRUE) 
      }, error = function(e) {
        showNotification(paste("多因素分析错误:", e$message), type = "error")
        NULL
      })
    })
    
    output$multiAnalysisOutput <- renderUI({
      req(results_logis())
      if (is.null(results_logis())) {
        HTML("<p style='color: red;'>多因素分析未能成功完成。</p>")
      } else {
        gt_table <- as_gt(results_logis())
        html_output <- gt::as_raw_html(gt_table)
        HTML(html_output)
      }
    })
    
    # 多因素回归结果下载
    output$downloadLogis <- downloadHandler(
      filename = function() {
        paste("Table2-Logistic-Regression", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        req(results_logis())
        flextable::save_as_docx(as_flex_table(results_logis()), path = file)
      }
    )
  })
}