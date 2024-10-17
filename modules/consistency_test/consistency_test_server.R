# modules/consistency_test/consistency_test_server.R

consistency_test_server <- function(id, data, selected_vars) {
  moduleServer(id, function(input, output, session) {
    # 使用传递的 selected_vars 进行一致性检验
    consistency_result <- eventReactive(input$do_consistency, {
      tryCatch({
        req(data())
        vars <- selected_vars()
        
        # 确保选择了自变量
        if (is.null(vars) || length(vars) == 0) {
          showNotification("请至少选择一个特征变量用于一致性检验。", type = "warning")
          return(NULL)
        }
        
        data_ml <- data() %>%
          select(Disease, all_of(vars)) %>%
          mutate(across(where(is.character), as.factor)) %>%
          mutate(across(where(is.numeric), as.numeric)) %>%
          mutate(Disease = as.factor(Disease))  # 保持Disease为因子型
        
        print("一致性检验模块：数据准备完成")
        print(head(data_ml))
        
        # 拟合逻辑回归模型
        m <- glm(Disease ~ ., data = data_ml, family = binomial)
        
        # 获取预测概率
        pred_probs <- predict(m, type = "response")
        
        # 进行 Hosmer-Lemeshow 一致性检验
        hl_test <- ResourceSelection::hoslem.test(data_ml$Disease == levels(data_ml$Disease)[2], pred_probs)
        
        # 返回检验结果
        hl_test
      }, error = function(e) {
        showNotification(paste("一致性检验错误:", e$message), type = "error")
        print(paste("一致性检验错误:", e$message))
        NULL
      })
    })
    
    # 显示一致性检验结果
    output$consistencyTest <- renderText({
      req(consistency_result())
      if (is.null(consistency_result())) {
        "一致性检验未能成功完成。"
      } else {
        paste("Hosmer-Lemeshow Test Results:\n",
              "Chi-squared:", round(consistency_result()$statistic, 2), "\n",
              "p-value:", format.pval(consistency_result()$p.value, digits = 4))
      }
    })
  })
}
