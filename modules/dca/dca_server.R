# modules/dca/dca_server.R

dca_server <- function(id, data, selected_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    values_dca <- reactiveValues(dca_plot = NULL)
    
    observeEvent(input$do_dca, {
      req(data())
      vars <- selected_vars()
      
      # 检查是否选择了变量
      if (is.null(vars) || length(vars) == 0) {
        showNotification("请至少选择一个特征变量用于DCA决策曲线生成。", type = "warning")
        return(NULL)
      }
      
      # 创建数据集并处理变量类型
      data_ml <- data() %>%
        select(Disease, all_of(vars)) %>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(across(where(is.numeric), as.numeric))
      
      # 检查并处理因子型变量
      for (var in vars) {
        if (is.factor(data_ml[[var]])) {
          # 如果变量是因子型，检查是否应转换为数值型
          levels_count <- length(levels(data_ml[[var]]))
          if (levels_count > 10) {  # 假设超过10个级别的因子实际上是数值型
            data_ml[[var]] <- as.numeric(as.character(data_ml[[var]]))
          }
        }
      }
      
      # 创建 nomo_dat 数据集并将 Disease 转换为数值型变量
      nomo_dat <<- data_ml  # 使用全局赋值
      nomo_dat$Disease <- as.numeric(nomo_dat$Disease) - 1
      
      # 确保 datadist 全局设置正确
      library(rms)
      dd <<- datadist(nomo_dat)
      options(datadist = "dd")
      
      # 检查 nomo_dat 数据集是否正确创建
      if (nrow(nomo_dat) == 0) {
        showNotification("nomo_dat 数据集为空，请检查数据处理步骤。", type = "error")
        return(NULL)
      }
      
      # 拟合逻辑回归模型
      lrm_model <- lrm(Disease ~ ., data = nomo_dat, x = TRUE, y = TRUE)
      
      # 检查模型是否成功拟合
      if (is.null(lrm_model)) {
        showNotification("逻辑回归模型拟合失败，请检查数据和变量类型。", type = "error")
        return(NULL)
      }
      
      # 计算DCA决策曲线
      library(ggDCA)
      dca_lrm <- dca(lrm_model, model.names = c('Model'))
      
      # 检查DCA结果
      if (is.null(dca_lrm)) {
        showNotification("DCA计算失败，请检查模型和数据。", type = "error")
        return(NULL)
      }
      
      # 绘制DCA曲线
      dca_plot <- ggplot(dca_lrm, aes(x = threshold, y = net.benefit, color = model)) +
        geom_line(size = 1) +
        theme_minimal() +
        labs(x = "阈值概率", y = "净受益") +
        theme(legend.position = "bottom")
      
      # 将结果存储在反应性对象中
      values_dca$dca_plot <- dca_plot
    })
    
    output$dcaPlot <- renderPlot({
      req(values_dca$dca_plot)
      values_dca$dca_plot
    })
    
    # 提供下载DCA曲线的按钮
    output$downloadDCA <- downloadHandler(
      filename = function() {
        paste("DCA决策曲线-", Sys.Date(), ".pptx", sep = "")
      },
      content = function(file) {
        req(values_dca$dca_plot)
        eoffice::topptx(values_dca$dca_plot, file, width = 8)
      }
    )
  })
}