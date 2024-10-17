# modules/nomogram/nomogram_server.R

nomogram_server <- function(id, data, selected_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 等待点击“计算列线图”按钮
    non_result <- eventReactive(input$do_non, {
      tryCatch({
        req(data())
        vars <- selected_vars()
        
        # 确保选择了自变量
        if (is.null(vars) || length(vars) == 0) {
          showNotification("请至少选择一个特征变量用于列线图生成。", type = "warning")
          return(NULL)
        }
        
        data_ml <- data() %>%
          select(Disease, all_of(vars)) %>%
          mutate(across(where(is.character), as.factor)) %>%
          mutate(across(where(is.numeric), as.numeric)) %>%
          mutate(Disease = as.factor(Disease))  # 保持Disease为因子型
        
        print("列线图模块：数据准备完成")
        print(head(data_ml))
        
        # 设置数据分布
        dd <<- rms::datadist(data_ml)
        options(datadist="dd")
        
        # 拟合逻辑回归模型
        f <- rms::lrm(Disease ~ ., data = data_ml, x = TRUE, y = TRUE)
        
        # 生成列线图
        non <- rms::nomogram(f, fun = function(x) 1 / (1 + exp(-x)), 
                        fun.at = c(0.05, seq(0.2, 0.9, by = 0.3), 0.95), 
                        funlabel = "Prob of Events", 
                        lp = FALSE, 
                        conf.int = FALSE, 
                        abbrev = FALSE)
        
        # 返回列线图对象和模型摘要
        list(nomogram = non, summary = summary(f))
      }, error = function(e) {
        showNotification(paste("列线图计算错误:", e$message), type = "error")
        print(paste("列线图计算错误:", e$message))
        NULL
      })
    })
    
    # 渲染列线图
    output$nomogramPlot <- renderPlot({
      req(non_result())
      plot(non_result()$nomogram)
    })
    
    # 显示模型摘要
    output$nomogramSummary <- renderPrint({
      req(non_result())
      non_result()$summary
    })
    
    # 下载列线图 PowerPoint
    output$downloadNomogram <- downloadHandler(
      filename = function() {
        paste("列线图-", Sys.Date(), ".pptx", sep = "")
      },
      content = function(file) {
        req(non_result())
        # 将列线图保存为临时 PNG 文件
        temp_plot <- tempfile(fileext = ".png")
        png(temp_plot, width = 250, height = 250, units = 'mm', res = 600)
        plot(non_result()$nomogram)
        dev.off()
        
        # 使用 officer 包创建 PowerPoint
        library(officer)
        library(magrittr)
        
        doc <- read_pptx()
        doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
        doc <- ph_with(doc, external_img(temp_plot, width = 8, height = 6), location = ph_location_type(type = "body"))
        print(doc, target = file)
      }
    )
  })
}
