# modules/calibration/calibration_server.R

calibration_server <- function(id, data, selected_vars) {
  moduleServer(id, function(input, output, session) {
    # 等待点击"计算校准曲线"按钮
    calibrate_result <- eventReactive(input$do_calibrate, {
      tryCatch({
        req(data())
        vars <- selected_vars()
        
        # 确保选择了自变量
        if (is.null(vars) || length(vars) == 0) {
          showNotification("请至少选择一个特征变量用于校准曲线生成。", type = "warning")
          return(NULL)
        }
        
        data_ml <- data() %>%
          select(Disease, all_of(vars)) %>%
          mutate(across(where(is.character), as.factor)) %>%
          mutate(across(where(is.numeric), as.numeric)) %>%
          mutate(Disease = as.factor(Disease))  # 保持Disease为因子型
        
        print("校准曲线模块：数据准备完成")
        print(head(data_ml))
        
        # 设置数据分布
        # 设置数据分布
        dd <<- rms::datadist(data_ml)
        dd[["limits"]]
        options(datadist="dd")
        
        # 拟合逻辑回归模型
        f <- rms::lrm(Disease ~ ., data = data_ml, x = TRUE, y = TRUE)
        
        # 创建一个临时文件路径
        temp_file <- tempfile(fileext = ".png")
        
        # 保存校准曲线到临时文件
        png(temp_file, width = 250, height = 250, units = 'mm', res = 600)
        cal1 <- rms::calibrate(f, method = "boot", B = 1000)
        plot(cal1,
             xlim = c(0,1),
             xlab = "Predicted Probability",
             ylab = "Observed Probability",
             legend = FALSE,
             subtitles = FALSE,
             cex.axis = 1.5,
             cex.lab = 1.5)
        abline(0, 1, col = "black", lty = 2, lwd = 2)
        lines(cal1[,c("predy","calibrated.orig")], type = "l", lwd = 2, pch = 16, col = '#D53E4F')
        lines(cal1[,c("predy","calibrated.corrected")], type = "l", lwd = 2, pch = 16, col = '#225EA8')
        legend("bottomright",
               c("Ideal", "Apparent", "Bias-corrected"),
               lty = c(2, 1, 1),
               lwd = c(2, 2, 2),
               col = c('black', '#D53E4F', '#225EA8'),
               bty = "n",
               cex = 1.5)
        dev.off()
        
        # 返回校准曲线图片的路径和模型摘要
        list(plot_path = temp_file, summary = summary(f))
      }, error = function(e) {
        showNotification(paste("校准曲线计算错误:", e$message), type = "error")
        print(paste("校准曲线计算错误:", e$message))
        NULL
      })
    })
    
    # 渲染校准曲线图片
    output$calibrationPlot <- renderImage({
      req(calibrate_result())
      if (is.null(calibrate_result())) return(NULL)
      list(src = calibrate_result()$plot_path,
           contentType = 'image/png',
           width = 400,
           height = 400,
           alt = "Calibration Plot")
    }, deleteFile = FALSE)
    
    # 显示模型摘要
    output$calibrationSummary <- renderPrint({
      req(calibrate_result())
      calibrate_result()$summary
    })
    
    # 下载校准曲线图片
    output$downloadCalibration <- downloadHandler(
      filename = function() {
        paste("校准曲线-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        req(calibrate_result())
        file.copy(calibrate_result()$plot_path, file)
      }
    )
  })
}
