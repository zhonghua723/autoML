# modules/data_import/data_import_server.R

data_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 提供示例数据文件下载
    output$downloadSample <- downloadHandler(
      filename = function() {
        "train.csv"
      },
      content = function(file) {
        # 生成一个示例数据框
        sample_data <- data.frame(
          Disease = sample(c(0, 1), 100, replace = TRUE),
          Age = rnorm(100, 50, 10),
          Smoking = sample(c("Yes", "No"), 100, replace = TRUE),
          BMI = rnorm(100, 25, 4)
        )
        write.csv(sample_data, file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    # 数据导入并预览
    data <- reactive({
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      df <- read.csv(inFile$datapath, stringsAsFactors = FALSE) %>% na.omit()
      df$Disease <- as.factor(df$Disease)  # 确保 Disease 是因子
      df
    })
    
    output$dataPreview <- renderTable({
      if (is.null(data())) return(NULL)
      head(data(), 10)
    })
    
    # 返回数据以供其他模块使用
    return(list(data = data))
  })
}