# modules/machine_learning/machine_learning_ui.R

machine_learning_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("mlVars"), "请根据多因素分析中有显著性的结果，并结合既往研究的经验，选择进行机器学习建模的变量", choices = NULL, multiple = TRUE),  # 保持ID为"mlVars"
        actionButton(ns("do"), "运行机器学习模型"),
        downloadButton(ns("downloadPerformance"), "下载模型性能"),
        downloadButton(ns("downloadROC"), "下载ROC曲线")
      ),
      mainPanel(
        h3("模型性能"),
        tableOutput(ns("mlPerformance")),
        h3("ROC曲线"),
        plotOutput(ns("rocPlot"))
      )
    )
  )
}