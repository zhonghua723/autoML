# modules/multi_analysis/multi_analysis_ui.R

multi_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("multiVars"), "请根据单因素分析中有显著性的结果，并结合既往研究的经验，选择进行多因素分析的变量（可多选）", choices = NULL, multiple = TRUE),
        actionButton(ns("runmulti"), "运行多因素回归分析"),
        downloadButton(ns("downloadLogis"), "下载回归结果")
      ),
      mainPanel(
        uiOutput(ns("multiAnalysisOutput"))
      )
    )
  )
}