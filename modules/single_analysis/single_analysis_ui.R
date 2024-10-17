# modules/single_analysis/single_analysis_ui.R

single_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(ns("factorVars"), "请勾选分类变量，后台会进行变量类型转换 （无需勾选连续性变量）", choices = NULL),
        actionButton(ns("runsingle"), "运行单因素分析"),
        downloadButton(ns("downloadTable"), "下载结果表")
      ),
      mainPanel(
        uiOutput(ns("singleAnalysisOutput"))
      )
    )
  )
}