# modules/nomogram/nomogram_ui.R

nomogram_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # 移除 selectInput，因为变量由机器学习模块选择
        actionButton(ns("do_non"), "计算列线图"),
        downloadButton(ns("downloadNomogram"), "下载列线图")
      ),
      mainPanel(
        plotOutput(ns("nomogramPlot")),
        verbatimTextOutput(ns("nomogramSummary"))
      )
    )
  )
}