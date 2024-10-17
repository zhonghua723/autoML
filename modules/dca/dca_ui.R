# modules/dca/dca_ui.R

dca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("do_dca"), "计算DCA决策曲线"),
    downloadButton(ns("downloadDCA"), "下载DCA决策曲线"),
    plotOutput(ns("dcaPlot"))
  )
}