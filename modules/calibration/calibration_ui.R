# modules/calibration/calibration_ui.R

calibration_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("do_calibrate"), "计算校准曲线"),
        downloadButton(ns("downloadCalibration"), "下载校准曲线")
      ),
      mainPanel(
        imageOutput(ns("calibrationPlot")),
        verbatimTextOutput(ns("calibrationSummary"))
      )
    )
  )
}