# modules/data_import/data_import_ui.R

data_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file1"), HTML(
          "🔔请上传csv格式的数据文件（数据包含'Disease'列和其他危险因素），<br>
              ⚠️注意表格内不能有中文和乱码字符！<br>
              🔍如何制作csv文件见链接：<a href='https://jingyan.baidu.com/article/1e5468f937ce3d094861b764.html' target='_blank'>点击这里</a>"
        ), accept = ".csv"),
        downloadButton(ns("downloadSample"), "下载示例数据")
      ),
      mainPanel(
        tableOutput(ns("dataPreview"))
      )
    )
  )
}