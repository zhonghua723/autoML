# app.R

library(shiny)
library(shinythemes)

# 载入全局设置
source("global.R")

# 载入所有模块
# 数据导入模块
source("modules/data_import/data_import_ui.R")
source("modules/data_import/data_import_server.R")

# 单因素分析模块
source("modules/single_analysis/single_analysis_ui.R")
source("modules/single_analysis/single_analysis_server.R")

# 多因素回归分析模块
source("modules/multi_analysis/multi_analysis_ui.R")
source("modules/multi_analysis/multi_analysis_server.R")

# 机器学习模块
source("modules/machine_learning/machine_learning_ui.R")
source("modules/machine_learning/machine_learning_server.R")

# 列线图模块
source("modules/nomogram/nomogram_ui.R")
source("modules/nomogram/nomogram_server.R")

# 校准曲线模块
source("modules/calibration/calibration_ui.R")
source("modules/calibration/calibration_server.R")

# 一致性检验模块
source("modules/consistency_test/consistency_test_ui.R")
source("modules/consistency_test/consistency_test_server.R")

# 决策曲线分析 (DCA) 模块
source("modules/dca/dca_ui.R")
source("modules/dca/dca_server.R")

# 定义 UI
ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("机器学习模型网页程序-beta"),
  tabsetPanel(
    tabPanel("1️⃣数据导入",
             data_import_ui("data_import")
    ),
    tabPanel("2️⃣单因素分析",
             single_analysis_ui("single_analysis")
    ),
    tabPanel("3️⃣多因素回归分析",
             multi_analysis_ui("multi_analysis")
    ),
    tabPanel("4️⃣机器学习",
             machine_learning_ui("machine_learning")
    ),
    tabPanel("5️⃣列线图",
             nomogram_ui("nomogram")
    ),
    tabPanel("6️⃣校准曲线",
             calibration_ui("calibration")
    ),
    tabPanel("7️⃣一致性检验",
             consistency_test_ui("consistency_test")
    ),
    tabPanel("8️⃣决策曲线分析（维护中）"
             # dca_ui("dca")
    )
  )
)

# 定义 Server
server <- function(input, output, session) {
  # 调用数据导入模块
  data_import <- data_import_server("data_import")
  
  # 调用机器学习模块，并获取选择的变量
  machine_learning <- machine_learning_server("machine_learning", data = data_import$data)
  
  # 调用单因素分析模块
  single_analysis_server("single_analysis", data = data_import$data)
  
  # 调用多因素回归分析模块
  multi_analysis_server("multi_analysis", data = data_import$data)
  
  machine_learning_server("machine_learning", data = data_import$data)
  
  # 调用列线图模块，并传递选择的变量
  nomogram_server("nomogram", data = data_import$data, selected_vars = machine_learning$selected_vars)
  
  # 调用校准曲线模块，并传递选择的变量
  calibration_server("calibration", data = data_import$data, selected_vars = machine_learning$selected_vars)
  
  # 调用一致性检验模块，并传递选择的变量
  consistency_test_server("consistency_test", data = data_import$data, selected_vars = machine_learning$selected_vars)
  
  # 调用决策曲线分析 (DCA) 模块，并传递选择的变量
  # dca_server("dca", data = data_import$data, selected_vars = machine_learning$selected_vars)
}

# 启动应用
shinyApp(ui, server)
