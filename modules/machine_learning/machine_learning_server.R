# modules/machine_learning/machine_learning_server.R

machine_learning_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 监听数据导入模块的数据变化，更新选择输入
    observe({
      df <- data()
      if (is.null(df)) return(NULL)
      
      # 检查"Disease"列是否存在
      if (!"Disease" %in% names(df)) {
        showNotification("数据中缺少'Disease'列。", type = "error")
        return(NULL)
      }
      
      updateSelectInput(session, "mlVars",  # 保持ID为"mlVars"
                        choices = names(df)[names(df) != "Disease"],
                        selected = NULL)
    })
    
    # 初始化 reactiveValues
    values <- reactiveValues(inclu = NULL, p = NULL, selected_vars = NULL)
    
    # 运行机器学习模型
    observeEvent(input$do, {
      tryCatch({
        req(data())
        selected_vars <- input$mlVars  # 确保读取的是"mlVars"
        if (is.null(selected_vars) || length(selected_vars) == 0) {
          showNotification("请至少选择一个特征变量进行机器学习模型训练。", type = "warning")
          return(NULL)
        }
        # 记录选择的变量
        values$selected_vars <- selected_vars
        
        data_ml <- data() %>%
          select(Disease, all_of(selected_vars)) %>%
          mutate(across(where(is.character), as.factor)) %>%
          mutate(across(where(is.numeric), as.numeric)) %>%
          mutate(Disease = as.factor(Disease))
        
        task2 <- mlr3::as_task_classif(data_ml, target = 'Disease', id = '1')
        
        # 初始化结果列表
        results <- list()
        common_ml_model <- c(
          "classif.glmnet", "classif.kknn", "classif.lda", "classif.log_reg",
          "classif.naive_bayes", "classif.nnet", "classif.qda",
          "classif.svm", "classif.xgboost", "classif.randomForest", "classif.gausspr",
          "classif.AdaBoostM1", "classif.ksvm", "classif.gbm", "classif.J48", "classif.rpart"
        )
        
        # 在长时间的操作中添加进度反馈
        withProgress(message = '模型正在训练中，请稍候...', value = 0, {
          n <- length(common_ml_model)
          for (i in seq_along(common_ml_model)) {
            setProgress(value = i / n, detail = paste("训练模型:", common_ml_model[i]))
            
            result <- tryCatch({
              learner <- mlr3::lrn(common_ml_model[i], predict_type = "prob")
              resampling <- mlr3::rsmp("cv", folds = 5) # 指定交叉验证折数
              rr <- mlr3::resample(task2, learner, resampling)
              list(success = TRUE, result = rr)
            }, error = function(e) {
              list(success = FALSE, error = e)
            })
            
            results[[common_ml_model[i]]] <- result
            if (!result$success) {
              cat("Error in", common_ml_model[i], ":", conditionMessage(result$error), "\n\n")
            }
          }
          
          # 过滤出成功的模型
          successful_models <- names(Filter(function(x) x$success, results))
          
          if (length(successful_models) == 0) {
            showNotification("没有成功训练任何模型。", type = "error")
            return(NULL)
          }
          
          design <- mlr3::benchmark_grid(
            tasks = task2,
            learners = mlr3::lrns(successful_models, predict_type = "prob"),
            resamplings = mlr3::rsmp("cv", folds = 5)
          )
          
          bmr2 <- mlr3::benchmark(design, store_models = TRUE)
          
          # 计算模型性能
          measures <- mlr3::msrs(c("classif.auc", "classif.acc", "classif.tpr", "classif.tnr", "classif.ppv", "classif.npv"))
          tab2 <- bmr2$aggregate(measures)
          
          # 处理和保存结果
          inclu <- tab2  %>% 
            dplyr::select(learner_id, task_id, classif.auc, classif.acc, classif.tpr, classif.tnr, classif.ppv, classif.npv) %>% 
            dplyr::mutate(classif.f1 = 2 * classif.tpr * classif.ppv / (classif.tpr + classif.ppv)) %>%
            as.data.frame()
          
          names(inclu) <- c('机器学习模型','任务ID','AUC','准确率','灵敏度','特异度','阳性预测值','阴性预测值','F1分数')
          
          for (i in 4:9) { # 根据实际列数调整
            inclu[,i] <- round(inclu[,i], 2) 
          }
          
          # 生成 ROC 曲线
          bmr_german <- bmr2$clone(deep = TRUE)$filter(task_ids = '1', resampling_ids = "cv")
          model_names <- bmr_german$learners$learner_id
          bootstrap_model <- tab2 %>% 
            dplyr::filter(resampling_id == 'cv') %>% 
            dplyr::arrange(desc(classif.auc))
          best_model <- bootstrap_model$learner_id[1]
          
          color_map <- setNames(rep('lightgrey', length(model_names)), model_names)
          color_map[best_model] <- "#ff42b3"
          color_map['classif.log_reg'] <- "#0ebeff"
          
          fill_map <- setNames(rep('lightgrey', length(model_names)), model_names)
          fill_map[best_model] <- "pink"
          fill_map['classif.log_reg'] <- "#44a2ee"
          
          plot_data <- autoplot(bmr_german, type = "roc", plot = FALSE)$data
          
          highlighted_models <- c(best_model, 'classif.log_reg')
          highlighted_data <- plot_data %>% dplyr::filter(modname %in% highlighted_models)
          other_data <- plot_data %>% dplyr::filter(!modname %in% highlighted_models)
          
          p <- ggplot2::ggplot() +
            ggplot2::geom_line(data = other_data, aes(x = x, y = y, color = modname, linetype = modname, fill = modname)) +
            ggplot2::geom_ribbon(data = other_data, aes(x = x, ymin = ymin, ymax = ymax, fill = modname), alpha = 0) +
            ggplot2::geom_line(data = highlighted_data, aes(x = x, y = y, color = modname, linetype = modname, fill = modname), size = 1) +
            ggplot2::geom_ribbon(data = highlighted_data, aes(x = x, ymin = ymin, ymax = ymax, fill = modname), alpha = 0.4) +
            ggplot2::scale_color_manual(values = color_map) +
            ggplot2::scale_fill_manual(values = fill_map) +
            ggplot2::scale_linetype_manual(values = rep("solid", length(model_names))) +
            ggplot2::theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
            ggthemes::theme_base()
          
          # 更新 reactiveValues
          values$inclu <- inclu
          values$p <- p
        })
      }, error = function(e) {
        showNotification(paste("发生错误：", e$message), type = "error")
      })
    })
    
    # 显示模型性能表格
    output$mlPerformance <- renderTable({
      req(values$inclu)
      values$inclu
    })
    
    # 显示 ROC 曲线
    output$rocPlot <- renderPlot({
      req(values$p)
      values$p
    })
    
    # 下载模型性能
    output$downloadPerformance <- downloadHandler(
      filename = function() {
        paste("model_performance-", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        req(values$inclu)
        write.xlsx(values$inclu, file)
      }
    )
    
    # 下载 ROC 曲线
    output$downloadROC <- downloadHandler(
      filename = function() {
        paste("ROC_curve-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        req(values$p)
        ggsave(file, plot = values$p, width = 8, height = 6, units = "in")
      }
    )
    
    # 返回选择的变量作为 reactive 表达式
    return(list(
      selected_vars = reactive({ input$mlVars })
    ))
  })
}
