# global.R

# 加载必要的包
library(shiny)
library(dplyr)
library(gtsummary)
library(flextable)
library(shinythemes)
library(mlr3)
library(mlr3verse)
library(tidyverse)
library(eoffice)
library(mlr3extralearners)
library(rms)
library(ResourceSelection)
library(ggDCA)
library(openxlsx)
library(ggplot2)
library(gt)
library(remotes)
library(rms)
library(openxlsx)

# 安装来自 GitHub 的包（如果需要）
if (!requireNamespace("mlr3extralearners", quietly = TRUE)) {
  remotes::install_github("mlr-org/mlr3extralearners")
}

if (!requireNamespace("eoffice", quietly = TRUE)) {
  remotes::install_github("r-dbi/eoffice")
}