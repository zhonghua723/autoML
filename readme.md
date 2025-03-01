# autoML应用程序

欢迎使用我的应用程序！本应用旨在提供便捷的数据分析和可视化功能。以下是使用该应用的详细步骤：

## 系统要求

- **操作系统**: Windows, macOS, 或 Linux
- **R版本**: 建议使用**R 4.4.0**及以上版本
- **RStudio**: 推荐使用最新版本的RStudio

## 使用说明

### 第一步：下载并解压所有文件

1. **下载代码**:
   - 点击绿色的 **Code** 按钮。
   - 选择 **Download ZIP** 选项，将所有项目文件下载到本地计算机。
2. **解压文件**: 下载完成后，解压缩ZIP文件到您希望存放项目的目录。

### 第二步：打开R项目并安装所需包

1. **打开RStudio**: 确保您已安装 [R](https://www.r-project.org/) 和 [RStudio](https://www.rstudio.com/)。
2. **打开项目文件**:
   - 点击文件夹内的 `.Rproj` 文件并打开。
3. **安装依赖包**:
   - 在RStudio的控制台中运行以下命令以安装所有必要的R包：
     ```r
     source("R-package-install.R")
     ```
   - 此脚本将自动安装`global.R`中列出的所有CRAN和GitHub包，确保应用程序的顺利运行。

### 第三步：运行应用程序

1. **启动Shiny应用**:
   - 在RStudio中，打开 `app.R` 文件。
   - 点击 **Run App** 按钮，或在控制台中运行以下命令：
     ```r
     shiny::runApp("app.R")
     ```
2. **使用网页界面**:
   - 应用启动后，您的默认浏览器将自动打开一个新的标签页，显示Shiny应用的用户界面。
   - 按照界面提示进行数据输入和选择，完成后续的计算和分析。

## 常见问题

- **包安装失败**: 请确保您的网络连接正常，并且R和RStudio已正确安装。您也可以尝试手动安装失败的包。
- **应用无法启动**: 检查是否所有依赖包均已成功安装。重新运行 `R-package-install.R` 脚本可能有助于解决问题。

## 贡献

欢迎对本项目提出建议或贡献代码！请提交[issue](https://github.com/zhonghua723/autoML/issues)或拉取请求（pull request）以参与项目改进。

## 许可证

本项目采用 [MIT 许可证](LICENSE) 进行许可。您可以自由使用、修改和分发本项目的代码。


感谢您的使用和支持！
