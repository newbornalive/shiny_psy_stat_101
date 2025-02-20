# shiny_psy_stat_101
runGitHub("shiny_psy_stat_101","newbornalive",ref="central-tendency");

runGitHub("shiny_psy_stat_101","newbornalive",ref="range-var-sd")

# Psychology Data Analysis Shiny App

## Overview
This Shiny app provides an **interactive exploratory data analysis (EDA) dashboard** for psychology research datasets. It allows users to visualize data, perform statistical tests, and run regression analyses in an intuitive interface.

## Features
- **Data Upload**: Supports CSV file uploads for analysis.
- **Exploratory Data Analysis (EDA)**:
  - Summary statistics table
  - Histogram and boxplot visualizations
- **Cross-Tabulation**:
  - Generates contingency tables for categorical variables
- **Statistical Tests**:
  - One-sample t-test
  - Two-sample t-test
  - Chi-square test
- **Regression Analysis**:
  - Simple Linear Regression (SLR)
  - Multiple Linear Regression (MLR)

## Installation & Setup
### Prerequisites
Ensure you have R and the required packages installed:
```r
install.packages(c("shiny", "ggplot2", "dplyr", "shinyWidgets", "summarytools", "DT", "car", "MASS"))
```

### Running the App
```r
library(shiny)
runApp("path/to/your/app")
```
Replace `path/to/your/app` with the actual directory path containing the Shiny app files.

## Usage
1. Upload a dataset (CSV format).
2. Select variables for analysis.
3. View EDA summaries and visualizations.
4. Perform statistical tests and regression analyses.
5. Interpret results and download insights.

## Author
Developed by Yun Yang (Erica Yang)
- **GitHub**: [github.com/newbornalive](https://github.com/newbornalive)
- **LinkedIn**: [www.linkedin.com/in/yunyang09](https://www.linkedin.com/in/yunyang09)

## License
This project is licensed under the MIT License - see the LICENSE file for details.

---

For suggestions or contributions, feel free to submit a pull request or reach out via GitHub or LinkedIn.

