# Separate R Script file containing ui to be used for creating R Shiny Dashboard later. You can call this R Script file from the other R Script file via the following code:
# source("~/ui.R")

ui <- fluidPage(
  titlePanel("비만 지표"),
  theme = shinythemes::shinytheme("darkly"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender","성별",unique(obesity_measure_WHtR_2019$성별)),
      selectInput("age_cohort","연령대",unique(obesity_measure_WHtR_2019$연령대)),
      numericInput("waist","허리 둘레(cm):",80,30,160,.1),
      numericInput("weight","체중(kg):",63,20,190,.1),
      numericInput("height","신장(cm):",165,120,210,.1),
      helpText("반드시 허리 둘레와 신장을 입력하셔야 체지방률을 알 수 있습니다."),
      br(),
      helpText("성별:"),
      helpText("F-여성"),
      helpText("M-남성"),
      br(),
      helpText("연령대:"),
      helpText("청소년-만 13세~18세"),
      helpText("성인-만 19세~64세"),
      helpText("노인-만 65세 이상"),
      actionButton("action","결과"),
      actionButton("help","도움말")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("허리 신장 비율(WHtR)",
                 textOutput("whtr"),
                 plotOutput("whtr_plot_cohort"),
                 plotOutput("whtr_plot")),
        tabPanel("체질량 지수(BMI)",
                 textOutput("bmi"),
                 plotOutput("bmi_plot_cohort"),
                 plotOutput("bmi_plot")),
        tabPanel("체지방률",
                 textOutput("bfp"),
                 plotOutput("bfp_plot_cohort"),
                 plotOutput("bfp_plot"))
      )
    )
  )
)