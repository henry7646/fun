# Separate R Script file containing server to be used for creating R Shiny Dashboard later. You can call this R Script file from the other R Script file via the following code:
# source("~/server.R")

server <- function(input,output){
  observeEvent(input$help,{
    showModal(modalDialog(
      helpText("이 대시보드는 국민체육진흥공단의 2019년 국민체력측정데이터를 기반으로 제작하였습니다.")
    ))
  })
  
  rval_whtr <- eventReactive(input$action,{
    input$waist/input$height
  })
  
  rval_whtr_rank_cohort <- reactive({
    ecdf(filter(obesity_measure_WHtR_2019,
                성별 == input$gender,
                연령대 == input$age_cohort)$WHtR)(rval_whtr())
  })
  
  rval_whtr_rank <- reactive({
    ecdf(obesity_measure_WHtR_2019$WHtR)(rval_whtr())
  })
  
  rval_bmi <- eventReactive(input$action,{
    input$weight/((input$height/100)^2)
  })
  
  rval_bmi_rank_cohort <- reactive({
        ecdf(filter(obesity_measure_WHtR_2019,
                성별 == input$gender,
                연령대 == input$age_cohort)$BMI)(rval_bmi())
  })
  
  rval_bmi_rank <- reactive({
    ecdf(obesity_measure_WHtR_2019$BMI)(rval_bmi())
  })
  
  rval_bfp <- reactive({
    coefficients(regress_on_WHtR_6)[1]*rval_whtr() +
      coefficients(regress_on_WHtR_6)[2]*ifelse(input$gender == "M",1,0) +
      coefficients(regress_on_WHtR_6)[2+month(Sys.Date())] +
      coefficients(regress_on_WHtR_6)[15]*rval_whtr()*ifelse(input$gender == "M",1,0)
  })
  
  rval_bfp_rank_cohort <- reactive({
    ecdf(filter(obesity_measure_WHtR_2019,
                성별 == input$gender,
                연령대 == input$age_cohort)$체지방율)(rval_bfp())
  })
  
  rval_bfp_rank <- reactive({
    ecdf(obesity_measure_WHtR_2019$체지방율)(rval_bfp())
  })
  
  output$whtr <- renderText({
    paste0("허리 신장 비율(WHtR): ",round(rval_whtr(),2),
          " / ", input$age_cohort, ", ", input$gender, " 중 백분위: ",
          100*round(rval_whtr_rank_cohort(),2),"%",
          " / ", "전체 인구 중 백분위: ",
          100*round(rval_whtr_rank(),2),"%")
  })
  
  output$whtr_plot_cohort <- renderPlot({
    obesity_measure_WHtR_2019 %>%
      filter(성별 == input$gender, 연령대 == input$age_cohort) %>%
      ggplot(aes(WHtR))+geom_density(fill = "blue", alpha = 0.5) + geom_vline(xintercept = rval_whtr(), color = "red", linetype = 2, size = 1.5) + scale_x_continuous("WHtR") + ggtitle(paste0("허리 신장 비율의 분포 (",input$age_cohort,", ",input$gender,")"))
  })
  
  output$whtr_plot <- renderPlot({
    obesity_measure_WHtR_2019 %>%
      ggplot(aes(WHtR))+geom_density(fill = "blue", alpha = 0.5) + geom_vline(xintercept = rval_whtr(), color = "red", linetype = 2, size = 1.5) + scale_x_continuous("WHtR") + ggtitle("허리 신장 비율의 분포 (전체)")
  })
  
  output$bmi <- renderText({
    paste0("체질량 지수(BMI): ",round(rval_bmi(),1),
           " / ", input$age_cohort, ", ", input$gender, " 중 백분위: ",
           100*round(rval_bmi_rank_cohort(),2),"%",
           " / ", "전체 인구 중 백분위: ",
           100*round(rval_bmi_rank(),2),"%")
  })
  
  output$bmi_plot_cohort <- renderPlot({
    obesity_measure_WHtR_2019 %>%
      filter(성별 == input$gender, 연령대 == input$age_cohort) %>%
      ggplot(aes(BMI))+geom_density(fill = "blue", alpha = 0.5) + geom_vline(xintercept = rval_bmi(), color = "red", linetype = 2, size = 1.5) + scale_x_continuous("BMI") + ggtitle(paste0("체질량 지수의 분포 (",input$age_cohort,", ",input$gender,")"))
  })
  
  output$bmi_plot <- renderPlot({
    obesity_measure_WHtR_2019 %>%
      ggplot(aes(BMI))+geom_density(fill = "blue", alpha = 0.5) + geom_vline(xintercept = rval_bmi(), color = "red", linetype = 2, size = 1.5) + scale_x_continuous("BMI") + ggtitle("체질량 지수의 분포 (전체)")
  })
  
  output$bfp <- renderText({
    paste0("체지방률: ",round(rval_bfp(),1),
           "%", " / ", input$age_cohort, ", ", input$gender, " 중 백분위: ",
           100*round(rval_bfp_rank_cohort(),2),"%",
           " / ", "전체 인구 중 백분위: ",
           100*round(rval_bfp_rank(),2),"%")
  })
  
  output$bfp_plot_cohort <- renderPlot({
    obesity_measure_WHtR_2019 %>%
      filter(성별 == input$gender, 연령대 == input$age_cohort) %>%
      ggplot(aes(체지방율))+geom_density(fill = "blue", alpha = 0.5) + geom_vline(xintercept = rval_bfp(), color = "red", linetype = 2, size = 1.5) + scale_x_continuous("체지방률") + ggtitle(paste0("체지방률의 분포 (",input$age_cohort,", ",input$gender,")"))
  })
  
  output$bfp_plot <- renderPlot({
    obesity_measure_WHtR_2019 %>%
      ggplot(aes(체지방율))+geom_density(fill = "blue", alpha = 0.5) + geom_vline(xintercept = rval_bfp(), color = "red", linetype = 2, size = 1.5) + scale_x_continuous("체지방률") + ggtitle("체지방률의 분포 (전체)")
  })
}
