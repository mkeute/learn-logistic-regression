require(shiny)
require(shinythemes)
require(DT)
require(ggplot2)
require(ggthemes)
require(MASS)


get_toydata <- function(mu = c(50, 7, 120), sigma = matrix(c(100, 14, 60,
                                                             14,   9, 0,
                                                             60,   0, 400)
                                                           ,nrow=3),
                        proportion_sex = 0, proportion_smoker = 0){
  

  set.seed(99)
  dmy = MASS::mvrnorm(n = 500, mu = mu, Sigma = sigma)
  dmy = round(dmy,2)
  toydata = data.frame(patient = 1:nrow(dmy), sick = ifelse(dmy[,1] < 50, 1, 0), 
                       CGI = dmy[,1], log_tcell = dmy[,2], BP_systolic = dmy[,3])
  toydata[toydata<0]=0
  toydata$sex = ifelse(toydata$sick == 1,
                       ifelse(runif(sum(toydata$sick == 1)) > proportion_sex, 0, 1),#
                       ifelse(runif(sum(toydata$sick == 0)) <= proportion_sex, 0, 1)#
  )
  toydata$smoker = ifelse(toydata$sick == 1,
                          ifelse(runif(sum(toydata$sick == 1)) > proportion_smoker, 1, 0),
                          ifelse(runif(sum(toydata$sick == 0)) <= proportion_smoker, 1, 0)#
  )
  return(toydata)
}



shinyServer(function(input, output) {
   
  
  
  
  toydata <- get_toydata()
  makeReactiveBinding("toydata")
  
  #reactive wrapper for get_toydata to enable user manipulations of data covariances
  toydata_reactive <- reactive({
    sigma = matrix(c(100, 0, 0,
                     0,   9, 0,
                     0,   0, 400),nrow=3)
    #convert user-provided correlations to covariances               
    sigma[1,2] <- input$Cov1 * sqrt(sigma[1,1] * sigma[2,2])
    sigma[2,1] <- sigma[1,2]
    sigma[1,3] <- input$Cov2 * sqrt(sigma[1,1] * sigma[3,3])
    sigma[3,1] <- sigma[1,3]
    
    #make sure the covariance matrix stays positive definite when users enter their own correlation
    while (any(eigen(sigma)$values <= 0)){
      print('adjusting sigma')
      print(sigma)
      sigma[3,2] <- runif(1,-1,1)*sqrt(sigma[3,3]*sigma[2,2])
      sigma[2,3] <- sigma[3,2]
    }
    
    toydata <- get_toydata(mu = c(50, 7, 120), sigma = sigma, proportion_sex = input$proportion_sex, proportion_smoker = input$proportion_smoker) })

  
  output$RawData <- DT::renderDataTable(toydata_reactive())
  
  output$scatter1 <- renderPlot({
    
    ggplot(toydata_reactive(),aes(x = log_tcell, y = CGI))+geom_point()+
      labs(x="log_tcell",y="CGI")+
      theme_clean()+geom_smooth(method="lm",se=F)
    
  })
  
  output$scatter2 <- renderPlot({
    
    ggplot(toydata_reactive(),aes(x = BP_systolic, y = CGI))+geom_point()+
      labs(x="BP_systolic",y="CGI")+
      theme_clean()+geom_smooth(method="lm",se=F)
    
  })
  
  output$scatter3 <- renderPlot({
    
    ggplot(toydata_reactive(),aes(x = sex, y = CGI))+geom_point()+
      labs(x="sex",y="CGI")+
      theme_clean()+stat_summary(col="blue")
    
  })
  
  output$scatter4 <- renderPlot({
    
    ggplot(toydata_reactive(),aes(x = smoker, y = CGI))+geom_point()+
      labs(x="smoker",y="CGI")+
      theme_clean()+stat_summary(col="blue")
    
  })
  toydata_mse <- reactive({
  set.seed(999)
  df = toydata_reactive()
  df = df[sample(1:nrow(df), 50, replace=F),] #select 50 random rows for better visibility
  df$BP_systolic = scale(df$BP_systolic)  #z-score variables for easier plotting of lm line
  df$CGI = scale(df$CGI)
  model = lm(CGI ~ BP_systolic,data = df)
  
  return(list(df, model))
  })
  
  {
  x=seq(1,400, 5)
  betalist = 1+exp(-.01*x)*3*sin(.1*x) #create decaying sinusoid to let the 
                                        #regression line wiggle back and 
                                        #forth before it converges
  betafac <- reactiveValues()
  betafac$fac <- 1
  betafac$val <- 0
  timr <- reactiveTimer(300)
  observe({
    timr()
    val = isolate(betafac$fac)
    betafac$fac = val+1
   })
  
  }
  
  #scatter plot with regression line
  output$scatter5 <- renderPlot({
    fac = betalist[betafac$fac %% length(betalist)] #use modulo to jump back to beginning of list
    tmp=toydata_mse()
    dat=tmp[[1]]
    true_beta = tmp[[2]]$coef[2]
    dat$y_hat = true_beta * fac * dat$BP_systolic
    MSE = round(mean((dat$CGI - dat$y_hat)**2),4)
    # print(true_beta * betafac$fac)
    ggplot(dat,aes(x = BP_systolic, y = CGI))+geom_point()+
      labs(x="CGI",y="BP_systolic")+
      theme_clean()+geom_line(aes(y=y_hat),size=1.5)+
      geom_segment(aes(xend=BP_systolic,y=CGI,yend=y_hat), color="red",linetype=2)+
      geom_text(x=-1.5,y=2,label=paste("beta: ", as.character(round(true_beta * fac,2)), 
                "\nMSE: ", as.character(MSE),"\noptimal beta: ", as.character(round(true_beta,2))))+
      xlim(-2,2) + ylim(-2,3)
    })

  output$console_message_linreg <- renderPrint(cat(capture.output(summary(toydata_mse()[[2]])),sep="\n"))
  
  
  toydata_glm <- reactive({
    set.seed(999)
    df = toydata_reactive()
    df = df[sample(1:nrow(df), 50, replace=F),] #select 50 random rows for better visibility
    df$BP_systolic = scale(df$BP_systolic)  #z-score variables for easier plotting of lm line
    model = glm(sick ~ BP_systolic,family=binomial(link="logit"), data = df)
    full_model = glm(sick ~ BP_systolic + log_tcell + smoker + sex,family=binomial(link="logit"), data = df)
    
    return(list(df, model, full_model))
  })
  
  #scatter plot for logistic regression
  output$scatter6 <- renderPlot({
    tmp=toydata_glm()
    dat=tmp[[1]]
    true_beta = tmp[[2]]$coef[2]
    
    dat$y_hat = 1/(1+exp(-true_beta * dat$BP_systolic))

    ggplot(dat,aes(x = BP_systolic, y = sick))+geom_point()+
      labs(x="BP_systolic",y="sick")+
      theme_clean()+geom_line(aes(y=y_hat),size=1.5)
  })
  
  
  output$console_message_logreg <- renderPrint(cat(capture.output(summary(toydata_glm()[[2]])),sep="\n"))
  
  output$console_message_multivar <- renderPrint(cat(capture.output(summary(toydata_glm()[[3]])),sep="\n"))
  
  
})
