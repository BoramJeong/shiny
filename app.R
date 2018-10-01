
## project 4: shiny app

# install.packages("shinydashboard")

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(DT)

header <- dashboardHeader(title="Option and Stock Invest", titleWidth = 300)


sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem(strong("Calculate Option and Stock"), tabName="option", icon=icon("calculator"),
             menuSubItem("Call/Put", tabName="option1")),
    numericInput(inputId="r", label="Interest rate(3% - 0.03)", value=0.02),
    textOutput({"Today's Stock price(won)"}),
    numericInput(inputId="S0_s", label="Samsung", value=2714000),
    numericInput(inputId="S0_h", label="Hyundai", value=151500),
    numericInput(inputId="S0_k", label="kT&G", value=106500),
    numericInput(inputId="S0_g", label="Gas", value=42350)),
  
  br(), 
  numericInput(inputId="t", label="Maturity(year)", value=1),
  actionButton(inputId="novalue", label="Strike Price = Today's Price"),
  actionButton(inputId="cal", label="Calculate"),
  
  tableOutput("table987")
  
  
  
  
)



body <- dashboardBody( 
  
  tags$head(tags$style(HTML(' 
                            body, label, input, button, select, header, element { 
                            font-family: "Arial"; font-size: 17px;
                            color: #000000;
                            }
                            .skin-blue .content-wrapper{
                            background-color: #e0e0eb;
                            }
                            /* logo */
                            .skin-blue .main-header .logo {
                            background-color: #000066;
                            }
                            .skin-black .main-sidebar .ELEMENT {
                            color: #000000;
                            }
                            /* logo when hovered */
                            .skin-blue .main-header .logo:hover {
                            background-color:#000066;
                            }
                            
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #000066;
                            }        
                            
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #e0e0eb;
                            }
                            
                            /* active selected tab in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #e0e0eb;
                            color: #000000;font-weight: bold;font-size: 18px;
                            }
                            
                            /* other links in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: #e0e0eb;
                            color: #000000;
                            }
                            
                            /* other links in the sidebarmenu when hovered */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                            background-color: #e0e0eb;
                            }
                            /* toggle button when hovered  */                    
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #e0e0eb;
                            }
                            '))),
  tabItems(
    # tabItem(tabName="home", br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
    #         div(h1("Welcome to Ewha Stock Company!", align="center"))),
    
    # ??????????????????????(?????????????????? ?????????? ????????????) 
    tabItem(tabName="option1", h2("Invest Option or Stock"),br(),
            fluidPage(
              box(width=13, solidHeader=F, collapsible = T,# background="blue", width=340
                  tabBox(width= 13,id = "tabBox_next_previous", 
                         tabPanel("Price",
                                  box(title = "Option Price by Type and Company",
                                      dataTableOutput("table32")),
                                  box(title = "Plot",
                                      plotOutput("plot45"))),
                         
                         tabPanel("Option Portfolio",
                                  box(
                                    radioButtons(inputId = "type1", label="Company Type",choices = c("Samsung"="samsung","Hyundai"="Hyundai","Ktng"="Ktng","Gas"="Gas"),inline = T, selected = NULL)
                                    , tableOutput("table1"), 
                                    numericInput(inputId = "call_n",label = "Number of Call Options",value = 100),
                                    numericInput(inputId = "put_n", label = "Number of Put Options", value = 50)),
                                  box(
                                    title = "Graph", plotlyOutput("plot3")),
                                  box(
                                    textOutput("text456")
                                  )
                         ),
                         tabPanel("Optimal Option Portfolio", 
                                  
                                  box(
                                    numericInput(inputId = "totalpayment", label = "The amount that you want to invest", value = 100000000)
                                  
                                 ,
                                    tableOutput("table789")
                                  )
                                  
                         ),
                         tabPanel("Optimal Stock Portfolio", 
                                  
                                  box(width = 12,solidHeader = TRUE,
                                      splitLayout(
                                        numericInput(inputId="V0", label="Initial Invest", value = 1),
                                        numericInput(inputId="i.star", label = "Target ratio(in year)", value = 0.04),
                                        
                                        checkboxInput(inputId="past", label ="Want use Past data?",value = FALSE),
                                        numericInput(inputId="n", label = "Sample data length(year)", value = 5))
                                  )
                                  ,
                                  
                                  
                                  box( width = 12, solidHeader = TRUE,
                                       checkboxInput(inputId = "ad_con", label = "Do you have additional condition?",value = FALSE),
                                       splitLayout(
                                         numericInput(inputId = "lower", label = "lowerbound", value = NULL),
                                         selectInput("equals1", "equ/inequ",
                                                     choices = c("=" = "lower1", "<="= "lower2")),
                                         numericInput(inputId = "t0", label = "Deposits", value = 0),
                                         numericInput(inputId = "t1", label = "Samsung", value = 0),
                                         numericInput(inputId = "t2", label = "Hyundai", value = 0),
                                         numericInput(inputId = "t3", label = "KT&G", value = 0),
                                         numericInput(inputId = "t4", label = "Gas", value = 0),
                                         selectInput("equals2", "equ/inequ",
                                                     choices = c("=" = "upper1", "<="= "upper2")),
                                         numericInput(inputId = "upper", label = "upperbound", value = NULL))
                                       
                                       
                                  ),
                                  box( width =  12,
                                       title = "Optimal Portfolio", solidHeader = TRUE,
                                       tableOutput("table101")
                                  ),
                                  box(
                                    title = "Future Stock Price",solidHeader = TRUE,
                                    plotlyOutput("plot101")
                                  ),
                                  box(
                                    title = "Expected Vt Plot", solidHeader = TRUE,
                                    plotlyOutput("plot102")
                                  )
                         ),
                         tags$script("
                                     $('body').mouseover(function() {
                                     list_tabs=[];
                                     $('#tabBox_next_previous li a').each(function(){
                                     list_tabs.push($(this).html())
                                     });
                                     Shiny.onInputChange('List_of_tab', list_tabs);})
                                     ")
                         
                         ),
                  uiOutput("Next_Previous"))
            ))))




ui <- dashboardPage(
  header,
  sidebar,
  body
)


server <- function(input, output,session) {
  Previous_Button=tags$div(actionButton("Prev_Tab",HTML('
                                                        <div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
                                                        ')))
  Next_Button=div(actionButton("Next_Tab",HTML('
                                               <div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>
                                               ')))
  
  
  output$Next_Previous=renderUI({
    tab_list=input$List_of_tab[-length(input$List_of_tab)]
    nb_tab=length(tab_list)
    if (which(tab_list==input$tabBox_next_previous)==nb_tab)
      column(1,offset=1,Previous_Button)
    else if (which(tab_list==input$tabBox_next_previous)==1)
      column(1,offset = 10,Next_Button)
    else
      div(column(1,offset=1,Previous_Button),column(1,offset=8,Next_Button))
  })
  
  
  observeEvent(input$Prev_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab-1])
               })
  
  observeEvent(input$Next_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab+1])
               })
  
  
  
  options(scipen = 100) 
  
  calcul.options_samsung <- reactive({
    S0 <- input$S0_s
    K <- input$S0_s
    r <- input$r
    t <- input$t
    sigma.hat <- 0.18
    option_price <- function(K, S0, t){
      M <-500
      C <- NULL
      P <- NULL
      tt <- round(250*t,0)
      S.bar <- NULL
      S <- NULL
      for(i in 1:M) {
        zz <- rnorm(tt)
        for(j in 2:tt){
          S[1] <- S0*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[1])
          S[j] <- S[j-1]*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[j])
        }
        S.bar[i] <- sum(S)/tt
        C[i] <- exp(-r*t)*max((S.bar[i]-K),0)
        P[i] <- exp(-r*t)*max((K-S.bar[i]),0)
      }
      c_MC <- sum(C)/M
      p_MC <- sum(P)/M
      return(list(call = c_MC,put = p_MC,stock = S,s_bar = mean(S.bar)))
    }
    
    
    option_price_MC <- function(K, S0, t){
      M <- 500
      z <- rnorm(M)
      S_MC <- NULL
      for(i in 1:M) {
        S_MC[i] <- S0*exp((r-(sigma.hat^2)/2)*t + sigma.hat*sqrt(t)*z[i])
      }
      c_MC <- exp(-r*t)* sum(pmax(S_MC-K,0)/M)
      p_MC <- exp(-r*t)* sum(pmax(K-S_MC,0)/M)
      return(list(call = c_MC, put = p_MC))
    }
    
    portfolio <- function(n_c, n_p, ct, pt,t){
      tt <- round(250*t,0)
      set.seed(1234)
      zz <- rnorm(input$t*250+65)
      S <- NULL
      S0 <- input$S0_s
      K <- input$S0_s
      r <- input$r
      sigma.hat <- 0.18
      for(j in 2:(input$t*250+65)){
        S[1] <- S0*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[1])
        S[j] <- S[j-1]*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[j])
      }
      s.real <- S[tt]
      total_invest <- n_c*ct + n_p*pt
      net_profit <- (n_c*pmax(s.real-K,0) + n_p*pmax(K-s.real,0)) -(n_c*ct + n_p*pt)
      profit_ratio <- net_profit/total_invest *100
      return(list(total_invest=total_invest,net_profit=net_profit,profit_ratio=profit_ratio,S=S,s.real = s.real))
    }
    
    haha_1 <- NULL
    haha_2 <- NULL
    hahaha_1 <- NULL
    hahaha_2 <- NULL
    for (i in 1:4){
      haha_1[i] <- option_price_MC(K,S0,max(t+0.25*(i-1),0))$call
      haha_2[i] <- option_price_MC(K,S0,max(t+0.25*(i-1),0))$put
      hahaha_1[i] <- option_price(K,S0,max(t+0.25*(i-1),0))$call
      hahaha_2[i] <- option_price(K,S0,max(t+0.25*(i-1),0))$put
    }
    
    pf.total <- NULL
    pf.profit <- NULL
    pf.ratio <- NULL
    
    for ( i in 1:4) {
      tttt <- input$t
      K <- input$S0_s
      S0 <- input$S0_s
      pf.total[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put),max(0,t+0.25*(i-3)))$total_invest
      pf.profit[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put), max(0,t+0.25*(i-3)))$net_profit
      pf.ratio[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put) ,max(0,t+0.25*(i-3)))$profit_ratio
    }
    
    
    ddd<- list(S.bar = option_price(K,S0,t)$s_bar, Normal.call = option_price_MC(K,S0,t)$call, Normal.put = option_price_MC(K,S0,t)$put, 
               Asian.call = option_price(K,S0,t)$call, Asian.put = option_price(K,S0,t)$put,
               n.1 = haha_1,n.2 = haha_2,a.1 = hahaha_1,a.2 = hahaha_2, pf.total = pf.total, pf.profit = pf.profit,
               pf.ratio = pf.ratio,   S = portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,0.25*input$t)$call), as.numeric(option_price_MC(K,S0,0.25*input$t)$put),0.25*input$t)$S,
               S.real = portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,0.25*input$t)$call), as.numeric(option_price_MC(K,S0,0.25*input$t)$put),0.25*input$t)$s.real
    )
    return(ddd)
    
  })
  
  
  calcul.options_hyundai <- reactive({
    S0 <- input$S0_h
    K <- input$S0_h
    r <- input$r
    t <- input$t
    sigma.hat <- 0.2
    option_price <- function(K, S0, t){
      M <-500
      C <- NULL
      P <- NULL
      tt <- 250*t
      S.bar <- NULL
      S <- NULL
      for(i in 1:M) {
        zz <- rnorm(tt)
        for(j in 2:tt){
          S[1] <- S0*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[1])
          S[j] <- S[j-1]*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[j])
        }
        S.bar[i] <- sum(S)/tt
        C[i] <- exp(-r*t)*max((S.bar[i]-K),0)
        P[i] <- exp(-r*t)*max((K-S.bar[i]),0)
      }
      c_MC <- sum(C)/M
      p_MC <- sum(P)/M
      return(list(call = c_MC,put = p_MC))
    }
    
    
    option_price_MC <- function(K, S0, t){
      M <- 500
      z <- rnorm(M)
      S_MC <- NULL
      for(i in 1:M) {
        S_MC[i] <- S0*exp((r-(sigma.hat^2)/2)*t + sigma.hat*sqrt(t)*z[i])
      }
      c_MC <- exp(-r*t)* sum(pmax(S_MC-K,0)/M)
      p_MC <- exp(-r*t)* sum(pmax(K-S_MC,0)/M)
      return(list(call = c_MC, put = p_MC))
    }
    
    portfolio <- function(n_c, n_p, ct, pt,t){
      tt <- round(250*t,0)
      set.seed(1234)
      zz <- rnorm(input$t*250+65)
      S <- NULL
      S0 <- input$S0_h
      K <- input$S0_h
      r <- input$r
      sigma.hat <- 0.2
      for(j in 2:(input$t*250+65)){
        S[1] <- S0*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[1])
        S[j] <- S[j-1]*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[j])
      }
      s.real <- S[tt]
      total_invest <- n_c*ct + n_p*pt
      net_profit <- (n_c*pmax(s.real-K,0) + n_p*pmax(K-s.real,0)) -(n_c*ct + n_p*pt)
      profit_ratio <- net_profit/total_invest *100
      return(list(total_invest=total_invest,net_profit=net_profit,profit_ratio=profit_ratio,S=S))
    } 
    
    haha_1 <- NULL
    haha_2 <- NULL
    hahaha_1 <- NULL
    hahaha_2 <- NULL
    for (i in 1:4){
      haha_1[i] <- option_price_MC(K,S0,max(t+0.25*(i-1),0))$call
      haha_2[i] <- option_price_MC(K,S0,max(t+0.25*(i-1),0))$put
      hahaha_1[i] <- option_price(K,S0,max(t+0.25*(i-1),0))$call
      hahaha_2[i] <- option_price(K,S0,max(t+0.25*(i-1),0))$put
    }
    
    pf.total <- NULL
    pf.profit <- NULL
    pf.ratio <- NULL
    for ( i in 1:4) {
      K <- input$S0_h
      S0 <- input$S0_h
      pf.total[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put),max(0,t+0.25*(i-3)))$total_invest
      pf.profit[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put), max(0,t+0.25*(i-3)))$net_profit
      pf.ratio[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put) ,max(0,t+0.25*(i-3)))$profit_ratio
      
    }
    
    
    ddd<- list(Normal.call = option_price_MC(K,S0,t)$call, Normal.put = option_price_MC(K,S0,t)$put, 
               Asian.call = option_price(K,S0,t)$call, Asian.put = option_price(K,S0,t)$put,
               n.1 = haha_1,n.2 = haha_2,a.1 = hahaha_1,a.2 = hahaha_2, pf.total = pf.total, pf.profit = pf.profit,
               pf.ratio = pf.ratio,  S = portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,0.25*input$t)$call), as.numeric(option_price_MC(K,S0,0.25*input$t)$put),0.25*input$t)$S
    )
    return(ddd)
    
  })
  
  
  
  calcul.options_ktng <- reactive({
    S0 <- input$S0_k
    K <- input$S0_k
    r <- input$r
    t <- input$t
    sigma.hat <- 0.16
    option_price <- function(K, S0, t){
      M <-500
      C <- NULL
      P <- NULL
      tt <- 250*t
      S.bar <- NULL
      S <- NULL
      for(i in 1:M) {
        zz <- rnorm(tt)
        for(j in 2:tt){
          S[1] <- S0*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[1])
          S[j] <- S[j-1]*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[j])
        }
        S.bar[i] <- sum(S)/tt
        C[i] <- exp(-r*t)*max((S.bar[i]-K),0)
        P[i] <- exp(-r*t)*max((K-S.bar[i]),0)
      }
      c_MC <- sum(C)/M
      p_MC <- sum(P)/M
      return(list(call = c_MC,put = p_MC))
    }
    
    
    option_price_MC <- function(K, S0, t){
      M <- 500
      z <- rnorm(M)
      S_MC <- NULL
      for(i in 1:M) {
        S_MC[i] <- S0*exp((r-(sigma.hat^2)/2)*t + sigma.hat*sqrt(t)*z[i])
      }
      c_MC <- exp(-r*t)* sum(pmax(S_MC-K,0)/M)
      p_MC <- exp(-r*t)* sum(pmax(K-S_MC,0)/M)
      return(list(call = c_MC, put = p_MC))
    }
    
    portfolio <- function(n_c, n_p, ct, pt,t){
      tt <- round(250*t,0)
      set.seed(1234)
      zz <- rnorm(input$t*250+65)
      S <- NULL
      S0 <- input$S0_k
      K <- input$S0_k
      r <- input$r
      sigma.hat <- 0.16
      for(j in 2:(input$t*250+65)){
        S[1] <- S0*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[1])
        S[j] <- S[j-1]*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[j])
      }
      s.real <- S[tt]
      total_invest <- n_c*ct + n_p*pt
      net_profit <- (n_c*pmax(s.real-K,0) + n_p*pmax(K-s.real,0)) -(n_c*ct + n_p*pt)
      profit_ratio <- net_profit/total_invest *100
      return(list(total_invest=total_invest,net_profit=net_profit,profit_ratio=profit_ratio,S=S))
    }
    
    haha_1 <- NULL
    haha_2 <- NULL
    hahaha_1 <- NULL
    hahaha_2 <- NULL
    for (i in 1:4){
      haha_1[i] <- option_price_MC(K,S0,max(t+0.25*(i-1),0))$call
      haha_2[i] <- option_price_MC(K,S0,max(t+0.25*(i-1),0))$put
      hahaha_1[i] <- option_price(K,S0,max(t+0.25*(i-1),0))$call
      hahaha_2[i] <- option_price(K,S0,max(t+0.25*(i-1),0))$put
    }
    
    pf.total <- NULL
    pf.profit <- NULL
    pf.ratio <- NULL
    for ( i in 1:4) {
      K <- input$S0_k
      S0 <- input$S0_k
      pf.total[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put),max(0,t+0.25*(i-3)))$total_invest
      pf.profit[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put), max(0,t+0.25*(i-3)))$net_profit
      pf.ratio[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put) ,max(0,t+0.25*(i-3)))$profit_ratio
      
    }
    
    
    ddd<- list(Normal.call = option_price_MC(K,S0,t)$call, Normal.put = option_price_MC(K,S0,t)$put, 
               Asian.call = option_price(K,S0,t)$call, Asian.put = option_price(K,S0,t)$put,
               n.1 = haha_1,n.2 = haha_2,a.1 = hahaha_1,a.2 = hahaha_2, pf.total = pf.total, pf.profit = pf.profit,
               pf.ratio = pf.ratio,   S = portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,0.25*input$t)$call), as.numeric(option_price_MC(K,S0,0.25*input$t)$put),0.25*input$t)$S
    )
    return(ddd)
    
  })
  
  
  
  calcul.options_g <- reactive({
    S0 <- input$S0_g
    K <- input$S0_g
    r <- input$r
    t <- input$t
    sigma.hat <- 0.27
    option_price <- function(K, S0, t){
      M <-500
      C <- NULL
      P <- NULL
      tt <- 250*t
      S.bar <- NULL
      S <- NULL
      for(i in 1:M) {
        zz <- rnorm(tt)
        for(j in 2:tt){
          S[1] <- S0*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[1])
          S[j] <- S[j-1]*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[j])
        }
        S.bar[i] <- sum(S)/tt
        C[i] <- exp(-r*t)*max((S.bar[i]-K),0)
        P[i] <- exp(-r*t)*max((K-S.bar[i]),0)
      }
      c_MC <- sum(C)/M
      p_MC <- sum(P)/M
      return(list(call = c_MC,put = p_MC,s_bar = S.bar))
    }
    
    
    option_price_MC <- function(K, S0, t){
      M <- 500
      z <- rnorm(M)
      S_MC <- NULL
      for(i in 1:M) {
        S_MC[i] <- S0*exp((r-(sigma.hat^2)/2)*t + sigma.hat*sqrt(t)*z[i])
      }
      c_MC <- exp(-r*t)* sum(pmax(S_MC-K,0)/M)
      p_MC <- exp(-r*t)* sum(pmax(K-S_MC,0)/M)
      return(list(call = c_MC, put = p_MC))
    }
    
    portfolio <- function(n_c, n_p, ct, pt,t){
      tt <- round(250*t,0)
      set.seed(1234)
      zz <- rnorm(input$t*250+65)
      S <- NULL
      S0 <- input$S0_g
      K <- input$S0_g
      r <- input$r
      sigma.hat <- 0.27
      for(j in 2:(input$t*250+65)){
        S[1] <- S0*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[1])
        S[j] <- S[j-1]*exp((r-(sigma.hat^2)/2)*(1/250)+sigma.hat*sqrt(1/250)*zz[j])
      }
      s.real <- S[tt]
      total_invest <- n_c*ct + n_p*pt
      net_profit <- (n_c*pmax(s.real-K,0) + n_p*pmax(K-s.real,0)) -(n_c*ct + n_p*pt)
      profit_ratio <- net_profit/total_invest *100
      return(list(total_invest=total_invest,net_profit=net_profit,profit_ratio=profit_ratio,S=S))
    }
    
    haha_1 <- NULL
    haha_2 <- NULL
    hahaha_1 <- NULL
    hahaha_2 <- NULL
    for (i in 1:4){
      haha_1[i] <- option_price_MC(K,S0,max(t+0.25*(i-1),0))$call
      haha_2[i] <- option_price_MC(K,S0,max(t+0.25*(i-1),0))$put
      hahaha_1[i] <- option_price(K,S0,max(t+0.25*(i-1),0))$call
      hahaha_2[i] <- option_price(K,S0,max(t+0.25*(i-1),0))$put
    }
    
    pf.total <- NULL
    pf.profit <- NULL
    pf.ratio <- NULL
    for ( i in 1:4) {
      K <-input$S0_g
      S0 <- input$S0_g
      pf.total[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put),max(0,t+0.25*(i-3)))$total_invest
      pf.profit[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put), max(0,t+0.25*(i-3)))$net_profit
      pf.ratio[i] <- portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$call), as.numeric(option_price_MC(K,S0,max(0,t+0.25*(i-3)))$put) ,max(0,t+0.25*(i-3)))$profit_ratio
      
    }
    
    
    ddd<- list(S.bar = option_price_MC(K,S0,t)$s_bar, Normal.call = option_price_MC(K,S0,t)$call, Normal.put = option_price_MC(K,S0,t)$put, 
               Asian.call = option_price(K,S0,t)$call, Asian.put = option_price(K,S0,t)$put,
               n.1 = haha_1,n.2 = haha_2,a.1 = hahaha_1,a.2 = hahaha_2, pf.total = pf.total, pf.profit = pf.profit,
               pf.ratio = pf.ratio,   S = portfolio(input$call_n,input$put_n,as.numeric(option_price_MC(K,S0,0.25*input$t)$call), as.numeric(option_price_MC(K,S0,0.25*input$t)$put),0.25*input$t)$S
    )
    return(ddd)
    
  })
  
  foroption_chart <- reactive({
    normal.call <- c(calcul.options_samsung()$Normal.call, calcul.options_hyundai()$Normal.call,calcul.options_ktng()$Normal.call,calcul.options_g()$Normal.call)
    normal.put <- c(calcul.options_samsung()$Normal.put, calcul.options_hyundai()$Normal.put,calcul.options_ktng()$Normal.put,calcul.options_g()$Normal.put)
    asian.call <- c(calcul.options_samsung()$Asian.call, calcul.options_hyundai()$Asian.call,calcul.options_ktng()$Asian.call,calcul.options_g()$Asian.call)
    asian.put <- c(calcul.options_samsung()$Asian.put, calcul.options_hyundai()$Asian.put,calcul.options_ktng()$Asian.put,calcul.options_g()$Asian.put)
    optiondata <- as.data.frame(rbind(normal.call, normal.put, asian.call, asian.put))
    colnames(optiondata) <- c("Samsung","Hyundai","Ktng","Gas")
    rownames(optiondata) <- c("Normal.Call", "Normal.Put", "Asian.Call","Asian.Put")
    return(optiondata)
  })
  
  # barplot(data, col=heat.colors(length(rownames(data))), width=2, beside=TRUE)
  # legend("topright",inset=c(-0.25,0), fill=heat.colors(length(rownames(data))), legend=rownames(data))
  # 
  output$plot45 <- renderPlot({
    barplot(as.matrix(foroption_chart()), col = heat.colors(length(rownames(foroption_chart()))),beside = TRUE)
    legend("topright",rownames(foroption_chart()),cex = 0.9, bty = "n", fill = heat.colors(4))
  })
  
  
  forchart_s <- reactive({
    SS <- NULL
    S0 <- input$S0_s
    t <- input$t
    for(i in 1:4){
      SS[i] <-(calcul.options_samsung()$S[max(0,62*(4*t+(i-3)))]-S0)/S0*100
    }
    return(SS)
  })
  
  forchart_h <- reactive({
    SS <- NULL
    S0 <- input$S0_h
    t <- input$t
    for(i in 1:4){
      SS[i] <-(calcul.options_hyundai()$S[max(0,62*(4*t+(i-3)))]-S0)/S0*100
    }
    return(SS)
  })
  
  forchart_k <- reactive({
    SS <- NULL
    S0 <- input$S0_k
    t <- input$t
    for(i in 1:4){
      SS[i] <-(calcul.options_ktng()$S[max(0,62*(4*t+(i-3)))]-S0)/S0*100
    }
    return(SS)
  })
  
  forchart_g <- reactive({
    SS <- NULL
    S0 <- input$S0_k
    t <- input$t
    for(i in 1:4){
      SS[i] <-(calcul.options_g()$S[max(0,62*(4*t+(i-3)))]-S0)/S0*100
    }
    return(SS)
  })
  
  barchart <- reactive({
    if(input$type1 == "samsung"){
      data3 <- data.frame("SorP"=c(rep("S.ratio",4),rep("pf.ratio",4)),"ratio"=c(forchart_s(),calcul.options_samsung()$pf.ratio),"maturity"= factor(rep(c("-2/4","-1/4","0","+1/4"),2),levels = c("-2/4","-1/4","0","+1/4")))
      return(ggplot(data= data3, aes(x=maturity,y = ratio,fill= SorP)) + geom_bar(stat= "identity",position=position_dodge())+ 
               scale_fill_brewer(palette = "Pastel1"))}else if(input$type1 == "Hyundai"){
                 data3 <- data.frame("SorP"=c(rep("S.ratio",4),rep("pf.ratio",4)),"ratio"=c(forchart_h(),calcul.options_hyundai()$pf.ratio),"maturity"= factor(rep(c("-2/4","-1/4","0","+1/4"),2),levels = c("-2/4","-1/4","0","+1/4")))
                 return(ggplot(data= data3, aes(x=maturity,y = ratio,fill= SorP)) + geom_bar(stat= "identity",position=position_dodge())+ 
                          scale_fill_brewer(palette = "Pastel1"))}else if(input$type1 == "Ktng"){
                            data3 <- data.frame("SorP"=c(rep("S.ratio",4),rep("pf.ratio",4)),"ratio"=c(forchart_k(),calcul.options_ktng()$pf.ratio),"maturity"= factor(rep(c("-2/4","-1/4","0","+1/4"),2),levels = c("-2/4","-1/4","0","+1/4")))
                            return(ggplot(data= data3, aes(x=maturity,y = ratio,fill= SorP)) + geom_bar(stat= "identity",position=position_dodge())+ 
                                     scale_fill_brewer(palette = "Pastel1"))}else if(input$type1 == "Gas"){
                                       data3 <- data.frame("SorP"=c(rep("S.ratio",4),rep("pf.ratio",4)),"ratio"=c(forchart_g(),calcul.options_g()$pf.ratio),"maturity"= factor(rep(c("-2/4","-1/4","0","+1/4"),2),levels = c("-2/4","-1/4","0","+1/4")))
                                       return(ggplot(data= data3, aes(x=maturity,y = ratio,fill= SorP)) + geom_bar(stat= "identity",position=position_dodge())+ 
                                                scale_fill_brewer(palette = "Pastel1"))  }
    
  })
  
  
  
  
  formaxratio <- reactive({
    library("Rsolnp")
    eqn <- function(theta){
      ct1 <- calcul.options_samsung()$Normal.call
      ct2 <- calcul.options_samsung()$Normal.put
      ct3 <- calcul.options_samsung()$Asian.call
      ct4 <- calcul.options_samsung()$Asian.put
      return(ct1*theta[1] + ct2*theta[2] + ct3*theta[3] + ct4*theta[4])
    }
    
    opt <- function(theta){
      K <- input$S0_s
      s.real <- calcul.options_samsung()$S.real
      s.bar <- calcul.options_samsung()$S.bar
      ct1 <- calcul.options_samsung()$Normal.call
      ct2 <- calcul.options_samsung()$Normal.put
      ct3 <- calcul.options_samsung()$Asian.call
      ct4 <- calcul.options_samsung()$Asian.put
      total_invest <- (ct1*theta[1] + ct2*theta[2] + ct3*theta[3] + ct4*theta[4])
      net_profit <- (theta[1]*pmax(s.real-K,0) + theta[2]*pmax(K-s.real,0) + theta[3]*pmax(s.bar-K,0) + theta[4]*pmax(K-s.bar,0))-(ct1*theta[1] + ct2*theta[2] + ct3*theta[3] + ct4*theta[4])
      profit_ratio <- -net_profit/total_invest *100
      return(profit_ratio)
    }
    
    result1 <- gosolnp(pars = NULL, fixed = NULL, opt, eqfun=eqn , eqB=input$totalpayment, LB = c(0,0,0,0), UB= c(400,400,400,400))
    iter <- 1 + result1$outer.iter
    maxrate <- result1$values[iter]
    maxrate <- -maxrate
    
    r3 <- as.numeric(c(t(as.vector(result1$pars)),maxrate))
    r3 <- t(as.data.frame(r3))
    colnames(r3) <- c("Normall.Call", "Normal.Put","Asian.Call","Asian.Put","Max.ratio")
    return(r3)
    
  })
  
  output$table789 <- renderTable({
    formaxratio()
  })
  
  output$plot3 <- renderPlotly({
    ggplotly(barchart())
  })
  
  totalpay <- reactive({
    if(input$type1 == "samsung"){
      return(calcul.options_samsung()$pf.profit[3])
    }else if(input$type1 == "Hyundai"){
      return(calcul.options_hyundai()$pf.profit[3])
    }else if(input$type1 == "Ktng"){
      return(calcul.options_ktng()$pf.profit[3])
    }else if(input$type1 == "Gas"){
      return(calcul.options_g()$pf.profit[3])
    }
  })
  
  
  
  
  output$text456 <- renderText({
    paste("Total Net Profit is   ",trunc(round(totalpay(),0)),"won.")
  })
  
  plotplot <- reactive({
    data2 <- data.frame("stock_price" = calcul.options()$S, "t" = seq(1,length(calcul.options()$S)))
    return(ggplot(data = data2, aes(x=t/250, y = stock_price)) + geom_line(colour = "slategray3")+
             theme_minimal() +xlab("time") + ylab("Stock price"))
  })
  optionPlot <- reactive({
    S0 <- input$K
    xx <- seq(from = S0 * 0.5 + 1, to =  S0 * 1.5, by = S0 * 0.01 )
    yC <- pmax(xx - S0, 0); yP <- pmax(S0 - xx, 0)
    optiondata <- data.frame(x = xx, yc = yC, yp = yP)
    
    if(input$type=="call"){
      
      return(ggplot(data=optiondata,aes(x=x, y=yc))+geom_line()+theme_minimal() +xlab("Stock price") + ylab("payoff"))
    } else if(input$type == "put") {
      return(ggplot(data=optiondata,aes(x=x, y=yp))+geom_line()+theme_minimal() +xlab("Stock price") + ylab("payoff"))
    }
  })
  
  output$table32 <- renderDataTable({
    datatable(data.frame("Option Type" = c("Normal.Call", "Normal.Put","Asian.Call","Asian.Put"),"Samsung" = foroption_chart()[,1],"Hyundai" = foroption_chart()[,2],"Ktng" = foroption_chart()[,3],"Gas" = foroption_chart()[,4])) %>% 
      formatRound(c("Samsung","Hyundai","Ktng","Gas"), digits = 0) 
  })   
  output$plot2 <- renderPlot({
    optionPlot()
  })
  output$plot1 <- renderPlot({
    plotplot()
  })
  
  mattable1 <- reactive({
    
    if(input$type1 == "samsung"){
      return(data.frame("maturity"= c("now","plus 3 month","plus 6 month","plus 12 month"),"Normal.call"=trunc(round(calcul.options_samsung()$n.1,0)),"Normal.put"=trunc(round(calcul.options_samsung()$n.2,0)),"Asian.call"=trunc(round(calcul.options_samsung()$a.1,0)),"Asian.put"=trunc(round(calcul.options_samsung()$a.2,0)))) 
      
    }else if(input$type1 == "Hyundai"){
      return(data.frame("maturity"= c("now","plus 3 month","plus 6 month","plus 12 month"),"Normal.call"=trunc(round(calcul.options_hyundai()$n.1,0)),"Normal.put"=trunc(round(calcul.options_hyundai()$n.2,0)),"Asian.call"=trunc(round(calcul.options_hyundai()$a.1,0)),"Asian.put"=trunc(round(calcul.options_hyundai()$a.2,0)))) 
      
    }else if(input$type1 == "Ktng"){
      return(data.frame("maturity"= c("now","plus 3 month","plus 6 month","plus 12 month"),"Normal.call"=trunc(round(calcul.options_ktng()$n.1,0)),"Normal.put"=trunc(round(calcul.options_ktng()$n.2,0)),"Asian.call"=trunc(round(calcul.options_ktng()$a.1,0)),"Asian.put"=trunc(round(calcul.options_ktng()$a.2,0))))
      
    }else if(input$type1 == "Gas"){
      return(data.frame("maturity"= c("now","plus 3 month","plus 6 month","plus 12 month"),"Normal.call"=trunc(round(calcul.options_g()$n.1,0)),"Normal.put"=trunc(round(calcul.options_g()$n.2,0)),"Asian.call"=trunc(round(calcul.options_g()$a.1,0)),"Asian.put"=trunc(round(calcul.options_g()$a.2,0))))
    }
  })
  
  output$table1 <- renderTable({
    mattable1()
  }) 
  
  output$text1 <- renderText("< Expected Profit by your Call & Put option Portfolio Choice >")
  
  
  
  output$table2 <- renderDataTable({
    datatable(data.frame("maturity"= c("1/4","1/2","3/4","1"),"Total.Invest"=calcul.options()$pf.total,"Net.Profit"=calcul.options()$pf.profit,
                         "Profit.Ratio" = calcul.options()$pf.ratio)) %>%
      formatRound(c("Total.Invest","Net.Profit","Profit.Ratio"), digits = 0)
    # %>%
    # formatCurrency(c("Total.Invest","Net.Profit"),currency = "W", interval = 3, mark = ",")
  })
  
  
  
  
  
  next.page <- eventReactive(input$nextpage,{
    return(show("Optimal Stock Portfolio"))
  })
  
  
  
  
  
  ####################project5 merge
  
  part3data <- read.csv("part3data.csv", header = T)
  part3data$cd <- part3data$cd/100
  part3data$i12 <- (1+part3data$cd)^(1/12)-1 
  d.lnSt0 <- diff(log(part3data$i12))
  r_1t <- diff(part3data$samsung)/(part3data$samsung[-length(part3data$samsung)])
  part3data$rt_s <- c(r_1t[1],r_1t)
  d.lnSt1 <-diff(log(part3data$samsung))
  r_2t <- diff(part3data$hyundai)/(part3data$hyundai[-length(part3data$hyundai)])
  part3data$rt_h <- c(r_2t[1], r_2t)
  d.lnSt2 <-diff(log(part3data$hyundai))
  r_3t <- diff(part3data$ktng)/(part3data$ktng[-length(part3data$ktng)])
  part3data$rt_k <- c(r_3t[1], r_3t)
  d.lnSt3 <-diff(log(part3data$ktng))
  r_4t <- diff(part3data$gas)/(part3data$gas[-length(part3data$gas)])
  part3data$rt_g <- c(r_4t[1], r_4t)
  d.lnSt4 <-diff(log(part3data$gas))
  
  
  forsigma <- reactive({
    
    N <- input$n 
    
    part3data <- part3data[(((length(part3data$i12)-N*12)+1):length(part3data$i12)),]### ??????12*4 = 48 ?????? ?????? ????????? ??????
    d.lnSt0 <- d.lnSt0[(((length(d.lnSt1)-N*12)+1):length(d.lnSt0))]
    d.lnSt1 <- d.lnSt1[(((length(d.lnSt1)-N*12)+1):length(d.lnSt1))]
    d.lnSt2 <- d.lnSt2[(((length(d.lnSt2)-N*12)+1):length(d.lnSt2))]
    d.lnSt3 <- d.lnSt3[(((length(d.lnSt3)-N*12)+1):length(d.lnSt3))]
    d.lnSt4 <- d.lnSt4[(((length(d.lnSt4)-N*12)+1):length(d.lnSt4))]
    
    M <- input$t * 12
    
    
    data1234 <- data.frame(d.lnSt0 = d.lnSt0,d.lnSt1 = d.lnSt1, d.lnSt2= d.lnSt2, d.lnSt3= d.lnSt3, d.lnSt4 = d.lnSt4)
    
    
    sigmaz<- NULL
    r <- input$r 
    
    for (i in 1:5) {
      S0 <- c(((r+1)^(1/12))-1, input$S0_s,input$S0_h,input$S0_k,input$S0_g)
      S0 <- S0[i]
      r <- log(1+r)
      u.bar <- sum(data1234[,i])/length(data1234[,i])  ###d.lnSt1 <-diff(log(part3data$samsung))
      mu.hat.star <- u.bar/(1/12)   
      s <- sqrt(sum((data1234[,i]-u.bar)^2)/(length(data1234[,i])-1))
      sigma.hat <- s/sqrt(1/12)
      mu.hat <- (u.bar+ (s^2)/2)/(1/12)
      
      
      sigmaz[i]<-sigma.hat
    }
    sigmaz <- sigmaz[-1]
    sigmaz <- t(as.data.frame(sigmaz))
    colnames(sigmaz) <- c("Samsung","Hyundai","KT&G","Gas")
    return(sigmaz)     ##colname ?????? : m, S(???????????????????????????),rt(????????????),S.1(????????????),rt.1(???????????????),S.2(????????????),rt.2(???????????????),,rt.4??????
    
  })
  
  
  
  
  
  
  expdata <- reactive({
    
    N <- input$n 
    
    part3data <- part3data[(((length(part3data$i12)-N*12)+1):length(part3data$i12)),]### ??????12*4 = 48 ?????? ?????? ????????? ??????
    d.lnSt0 <- d.lnSt0[(((length(d.lnSt1)-N*12)+1):length(d.lnSt0))]
    d.lnSt1 <- d.lnSt1[(((length(d.lnSt1)-N*12)+1):length(d.lnSt1))]
    d.lnSt2 <- d.lnSt2[(((length(d.lnSt2)-N*12)+1):length(d.lnSt2))]
    d.lnSt3 <- d.lnSt3[(((length(d.lnSt3)-N*12)+1):length(d.lnSt3))]
    d.lnSt4 <- d.lnSt4[(((length(d.lnSt4)-N*12)+1):length(d.lnSt4))]
    
    M <- input$t * 12
    
    
    data1234 <- data.frame(d.lnSt0 = d.lnSt0,d.lnSt1 = d.lnSt1, d.lnSt2= d.lnSt2, d.lnSt3= d.lnSt3, d.lnSt4 = d.lnSt4)
    
    
    expr <- list()
    r <- input$r 
    
    for (i in 1:5) {
      S0 <- c(((r+1)^(1/12))-1, input$S0_s,input$S0_h,input$S0_k,input$S0_g)
      S0 <- S0[i]
      r <- log(1+r)
      u.bar <- sum(data1234[,i])/length(data1234[,i])  ###d.lnSt1 <-diff(log(part3data$samsung))
      mu.hat.star <- u.bar/(1/12)   
      s <- sqrt(sum((data1234[,i]-u.bar)^2)/(length(data1234[,i])-1))
      sigma.hat <- s/sqrt(1/12)
      mu.hat <- (u.bar+ (s^2)/2)/(1/12)
      
      zz <- rnorm(M)
      S <- NULL
      rt <- NULL
      
      for(j in 2:M){
        S[1] <- S0*exp((r-(sigma.hat^2)/2)*(1/12)+sigma.hat*sqrt(1/12)*zz[1])
        S[j] <- S[j-1]*exp((r-(sigma.hat^2)/2)*(1/12)+sigma.hat*sqrt(1/12)*zz[j])
        rt <- diff(S)/(S[-length(S)])
        rt <- c((S[1]-S0)/S0,rt)
      }
      expr[[i]] <-data.frame(S=S,rt=rt)
    }
    
    er0 <- expr[[1]]
    es1 <- expr[[2]]
    es2 <- expr[[3]]
    es3 <- expr[[4]]
    es4 <- expr[[5]]  ##S and rt
    
    expdata1 <- data.frame(m = seq(1,M, by = 1),er0,es1,es2,es3,es4)
    return(expdata1)     ##colname ?????? : m, S(???????????????????????????),rt(????????????),S.1(????????????),rt.1(???????????????),S.2(????????????),rt.2(???????????????),,rt.4??????
    
  })
  
  
  
  
  
  findtheta <- reactive({
    if(input$past == FALSE){
      N <- input$n
      # part3data <- part3data[(((length(part3data$i12)-N*12)+1):length(part3data$i12)),]
      i.star <- input$i.star
      i.star.12 <- ((1+i.star)^(1/12)) -1
      library("Rsolnp")
      
      if(input$ad_con == FALSE){
        eqn <- function(theta){
          sum(theta)
        }
        
        opt<- function(theta){
          i.star <- input$i.star
          i.star.12 <- ((1+i.star)^(1/12)) -1
          # r0<- part3data$i12
          # r1<- part3data$rt_s
          # r2 <- part3data$rt_h
          # r3 <-part3data$rt_k
          # r4 <- part3data$rt_g
          r0 <- expdata()$S
          r1 <- expdata()$rt.1
          r2 <- expdata()$rt.2
          r3 <- expdata()$rt.3
          r4 <- expdata()$rt.4
          rp <- theta[1]*r0 + theta[2]*r1 +theta[3]*r2 +theta[4]*r3 + theta[5]*r4
          rp.bar <- sum(rp)/(N*12)
          sp <- sqrt(sum((rp-rp.bar)^2)/((N*12)-1))
          Z <- -(rp.bar - i.star.12)/sp
          return(Z)
        }
        
        
        result1 <- gosolnp(pars=NULL, fixed= NULL ,opt, eqfun=eqn, eqB=1, LB=c(0,0,0,0,0),UB = c(1,1,1,1,1))
        iter <- 1+result1$outer.iter
        sharpe <- result1$values[iter]
        sharpe <- -sharpe
        r2 <- as.numeric(c(t(as.vector(result1$pars)),sharpe))
        r2 <- t(as.data.frame(r2))
        colnames(r2) <- c("Deposit","Samsung","Hyundai","KT&G","Gas","Sharpe ratio")
        return(r2)
      }
      
      else if (input$ad_con == TRUE){
        if(input$equals2 == "upper1"){
          eqn <- function(theta){
            z1 = sum(theta)
            z2 = input$t0*theta[1] +input$t1* theta[2] + input$t2*theta[3] + input$t3*theta[4] + input$t4*theta[5]
            return(c(z1,z2))
          }
          
          opt<- function(theta){
            i.star <- input$i.star
            i.star.12 <- ((1+i.star)^(1/12)) -1
            # r0<- part3data$i12
            # r1<- part3data$rt_s
            # r2 <- part3data$rt_h
            # r3 <-part3data$rt_k
            # r4 <- part3data$rt_g
            r0 <- expdata()$S
            r1 <- expdata()$rt.1
            r2 <- expdata()$rt.2
            r3 <- expdata()$rt.3
            r4 <- expdata()$rt.4
            rp <- theta[1]*r0 + theta[2]*r1 +theta[3]*r2 +theta[4]*r3 + theta[5]*r4
            rp.bar <- sum(rp)/(N*12)
            sp <- sqrt(sum((rp-rp.bar)^2)/((N*12)-1))
            Z <- -(rp.bar - i.star.12)/sp
            return(Z)
          }
          
          
          result1 <- gosolnp(pars=NULL, fixed= NULL ,opt, eqfun=eqn, eqB=c(1,input$upper), LB=c(0,0,0,0,0),UB = c(1,1,1,1,1))
          iter <- 1+result1$outer.iter
          sharpe <- result1$values[iter]
          sharpe <- -sharpe
          r2 <- as.numeric(c(t(as.vector(result1$pars)),sharpe))
          r2 <- t(as.data.frame(r2))
          colnames(r2) <- c("Deposit","Samsung","Hyundai","KT&G","Gas","Sharpe ratio")
          return(r2)
        }
        else if (input$equals2 == "upper2" | input$equals1 == "lower2"){
          eqn <- function(theta){
            z1 = sum(theta)
            return(z1)
          }
          ineqn <- function(theta){
            z2 = input$t0*theta[1] +input$t1* theta[2] + input$t2*theta[3] + input$t3*theta[4] + input$t4*theta[5]
            return(z2)
          }
          
          opt<- function(theta){
            i.star <- input$i.star
            i.star.12 <- ((1+i.star)^(1/12)) -1
            # r0<- part3data$i12
            # r1<- part3data$rt_s
            # r2 <- part3data$rt_h
            # r3 <-part3data$rt_k
            # r4 <- part3data$rt_g
            r0 <- expdata()$S
            r1 <- expdata()$rt.1
            r2 <- expdata()$rt.2
            r3 <- expdata()$rt.3
            r4 <- expdata()$rt.4
            rp <- theta[1]*r0 + theta[2]*r1 +theta[3]*r2 +theta[4]*r3 + theta[5]*r4
            rp.bar <- sum(rp)/(N*12)
            sp <- sqrt(sum((rp-rp.bar)^2)/((N*12)-1))
            Z <- -(rp.bar - i.star.12)/sp
            return(Z)
          }
          
          
          result1 <- gosolnp(pars=NULL, fixed= NULL ,opt, eqfun=eqn, eqB=1,ineqfun = ineqn,ineqLB = input$lower,ineqUB = input$upper, LB=c(0,0,0,0,0),UB = c(1,1,1,1,1))
          iter <- 1+result1$outer.iter
          sharpe <- result1$values[iter]
          sharpe <- -sharpe
          r2 <- as.numeric(c(t(as.vector(result1$pars)),sharpe))
          r2 <- t(as.data.frame(r2))
          colnames(r2) <- c("Deposit","Samsung","Hyundai","KT&G","Gas","Sharpe ratio")
          return(r2)
        }
      }
    }
    else if (input$past == TRUE){
      N <- input$n
      part3data <- part3data[(((length(part3data$i12)-N*12)+1):length(part3data$i12)),]
      i.star <- input$i.star
      i.star.12 <- ((1+i.star)^(1/12)) -1
      library("Rsolnp")
      
      if(input$ad_con == FALSE){
        eqn <- function(theta){
          sum(theta)
        }
        
        opt<- function(theta){
          i.star <- input$i.star
          i.star.12 <- ((1+i.star)^(1/12)) -1
          r0<- part3data$i12
          r1<- part3data$rt_s
          r2 <- part3data$rt_h
          r3 <-part3data$rt_k
          r4 <- part3data$rt_g
          #r0 <- expdata()$S
          #r1 <- expdata()$rt.1
          #r2 <- expdata()$rt.2
          #r3 <- expdata()$rt.3
          #r4 <- expdata()$rt.4
          rp <- theta[1]*r0 + theta[2]*r1 +theta[3]*r2 +theta[4]*r3 + theta[5]*r4
          rp.bar <- sum(rp)/(N*12)
          sp <- sqrt(sum((rp-rp.bar)^2)/((N*12)-1))
          Z <- -(rp.bar - i.star.12)/sp
          return(Z)
        }
        
        
        result1 <- gosolnp(pars=NULL, fixed= NULL ,opt, eqfun=eqn, eqB=1, LB=c(0,0,0,0,0),UB = c(1,1,1,1,1))
        iter <- 1+result1$outer.iter
        sharpe <- result1$values[iter]
        sharpe <- -sharpe
        r2 <- as.numeric(c(t(as.vector(result1$pars)),sharpe))
        r2 <- t(as.data.frame(r2))
        colnames(r2) <- c("Deposit","Samsung","Hyundai","KT&G","Gas","Sharpe ratio")
        return(r2)
      }
      
      else if (input$ad_con == TRUE){
        if(input$equals2 == "upper1"){
          eqn <- function(theta){
            z1 = sum(theta)
            z2 = input$t0*theta[1] +input$t1* theta[2] + input$t2*theta[3] + input$t3*theta[4] + input$t4*theta[5]
            return(c(z1,z2))
          }
          
          opt<- function(theta){
            i.star <- input$i.star
            i.star.12 <- ((1+i.star)^(1/12)) -1
            r0<- part3data$i12
            r1<- part3data$rt_s
            r2 <- part3data$rt_h
            r3 <-part3data$rt_k
            r4 <- part3data$rt_g
            #r0 <- expdata()$S
            #r1 <- expdata()$rt.1
            #r2 <- expdata()$rt.2
            #r3 <- expdata()$rt.3
            #r4 <- expdata()$rt.4
            rp <- theta[1]*r0 + theta[2]*r1 +theta[3]*r2 +theta[4]*r3 + theta[5]*r4
            rp.bar <- sum(rp)/(N*12)
            sp <- sqrt(sum((rp-rp.bar)^2)/((N*12)-1))
            Z <- -(rp.bar - i.star.12)/sp
            return(Z)
          }
          
          
          result1 <- gosolnp(pars=NULL, fixed= NULL ,opt, eqfun=eqn, eqB=c(1,input$upper), LB=c(0,0,0,0,0),UB = c(1,1,1,1,1))
          iter <- 1+result1$outer.iter
          sharpe <- result1$values[iter]
          sharpe <- -sharpe
          r2 <- as.numeric(c(t(as.vector(result1$pars)),sharpe))
          r2 <- t(as.data.frame(r2))
          colnames(r2) <- c("Deposit","Samsung","Hyundai","KT&G","Gas","Sharpe ratio")
          return(r2)
        }
        else if (input$equals2 == "upper2" | input$equals1 == "lower2"){
          eqn <- function(theta){
            z1 = sum(theta)
            return(z1)
          }
          ineqn <- function(theta){
            z2 = input$t0*theta[1] +input$t1* theta[2] + input$t2*theta[3] + input$t3*theta[4] + input$t4*theta[5]
            return(z2)
          }
          
          opt<- function(theta){
            i.star <- input$i.star
            i.star.12 <- ((1+i.star)^(1/12)) -1
            r0<- part3data$i12
            r1<- part3data$rt_s
            r2 <- part3data$rt_h
            r3 <-part3data$rt_k
            r4 <- part3data$rt_g
            #r0 <- expdata()$S
            #r1 <- expdata()$rt.1
            #r2 <- expdata()$rt.2
            #r3 <- expdata()$rt.3
            #r4 <- expdata()$rt.4
            rp <- theta[1]*r0 + theta[2]*r1 +theta[3]*r2 +theta[4]*r3 + theta[5]*r4
            rp.bar <- sum(rp)/(N*12)
            sp <- sqrt(sum((rp-rp.bar)^2)/((N*12)-1))
            Z <- -(rp.bar - i.star.12)/sp
            return(Z)
          }
          
          
          result1 <- gosolnp(pars=NULL, fixed= NULL ,opt, eqfun=eqn, eqB=1,ineqfun = ineqn,ineqLB = input$lower,ineqUB = input$upper, LB=c(0,0,0,0,0),UB = c(1,1,1,1,1))
          iter <- 1+result1$outer.iter
          sharpe <- result1$values[iter]
          sharpe <- -sharpe
          r2 <- as.numeric(c(t(as.vector(result1$pars)),sharpe))
          r2 <- t(as.data.frame(r2))
          colnames(r2) <- c("Deposit","Samsung","Hyundai","KT&G","Gas","Sharpe ratio")
          return(r2)
        }
      }
    }
    
  })
  
  vtgraph <- reactive({
    # ##colname ?????? : m, S(???????????????????????????),rt(????????????),S.1(????????????),rt.1(???????????????),S.2(????????????),rt.2(???????????????),,rt.4??????
    M <- input$t *12
    rebal_opt<- function(theta){
      V0 <- input$V0
      Vt <- NULL
      rp <- NULL
      M <- input$t *12
      for (m in 1:M){
        r0<- expdata()$S[m]
        r1<- expdata()$rt.1[m]
        r2 <-expdata()$rt.2[m]
        r3 <- expdata()$rt.3[m]
        r4 <- expdata()$rt.4[m]
        rp[m] <- theta[1]*r0 + theta[2]*r1 +theta[3]*r2 +theta[4]*r3 + theta[5]*r4
        Vt[m] <- V0*prod((1+rp)[1:m])
      }
      return(Vt)
    }
    
    theta.hat <- findtheta()[-6] ##?????????????????? ??????
    M = input$t *12
    data3 <- data.frame("m" = seq(1,M,by =1),"eVt" =  rebal_opt(theta.hat))
    return(data3)
    #    ggplot(data=data333, aes(x=m, y=eVt)) + geom_line(color = "dark blue",size = 1) + ggtitle("Expected Vt plot")
    #     return(ggplot(data=data333, aes(x=m, y=eVt)) + geom_line(color = "dark blue",size = 1) + ggtitle("Expected Vt plot")
    # + labs(x = "m", y = "Vt") + theme(plot.title = element_text(color="black",size=14,face= "bold")))
    #     
  })
  
  vtgraph2 <- reactive({
    
    datag <- vtgraph()
    return(ggplot(data=vtgraph(), aes(x=m/12, y=eVt)) + geom_line(color = "dark blue",size = 1) + ggtitle("Expected Vt plot")+ labs(x = "m", y = "Vt") + geom_vline(xintercept = datag[datag$eVt == max(datag$eVt),][,1]/12,color="red"))
  })
  
  
  showdata <- reactive({
    return(expdata())
  })
  
  
  #   
  #   barchart <- reactive({
  #     data3 <- data.frame("SorP"=c(rep("S.ratio",4),rep("pf.ratio",4)),"ratio"=c(forchart(),calcul.options()$pf.ratio),"maturity"= factor(rep(c("1/4","1/2","3/4","1"),2),levels = c("1/4","1/2","3/4","1")))
  #     return(ggplot(data= data3, aes(x=maturity,y = ratio,fill= SorP)) + geom_bar(stat= "identity",position=position_dodge(),colour="black"))  })
  #   output$plot3 <- renderPlot({
  #     barchart()
  #   })
  #   plotplot <- reactive({
  #     data2 <- data.frame("stock_price" = calcul.options()$S, "t" = seq(1,length(calcul.options()$S)))
  #     return(ggplot(data = data2, aes(x=t/250, y = stock_price)) + geom_line(colour = "slategray3")+
  #              theme_minimal() +xlab("time") + ylab("Stock price"))
  #   })
  #   optionPlot <- reactive({
  #     S0 <- input$K
  #     xx <- seq(from = S0 * 0.5 + 1, to =  S0 * 1.5, by = S0 * 0.01 )
  #     yC <- pmax(xx - S0, 0); yP <- pmax(S0 - xx, 0)
  #     optiondata <- data.frame(x = xx, yc = yC, yp = yP)
  #     
  #     if(input$type=="call"){
  #       
  #       return(ggplot(data=optiondata,aes(x=x, y=yc))+geom_line()+theme_minimal() +xlab("Stock price") + ylab("payoff"))
  #     } else if(input$type == "put") {
  #       return(ggplot(data=optiondata,aes(x=x, y=yp))+geom_line()+theme_minimal() +xlab("Stock price") + ylab("payoff"))
  #     }
  #   })
  #   
  
  expdata1 <- reactive({
    dff <- expdata()
    M <- input$t * 12
    SS <- c(dff$S.1, dff$S.2, dff$S.3, dff$S.4)
    expdata1 <- data.frame(m = c(seq(1, M, by = 1),seq(1, M, by = 1),seq(1, M, by = 1),seq(1, M, by = 1)),SS=SS, Stock = c(rep("Samsung", M), rep("Hyundai", M), rep("KTnG", M), rep("Gas", M)))
    return(expdata1)
  })
  
  plot11 <- reactive({
    return(ggplot(data = expdata1(), mapping = aes(x = m, y = SS, shape = Stock, colour = Stock)) + geom_point() + geom_line() + facet_grid(facets = Stock ~., scale = "free_y"))
  })
  
  # plot11 <- reactive({
  #   ggplot(data=expdata(), aes(x=m, y = S.1)) +ggtitle("Expected Stock price")+ geom_line(data = expdata(),aes(x =m, y = S.1,colour = "Samsung"))+ geom_line(data = expdata(),aes(x =m, y = S.2,colour = "Hyundai")) +  geom_line(data = expdata(),aes(x =m, y = S.3,colour = "KTnG"))+ geom_line(data = expdata(),aes(x =m, y = S.4,colour = "Gas"))
  #   return(ggplot(data=expdata(), aes(x=m, y = S.1)) + geom_line(data = expdata(),aes(x =m, y = S.1,colour = "Samsung"))+ geom_line(data = expdata(),aes(x =m, y = S.2,colour = "Hyundai")) +  geom_line(data = expdata(),aes(x =m, y = S.3,colour = "KTnG"))+ geom_line(data = expdata(),aes(x =m, y = S.4,colour = "Gas"))
  #          +scale_colour_manual(name="Stock type",values=c(Samsung= "black",Hyundai= "tan2",KTnG= "dark blue",Gas= "limegreen")) + labs(y = "St")
  #   )
  # })
  
  # plot12 <- reactive({
  #   return(ggplot(data=expdata(), aes(x=m, y = S)) +ggtitle("rt(monthly profit ratio)")+geom_line(data= expdata(),aes(x = m, y = S, colour = "r0"))+ geom_line(data = expdata(),aes(x =m, y = rt.1,colour = "Samsung"))+ geom_line(data = expdata(),aes(x =m, y = rt.2,colour = "Hyundai")) +  geom_line(data = expdata(),aes(x =m, y = rt.3,colour = "KTnG"))+ geom_line(data = expdata(),aes(x =m, y = rt.4,colour = "Gas"))
  #          +scale_colour_manual(name="Type",values=c(r0 = "red",Samsung= "black",Hyundai= "tan2",KTnG= "dark blue",Gas= "limegreen")) + labs(y = "rt")
  #   )
  # })
  # 
  
  # 
  # output$plot2 <- renderPlotly({
  #   ggplotly(plot12())
  # })
  output$plot101 <- renderPlotly({
    ggplotly(plot11())
  })
  #   output$option_price <- renderText(paste("Option price is ",trunc(round(calcul.price(),0)),"won."))
  #   output$table1 <- renderDataTable({
  #     datatable(data.frame("maturity"= c("now","plus 3 month","plus 6 month","plus 12 month"),"Normal.call"=calcul.options()$n.1,"Normal.put"=calcul.options()$n.2,"Asian.call"=calcul.options()$a.1,"Asian.put"=calcul.options()$a.2)) %>%
  #       formatRound(c("Normal.call","Normal.put","Asian.call","Asian.put"),digits = 0)})
  #   output$text1 <- renderText("< Expected Profit by your Call & Put option Portfolio Choice >")
  #   output$table2 <- renderDataTable({
  #     datatable(data.frame("maturity"= c("1/4","1/2","3/4","1"),"Total.Invest"=calcul.options()$pf.total,"Net.Profit"=calcul.options()$pf.profit,
  #                          "Profit.Ratio" = calcul.options()$pf.ratio)) %>%
  #       formatRound(c("Total.Invest","Net.Profit","Profit.Ratio"), digits = 0)
  #       })
  # }
  output$table101 <- renderTable({
    findtheta()
  })
  output$plot102 <- renderPlotly({
    ggplotly(vtgraph2())
  })
  # output$table2 <- renderTable({
  #   expdata()
  # })
  # 
  output$table987 <- renderTable({
    forsigma()
  })
}



shinyApp(ui=ui, server=server)



