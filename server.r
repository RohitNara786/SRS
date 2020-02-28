library(RMySQL)
library(DBI)
library(plotly)
library(ggplot2)
library(shiny)
library(data.table)
library(plyr)

server<- function(input,output,session){
  
  autoInvalidate <- reactiveTimer(30000)
  observe({
    autoInvalidate()
    #---------------------------------------
    output$scheduler<-renderValueBox({
      valueBox(value = tags$p("Scheduler", style = "font-size: 60%;"), subtitle=sum(error_status$Fail_count), icon = icon("clock"), color = "yellow", width = 4,
               href = NULL)
    })
    output$event<-renderValueBox({
      valueBox(value = tags$p("Event", style = "font-size: 60%;"), subtitle=sum(error_statusAPI$Fail_count), icon = icon("area-chart"), color = "yellow", width = 4,
               href = NULL)
    })
    output$log<-renderValueBox({
      valueBox(value = tags$p("Log", style = "font-size: 60%;"), subtitle="log data", icon = icon("bug"), color = "yellow", width = 4,
               href = NULL)
    })
   
    
    #--------------schduler graph-----------------------
    output$plot1_f <- renderPlotly({
      p <- plot_ly(jobcard) %>%
        add_trace(x = ~Start_Date, y = ~Time_taken,key=paste('H1'), type = 'bar', name = 'Jobcard',text=paste("Scheduler Type:",jobcard$type,"<br>Time Taken:",jobcard$Time_taken),
                  hoverinfo = 'text', marker = list(color = '#C9EFF9')) %>%
        add_trace(retail, x = ~retail$Start_Date, y =~retail$Time_taken,key=paste('H1'), type = 'bar',name = 'Retail', text=paste("Scheduler Type:",retail$type,"<br>Time Taken:",retail$Time_taken),
                  hoverinfo = 'text',
                  marker = list(color = '#F7C19B')) %>%
        add_trace(updatecalls, x = ~updatecalls$Start_Date, y = ~updatecalls$Time_taken,key=paste('H1'),text=paste("Scheduler Type:",updatecalls$type,"<br>Time Taken:",updatecalls$Time_taken),
                  hoverinfo = 'text', type = 'bar',name = 'UpdateCAlls',
                  marker = list(color = '#67E5F7')) %>%
        add_trace(booking, x = ~booking$Start_Date, y = ~booking$Time_taken,key=paste('H1'),text=paste("Scheduler Type:",booking$type,"<br>Time Taken:",booking$Time_taken),
                  hoverinfo = 'text', type = 'bar',name = 'Booking',
                  marker = list(color = '#FAFAAF')) %>%
        layout(xaxis = list(title = "Start date"),
               yaxis = list(title = 'time taken'))
    })
    observeEvent(event_data("plotly_click"),{
      aa <- event_data("plotly_click")
      showModal(modalDialog(size = 'l',
                            if(aa$key=='H1')
                            output$tab <- DT::renderDataTable(Failure_schduler,rownames=FALSE)
                            else if(aa$key=='H2')
                              output$tab <- DT::renderDataTable(Failure_API,rownames=FALSE)))
      })
      
      
      
      
      
      
    output$plot2_f <- renderPlotly({
      plot_ly(jobcard_count, x = ~jobcard_count$StartDate, y = ~jobcard_count$count, type = 'scatter', mode = 'lines', name = 'Jobcard Count', 
              line = list(color = '#C9EFF9')) %>%
        add_trace(retail_count, x = ~retail_count$StartDate, y = ~retail_count$count, type = 'scatter', mode = 'lines', name = 'RetailCount', 
                  line = list(color = '#F7C19B')) %>%
        add_trace(updatecalls_count, x = ~updatecalls_count$StartDate, y = ~updatecalls_count$count, type = 'scatter', mode = 'lines', name = 'UpdateCalls Count',
                  line = list(color = '#67E5F7')) %>%
        add_trace(booking_count, x = ~booking_count$StartDate, y = ~booking_count$count, type = 'scatter', mode = 'lines', name = 'Booking Count',
                  line = list(color = '#FAFAAF')) %>%
        layout(xaxis = list(title = 'Start Date'),
               yaxis = list(title = 'Scheduler run count'))
      
    })
    
    output$plot3_f <- renderPlotly({
      plot_ly(error_status, x = error_status$type, y = error_status$succ_count,type = 'bar',name = 'Success') %>%
          add_trace(y = error_status$Fail_count, name = 'Failed')%>%
          layout(xaxis = list(title = "status count"),barmode = 'group',
                 yaxis = list(title = "no of status count"))
      
    })
    #-----------------------api-------------------
    output$plot4_f <- renderPlotly({
      plot_ly(login) %>%
      add_trace(x = ~Start_Date, y = ~Time_takenAPI,key=paste('H2'), type = 'bar', name = 'login',text=paste("Scheduler Type:",login$apiName,"<br>Time Taken:",login$Time_takenAPI),
                hoverinfo = 'text', marker = list(color = '#C9EFF9')) %>%
      add_trace(logout, x = ~logout$Start_Date, y =~logout$Time_takenAPI,key=paste('H2'), type = 'bar',name = 'logout', text=paste("Scheduler Type:",logout$apiName,"<br>Time Taken:",logout$Time_takenAPI),
                hoverinfo = 'text',marker = list(color = '#F7C19B')) %>%
        layout(xaxis = list(title = "Start date"),
               yaxis = list(title = 'time taken in min.'))
    })
    
    output$plot5_f <- renderPlotly({
      plot_ly(login_count, x = ~login_count$StartDate, y = ~login_count$count, type = 'scatter', mode = 'lines', name = 'Login Count', 
              line = list(color = '#C9EFF9')) %>%
        add_trace(logout_count, x = ~logout_count$StartDate, y = ~logout_count$count, type = 'scatter', mode = 'lines', name = 'Logout Count', 
                  line = list(color = '#F7C19B')) %>%
        layout(xaxis = list(title = 'Start Date'),
               yaxis = list(title = 'API run count'))
    })
    
    output$plot6_f <- renderPlotly({
      plot_ly(error_statusAPI, x = error_statusAPI$apiName, y = error_statusAPI$succ_count,type = 'bar',name = 'Success') %>%
        add_trace(y = error_statusAPI$Fail_count, name = 'Failed')%>%
        layout(xaxis = list(title = "status count"),barmode = 'group',
               yaxis = list(title = "no of status count"))
      
    })
    
    
    
#------------------------------
    output$plot1 <- renderPlotly({
      plot_ly(Time_taken_jobacard, x = ~Time_taken_jobacard$StartDate, y = ~Time_taken_jobacard$time_taken,text=paste("count:",Time_taken_jobacard$counts,"run_time:",Time_taken_jobacard$time_taken,"Start_Date:",Time_taken_jobacard$StartDate), type = 'bar',
              marker = list(color = '#C9EFF9')) %>%
        layout(xaxis = list(title = "Jobcard "),
               yaxis = list(title = "Time in min."))
    })
    
    output$plot2 <- renderPlotly({
      plot_ly(Time_taken_Retail, x = ~Time_taken_Retail$StartDate, y = ~Time_taken_Retail$time_taken,text=paste("count:",Time_taken_Retail$counts,"run_time:",Time_taken_Retail$time_taken,"Start_Date:",Time_taken_Retail$StartDate), type = 'bar',
              marker = list(color = '#F7C19B')) %>%
        layout(xaxis = list(title = "Retail"),
               yaxis = list(title = "Time in min. "))
      
    })
    
    output$plot3 <- renderPlotly({
      plot_ly(Time_taken_UpdateCalls, x = ~Time_taken_UpdateCalls$StartDate, y = ~Time_taken_UpdateCalls$time_taken,text=paste("count:",Time_taken_UpdateCalls$counts,"run_time:",Time_taken_UpdateCalls$time_taken,"Start_Date:",Time_taken_UpdateCalls$StartDate), type = 'bar',
              marker = list(color = '#67E5F7')) %>%
        layout(xaxis = list(title = "Updatecalls"),
               yaxis = list(title = "Time in min. "))
      
    })
    
    output$plot4 <- renderPlotly({
      plot_ly(Time_taken_Booking, x = ~Time_taken_Booking$StartDate, y = ~Time_taken_Booking$time_taken,text=paste("count:",Time_taken_Booking$counts,"run_time:",Time_taken_Booking$time_taken,"Start_Date:",Time_taken_Booking$StartDate), type = 'bar',
              marker = list(color = '#FAFAAF')) %>%
        layout(xaxis = list(title = "Booking"),
               yaxis = list(title = "Time in min. "))
      
    })
    
    output$plot5 <- renderPlotly({
      plot_ly(success_Failure_schduler, x = success_Failure_schduler$type, y = success_Failure_schduler$error_count_success,type = 'bar',name = 'Success') %>%
        add_trace(y = success_Failure_schduler$error_count_fail, name = 'Failed')%>%
        layout(xaxis = list(title = "status_count"),barmode = 'group',
               yaxis = list(title = "no of status_count"))
    })
    
    output$plot6 <- renderPlotly({
      plot_ly(no_of_schduler_called, x = ~no_of_schduler_called$type, y = ~no_of_schduler_called$run_count,text=paste("count:",no_of_schduler_called$run_count,"run_count:",no_of_schduler_called$run_count), type = 'bar',
              marker = list(color = '#C9EFF9')) %>%
        layout(title = "no_of_schduler_called",
               yaxis = list(title = "run_count "))
      
    })
    
    #-----------------ON CURRENT DATE---------------------
    
    output$plot7 <- renderPlotly({
      plot_ly(Time_taken_jobacard_cd, x = ~Time_taken_jobacard_cd$startTime, y = ~Time_taken_jobacard_cd$time_taken,text=paste("time_taken:",Time_taken_jobacard_cd$time_taken,"start_time:",Time_taken_jobacard_cd$startTime), type = 'bar',
              marker = list(color = '#C9EFF9')) %>%
        layout(xaxis = list(title = "Jobcard "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot8 <- renderPlotly({
      plot_ly(Time_taken_Retail_cd, x = ~Time_taken_Retail_cd$startTime, y = ~Time_taken_Retail_cd$time_taken,text=paste("run_time:",Time_taken_Retail_cd$time_taken,"start_time:",Time_taken_Retail_cd$startTime), type = 'bar',
              marker = list(color = '#F7C19B')) %>%
        layout(xaxis = list(title = "Retail "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot9 <- renderPlotly({
      plot_ly(Time_taken_UpdateCalls_cd, x = ~Time_taken_UpdateCalls_cd$startTime, y = ~Time_taken_UpdateCalls_cd$time_taken,text=paste("run_time:",Time_taken_UpdateCalls_cd$time_taken,"start_time:",Time_taken_UpdateCalls_cd$startTime), type = 'bar',
              marker = list(color = '#67E5F7')) %>%
        layout(xaxis = list(title = "UpdateCalls "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot10 <- renderPlotly({
      plot_ly(Time_taken_Booking_cd, x = ~Time_taken_Booking_cd$startTime, y = ~Time_taken_Booking_cd$time_taken,text=paste("run_time:",Time_taken_Booking_cd$time_taken,"start_time:",Time_taken_Booking_cd$startTime), type = 'bar',
              marker = list(color = '#FAFAAF')) %>%
        layout(xaxis = list(title = "Booking "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot11 <- renderPlotly({
      plot_ly(success_Failure_schduler_cd, x = success_Failure_schduler_cd$type, y = success_Failure_schduler_cd$error_count_Success,type = 'bar',name = 'Success') %>%
        add_trace(y = success_Failure_schduler_cd$error_count_faile3, name = 'Failed')%>%
        layout(xaxis = list(title = "status_count"),barmode = 'group',
               yaxis = list(title = "no of status_count"))
    })
    
    output$plot12 <- renderPlotly({
      plot_ly(no_of_schduler_called_cd, x = ~no_of_schduler_called_cd$type, y = ~no_of_schduler_called_cd$run_count,text=paste("run_count:",no_of_schduler_called_cd$run_count), type = 'bar',
              marker = list(color = '#FAFAAF')) %>%
        layout(xaxis = list(title = "Type of scheduler"),
               yaxis = list(title = "run_count "))
      
    })
    
    #----------------------------API GRAPHS------------------
    
    output$plot15 <- renderPlotly({
      plot_ly(Time_taken_login, x = ~Time_taken_login$StartDate, y = ~Time_taken_login$time_taken,text=paste("run_time:",Time_taken_login$time_taken,"Start_Date:",Time_taken_login$StartDate), type = 'bar',
              marker = list(color = '#C9EFF9')) %>%
        layout(xaxis = list(title = "LOGIN "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot16 <- renderPlotly({
      plot_ly(Time_taken_logout, x = ~Time_taken_logout$StartDate, y = ~Time_taken_logout$time_taken,text=paste("run_time:",Time_taken_logout$time_taken,"Start_Date:",Time_taken_logout$StartDate), type = 'bar',
              marker = list(color = '#F7C19B')) %>%
        layout(xaxis = list(title = "LOGOUT "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot17 <- renderPlotly({
      plot_ly(success_Failure_API, x = success_Failure_API$apiName, y = success_Failure_API$error_count_Success,type = 'bar',name = 'Success') %>%
        add_trace(y = success_Failure_API$error_count_fail, name = 'Failed')%>%
        layout(xaxis = list(title = "status_count"),barmode = 'group',
               yaxis = list(title = "no of status_count"))
    })
    
    output$plot18 <- renderPlotly({
      plot_ly(no_of_API_called, x = ~no_of_API_called$apiName, y = ~no_of_API_called$run_count,text=paste("run_count:",no_of_API_called$run_count), type = 'bar',
              marker = list(color = '')) %>%
        layout(xaxis = list(title = "Event Type"),
               yaxis = list(title = "run_count "))
      
    })
    
    #-----------------ON CURRENT DATE----------------------
    
    output$plot19 <- renderPlotly({
      plot_ly(Time_taken_login_cd, x = ~Time_taken_login_cd$StartDate, y = ~Time_taken_login_cd$time_taken,text=paste("run_time:",Time_taken_login_cd$time_taken,"Start_Date:",Time_taken_login_cd$StartDate), type = 'bar',
              marker = list(color = '#C9EFF9')) %>%
        layout(xaxis = list(title = "LOGIN "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot20 <- renderPlotly({
      plot_ly(Time_taken_logout_cd, x = ~Time_taken_logout_cd$StartDate, y = ~Time_taken_logout_cd$time_taken,text=paste("run_time:",Time_taken_logout_cd$time_taken,"Start_Date:",Time_taken_logout_cd$StartDate), type = 'bar',
              marker = list(color = '#F7C19B')) %>%
        layout(xaxis = list(title = "LOGOUT "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot21 <- renderPlotly({
      plot_ly(success_Failure_API_cd, x = success_Failure_API_cd$apiName, y = success_Failure_API_cd$error_count_Success,type = 'bar',name = 'Success') %>%
        add_trace(y = success_Failure_API_cd$error_count_fail, name = 'Failed')%>%
        layout(xaxis = list(title = "status_count"),barmode = 'group',
               yaxis = list(title = "no of status_count"))
    })
    
    output$plot22 <- renderPlotly({
      plot_ly(no_of_API_called_cd, x = ~no_of_API_called_cd$apiName, y = ~no_of_API_called_cd$run_count,text=paste("run_count:",no_of_API_called_cd$run_count), type = 'bar',
              marker = list(color = '#C9EFF9')) %>%
        layout(xaxis = list(title = "event type"),
               yaxis = list(title = "run_count "))
      
    })
    #-----------graph plot on date ranges of Schduler--------------------------------
    
    output$plot27 <- renderPlotly({
      plot_ly(daterange_Jobcard(), x = daterange_Jobcard()$StartDate, y = daterange_Jobcard()$time_taken,text=paste("count:",daterange_Jobcard()$counts,"run_time:",daterange_Jobcard()$time_taken,"Start_Date:",daterange_Jobcard()$StartDate), type = 'bar',
              marker = list(color = '#C9EFF9')) %>%
        layout(xaxis = list(title = "Jobcard "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot28 <- renderPlotly({
      plot_ly(daterange_UpdateCalls(), x = daterange_UpdateCalls()$StartDate, y = daterange_UpdateCalls()$time_taken,text=paste("count:",daterange_UpdateCalls()$counts,"run_time:",daterange_UpdateCalls()$time_taken,"Start_Date:",daterange_UpdateCalls()$StartDate), type = 'bar',
              marker = list(color = '#F7C19B')) %>%
        layout(xaxis = list(title = "UpdateCalls "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot29 <- renderPlotly({
      plot_ly(daterange_Retail(), x = daterange_Retail()$StartDate, y = daterange_Retail()$time_taken,text=paste("count:",daterange_Retail()$counts,"run_time:",daterange_Retail()$time_taken,"Start_Date:",daterange_Retail()$StartDate), type = 'bar',
              marker = list(color = '#67E5F7')) %>%
        layout(xaxis = list(title = "Retail "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot30 <- renderPlotly({
      plot_ly(daterange_Booking(), x = daterange_Booking()$StartDate, y = daterange_Booking()$time_taken,text=paste("count:",daterange_Booking()$counts,"run_time:",daterange_Booking()$time_taken,"Start_Date:",daterange_Booking()$StartDate), type = 'bar',
              marker = list(color = '#FAFAAF')) %>%
        layout(xaxis = list(title = "Booking"),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot31 <- renderPlotly({
      plot_ly(success_Failure_schduler_daterange(), x = success_Failure_schduler_daterange()$type, y = success_Failure_schduler_daterange()$error_count_success,type = 'bar',name = 'Success') %>%
        add_trace(y = success_Failure_schduler_daterange()$error_count_fail, name = 'Failed')%>%
        layout(title = "status count of scheduler",
               xaxis = list(title = "status_count"),barmode = 'group',
               yaxis = list(title = "no of status_count"))
    })
    
    output$plot32 <- renderPlotly({
      plot_ly(run_count_schduler_daterange(), x = run_count_schduler_daterange()$type, y = run_count_schduler_daterange()$run_count,text=paste("count:",run_count_schduler_daterange()$run_count,"run_count:",run_count_schduler_daterange()$run_count), type = 'bar',
              marker = list(color = '#C9EFF9')) %>%
        layout(title = "no of schduler called",
               yaxis = list(title = "run_count "))
      
    })
    
    #-----------graph plot on date ranges of API-------------------------
    output$plot33 <- renderPlotly({
      plot_ly(daterange_login(), x = daterange_login()$StartDate, y = daterange_login()$time_taken,text=paste("run_time:",daterange_login()$time_taken,"Start_Date:",daterange_login()$StartDate), type = 'bar',
              marker = list(color = '#C9EFF9')) %>%
        layout(xaxis = list(title = "LOGIN "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot34 <- renderPlotly({
      plot_ly(daterange_logout(), x = daterange_logout()$StartDate, y = daterange_logout()$time_taken,text=paste("run_time:",daterange_logout()$time_taken,"Start_Date:",daterange_logout()$StartDate), type = 'bar',
              marker = list(color = '#F7C19B')) %>%
        layout(xaxis = list(title = "LOGOUT "),
               yaxis = list(title = "Time in min. "))
    })
    
    output$plot35 <- renderPlotly({
      plot_ly(daterange_error_status_API(), x = daterange_error_status_API()$apiName, y = daterange_error_status_API()$error_count_Success,type = 'bar',name = 'Success') %>%
        add_trace(y = daterange_error_status_API()$error_count_fail, name = 'Failed')%>%
        layout(title = "status count of API",
               xaxis = list(title = "status_count"),barmode = 'group',
               yaxis = list(title = "no of status_count"))
    })
    
    output$plot36 <- renderPlotly({
      plot_ly(daterange_run_count_API(), x = daterange_run_count_API()$apiName, y = daterange_run_count_API()$run_count,text=paste("run_count:",daterange_run_count_API()$run_count), type = 'bar',
              marker = list(color = 'rgba(204,204,204,1)',
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5))) %>%
        layout(xaxis = list(title = "no_of_API_called"),
               yaxis = list(title = "run_count "))
      
    })
  })  
}
