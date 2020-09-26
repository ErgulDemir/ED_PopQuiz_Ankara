library(shiny)

ui <- fluidPage(
  titlePanel("PopQuiz: Ankaralı Olmak"),
  strong(em("Açıklama: Toplam 10 adet çoktan seçmeli soru göreceksiniz. 
     Yanıtlarınızı, sağ taraftaki menüden seçerek işaretleyebilirsiniz.")),
  hr(),

  tabsetPanel(type = "pills",
    
    tabPanel(
      strong("Sorular"),

      fluidRow(
        style = "height:600px; background-color: lightblue;",
        
        tags$head(tags$style(type = "text/css", "label.control-label,
                .selectize-control.single{display: table-cell; 
                text-align: center; vertical-align: middle; }
                .form-group { display: table-row; }")),

        column(9, 
          h4(strong(textOutput("text"))),
          imageOutput("image")
        ),

        column(3, 
          h4(strong("YANIT ANAHTARI")),
          h4(
            hr(),
            radioButtons("rb1", "1 :", c("A"=1, "B"=2, "C"=3, "D"=4), 
              selected = FALSE, inline = TRUE),
            radioButtons("rb2", "2 :", c("A"=1, "B"=2, "C"=3, "D"=4), 
              selected = FALSE, inline = TRUE),
            radioButtons("rb3", "3 :", c("A"=1, "B"=2, "C"=3, "D"=4), 
              selected = FALSE, inline = TRUE),
            radioButtons("rb4", "4 :", c("A"=1, "B"=2, "C"=3, "D"=4), 
              selected = FALSE, inline = TRUE),
            radioButtons("rb5", "5 :", c("A"=1, "B"=2, "C"=3, "D"=4), 
              selected = FALSE, inline = TRUE),
            radioButtons("rb6", "6 :", c("A"=1, "B"=2, "C"=3, "D"=4), 
              selected = FALSE, inline = TRUE),
            radioButtons("rb7", "7 :", c("A"=1, "B"=2, "C"=3, "D"=4), 
              selected = FALSE, inline = TRUE),
            radioButtons("rb8", "8 :", c("A"=1, "B"=2, "C"=3, "D"=4), 
              selected = FALSE, inline = TRUE),
            radioButtons("rb9", "9 :", c("A"=1, "B"=2, "C"=3, "D"=4), 
              selected = FALSE, inline = TRUE),
            radioButtons("rb10", "10 :", c("A"=1, "B"=2, "C"=3, "D"=4), 
              selected = FALSE, inline = TRUE)
            )
          )
        ),

        fluidRow(

          column(2,
            br(),
            actionButton("turn", strong("< Önceki soru"), 
                class = "btn-primary"),
            hr()
          ),

          column(3,

            tags$head(tags$style(HTML("
              .selectize-input {
                height: 10px;
                 width: 120px;
                 font-size: 10pt;
                 padding-top: 4px;
                 border: 2px solid navy; 
              }"))
            ),

            br(),
            selectInput("quest", h4("Soru seç :"), choices = list(
              "Soru 1" = 1, "Soru 2" = 2, "Soru 3" = 3, "Soru 4" = 4,
              "Soru 5" = 5, "Soru 6" = 6, "Soru 7" = 7, "Soru 8" = 8,
              "Soru 9" = 9, "Soru 10" = 10)),
             hr()
          ),
    
          column(4,
            br(),
            actionButton("go", strong("Sonraki soru >"), 
              class = "btn-primary"),
            hr()
          ),

          column(1,
            br(),
            actionButton("submit", strong("Testi Bitir"),   
              class = "btn-success"),
            hr()
          ),

          column(2,
            br(),
            actionButton("renew", strong("Yeniden başlat"), 
              class = "btn-secondary"),
            hr()
          )
        )
      ),
   

    tabPanel(
      strong("Sonucunuz"),

      fluidRow(

        column(4,
          h4(
           tableOutput("ans")
          )
        ),

        column(8,
           verbatimTextOutput("yorum", placeholder = T),
           br(),
           verbatimTextOutput("res", placeholder = T)
        )
      )
    )
  )
     
)


server <- function(input, output, session){
  
  minVal = 1
  maxVal = 10
  cvp = c("D", "C", "A", "A", "B", "D", "B", "A", "C", "C")

  val <- reactiveValues(i = 0)

  observe({
    input$quest
    isolate({
      val$i = as.numeric(input$quest)
    })
  })

  observe({
    input$go 
    isolate({ 
      val$i = val$i + 1 
      if(val$i > maxVal) val$i = maxVal 
    })
  })

  observe({
    input$turn 
    isolate({ 
      val$i = val$i - 1 
      if(val$i < minVal) val$i = minVal 
    })
  })

  observe({
    input$go
    isolate({ 
      updateSelectInput(session, "quest", "Soru seç :", choices = list(
              "Soru 1" = 1, "Soru 2" = 2, "Soru 3" = 3, "Soru 4" = 4,
              "Soru 5" = 5, "Soru 6" = 6, "Soru 7" = 7, "Soru 8" = 8,
              "Soru 9" = 9, "Soru 10" = 10), selected = val$i)
    })
  })

  observe({
    input$turn
    isolate({ 
      updateSelectInput(session, "quest", "Soru seç :", choices = list(
              "Soru 1" = 1, "Soru 2" = 2, "Soru 3" = 3, "Soru 4" = 4,
              "Soru 5" = 5, "Soru 6" = 6, "Soru 7" = 7, "Soru 8" = 8,
              "Soru 9" = 9, "Soru 10" = 10), selected = val$i)
    })
  })
  
  output$text <- renderText({
    paste("Soru", val$i)
  }) 

  output$image <- renderImage({
    file <- paste0("www/ank_", val$i, ".JPG")
    outputArgs = list(src = file)
  }, deleteFile = FALSE)
  
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = strong("Testi tamamladınız!"),
      strong(em("Sonuçlarınızı sol-üstte 'Sonucunuz' panelinden görebilirsiniz.")),
      footer = tagList( modalButton(strong("Tamam"))), 
      size = "s"
    ))
   })

  rd <- reactiveValues(df = NULL)
  observe({
    input$submit
    isolate({
    ch <- c("A", "B", "C", "D", "Boş")
    answers <- c("1" = ch[as.numeric(input$rb1)], "2" = ch[as.numeric(input$rb2)], 
        "3" = ch[as.numeric(input$rb3)], "4" = ch[as.numeric(input$rb4)], 
        "5" = ch[as.numeric(input$rb5)], "6" = ch[as.numeric(input$rb6)], 
        "7" = ch[as.numeric(input$rb7)], "8" = ch[as.numeric(input$rb8)], 
        "9" = ch[as.numeric(input$rb9)], "10" = ch[as.numeric(input$rb10)])
    data1 <- data.frame("Soru" = names(answers), "Yanıtınız" = answers)
      
    bs <- setdiff(as.character(1:maxVal), names(answers))
    yt <- rep("Boş", length(bs))
    data2 <- data.frame("Soru" = bs, "Yanıtınız" = yt)

    data <- rbind(data1, data2)
    data$Soru <- as.integer(data$Soru)
    data <- data[order(data$Soru),]

    sonuç <- character()
    for(i in 1:maxVal){
      if(cvp[i] == data[,2][i]){ sonuç[i] <- "Doğru"
      } else if(data[,2][i] == "Boş"){ sonuç[i] <- "Boş"
      } else{ sonuç[i] <- "Yanlış"}
    }
    data$Sonuç <- sonuç
    })
    rd$df <- data
  })

  output$ans <- renderTable({
    rd$df
  })
 


  ist <- reactiveValues(d = NULL, y = NULL, b = NULL, not = NULL)
  observeEvent(input$submit,{
      ist$d <- sum(rd$df$Sonuç == "Doğru")
      ist$y <- sum(rd$df$Sonuç == "Yanlış")
      ist$b <- sum(rd$df$Sonuç == "Boş")
      ist$not <- round(ist$d / (ist$d + ist$y + ist$b) * 100)
  })

  output$yorum <- renderText({
    input$submit
    isolate({
      if(input$submit == FALSE){paste("SONUÇ:...")
      } else{paste("SONUÇ:", ist$d, "doğru yanıtınız,", ist$y, "yanlış yanıtınız var.", 
          ist$b, "soruyu işaretlemediniz. Notunuz 100 üzerinden", ist$not)}   
    })
  })

  output$res <- renderText({
    input$submit
    isolate({
      if(input$submit == FALSE){"YORUM:..."  
      } else if(ist$not >= 90) {"YORUM: Angaralısınız"
      } else if(ist$not >= 80 & ist$not < 90) {"YORUM: Angaralı olmasa da Ankaralısınız diyebiliriz."
      } else if(ist$not >= 60 & ist$not < 80) {"YORUM: Hafiften bir Ankara'yı zamanla sevdim durumu var gibi."
      } else if(ist$not >= 40 & ist$not < 60) {"YORUM: Ankara'ya geçerken yolunuz düşmüş gibi, ha Ankara ha başka bir şehir, farketmez modundasınız sanki."
      } else if(ist$not >= 20 & ist$not < 40) {"YORUM: Fırsatını bulursam başka şehre giderim modundasınız gibi."
      } else {"YORUM: Ankara denilince biraz içi sıkılıp İzmir ya da İstanbul aklına gelenlerdensiniz gibi."} 
    })
  }) 

  observeEvent(input$renew,{
      session$reload()
  })

}

shinyApp(ui, server)
