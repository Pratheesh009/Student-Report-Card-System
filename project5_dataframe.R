# Project 5: Data Frames - Student Report Card
# Setup once: install.packages("shiny")   |   Run: click "Run App" in RStudio
library(shiny)

ui <- fluidPage(
  titlePanel("Student Report Card (Data Frame)"),
  sidebarLayout(
    sidebarPanel(
      textInput("sn", "Student name"),
      numericInput("sm", "Maths", 80, 0, 100),
      numericInput("ss", "Science", 80, 0, 100),
      numericInput("se", "English", 80, 0, 100),
      actionButton("sadd", "Add student")
    ),
    mainPanel(tableOutput("stable"), plotOutput("gplot"),
              verbatimTextOutput("gtext"))
  )
)

server <- function(input, output, session) {
  stu <- reactiveVal(data.frame(
    Name    = c("Arun", "Priya", "Karthik", "Divya", "Meena"),
    Maths   = c(85, 92, 67, 78, 95),
    Science = c(90, 88, 72, 85, 91),
    English = c(75, 95, 60, 82, 89),
    stringsAsFactors = FALSE))

  observeEvent(input$sadd, {
    req(nzchar(input$sn))
    stu(rbind(stu(), data.frame(Name = input$sn, Maths = input$sm,
                                Science = input$ss, English = input$se)))
  })

  full <- reactive({
    d <- stu()
    d$Total   <- d$Maths + d$Science + d$English
    d$Average <- round(d$Total / 3, 1)
    d$Grade   <- ifelse(d$Average >= 90, "A", ifelse(d$Average >= 75, "B", "C"))
    d[order(-d$Total), ]
  })

  output$stable <- renderTable(full())

  output$gplot <- renderPlot({
    d <- full()
    barplot(d$Average, names.arg = d$Name, col = "steelblue",
            main = "Average marks", ylim = c(0, 100))
  })

  output$gtext <- renderText({
    d <- full()
    paste0("Topper: ", d$Name[1],
           "\nGrade A students: ", paste(d$Name[d$Grade == "A"], collapse = ", "),
           "\nSubject averages -> Maths: ", round(mean(d$Maths), 1),
           ", Science: ", round(mean(d$Science), 1),
           ", English: ", round(mean(d$English), 1))
  })
}

shinyApp(ui, server)
