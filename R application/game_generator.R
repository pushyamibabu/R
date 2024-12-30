# Required Libraries
library(shiny)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("Trivia Game Generator"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Create Quiz",
          textInput("quiz_name", "Quiz Name:"),
          actionButton("add_question", "Add Question"),
          uiOutput("questions_ui"),
          actionButton("save_quiz", "Save Quiz")
        ),
        tabPanel(
          "Play Quiz",
          selectInput("select_quiz", "Select Quiz:", choices = NULL),
          actionButton("start_quiz", "Start Quiz"),
          uiOutput("quiz_ui"),
          actionButton("submit_answers", "Submit Answers"),
          verbatimTextOutput("quiz_score")
        ),
        tabPanel(
          "Leaderboard",
          dataTableOutput("leaderboard_table")
        )
      )
    ),
    mainPanel(
      h4("Instructions"),
      p("Create a custom quiz, play it, and track scores on the leaderboard.")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store quizzes and scores
  values <- reactiveValues(
    quizzes = list(),
    current_quiz = NULL,
    leaderboard = data.frame(Player = character(), Quiz = character(), Score = numeric(), stringsAsFactors = FALSE)
  )
  
  # UI for adding questions dynamically
  output$questions_ui <- renderUI({
    req(input$add_question)
    n <- input$add_question
    lapply(1:n, function(i) {
      tagList(
        textInput(paste0("question_", i), paste("Question", i)),
        textInput(paste0("answer_", i), "Answer"),
        textInput(paste0("option1_", i), "Option 1"),
        textInput(paste0("option2_", i), "Option 2"),
        textInput(paste0("option3_", i), "Option 3")
      )
    })
  })
  
  # Save quiz
  observeEvent(input$save_quiz, {
    req(input$quiz_name)
    n <- input$add_question
    questions <- lapply(1:n, function(i) {
      list(
        question = input[[paste0("question_", i)]],
        answer = input[[paste0("answer_", i)]],
        options = c(input[[paste0("answer_", i)]], 
                    input[[paste0("option1_", i)]], 
                    input[[paste0("option2_", i)]], 
                    input[[paste0("option3_", i)]])
      )
    })
    values$quizzes[[input$quiz_name]] <- questions
    
    # Update quiz dropdown
    updateSelectInput(session, "select_quiz", choices = names(values$quizzes))
    showNotification("Quiz saved successfully!", type = "message")
  })
  
  # Play quiz
  observeEvent(input$start_quiz, {
    req(input$select_quiz)
    quiz <- values$quizzes[[input$select_quiz]]
    values$current_quiz <- quiz
    
    output$quiz_ui <- renderUI({
      n <- length(quiz)
      lapply(1:n, function(i) {
        question <- quiz[[i]]$question
        options <- sample(quiz[[i]]$options) # Shuffle options
        radioButtons(paste0("quiz_answer_", i), question, choices = options)
      })
    })
  })
  
  # Submit answers
  observeEvent(input$submit_answers, {
    req(values$current_quiz)
    quiz <- values$current_quiz
    n <- length(quiz)
    correct <- 0
    
    for (i in 1:n) {
      selected <- input[[paste0("quiz_answer_", i)]]
      if (!is.null(selected) && selected == quiz[[i]]$answer) {
        correct <- correct + 1
      }
    }
    
    score <- round((correct / n) * 100, 2)
    output$quiz_score <- renderText({
      paste("Your score:", score, "%")
    })
    
    # Save to leaderboard
    values$leaderboard <- rbind(
      values$leaderboard,
      data.frame(Player = Sys.getenv("USERNAME"), Quiz = input$select_quiz, Score = score, stringsAsFactors = FALSE)
    )
  })
  
  # Leaderboard table
  output$leaderboard_table <- renderDataTable({
    values$leaderboard
  })
}

# Run the app
shinyApp(ui, server)