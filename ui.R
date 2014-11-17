library(shiny)
library(markdown)

### Layout with navigation bar

shinyUI(navbarPage(title = 'Content-analysis',
   
   ### Page to upload files in copruses
   
   tabPanel('Upload files', 
   
   fluidPage(titlePanel("Upload files"),
   
   # Боковая панель
   
   sidebarLayout(
    sidebarPanel(fileInput('file1', 'Upload files for main corpus', accept=c('text/plain', '.txt'), multiple = T),
     tags$hr(),
     fileInput('file2', 'Upload files for comparison', accept=c('text/plain', '.txt'), multiple = T),
     br()
    ),
    
    # Page with uploaded corpuses
    
    mainPanel(
    fluidRow(
    column(6,h4("Files in main corpus"),tableOutput('filelist1')),
    column(6,h4("Files in extra corpus"),tableOutput('filelist2'))
    ))
   ))),
   
   ### Page with main corpus
   
   tabPanel('Main corpus',
   
   fluidPage(titlePanel("Analysis of main corpus"),
   
   
   sidebarLayout(
    
    # Side Panel
    sidebarPanel(
    checkboxGroupInput("texts1_options", label = h4("Text processing settings"), 
                       choices = list("Convert to lowercase" = "t1", "Remove punctuation" = "t2", 
                                      "Remove numbers" = "t3", "Remove stop-words" = "t4", "Apply stemming" = "t5"),
     selected = list("t1","t2","t3","t4","t5")),
    tags$hr(),
    selectInput("texts1_lang", label = h4("Выберите язык текстов"), 
                choices = list("English" = "english", "Russian" = "russian", "Danish" = "danish", "Dutch" = "dutch", 
                               "Finnish" = "finnish", "French" = "french", "German" = "german", "Hungarian" = "hungarian", 
                               "Italian" = "italian", "Norwegian" = "norwegian", "Portuguese" = "portuguese", "Spanish" = "spanish",
                               "Swedish" = "swedish"), 
     selected = "english"),
    tags$hr(),
    sliderInput("texts1_filter1", label = h4("Filter by the word length"), min = 0, 
     max = 50, value = c(0, 50)),
    tags$hr(),
    sliderInput("texts1_filter2", label = h4("Use only words which mentioned more than in % of documents"), min = 0.1,
     max = 99.9, step = 0.1, value = c(95.5)),
    tags$hr(),
    checkboxGroupInput("texts1_methods", label = h4("Methods"), 
                       choices = list("Histogram of word frequencies" = "a1", "Table of word frequencies" = "a2",
                       "Wordcloud" = "a3", "Relations between the most frequent words" = "a4", 
                       "Build graph" = "a5"),
     selected = list("a1","a2","a3")),
    tags$hr(),
    sliderInput("texts1_filter3", label = h4("Correlation between words in the graph"), 
     min = 0, max = 1, step = 0.01, value = c(0.9)),
    tags$hr(),
    actionButton("texts1_analyze", "Start")
    ),
    
    # Main corpus results
    mainPanel(
    fluidRow(
    column(12,htmlOutput("t0_result1"),plotOutput("t1_result1"),
     dataTableOutput("t2_result1"),plotOutput("t3_result1"),
     plotOutput("t4_result1",height="800px"), plotOutput("t5_result1")
    )
    )
    )
   )
   
   )),
   
   ### Extra corpus page
   
   tabPanel('Extra corpus',
   
   fluidPage(titlePanel("Analysis of extra corpus"),
   # Боковая панель
   
   sidebarLayout(
    
    # Боковая панель
    sidebarPanel(
      checkboxGroupInput("texts2_options", label = h4("Text processing settings"), 
                         choices = list("Convert to lowercase" = "t1", "Remove punctuation" = "t2", 
                                        "Remove numbers" = "t3", "Remove stop-words" = "t4", "Apply stemming" = "t5"),
                         selected = list("t1","t2","t3","t4","t5")),
    tags$hr(),
    selectInput("texts2_lang", label = h4("Choose language"), 
                choices = list("English" = "english", "Russian" = "russian", "Danish" = "danish", "Dutch" = "dutch", 
                               "Finnish" = "finnish", "French" = "french", "German" = "german", "Hungarian" = "hungarian", 
                               "Italian" = "italian", "Norwegian" = "norwegian", "Portuguese" = "portuguese", "Spanish" = "spanish",
                               "Swedish" = "swedish"), 
                selected = "english"),
    tags$hr(),
    sliderInput("texts2_filter1", label = h4("Filter by the word length"), min = 0, 
     max = 50, value = c(0, 50)),
    tags$hr(),
    sliderInput("texts2_filter2", label = h4("Use only words which mentioned more than in % of documents"), min = 0.1,
     max = 99.9, step = 0.1, value = c(95.5)),
    tags$hr(),
    checkboxGroupInput("texts2_methods", label = h4("Methods"), 
     choices = list("Histogram of word frequencies" = "a1", "Table of word frequencies" = "a2",
     "Wordcloud" = "a3", "Relations between the most frequent words" = "a4", 
      "Build graph" = "a5"),
     selected = list("a1","a2","a3")),
    tags$hr(),
    sliderInput("texts2_filter3", label = h4("Correlation between words in the graph"), 
     min = 0, max = 1, step = 0.01, value = c(0.9)),
    tags$hr(),
    actionButton("texts2_analyze", "Start")
    ),
    
    # Extra corpus results
    mainPanel(
    fluidRow(
    column(12,htmlOutput("t0_result2"),plotOutput("t1_result2"),
     dataTableOutput("t2_result2"),plotOutput("t3_result2"),
     plotOutput("t4_result2",height="800px"), plotOutput("t5_result2")
    )
    )
    )
   )
   
   )),
   
   ### Comparative analysis
   
   tabPanel('Comparative analysis',
   
   fluidPage(titlePanel("Comparative analysis"),
   sidebarLayout(
   # Side Panel
   sidebarPanel(
    checkboxGroupInput("texts3_options", label = h4("Text processing settings"), 
     choices = list("Convert to lowercase" = "t1", "Remove punctuation" = "t2", 
      "Remove numbers" = "t3", "Remove stop-words" = "t4", "Apply stemming" = "t5"),
     selected = list("t1","t2","t3","t4","t5")),
    tags$hr(),
    selectInput("texts3_lang", label = h4("Choose language"), 
     choices = list("English" = "english", "Russian" = "russian", "Danish" = "danish", "Dutch" = "dutch", 
      "Finnish" = "finnish", "French" = "french", "German" = "german", "Hungarian" = "hungarian", 
      "Italian" = "italian", "Norwegian" = "norwegian", "Portuguese" = "portuguese", "Spanish" = "spanish",
      "Swedish" = "swedish"), 
     selected = "english"),
    tags$hr(),
    sliderInput("texts3_filter1", label = h4("Filter by the word length"), min = 0, 
     max = 50, value = c(0, 50)),
    tags$hr(),
    sliderInput("texts3_filter2", label = h4("Use only words which mentioned more than in % of documents"), min = 0.1,
     max = 99.9, step = 0.1, value = c(95.5)),
    tags$hr(),
    checkboxGroupInput("texts3_methods", label = h4("Methods"), 
     choices = list("Histogram of word frequencies" = "a1", "Table of word frequencies" = "a2"),
     selected = list("a1","a2")),
    tags$hr(),
    actionButton("texts3_analyze", "Start")
   ),
   
   # Page with comparative analysis
   
   mainPanel(
    fluidRow(
      column(6,htmlOutput("t0_result31")),
      column(6,htmlOutput("t0_result32"))),
    fluidRow(
    column(12,plotOutput("t1_result3"))),
    fluidRow(
      column(6,dataTableOutput("t2_result31")),
      column(6,dataTableOutput("t2_result32")))
   )
   )
   
   )),
   tabPanel('About',
            
            fluidPage(includeMarkdown("README.md")
            ))
   ))
