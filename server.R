# Load necessary libraries

library(tm)
library(SnowballC)
library(wordcloud)
library(MASS)
library(ggplot2)
library(Rgraphviz)
library(graph)
library(RJSONIO)
library(igraph)

# Server

shinyServer(function(input, output) {
  
  ### Reactive values, which are being used by different functions
  values <- reactiveValues()
  
  ### File titles and sizes
  
  output$filelist1 <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    names(inFile) <- c("Title","Size","Type","Path")
    as.data.frame(inFile[c("Title","Size")])
  })
  
  output$filelist2 <- renderTable({
    inFile <- input$file2
    if (is.null(inFile))
      return(NULL)
    names(inFile) <- c("Title","Size","Type","Path")
    as.data.frame(inFile[c("Title","Size")])
  })
  
  ### Main corpus analysis
  # Creation of corpus, frequency table and general stats
  
  output$t0_result1 <- renderUI({
    input$texts1_analyze
    inFile <- input$file1
    if (is.null(inFile))
      return("You haven't chosen any texts yet.")
    
    if (input$texts1_analyze == 0) {
      return(paste("You uploaded ", nrow(inFile)," texts for analysis. 
                   Customize analysis options and press the 'Start' button."))}
    
    input$texts1_analyze
    isolate(texts1_options <- input$texts1_options)
    isolate(texts1_lang <- input$texts1_lang)
    isolate(texts1_filter1 <- input$texts1_filter1)
    isolate(texts1_filter2 <- input$texts1_filter2)
    corpus = Corpus(URISource(inFile$datapath))
    if("t1" %in% texts1_options) {corpus <- tm_map(corpus, content_transformer(tolower))}
    if("t2" %in% texts1_options) {corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)}
    if("t3" %in% texts1_options) {corpus <- tm_map(corpus, removeNumbers)}
    if("t4" %in% texts1_options) {corpus <- tm_map(corpus, removeWords, stopwords(texts1_lang))}
    if("t5" %in% texts1_options) {
      stemAll <- function(x) {
        words <- unlist(strsplit(x,split=c(" |-")))
        words <- words[sapply(words,nchar) > 0]
        text <-paste(wordStem(words, language = input$texts1_lang), collapse=" ")
        return(text)}
      corpus <- tm_map(corpus, content_transformer(stemAll))}
    
    frequencies <- DocumentTermMatrix(corpus,control = list(tolower=F, wordLengths = c(texts1_filter1[1],texts1_filter1[2])))
    if(texts1_filter2 != 100) {frequencies <- removeSparseTerms(frequencies,(1-texts1_filter2/100))}
    
    values$freq1 <- frequencies
    values$wordsMatrix1 <- as.matrix(frequencies)
    values$wordsVector1 <- as.data.frame(apply(values$wordsMatrix1,2,sum))
    isolate(values$wordsMatrix1)
    isolate(values$wordsVector1)
    return(HTML(paste("<h2>Results</h2>",
                      "<p>Number of texts: ", nrow(inFile), "</p>",
                      "<p>Number of words: ", sum(values$wordsMatrix1), "</p>",
                      "<p>Number of unique words: ", ncol(values$wordsMatrix1), "</p>")))
    })
  
  # Plot histogram
  
  output$t1_result1 <- renderPlot({
    if (is.null(values$wordsVector1))
      return(NULL)
    input$texts1_analyze
    isolate(texts1_methods <- input$texts1_methods)
    isolate(wordsVector1 <- values$wordsVector1)
    names(wordsVector1) = "words"
    if (!("a1" %in% texts1_methods) | nrow(wordsVector1)==0) {
      return(NULL)
    }
    text1_plot1 <- ggplot(wordsVector1, aes(x=words))
    text1_plot1 + geom_histogram(aes(fill = ..count..),binwidth=1) +
      scale_fill_continuous(name="Number\nof mentions") +
      xlab("Number of word mentions") + ylab("Frequency")
  })
  
  # Table of frequencies
  
  output$t2_result1 <- renderDataTable({
    
    if (is.null(values$wordsVector1))
      return(NULL)
    
    input$texts1_analyze
    isolate(texts1_methods <- input$texts1_methods)
    isolate(wordsVector1 <- values$wordsVector1)
    if (!("a2" %in% texts1_methods)) {
      return(NULL)
    }
    text1Table <- cbind(rownames(wordsVector1),wordsVector1,
                        format(wordsVector1*100/sum(wordsVector1), digits=1, nsmall=1))
    names(text1Table) <- c("Word", "Number", "%")
    text1Table[order(text1Table[,2],decreasing = T),]
  }, options = list(pageLength = 15))
  
  # Wordcloud
  
  output$t3_result1 <- renderPlot({
    if (is.null(values$wordsVector1))
      return(NULL)
    input$texts1_analyze
    isolate(texts1_methods <- input$texts1_methods)
    isolate(wordsVector1 <- values$wordsVector1)
    if (!("a3" %in% texts1_methods) | nrow(wordsVector1)==0) {
      return(NULL)
    }
    cloudWords <- cbind(rownames(wordsVector1),wordsVector1)
    names(cloudWords) <- c("word", "num")
    wordcloud(cloudWords$word, cloudWords$num, min.freq=3, random.order=F, random.color=T, colors=brewer.pal(8, "Dark2"),scale=c(8,1))
  })
  
  # Graph of relations between words
  
  output$t4_result1 <- renderPlot({
    if (is.null(values$wordsVector1))
      return(NULL)
    input$texts1_analyze
    isolate(cor <- input$texts1_filter3)
    isolate(texts1_methods <- input$texts1_methods)
    isolate(wordsVector1 <- values$wordsVector1)
    isolate(freq <- values$freq1)
    if (!("a4" %in% texts1_methods) | nrow(wordsVector1)==0) {
      return(NULL)
    }
    names(wordsVector1) <- "num"
    lf <- sort(wordsVector1$num,decreasing=T)[20]
    plot(freq, terms=findFreqTerms(freq,lowfreq=lf),corThreshold=cor)
  }, height = 800, width=800)
  
  # Graph of relations between words and texts
  
  output$t5_result1 <- renderPlot({
    if (is.null(values$wordsVector1))
      return(NULL)
    input$texts1_analyze
    isolate(texts1_methods <- input$texts1_methods)
    isolate(wordsMatrix <- values$wordsMatrix1)
    if (!("a5" %in% texts1_methods) | ncol(wordsMatrix)==0) {
      return(NULL)
    }
    termsMatrix <- wordsMatrix
    termsMatrix<-termsMatrix[,names(sort(colSums(termsMatrix), decreasing = T))[1:20]]
    g <- graph.incidence(t(termsMatrix), mode=c("all"))
    nTerms <- ncol(termsMatrix)
    nDocs <- nrow(termsMatrix)    
    idx.terms <- 1:nTerms
    idx.docs <- (nTerms+1):(nTerms+nDocs)
    V(g)$degree <- degree(g)
    V(g)$color[idx.terms] <- rgb(1,0,0,.5)
    V(g)$size[idx.terms] <- 2
    V(g)$color[idx.docs] <- rgb(0,1,0,.4)
    V(g)$size[idx.docs] <- 4
    V(g)$frame.color <- NA
    V(g)$label <- V(g)$name
    V(g)$label.color <- rgb(0,0,0,0.5)
    V(g)$label.cex <- 1.4*V(g)$degree/max(V(g)$degree)+1
    E(g)$width <- .3
    E(g)$color <- rgb(.5,.5,0,.3)
    set.seed(958)
    plot(g,layout=layout.fruchterman.reingold)
  },height = 800, width=800)
  
  ### Extra corpus analysis
  # Creation of corpus, frequency table and general stats
  
  output$t0_result2 <- renderUI({
    input$texts2_analyze
    inFile <- input$file2
    if (is.null(inFile))
      return("You haven't chosen any texts yet.")
    
    if (input$texts2_analyze == 0) {
      return(paste("You uploaded ", nrow(inFile)," texts for analysis. 
                   Customize analysis options and press the 'Start' button."))}
    
    input$texts2_analyze
    isolate(texts2_options <- input$texts2_options)
    isolate(texts2_lang <- input$texts2_lang)
    isolate(texts2_filter1 <- input$texts2_filter1)
    isolate(texts2_filter2 <- input$texts2_filter2)
    corpus = Corpus(URISource(inFile$datapath))
    if("t1" %in% texts2_options) {corpus <- tm_map(corpus, content_transformer(tolower))}
    if("t2" %in% texts2_options) {corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)}
    if("t3" %in% texts2_options) {corpus <- tm_map(corpus, removeNumbers)}
    if("t4" %in% texts2_options) {corpus <- tm_map(corpus, removeWords, stopwords(texts2_lang))}
    if("t5" %in% texts2_options) {
      stemAll <- function(x) {
        words <- unlist(strsplit(x,split=c(" |-")))
        words <- words[sapply(words,nchar) > 0]
        text <-paste(wordStem(words, language = input$texts2_lang), collapse=" ")
        return(text)}
      corpus <- tm_map(corpus, content_transformer(stemAll))}
    
    frequencies <- DocumentTermMatrix(corpus,control = list(tolower=F, wordLengths = c(texts2_filter1[1],texts2_filter1[2])))
    if(texts2_filter2 != 100) {frequencies <- removeSparseTerms(frequencies,(1-texts2_filter2/100))}
    
    values$freq2 <- frequencies
    values$wordsMatrix2 <- as.matrix(frequencies)
    values$wordsVector2 <- as.data.frame(apply(values$wordsMatrix2,2,sum))
    isolate(values$wordsMatrix2)
    isolate(values$wordsVector2)
    return(HTML(paste("<h2>Results</h2>",
                      "<p>Number of texts: ", nrow(inFile), "</p>",
                      "<p>Number of words: ", sum(values$wordsMatrix2), "</p>",
                      "<p>Number of unique words: ", ncol(values$wordsMatrix2), "</p>")))
    })
  
  # Plot histogram
  
  output$t1_result2 <- renderPlot({
    if (is.null(values$wordsVector2))
      return(NULL)
    input$texts2_analyze
    isolate(texts2_methods <- input$texts2_methods)
    isolate(wordsVector2 <- values$wordsVector2)
    names(wordsVector2) = "words"
    if (!("a1" %in% texts2_methods) | nrow(wordsVector2)==0) {
      return(NULL)
    }
    text2_plot1 <- ggplot(wordsVector2, aes(x=words))
    text2_plot1 + geom_histogram(aes(fill = ..count..),binwidth=1) + 
      scale_fill_continuous(name="Number\nof mentions") +
      xlab("Number of word mentions") + ylab("Frequency")
  })
  
  # Table of frequencies
  
  output$t2_result2 <- renderDataTable({
    
    if (is.null(values$wordsVector2))
      return(NULL)
    
    input$texts2_analyze
    isolate(texts2_methods <- input$texts2_methods)
    isolate(wordsVector2 <- values$wordsVector2)
    if (!("a2" %in% texts2_methods)) {
      return(NULL)
    }
    text2Table <- cbind(rownames(wordsVector2),wordsVector2,
                        format(wordsVector2*100/sum(wordsVector2), digits=1, nsmall=1))
    names(text2Table) <- c("Word", "Number", "%")
    text2Table[order(text2Table[,2],decreasing = T),]
  }, options = list(pageLength = 15))
  
  # Wordcloud
  
  output$t3_result2 <- renderPlot({
    if (is.null(values$wordsVector2))
      return(NULL)
    input$texts2_analyze
    isolate(texts2_methods <- input$texts2_methods)
    isolate(wordsVector2 <- values$wordsVector2)
    if (!("a3" %in% texts2_methods) | nrow(wordsVector2)==0) {
      return(NULL)
    }
    cloudWords <- cbind(rownames(wordsVector2),wordsVector2)
    names(cloudWords) <- c("word", "num")
    wordcloud(cloudWords$word, cloudWords$num, min.freq=3, random.order=F, random.color=T, colors=brewer.pal(8, "Dark2"),scale=c(8,1))
  })
  
  # Graph of relations between words
  
  output$t4_result2 <- renderPlot({
    if (is.null(values$wordsVector2))
      return(NULL)
    input$texts2_analyze
    isolate(cor <- input$texts2_filter3)
    isolate(texts2_methods <- input$texts2_methods)
    isolate(wordsVector2 <- values$wordsVector2)
    isolate(freq <- values$freq2)
    if (!("a4" %in% texts2_methods) | nrow(wordsVector2)==0) {
      return(NULL)
    }
    names(wordsVector2) <- "num"
    lf <- sort(wordsVector2$num,decreasing=T)[20]
    plot(freq, terms=findFreqTerms(freq,lowfreq=lf),corThreshold=cor)
  }, height = 800, width=800)
  
  # Graph of relations between words and texts
  
  output$t5_result2 <- renderPlot({
    if (is.null(values$wordsVector2))
      return(NULL)
    input$texts2_analyze
    isolate(texts2_methods <- input$texts2_methods)
    isolate(wordsMatrix <- values$wordsMatrix2)
    if (!("a5" %in% texts2_methods) | ncol(wordsMatrix)==0) {
      return(NULL)
    }
    termsMatrix <- wordsMatrix
    termsMatrix<-termsMatrix[,names(sort(colSums(termsMatrix), decreasing = T))[1:20]]
    g <- graph.incidence(t(termsMatrix), mode=c("all"))
    nTerms <- ncol(termsMatrix)
    nDocs <- nrow(termsMatrix)    
    idx.terms <- 1:nTerms
    idx.docs <- (nTerms+1):(nTerms+nDocs)
    V(g)$degree <- degree(g)
    V(g)$color[idx.terms] <- rgb(1,0,0,.5)
    V(g)$size[idx.terms] <- 2
    V(g)$color[idx.docs] <- rgb(0,1,0,.4)
    V(g)$size[idx.docs] <- 4
    V(g)$frame.color <- NA
    V(g)$label <- V(g)$name
    V(g)$label.color <- rgb(0,0,0,0.5)
    V(g)$label.cex <- 1.4*V(g)$degree/max(V(g)$degree)+1
    E(g)$width <- .3
    E(g)$color <- rgb(.5,.5,0,.3)
    set.seed(958)
    plot(g,layout=layout.fruchterman.reingold)
  },height = 800, width=800)
  
  ### Comparison of two corpuses

  # General stats
  
  output$t0_result31 <- renderUI({
    inFile1 <- input$file1
    inFile2 <- input$file2
    if (is.null(inFile1) | is.null(inFile2))
      return("You haven't chosen texts for comparison.")
    
    if (input$texts3_analyze == 0) {
      return(paste0("There are : ", nrow(inFile1)," texts in main corpus. There are ", nrow(inFile2)," texts in extra corpus. 
                    Press 'Start' to perform analysis."))}
    
    input$texts3_analyze
    isolate(texts3_options <- input$texts2_options)
    isolate(texts3_lang <- input$texts2_lang)
    isolate(texts3_filter1 <- input$texts2_filter1)
    isolate(texts3_filter2 <- input$texts2_filter2)
    inFile1 <- data.frame(inFile1,text=rep(1,nrow(inFile1)))
    inFile2 <- data.frame(inFile2,text=rep(2,nrow(inFile2)))
    inFile = as.data.frame(rbind(inFile1,inFile2))
    values$all_texts <- inFile
    isolate(values$all_texts)
    corpus = Corpus(URISource(inFile$datapath))
    if("t1" %in% texts3_options) {corpus <- tm_map(corpus, content_transformer(tolower))}
    if("t2" %in% texts3_options) {corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)}
    if("t3" %in% texts3_options) {corpus <- tm_map(corpus, removeNumbers)}
    if("t4" %in% texts3_options) {corpus <- tm_map(corpus, removeWords, stopwords(texts3_lang))}
    if("t5" %in% texts3_options) {
      stemAll <- function(x) {
        words <- unlist(strsplit(x,split=c(" |-")))
        words <- words[sapply(words,nchar) > 0]
        text <-paste(wordStem(words, language = input$texts3_lang), collapse=" ")
        return(text)}
      corpus <- tm_map(corpus, content_transformer(stemAll))}
    
    frequencies <- DocumentTermMatrix(corpus,control = list(tolower=F, wordLengths = c(texts3_filter1[1],texts3_filter1[2])))
    if(texts3_filter2 != 100) {frequencies <- removeSparseTerms(frequencies,(1-texts3_filter2/100))}
    
    values$freq3 <- frequencies
    values$wordsMatrix3 <- as.matrix(frequencies)
    values$wordsVector3 <- as.data.frame(apply(values$wordsMatrix3,2,sum))
    isolate(values$wordsMatrix3)
    isolate(values$wordsVector3)
    values$w1 <- values$wordsMatrix3[inFile$text==1,]
    values$w2 <- values$wordsMatrix3[inFile$text==2,]
    words1 <- sum(values$w1)
    if (is.data.frame(values$w1)) {
      values$vw1 <- colSums(values$wordsMatrix3[inFile$text==1,])
      temp <- values$wordsMatrix3[inFile$text==1,values$vw1>0]
      unique1 <- nrow(temp)
    } else { 
      values$vw1 <- values$w1
      unique1 <- length(values$w1)}
    
    values$words2 <- sum(values$w2)
    if (is.data.frame(values$w2)) {
      values$vw2 <- colSums(values$wordsMatrix3[inFile$text==2,])
      temp <- values$wordsMatrix3[inFile$text==2,values$vw2>0]
      values$unique2 <- nrow(temp)
    } else { 
      values$vw2 <- values$w2
      values$unique2 <- length(values$w2)}
    
    return(HTML(paste("<h2>Main corpus</h2>",
                      "<p>Number of texts: ", nrow(inFile1), "</p>",
                      "<p>Number of words: ", words1, "</p>",
                      "<p>Number of unique words: ", unique1, "</p>")))
  })
  
  output$t0_result32 <- renderUI({
    inFile1 <- input$file1
    inFile2 <- input$file2
    if (is.null(inFile1) | is.null(inFile2))
      return(NULL)
    
    if (input$texts3_analyze == 0)
      return(NULL)
    
    input$texts3_analyze
    return(HTML(paste("<h2>Extra corpus</h2>",
                      "<p>Number of texts: ", nrow(inFile2), "</p>",
                      "<p>Number of words: ", values$words2, "</p>",
                      "<p>Number of unique words: ", values$unique2, "</p>")))
    
  })
  
  # Common histogram
  
  output$t1_result3 <- renderPlot({
    if (is.null(values$wordsVector3))
      return(NULL)
    input$texts3_analyze
    isolate(texts3_methods <- input$texts3_methods)
    isolate(w1 <- values$vw1)
    isolate(w2 <- values$vw2)
    isolate(wordsVector3 <- values$wordsVector3)
    if (!("a1" %in% texts3_methods)) {
      return(NULL)
    }
    hist_data <- data.frame(texts=c(rep(1,length(w1)),rep(2,length(w2))),words=c(w1,w2))
    text2_plot1 <- ggplot(hist_data, aes(x=words))
    text2_plot1 + 
      geom_histogram(data=subset(hist_data,texts == 1), fill = "blue", alpha = 0.4, binwidth=1) +
      geom_histogram(data=subset(hist_data,texts == 2), fill = "red", alpha = 0.4, binwidth=1) +
      xlab("Количество упоминаний слов") + ylab("Частота")
  })
  
  # Two tables of frequencies  
  
  output$t2_result31 <- renderDataTable({
    
    if (is.null(values$wordsVector3))
      return(NULL)
    
    input$texts3_analyze
    isolate(texts3_methods <- input$texts3_methods)
    isolate(w1 <- values$vw1)
    if (!("a2" %in% texts3_methods)) {
      return(NULL)
    }
    
    if (!(is.vector(values$w1))) {
      w1 <- colSums(w1)
    }
    
    text1Table <- as.data.frame(cbind(rownames(values$wordsVector3),w1,
                        format(w1*100/sum(w1), digits=1, nsmall=1)))
    names(text1Table) <- c("Word", "Number", "%")
    text1Table[order(text1Table[,3],decreasing = T),]
  }, options = list(pageLength = 15))
  
  
  output$t2_result32 <- renderDataTable({
    
    if (is.null(values$wordsVector3))
      return(NULL)
    
    input$texts3_analyze
    isolate(texts3_methods <- input$texts3_methods)
    isolate(w2 <- values$vw2)
    if (!("a2" %in% texts3_methods)) {
      return(NULL)
    }
    
    if (!(is.vector(values$w2))) {
      w2 <- colSums(w2)
    }
    
    text2Table <- as.data.frame(cbind(rownames(values$wordsVector3),w2,
                        format(w2*100/sum(w2), digits=1, nsmall=1)))
    names(text2Table) <- c("Word", "Number", "%")
    text2Table[order(text2Table[,3],decreasing = T),]
  }, options = list(pageLength = 15))
  
})
