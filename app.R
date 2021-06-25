################################################################################
# load packages, data, and functions
################################################################################

library(tidyverse)
library(tidytext)
library(tidylo)
library(wordcloud)
library(wordcloud2)
library(shiny)
tokens <- readRDS("data/tokens_with_IDs.RDS")
ratings <- readRDS("data/sentiment_scores.RDS") 

# add a column of overall rating to the tokens for filtering purposes
tokens <- left_join(tokens, ratings[, c("reviewId", "overall", "pct_group")])


################################################################################
# define UI
################################################################################


ui <- fluidPage(
    
    titlePanel("Ratebeer Brewer Text Analysis"),
    
    sidebarLayout(
        sidebarPanel(
            
            # select a brewer Id
            selectInput(inputId = "brewer", 
                        label = strong("Select your Brewer ID"),
                        choices = unique(ratings$brewerId),
                        selected = unique(ratings$brewerId)[1]
            ),
            
            # select a beer
            selectInput(inputId = "beer", 
                        label = strong("Select your Beer"),
                        choices = NULL
            ),
            
            # subset to certain ratings  
            sliderInput(inputId = "rate_range",
                        label = "Select a subset of overall ratings. This only affects Sentiment Scores and Wordclouds.",
                        min = 1,
                        max = 20,
                        value = c(1, 20),
                        step = 1
            ),
            
            # select number of words for wordclouds
            sliderInput(inputId = "wordcloud_num", 
                        label = "Choose the number of words to put into the wordcloud.",
                        min = 50,
                        max = 500,
                        value = 200,
                        step = 50),
            
            br(), br(),
            
            # credits for app
            p("This app was produced by ", strong("Kim Cressman, Angela Huynh"), " and ", strong("Maple So"), " as part of a capstone project for the Colorado State University Master of Applied Statistics Program."),
            
            p(strong("Data source: "), a('RateBeer, ', href = 'https://www.ratebeer.com/', target = '_blank'), "specifically the ", a('SNAP dataset.', href = 'https://www.ratebeer.com/api.asp', target = '_blank'), "This is a prototype on a small subset of brewers and beers."),
            
            
            p("Source code for this app can be found on ", a('github.', href = 'https://github.com/kimarsh/STAA556_ShinyApp', target = '_blank'))
            
        ),
        
        
        mainPanel(
            
            # tabs
            tabsetPanel(type = "tabs",
                        id = "tabselected",
                        
                        tabPanel("Overall Summary", value = 1,
                                 br(),
                                 textOutput(outputId = "summary_stats"),
                                 br(), br(),
                                 plotOutput(outputId = "hist_overall",
                                            height = 300,
                                            width = 500),
                                 plotOutput(outputId = "hist_char",
                                            height = 400,
                                            width = 600),
                                 br(), br()
                        ),
                        
                        tabPanel("Sentiment Scores", value = 2,
                                 br(),
                                 textOutput(outputId = "score_summ"),
                                 br(),
                                 plotOutput(outputId = "hist_affin",
                                            height = 300,
                                            width = 450),
                                 br(),
                                 plotOutput(outputId = "pct_affin",
                                            width = 400,
                                            height = 400),
                                 br(), br()
                        ),
                        
                        tabPanel("Key Words", value = 3,
                                 br(),
                                 textOutput(outputId = "n_tokens"),
                                 # br(),
                                 # plotOutput(outputId = "wordcloud_all",
                                 #            height = 300,
                                 #            width = 300),
                                 # br(),
                                 wordcloud2Output(outputId = "wc_all2",
                                                  height = "300px"),
                                 br(),
                                 sliderInput(inputId = "n_words",
                                             label = "Select how many distinguishing words you want to see below.",
                                             min = 1, max = 25,
                                             step = 1,
                                             value = 5),
                                 "You may have to move the slider to make the plot appear the first time.",
                                 br(), br(),
                                 plotOutput(outputId = "logodds"),
                                 br(), br()
                        )
            )
        )
        
    )
)



###############################################################################
# Define server function
###############################################################################


server <- function(input, output) {
    
    
    ##############################################
    # reactive data operations
    ##############################################
    
    # subset ratings dataframe, reactively
    ratings_sub <- reactive({
        req(input$brewer)
        ratings %>%
            filter(brewerId == input$brewer)
    })
    # use this subset to update available beer choices
    observeEvent(ratings_sub(), {
        choices <- unique(ratings_sub()$name)
        updateSelectInput(inputId = "beer", choices = choices) 
    })
    
    # make data frame for individual beer
    beer_sub <- reactive({
        req(input$beer)
        req(input$rate_range)
        ratings_sub() %>% 
            filter(name == input$beer)
                   
    })
    
    
    # subset tokenized data, reactively
    tokens_sub <- reactive({
        req(input$brewer)
        req(input$beer)
        tokens %>%
            filter(brewerId == input$brewer,
                   name == input$beer)
    })
    
    
    # log-odds ratios
    tokens_lo <- reactive({
        tokens_sub() %>% 
            filter(pct_group != "none") %>% 
            group_by(pct_group) %>% 
            count(word, sort = TRUE) %>%
            ungroup() %>% 
            bind_log_odds(set = pct_group, feature = word, n = n) 
            
    })
    
    # tf-idf
    tokens_tfidf <- reactive({
        tokens_sub() %>% 
            filter(pct_group != "none") %>% 
            group_by(pct_group) %>% 
            count(word, sort = TRUE) %>% 
            ungroup() %>% 
            bind_tf_idf(term = word, document = pct_group, n = n)
    })
    

    ##############################################
    # Text
    ##############################################
    
    # summary of beer ratings  
    rev_count <- reactive({
        nrow(beer_sub())
    })
    
    rev_mean <- reactive({
        round(mean(beer_sub()$overall, na.rm = TRUE), 2)
    })
    
    rev_median <- reactive({
        median(beer_sub()$overall, na.rm = TRUE)
    })
    
    output$summary_stats <- renderText({
        paste("This beer has", rev_count(), "reviews. Overall mean rating is", rev_mean(), "and overall median rating is ", rev_median(), ".")
    })
    
    # tokens
    token_count <- reactive({
        length(unique(tokens_sub()$word))
    })
    
    output$n_tokens <- renderText({
        paste("There are", token_count(), "unique adjectives in these reviews. Up to",
              min(token_count(), input$wordcloud_num), "are represented below. This word cloud shows the most frequently used words across all selected reviews (see sidebar input to narrow the range of ratings).")
    })
    
    
    # polarity score summary
    scored_count <- reactive({
        sum(!is.na(beer_sub()$AFFIN))
    })
    
    scored_mean <- reactive({
        round(mean(beer_sub()$AFFIN, na.rm = TRUE), 2)
    })
    
    scored_median <- reactive({
        median(beer_sub()$AFFIN, na.rm = TRUE)
    })
    
    output$score_summ <- renderText({
        paste(scored_count(), "reviews had scoreable words in the AFFIN dictionary. Mean sentiment score is", scored_mean(), "and median sentiment score is", scored_median(), ".")
    })
    
    ##############################################
    # Plots
    ##############################################
    
    
    # create histogram of overall ratings
    output$hist_overall <- renderPlot({
        req(input$beer)
        ggplot(beer_sub()) +
            geom_histogram(aes(x = overall),
                           fill = "cadetblue3",
                           col = "gray60",
                           binwidth = 1) +
            coord_cartesian(xlim = c(0, 20)) +
            labs(title = "Overall Ratings",
                 x = "rating",
                 y = "") +
            theme_bw()
    })
    
    # histogram of ratings by characteristic
    output$hist_char <- renderPlot({
        q <- beer_sub() %>% 
            select(reviewId, appearance, aroma, palate, taste) %>% 
            pivot_longer(appearance:taste, names_to = "char", values_to = "value")
        
        ggplot(q) +
            geom_histogram(aes(x = value, fill = char),
                           col = "gray60",
                           binwidth = 1) +
            scale_fill_brewer(palette = "Set1") +
            facet_wrap(~char, ncol = 2) +
            coord_cartesian(xlim = c(0, 10)) +
            labs(title = "Ratings by characteristic",
                 x = "rating",
                 y = "") +
            theme_bw() +
            theme(legend.position = "none") 
        
    })
    
    
    # histogram of AFFIN polarity scores
    output$hist_affin <- renderPlot({
        req(input$beer)
        req(input$rate_range)
        beer_sub() %>% 
            filter(overall >= input$rate_range[1],
                   overall <= input$rate_range[2]) %>% 
        ggplot() +
            geom_histogram(aes(x = AFFIN),
                           fill = "orange3",
                           col = "gray50",
                           binwidth = 0.5) +
            geom_vline(xintercept = 0, col = "navy", size = 1) +
            coord_cartesian(xlim = c(-5, 5)) +
            labs(title = "AFFIN polarity scores for reviews \nin the selected range of ratings (see sidebar)",
                 subtitle = "0 is neutral",
                 x = "score",
                 y = "") +
            theme_bw()
    })
    
    # density plot of sentiment scores
    output$pct_affin <- renderPlot({
        req(input$beer)
        
        r <- beer_sub() %>% 
            filter(pct_group != "none")
        
        ggplot(r) +
            geom_density(aes(x = AFFIN,
                             col = pct_group,
                             fill = pct_group),
                         binwidth = 0.5,
                         alpha = 0.5) +
            scale_fill_brewer(palette = "Set1") +
            scale_color_brewer(palette = "Set1") +
            geom_vline(xintercept = 0, col = "navy", size = 1) +
            coord_cartesian(xlim = c(-5, 5)) +
            facet_wrap(~pct_group, ncol = 1) +
            labs(title = "AFFIN polarity scores by ratings group",
                 subtitle = "high = top 10% of overall ratings \nmid = median overall rating \nlow = bottom 10% of overall ratings",
                 x = "score",
                 y = "") +
            theme_bw() +
            theme(legend.position = "none")
    })
    
    
    # wordcloud
    output$wordcloud_all <- renderPlot({
        req(input$beer)
        req(input$rate_range)
        v <- tokens_sub() %>% 
            filter(overall >= input$rate_range[1],
                   overall <= input$rate_range[2]) %>% 
            group_by(word) %>% 
            summarize(n = n()) %>% 
            arrange(desc(n))
        
        wordcloud(words = v$word,
                      freq = v$n,
                      max.words = input$wordcloud_num,
                      random.order=FALSE,
                      rot.per=0.35,
                      colors=brewer.pal(6, "Dark2"))
    })
    
    
    output$wc_all2 <- renderWordcloud2({
        req(input$wordcloud_num)
        req(input$rate_range)
        wc2 <- tokens_sub() %>%
            filter(overall >= input$rate_range[1],
                   overall <= input$rate_range[2]) %>% 
            group_by(word) %>%
            summarize(n = n()) %>%
            slice_max(n, n = input$wordcloud_num) %>% 
            arrange(desc(n))
        wordcloud2(wc2,
                   size = 0.6,
                   fontWeight = 'normal',
                   color = rep(brewer.pal(6, "Dark2"), length.out = nrow(wc2)),
                   rotateRatio = 0.1,
                   minRotation = pi/2,
                   maxRotation = pi/2)
    })
    
    
    
    # log-odds plot
    output$logodds <- renderPlot({
        req(input$n_words)
        tokens_lo() %>% 
            group_by(pct_group) %>% 
            slice_max(log_odds_weighted, n = input$n_words) %>%
            ungroup() %>% 
            ggplot() +
            geom_col(aes(x = log_odds_weighted, 
                         y = fct_reorder(word, log_odds_weighted),
                         fill = pct_group),
                     show.legend = FALSE) +
            scale_fill_brewer(palette = "Set1") +
            facet_wrap(~pct_group, scales = "free") +
            theme_bw() +
            labs(title = "Most distinctive words to each ratings group",
                 subtitle = "high = top 10% of overall ratings \nmid = median overall rating \nlow = bottom 10% of overall ratings",
                 x = "Weighted Log-Odds Ratio", y = NULL)
    })
    
    
    # tf-idf plot
    # output$tfidf <- renderPlot({
    #     tokens_tfidf() %>% 
    #         group_by(pct_group) %>% 
    #         slice_max(tf_idf, n = input$n_words) %>%
    #         ungroup() %>% 
    #         ggplot() +
    #         geom_col(aes(x = tf_idf, 
    #                      y = fct_reorder(word, tf_idf),
    #                      fill = pct_group),
    #                  show.legend = FALSE) +
    #         scale_fill_brewer(palette = "Set1") +
    #         facet_wrap(~pct_group, scales = "free") +
    #         theme_bw() +
    #         labs(title = "Terms most common to certain percentile groups",
    #              x = "term frequency - inverse term frequency", y = NULL)
    # })
    
    
}

###############################################################################
# Create Shiny object
###############################################################################
shinyApp(ui = ui, server = server)