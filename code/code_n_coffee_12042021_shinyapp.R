# I don't know much about web servers or front end or anything like that.
# just follow here: https://shiny.rstudio.com/tutorial/
  
# you can run the app on your local computer or on a server

# architechture:
  # the ap is a webpage, it shows the R outputs, has inputs
  # behind the webpage, is the computer that is running R
    # can be your computer or can be a server running r script to maintaining the webpage
      # R studio will do this for small traffic. Then anyone can visit,

# 2 components
  # the user interface. It uses html, but we don't need to know this
      # this is the ui
  # the R script instructions on how it should behave
      # this is what the server runs. This is all the analysis and visualization code

# its inputs and outputs
#inputs
  # inputs have 
    # an ID, this is how you refer to it in the code
    # label, this what you see on the webpage
    # other stuff

# outputs
    # outputs have a label. they are reactive
    # the work for the outputs is done by the server

# server
  # where you specify how to build output object from the inputs

# three rules:
  # building output objects as output$name <- code, output is a list the name is a specific object that you are buliding and will be referred to in the ui 
  # what you save into output uses a render function, renderPlot({}), this is the rhs of output$name <-
  # access input values with input$

# reactive values must be in a reactive function

# sharing the app over the web
  # replace own computer with a server
  # need to always to format your files in the same way such that the server will see what it expects
    # any files in the directory. R script must be app.R
  # shinyapps.io adn create an account
  # associate your R Studio IDE with the account by using the shinyapps from github
  # then you can publish from RStudio
  # can also do something with a Shiny Server



library(tidyverse)
library(shiny)
ui <- fluidPage(
  # Input() functions. These are reactive inputs.
  sliderInput(inputId = "my_sass", label = "Sassiness",
              value = 2, min = 0, max = 10)
  ,
  
  checkboxGroupInput(inputId = "sass_factors", label = "Have I:",
               c("Won Last Game?" = "wlg",
                 "Had a Cocktail?" = "had",
                 "Noticed Mercury in Retrograde?" = "mir")
  )
  ,
  
  textInput(inputId = "custom_title", label = "New Hist Title", 
            value = "Do I Think I Can Win the Next Game")
  ,
  
  selectInput(inputId = "custom_colours", label = "My Favourite Colour",
              c("Grey" = "grey",
                "Red" = "red",
                "Blue" = "blue",
                "Green" = "green",
                "Life is Pain" = "black"))
  ,
  
  # radioButtons(inputId = "winner", label = "Which Games?",
  #                    c("All" = "all",
  #                      "Marshall's Wins" = "maw",
  #                      "Miriam's Wins" = "miw")
  #              )
  # ,
  # 
  # dateRangeInput(inputId = "date", label = "Period",
  #                start = "2021-01-03", end = "2021-04-06",
  #                min = "2021-01-03", max = "2021-04-06")
  # ,
  
  # Output() functions. These are the result of the reactive inputs being processed by the server
  plotOutput("hist"),
  tableOutput("table"),
  plotOutput("cum_wins"),
  )

server <- function(input, output) {
  
  # this is where the server processes the reactive inputs into outputs. Reactive inputs can only appear within a reactive function
  output$hist <- renderPlot({
    # access the reactive input using input$inputId
    title <- input$custom_title
    scaled_sass <- input$my_sass*10
    mean_sass <- scaled_sass + 
      (("wlg" %in% input$sass_factors)*20) - # this reactive input will give a logical
      (("mir" %in% input$sass_factors)*30)
    noise <-  20 + 
      (("had" %in% input$sass_factors)*(10))
    sim_sass <- rnorm(100, mean = mean_sass, sd = noise)
    
    hist(sim_sass, 
         main = title, 
         xlab = "Confidence",
         xlim = c(-50, 150),
         ylim = c(0, 50),
         col = input$custom_colours) # we defined the reactive input to be usable in hist(col = )
  })
  
  # # these are not meant to be reactive, just reading in data. Can keep out of the reactive function (ie Render)
  # dat <- read_csv("mm_wingspan_oceana_scores.csv") %>% na.omit()
  # 
  # game_totals <- 
  #   dat %>% group_by(Ga) %>% summarise(tot_Mi = sum(Mi), tot_Ma = sum(Ma)) %>%
  #   mutate(winner = ifelse(tot_Mi > tot_Ma, "mi", 
  #                          ifelse(tot_Mi < tot_Ma, "ma", "tie")
  #                          )
  #          )
  # 
  # cum_wins <- 
  #   game_totals %>%  
  #   pivot_wider(names_from = winner, values_from = winner, values_fn = is.character, values_fill = FALSE) %>%
  #   mutate(cum_sum_Mi = cumsum(mi), cum_sum_Ma = cumsum(ma),
  #          cum_sum_win_Mi = cumsum(mi), cum_sum_win_Ma = cumsum(ma))
  # 
  # output$table <- renderTable({
  #   
  #   # reactive values, when checked, they are in the list it's TRUE
  #   all <-  "all" %in% input$winner
  #   miw <-  "miw" %in% input$winner
  #   maw <-  "maw" %in% input$winner
  #   
  #   # some data manipulation here, but there is also reactive values in this pipe. This would not work outside of the reactive function
  #   left_join(dat, game_totals) %>%
  #     mutate(cat = rep(c("Bird Points", "Bonus Cards", "End Rounds Goals", "Eggs", "Cached Food", "Tuckies", "Nectar"), max(Ga))) %>%
  #     filter(winner %in% ifelse(all, c("mi", "ma", "tie"),  # reactive values, if TRUE 
  #                             ifelse(miw, "mi", "ma"))
  #              ) %>%
  #     group_by(cat) %>% 
  #     summarise(mi_cat = mean(Mi) %>% round(1), ma_cat = mean(Ma) %>% round(1)) %>% 
  #     rename(Category = cat, Miriam = mi_cat, Marshall = ma_cat)
  #   
  # })
  # 
  # output$cum_wins <- renderPlot({
  #   
  #   ggplot(data = cum_wins %>% pivot_longer(cols = 9:10), aes(x = Ga, y = value, color = as.factor(name), size = 2)) +
  #     geom_line() + theme_bw() + xlab("Game Number") + ylab("Wins") +
  #     scale_color_manual(name = "Birder", labels = c("Marshall", "Miriam"), values = c("red", "blue")) +
  #     scale_size(guide = F) +
  #     theme(text = element_text(size=20)) +
  #     ggtitle("Wingspan Oceana Wins 2021")
  #   
  # })
  
}

shinyApp(ui = ui, server = server)



