# server

server <- function(input,output,session){
  rec_text <- eventReactive(input$click_u,{
    input$username_ui
    })
  
  observeEvent(input$click_u,{
    if(input$check_terms == TRUE){
      newtab <- switch(input$tabs,
                       "twitter" = "space","space" = "twitter")
      updateTabItems(session,inputId = "tabs",newtab)
      
      output$pname <- renderText({
        paste(rec_text()," Personality Prediction")
      })
      
      # get user
      user_tl <- get_timeline(user = rec_text(),n = 300,token = token)
      
      # calculate persoanality
      person <- get_personality(user_tl)
      
      # personlaity result
      output$prof_pict <- renderUI({
        img(src = user_tl$profile_image_url[1],
            height = 80, width = 80)
      })
      
      output$detail <- renderText({
        paste("Name: ",user_tl$name[1],"<br>",
              "Followers: ",user_tl$followers_count[1],"<br>",
              "Following: ",user_tl$friends_count[1],"<br>",
              "Tweets: ",user_tl$statuses_count[1])
      })
      
      output$last_tweet <- renderText({
        paste("Last tweet: ",user_tl$text[1])
      })
      
      output$radar <- renderPlotly({
        
        plotlong <- person %>% 
          pivot_longer(cols = c(Extraversion,Agreeableness,Conscientiousness,Neuroticism,Openness)) %>%
          mutate(txt = paste(name,":",round(value,2)))
        
        fig <- plot_ly(plotlong,type = "scatterpolar",r = plotlong$value,theta = plotlong$name, 
                       fill = "toself",hoverinfo = "text",text = plotlong$txt)
        
        fig %>% layout(
          polar = list(
            radialaxis = list(
              visible = T,range = c(0,5)
            )
          ), showlegend = F)
      })
      
      output$MBTI <- renderUI({
        tags$a(href = "https://www.16personalities.com/intj-personality",person$MBTI)
      })
      
      output$mbti_icon <- renderUI({
        img(src = "counselor-infj.png",height = 190, width = 110)
      })
      
      output$more <- renderUI({
        ref <- paste("https://twitter.com/",input$username_ui)
        ref <- tags$a(href=ref,user_tl$name[1])
        tagList("More about ",ref," personality")
      })
      
      output$sentiment <- renderPlotly({
        sent <- sapply(user_tl %>% select(text), FUN = textcleaner_twt) %>%
          data.frame()
        sent <- sent %>% unnest_tokens(word,text)
        sent_plot <- sent %>% inner_join(get_sentiments("bing")) %>%
          group_by(sentiment) %>%
          summarise(n = n()) %>%
          mutate(perc = n/sum(n),
                 text = paste(sentiment,": ",n,"(%",round(perc*100,1),")"))
        
        plot_sent <- sent_plot %>%
          ggplot(aes(x= n, y = sentiment, fill = sentiment, text = text)) +
          geom_col(width = 0.5) + labs(x="", y = "",fill="") + theme_minimal()
        
        ggplotly(plot_sent,tooltip = "text") %>%
          layout(height = 200,width = 420) %>%
          config(displayModeBar = F)
      })
      
      output$ext_f <- renderText({
        ext_lh <- ifelse(person$Extraversion <= 2.5,"Low","High")
        ext_col <- ifelse(person$Extraversion <= 2.5,"<font color=\"#26ad0e\"><b>","<font color=\"#c92812\"><b>")
        paste("Extraversion: ",ext_col,ext_lh,"</b></font>")
      })
      
      output$ext_explain <- renderUI({
        ext_high1 <- "You have high level on Extraversion, it means you are generally assertive, sociable, fun-loving, and outgoing. You thrive in social situations and feel comfortable voicing their opinions. You tend to gain energy and become excited from being around others."
        ext_high2 <- "You seems to be related to the emotional payoff that a person gets from achieving a goal. People who have high Extraversion, also called extroverts, while experience a victories, it seems that extroverts are especially thrilled by these victories, especially when they earn the attention of others. Getting a promotion, finding a new romance, or winning an award are all likely to bring an extrovert great joy."
        
        ext_low1 <- "You have low level on Extraversion, it means you often referred to as introverts. You tend to be more reserved and quieter. You prefer listening to others rather than needing to be heard."
        ext_low2 <- "Introverts like you often need periods of solitude in order to regain energy as attending social events can be very tiring. Of importance to note is that introverts do not necessarily dislike social events, but instead find them tiring. You don't experience as much of a “high” from social achievements like poeple with high extroversion did. You tend to be more content with simple, quiet lives, and rarely seek attention from others"
        
        HTML(ifelse(person$Extraversion <= 2.5,
                    paste(ext_low1,'<br/>','<br/>', ext_low2),
                    paste(ext_high1,'<br/>','<br/>',ext_high2)))
      })
      
      
      output$opn_f <- renderText({
        opn_lh <- ifelse(person$Openness <= 2.5,"Low","High")
        opn_col <- ifelse(person$Openness <= 2.5,"<font color=\"#26ad0e\"><b>","<font color=\"#c92812\"><b>")
        paste("Openness: ",opn_col,opn_lh,"</b></font>")
      })
      
      output$opn_explain <- renderUI({
        opn_high1 <- "People with high Openness like you tend to open to experience are perceived as creative and artistic. You prefer variety and value independence. You are curious about your surroundings and enjoy traveling and learning new things"
        opn_high2 <- "Openness is strongly related to a person’s interest in art and culture. People who are high in openness tend to enjoy the arts and seek out unusual, complex forms of self-expression."
        
        opn_low1 <- "People with low Opennes like you tend to prefer enjoying current routines. You don't seek something new and are uncomfortable with changes, You prefer the familiar over the unknown. Because you are a practical people, you often find it difficult to think creatively or abstractly."
        opn_low2 <- "Openness is strongly related to a person’s interest in art and culture. People who are low in openness are often suspicious of the arts and prefer to focus on more practical pursuits. "
        
        HTML(ifelse(person$Openness <= 2.5,
                    paste(opn_low1,'<br/>','<br/>', opn_low2),
                    paste(opn_high1,'<br/>','<br/>', opn_high2)))
      })
      
      output$agr_f <- renderText({
        agr_lh <- ifelse(person$Agreeableness <= 2.5,"Low","High")
        agr_col <- ifelse(person$Agreeableness <= 2.5,"<font color=\"#26ad0e\"><b>","<font color=\"#c92812\"><b>")
        paste("Agreeableness: ",agr_col,agr_lh,"</b></font>")
      })
      
      output$agr_explain <- renderUI({
        agr_high1 <- "Those high in agreeableness,like you, can be described as soft-hearted, trusting, and well-liked. You are sensitive to the needs of others and are helpful and cooperative. People regard them as trustworthy and altruistic."
        agr_high2 <- "Agreeableness describes a person’s tendency to put others’ needs ahead of their own, and to cooperate rather than compete with others. You often experience a great deal of empathy and tend to get pleasure out of serving and taking care of others."
        
        agr_low1 <- "Those low in agreeableness,like you, may be perceived as suspicious, manipulative, and uncooperative. You may be antagonistic when interacting with others, making you less likely to be well-liked and trusted."
        agr_low2 <- "You tend to experience less empathy and put their own concerns ahead of others. To make things even worse, psychologic said people like you have more conflictual relationships and often fall out with people."
        
        HTML(ifelse(person$Agreeableness <= 2.5,
                    paste(agr_low1,'<br/>','<br/>',agr_low2),
                    paste(agr_high1,'<br/>','<br/>',agr_high2)))
      })
      
      output$con_f <- renderText({
        con_lh <- ifelse(person$Conscientiousness <= 2.5,"Low","High")
        con_col <- ifelse(person$Conscientiousness <= 2.5,"<font color=\"#26ad0e\"><b>","<font color=\"#c92812\"><b>")
        paste("Conscientiousness: ",con_col,con_lh,"</b></font>")
      })
      
      output$con_explain <- renderUI({
        con_high1 <- "You have high score on conscientiousness. You can be described as organized, disciplined, detail-oriented, thoughtful, and careful person. You also have good impulse control, which allows you to complete tasks and achieve goals. "
        con_high2 <- "Conscientiousness describes a person’s ability to exercise self-discipline and control in order to pursue their goals. You are able to forego immediate gratification for the sake of long-term achievement."
        
        con_low1 <- "Those who score low on conscientiousness may struggle with impulse control, leading to difficulty in completing tasks and fulfilling goals. People like you end to be more disorganized and may dislike too much structure. They may also engage in more impulsive and careless behavior."
        con_low2 <- "The concept of Conscientiousness focuses on a dilemma we all face: shall I do what feels good now, or instead do what is less fun but will pay off in the future? People like you will choose fun in the moment"
        
        HTML(ifelse(person$Conscientiousness <= 2.5,
                    paste(con_low1,'<br/>','<br/>',con_low2),
                    paste(con_high1,'<br/>','<br/>',con_high2)))
      })
      
      output$neu_f <- renderText({
        neu_lh <- ifelse(person$Neuroticism <= 2.5,"Low","High")
        neu_col <- ifelse(person$Neuroticism <= 2.5,"<font color=\"#26ad0e\"><b>","<font color=\"#c92812\"><b>")
        paste("Neuroticism: ",neu_col,neu_lh,"</b></font>")
      })
      
      output$neu_explain <- renderUI({
        neu_high1 <- "Neuroticism describes a person’s tendency to experience negative emotions, including fear, sadness, anxiety, guilt, and shame. Those who score high on neuroticism, like you, often feel anxious, insecure and self-pitying. Your mood can be changed quickly. You re prone to excessive sadness and low self-esteem."
        neu_high2 <- "This trait can be thought of as an alarm system. People experience negative emotions as a sign that something is wrong in the world. High Neuroticism scorers are more likely to react to a situation with fear, anger, sadness, and the like."
        
        neu_low1 <- "Neuroticism describes a person’s tendency to experience negative emotions, including fear, sadness, anxiety, guilt, and shame. Those who score low on neuroticism,like you, are more likely to calm, secure and self-satisfied. You are more likely to have high self-esteem and remain resilient."
        neu_low2 <- "This trait can be thought of as an alarm system. People experience negative emotions as a sign that something is wrong in the world. Low Neuroticism scorers are more likely to brush off their misfortune and move on."
        
        HTML(ifelse(person$Neuroticism <= 2.5,
                    paste(neu_low1,'<br/>','<br/>',neu_low2),
                    paste(neu_high1,'<br/>','<br/>',neu_high2)))
      })
      
      output$MBTI_2 <- renderUI({
        mbti_link <- tags$a(href = "https://www.16personalities.com/intj-personality",person$MBTI)
        tagList(user_tl$name[1]," MBTI type is: ",mbti_link)
      })
      
      output$plot_ei <- renderPlotly({
        prob_ei <- person %>% select(Extraverted,Introverted) %>% mutate(type="EI") %>%
          pivot_longer(cols = c(Extraverted,Introverted))
        
        ei <- prob_ei %>%
          mutate(text = paste(name,round(value*100,1),"%")) %>%
          ggplot(aes(x= value, y= type, fill = name,text = text)) +
          geom_col(width = 0.2) + theme_minimal() +
          labs(x = "",y = "",fill = "") +
          scale_y_discrete(labels = NULL) + 
          scale_fill_manual(values = c("#51a9ab","#e5e6e7")) +
          theme(plot.margin = unit(c(5,10,5,10),"pt"))
        
        ggplotly(ei,tooltip = "text") %>%
          layout(height = 160,width = 600,
                 legend = list(orientation = "h", x = 0.27, y = -0.2)) %>%
          config(displayModeBar = F)
      })
      
      output$plot_ns <- renderPlotly({
        prob_ns <- person %>% select(Intuitive,Observant) %>% mutate(type="NS") %>%
          pivot_longer(cols = c(Intuitive,Observant))
        
        ns <- prob_ns %>%
          mutate(text = paste(name,round(value*100,1),"%")) %>%
          ggplot(aes(x= value, y= type, fill = name,text = text)) +
          geom_col(width = 0.2) + theme_minimal() +
          labs(x = "",y = "",fill = "") +
          scale_y_discrete(labels = NULL) + 
          scale_fill_manual(values = c("#e5e6e7","#e2a942")) +
          theme(plot.margin = unit(c(5,10,5,10),"pt"))
        
        ggplotly(ns,tooltip = "text") %>%
          layout(height = 160,width = 600,
                 legend = list(orientation = "h", x = 0.27, y = -0.2)) %>%
          config(displayModeBar = F)
      })
      
      output$plot_tf <- renderPlotly({
        prob_tf <- person %>% select(Thinking,Feeling) %>% mutate(type="TF") %>%
          pivot_longer(cols = c(Thinking,Feeling))
        
        tf <- prob_tf %>%
          mutate(text = paste(name,round(value*100,1),"%")) %>%
          ggplot(aes(x= value, y= type, fill = name,text = text)) +
          geom_col(width = 0.2) + theme_minimal() +
          labs(x = "",y = "",fill = "") +
          scale_y_discrete(labels = NULL) + 
          scale_fill_manual(values = c("#56ac8a","#e5e6e7")) +
          theme(plot.margin = unit(c(5,10,5,10),"pt"))
        
        ggplotly(tf,tooltip = "text") %>%
          layout(height = 160,width = 600,
                 legend = list(orientation = "h", x = 0.27, y = -0.2)) %>%
          config(displayModeBar = F)
      })
      
      output$plot_jp <- renderPlotly({
        prob_jp <- person %>% select(Judging,Prospecting) %>% mutate(type="JP") %>%
          pivot_longer(cols = c(Judging,Prospecting))
        
        jp <- prob_jp %>%
          mutate(text = paste(name,round(value*100,1),"%")) %>%
          ggplot(aes(x= value, y= type, fill = name,text = text)) +
          geom_col(width = 0.2) + theme_minimal() +
          labs(x = "",y = "",fill = "") +
          scale_y_discrete(labels = NULL) + 
          scale_fill_manual(values = c("#e5e6e7","#cfa0b6")) +
          theme(plot.margin = unit(c(5,10,5,10),"pt"))
        
        ggplotly(jp,tooltip = "text") %>%
          layout(height = 160,width = 600,
                 legend = list(orientation = "h", x = 0.27, y = -0.2)) %>%
          config(displayModeBar = F)
      })
      
      
      
      
      shinyjs::addClass(selector = "body",class = "sidebar-collapse")
      
    }else{
      showModal(modalDialog("Please read the terms & condition first"))
    }
    
  })
  
}

