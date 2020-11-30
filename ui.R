#ui
ui <- dashboardPage(title = "Tweet Personality Prediction",skin = "blue",
                    dashboardHeader(title = "Tweet Personality Prediction"),
                    dashboardSidebar(
                      sidebarMenu(id = "tabs",
                                  menuItem(text = "Twitter",tabName = "twitter",
                                           icon = icon("twitter")),
                                  menuItem(text = "Text Input", tabName = "text",
                                           icon = icon("comment-dots")),
                                  menuItem(text = "About",tabName = "about",
                                           icon = icon("question")),
                                  menuItem(text = "",tabName = "space")
                      )
                    ),
                    dashboardBody(
                      tags$head(tags$style(HTML('.box {margin: 3px;}
                                          div.col-sm-12 {padding:1px;}'),
                                           HTML(".icon_size { font-size: 38px; }"),
                                           HTML(".icon-large {bottom: -15px;}"),
                                           HTML(".inner {height: 65px;}"),
                                           HTML(".inner a {color: #f7f7f7;"),
                                           HTML(".col-sm-6:nth-child(1) {padding-right:3px; height: 75px;}"),
                                           HTML(".col-sm-6+ .col-sm-6 {padding-left:3px; height: 75px;}"),
                                           HTML("hr+ .row .col-sm-12 { padding-left: 12px; }")
                      ),
                      ),
                      shinyjs::useShinyjs(),
                      tabItems(
                        # First Page
                        tabItem(tabName = "twitter",
                                fluidPage(
                                  div(h1("Twitter-Text Personality Prediction", align = "center")),
                                  div(h3("Big 5 Personality Traits & Myer-Briggs Type Indicator",align = "center")),
                                  tags$br(),
                                  box(width = 12,status = "info",align = "center",
                                      img(src = "twitter_icon.png",height = 80, width = 80),
                                      textInput(inputId = "username_ui",label = NULL, width = "30%",
                                                value = "Twitter Username..."),
                                      actionButton(inputId = "click_u",label = "Predict!",class = "btn-primary"),
                                      checkboxInput(inputId = "check_terms",
                                                    label = "I've read and accept the terms & conditions")
                                  ),
                                  tags$br(),
                                  tags$hr(),
                                  tags$br(),
                                  fluidRow(
                                    box(width = 6, title = "Service's Terms",solidHeader = TRUE,status = "info",
                                        p("- These terms apply to all visitors, users, and others who access or use the service"),
                                        p(strong("- Our service are provided for information and entertainment purposes only and for no other purpose.")),
                                        p("- We make no claims or representations in relation to the emotional, health or commercial benefits of using our Products and the information provided on the Website is no substitute for professional medical or psychiatric advice where applicable."),
                                        p("- You must be over the age of 13 years (or above the relevant age of consent in your country) to use the service and most importantly the Twitter itself."),
                                        p("- By accessing this service you agree to be bound by these terms. If you disagree with any part of these terms but do the predict anyway then probably you are not reading this but that's ok since i can't do anything"),
                                        p("- Our service should not be regarded as or relied upon as being a comprehensive opinion or assessment concerning your psychological well-being."),
                                        p("- By clicking 'Predict!' you are agreed to help to improve this app by let us save your Twitter data and the personality results"),
                                        p("- Your Twitter data will be used for this app model improvement only and not to be shared publicly")
                                    ),
                                    box(width = 6, title = "Conditions",solidHeader = TRUE,status = "info",
                                        p("The Twitter account need to meet this criteria for the best experience:"),
                                        p("- The tweets must be written in english"),
                                        p("- The Twitter account need to be set in public"),
                                        p("- The Twitter account need to have at least 300 tweets"),
                                        p("- The big 5 personality traits and Myer-Briggs Type Indicator (also known as MBTI) result may not accurate for you since the real test is using creadibile research and mostly not by text"))
                                  )
                                  
                                )
                        ),
                        # Second page
                        tabItem(tabName = "text"),
                        
                        # Third Page
                        tabItem(tabName = "about",
                                fluidPage(
                                  fluidRow(
                                    column(width = 4,
                                           box(width = 12,status = "info",
                                               div(h3("Author",align = "center")),
                                               div(align = "center",img(src = "profile_icon.png",height = 80, width = 80)),
                                               column(width = 12,
                                                      div(h5("Joe Cristian is a data science instructor at Algoritma Data Science Education Center located in Jakarta, Indonesia. Have a degree in Management Business Telecomuncation and Informatics and are interested in social computing or human behaviour based data science projects ever since"))),
                                               fluidRow(valueBox(value = tags$p(tags$a(href = "https://github.com/western11/Twitter-Text_Personality_Prediction","Github"),style = "font-size: 67%;"),
                                                                 icon = icon("github",class = "icon_size"),color = "olive",subtitle = HTML("&nbsp;"),width = 6),
                                                        valueBox(value = tags$p(tags$a(href = "https://twitter.com/JoeChristianP","Twitter"),style = "font-size: 67%;"),
                                                                 icon = icon("twitter",class = "icon_size"),color = "teal",subtitle = HTML("&nbsp;"),width = 6)
                                               ),
                                               div(h4("This ShinyApp is build with guidance by",align = "center")),
                                               div(align = "center",tags$a(href = "https://algorit.ma/",tags$img(src = "algo_logo.png",height = 56, width = 212)))
                                           )),
                                    column(width = 8,
                                           column(width = 11,
                                                  div(h2(strong("What WebApp is this?"))),
                                                  p("There are lots of academic publications that prove human personality can be predicted by their writings,",tags$a(href="https://ieeexplore.ieee.org/abstract/document/7436992","this "),"and ",tags$a(href="https://link.springer.com/article/10.1007/s10489-018-1212-4","this"), " for example. This webapp is built to predict Big 5 Personality Traits and Myer-Briggs Type Indicator (MBTI) based on Twitter' user tweets. The main objective of this project is to help organizations, especially HR department, to understand people' personalities based on what they write on social media. It surely can help them in the filtering process before recruiting a new employee or member. This project is open to public in order to 'make it perfect', i believe with more input from the community can make the model predictions better and more precise."),
                                                  div(h2(strong("How it works?"))),
                                                  p("First we take user' tweets (not including retweets) using", tags$a(href="https://developer.twitter.com/en"," Twtter rest API "),"and clean the text. We only use english text because the only available personality-text dataset is written in english. The final step on the text cleaning process is only to use selected words that match the prediction model (approximately 900+ words). We use classification method to predict the MBTI, one model for each traits (so there are 4 binary classification model for MBTI). And regression method to predict the 5 personality traits score, one model for each traits (so there are 5 regression models for big 5 personality traits). Every model using Random Forest Algorithm with different parameter and performance (see below). The output then visualized and the final report is generated based on the output, both 5 personlity traits and the MBTI."))
                                    )
                                  ),
                                  tags$hr(),
                                  fluidRow(
                                    column(width = 12,
                                           div(h2(strong("How is the model' performance?"))),
                                           column(width = 6, plotlyOutput(outputId = "mbti_eval")),
                                           column(width = 6,plotlyOutput(outputId = "person_eval"))
                                    )
                                  )
                                )
                        ),
                        tabItem(tabName = "space",
                                fluidPage(
                                  div(h2(textOutput(outputId = "pname"),align = "center")),
                                  tags$hr(),
                                  fluidRow(
                                    column(width = 4,
                                           fluidRow(box(width = 12, height = 230,title = "Twitter Profile",status = "info",
                                                        fluidRow(
                                                          column(3,uiOutput("prof_pict")),
                                                          column(9,htmlOutput("detail"))
                                                        ),
                                                        fluidRow(
                                                          tags$br(),
                                                          column(11,textOutput("last_tweet"))
                                                        ))),
                                           fluidRow(box(width = 12,height = 270,title = "Tweet Sentiment",status = "info",
                                                        plotlyOutput("sentiment")))
                                    ),
                                    column(width = 5,                
                                           box(width = 12, height = 507, title = "Personality Traits",status = "info", solidHeader = TRUE,
                                               plotlyOutput("radar",width = "100%")
                                           )),
                                    column(width = 3,
                                           fluidRow(box(width = 12,height = 300, status = "info",align = "center",
                                                        div(h4("Myer-Briggs Type Indicator",align = "center")),
                                                        div(h2(htmlOutput("MBTI"),align = "center")),
                                                        uiOutput("mbti_icon"))),
                                           fluidRow(box(width = 12,height = 200,status = "info",
                                                        div(h4("Cooming soon",align = "center")))))
                                  ),
                                  tags$hr(),
                                  div(h2(htmlOutput("more")),align = "center"),
                                  fluidRow(
                                    box(width = 6,height = 1370, status = "primary",
                                        column(width = 11,
                                               div(h3(strong("Personality Traits Scores"))),
                                               p("This Big Five assessment measures your scores on five major dimensions of personality: Openness, Conscientiousness, Extraversion, Agreeableness, and Neuroticism (sometimes abbreviated OCEAN). It is important to note that each of the five personality factors represents a range between two extremes. For example, extraversion represents a continuum between extreme extraversion and extreme introversion. In the real world, most people lie somewhere in between the two polar ends of each dimension."),
                                               
                                               #p("Extraversion reflects the tendency and intensity to which someone seeks interaction with their environment, particularly socially. It encompasses the comfort and assertiveness levels of people in social situations. Additionally, it also reflects the sources from which someone draws energy."),
                                               div(h3(htmlOutput("ext_f"))),
                                               p(htmlOutput("ext_explain")),
                                               #p("Extraversion describes a person’s inclination to seek stimulation from the outside world, especially in the form of attention from other people. Extraverts engage actively with others to earn friendship, admiration, power, status, excitement, and romance. Introverts, on the other hand, conserve their energy, and do not work as hard to earn these social rewards."),
                                               #p("Extraversion seems to be related to the emotional payoff that a person gets from achieving a goal. While everyone experiences victories in life, it seems that extroverts are especially thrilled by these victories, especially when they earn the attention of others. Getting a promotion, finding a new romance, or winning an award are all likely to bring an extrovert great joy. In contrast, introverts do not experience as much of a “high” from social achievements. They tend to be more content with simple, quiet lives, and rarely seek attention from others."),
                                               div(h3(htmlOutput("opn_f"))),
                                               p(htmlOutput("opn_explain")),
                                               #p("Openness describes a person’s tendency to think in abstract, complex ways. High scorers tend to be creative, adventurous, and intellectual. They enjoy playing with ideas and discovering novel experiences. Low scorers tend to be practical, conventional, and focused on the concrete. They tend to avoid the unknown and follow traditional ways."),
                                               #p("Openness is strongly related to a person’s interest in art and culture. People who are high in openness tend to enjoy the arts and seek out unusual, complex forms of self-expression. People who are low in openness are often suspicious of the arts and prefer to focus on more practical pursuits."),
                                               div(h3(htmlOutput("agr_f"))),
                                               p(htmlOutput("agr_explain")),
                                               #p("Agreeableness describes a person’s tendency to put others’ needs ahead of their own, and to cooperate rather than compete with others. People who are high in Agreeableness experience a great deal of empathy and tend to get pleasure out of serving and taking care of others. They are usually trusting and forgiving."),
                                               #p("People who are low in Agreeableness tend to experience less empathy and put their own concerns ahead of others. Low scorers are often described as hostile, competitive, and antagonistic. They tend to have more conflictual relationships and often fall out with people."),
                                               div(h3(htmlOutput("con_f"))),
                                               p(htmlOutput("con_explain")),
                                               #p("Conscientiousness describes a person’s ability to exercise self-discipline and control in order to pursue their goals. High scorers are organized and determined, and are able to forego immediate gratification for the sake of long-term achievement. Low scorers are impulsive and easily sidetracked."),
                                               #p("The concept of Conscientiousness focuses on a dilemma we all face: shall I do what feels good now, or instead do what is less fun but will pay off in the future? Some people are more likely to choose fun in the moment, and thus are low in Conscientiousness. Others are more likely to work doggedly toward their goals, and thus are high in this trait."),
                                               div(h3(htmlOutput("neu_f"))),
                                               p(htmlOutput("neu_explain"))
                                               #p("Neuroticism describes a person’s tendency to experience negative emotions, including fear, sadness, anxiety, guilt, and shame. While everyone experiences these emotions from time to time, some people are more prone to them than others."),
                                               #p("This trait can be thought of as an alarm system. People experience negative emotions as a sign that something is wrong in the world. You may be in danger, so you feel fear. Or you may have done something morally wrong, so you feel guilty. However, not everyone has the same reaction to a given situation. High Neuroticism scorers are more likely to react to a situation with fear, anger, sadness, and the like. Low Neuroticism scorers are more likely to brush off their misfortune and move on.")
                                        )
                                    ),
                                    box(width = 6,height = 1370,status = "primary",
                                        column(width = 11,
                                               div(h3(strong("Myer-Briggs Type Indicator (MBTI)"))),
                                               p("The MBTI is an introspective self-report questionnaire indicating differing psychological preferences in how people perceive the world and make decisions. The MBTI is based on the conceptual theory proposed by Swiss psychiatrist Carl Jung, who had speculated that people experience the world using four principal psychological functions – sensation, intuition, feeling, and thinking – and that one of these four functions is dominant for a person most of the time. The four categories are Introversion/Extraversion, Sensing/Intuition, Thinking/Feeling, Judging/Perception. Each person is said to have one preferred quality from each category, producing 16 unique types."),
                                               tags$br(),
                                               div(h4(uiOutput("MBTI_2"))),
                                               tags$br(),
                                               fluidRow(div(h4("Mind",align = "center")),
                                                        div(h5("This trait determines how we interact with our environment.",align="center")),
                                                        column(width = 11, align = "center",plotlyOutput("plot_ei",height = "200px"))),
                                               fluidRow(div(h4("Energy",align="center")),
                                                        div(h5("This trait shows where we direct our mental energy.",align = "center")),
                                                        column(width = 11, align = "center",plotlyOutput("plot_ns",height = "200px"))),
                                               fluidRow(div(h4("Nature",align="center")),
                                                        div(h5("This trait determines how we make decisions and cope with emotions.",align = "center")),
                                                        column(width = 11, align = "center",plotlyOutput("plot_tf",height = "200px"))),
                                               fluidRow(div(h4("Tactics",align="center")),
                                                        div(h5("This trait reflects our approach to work, planning and decision-making.",align = "center")),
                                                        column(width = 11, align = "center",plotlyOutput("plot_jp",height = "200px")))
                                        )
                                    )
                                  )
                                )
                        )
                      )
                    )
)


