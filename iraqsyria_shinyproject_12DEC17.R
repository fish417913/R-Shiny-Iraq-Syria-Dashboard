library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
library(ggthemes)
library(shinythemes)




ui <- fluidPage(theme = shinytheme("cyborg"),
                
                titlePanel("Terrorism in Syria and Iraq"),
                
                
                
                navlistPanel(
                  "Syria Overview",
                  tabPanel("Summary",
                           h1("Summary of Terrrorism in Syria (1974-2016)"),
                           p("This project focuses on the problem set of terrorism in the Syrian Arab Republic.  Though Syria
                             is no stranger to the problem of violence commmitted by terrorist organizations, it has been
                             largely unscathed since the dawn of modern terrorism in the mid 20th century.  However, there are 
                             two exceptions to this trend."),
                           p("The first trend ranges from the early to mid 1980's.  An uprising launched by forces associated
                             with the Muslim Brotherhood launched a campaign of terrorist violence which targeted the Syrian
                             government, led by the regime of Hafez al-Asad.  This period of violence was brutally and effectively 
                             ended by Syrian security forces during the infamous Hama Massacre in 1982."),
                           p("The Muslim Brotherhood and those sympathetic to it quickly vacated Syria in the aftermath of Hama.
                             Even in the aftermath of 9/11 and the subsequent American-led invasion of neighbor Iraq, this
                             would change with chaotic conditions induced by the Arab Spring of 2011."),
                           p("After regime forces brutally tortured a group of non-violent teenage protestors in March 2011,
                             the heavy handed over-reaction of Bashar al-Assad's security forces would light the spark for what
                             would become a tragically violent civil war.  This war would quickly become usurped by a horiffically
                             violent terrorist group called the Islamic State of Iraq and Syria (ISIS), as well as other religiously
                             and ethnically motivated groups and militias.  In the name of defeating terrorism, the American-led
                             military coalition would engage in direct combat against ISIS and other violent organizations, while
                             engaged in proxy warfare with not only the Syrian regime, but also Russia, Iran and Turkey."),
                           p("Even with such a heavy presence of multi-national military forces, Syria continues to suffer from
                             terrorism.  As world powers move into the next phase of their strategic maneuvering, Syria will
                             no doubt continue to be plagued by terrorism well into the future."),
                           p("This application seeks to use analytics in order to identify patterns in data recorded
                             by the National Consortium For The Study Of Terrorism And Responses To Terrorism which may serve
                             as a force multiplier for military decision makers as they seek to make make data driven decisions
                             on this matter and those like it in the future.")
                           ),
                  
                  
                  tabPanel("Key Trends", 
                           h1("Themes Discovered In The START Global Terrorism Database"),
                           tags$ol(
                             tags$li("During the aforementioned 1980's terrorism, annual attacks spiked at about 50.  
                                     After the Muslim Brotherhood was quelled, Syria was virtually terror free until 2011.
                                     2011's Arab Spring ushered in civil strive in Syria, which was harshly repressed by 
                                     the Assad regime.  The populace rose up against the oppression, which was met with even
                                     harsher violence by the regime.  At the same time, foreign terrorist organizations and 
                                     domestic militias infiltrated the conflict and became embroiled in bitter and intense
                                     warfare.  Specifically, 2011 saw less than 100 attacks in the intial stages of violence.
                                     In linear fashion, terrorist attacks increased every year, with an astounding 490 in 2015.
                                     It is perhaps slightly comforting to note the slight decrease in violence from 2015 to 2016."),
                             tags$li("The tactic of choice for terrorists in Syria has been the category of Explosives/Bombs/Dynamite. 
                                     This indicates that terrorists are primarily intersted in conducting standoff attacks, 
                                     allowing them to attack the target without risking casualties in a counter-attack.
                                     It is worth noting that use of chemcical weapons has increased every year since 2012."),
                             tags$li("Aside from using explosives as a primary attack method, armed assaults are a 
                                     close second.  It is posssible that this is due to the influx of ISIS foreign fighters with
                                     previous combat experience, as well as  experienced proxy forces loyal to external parties."),
                             tags$li("The number of deaths in  the 1980's was minimal, with an outlier at 
                                     around 100.  Beginning in 2011, most deaths were still trending at a low
                                     level but with significantly more outliers.  Eventually, we can see numerous data points between 
                                     200-500 casualties.   Keeping in mind the widespread use of bombings in Syria during this
                                     timeframe, it paints an accurate picture of the psychological and physical horror inflicted
                                     on the Syrian people."),
                             tags$li("Almost every victim of terrorism in Syria, regardless of location, is a Syrian, while
                                     the majority of terrorism occurs in Aleppo, Damascus, Idlib and Hasakah.")
                             )),
                  
                  "Iraq Overview",
                  tabPanel("Summary",
                           h1("Summary of Terrorism in Iraq (1975-2016)"),
                           p("This project focuses on the problem set of terrorism in the Republic of Iraq.  Much like Syria, Iraq was virtually immune to terrorism, until the commencement of Operation Iraqi Freedom (OIF) in March 2003. 
                             Clearly, the repressive regime of Saddam Hussein created a non-permissive environment for terrorism, though very small numbers of attacks occurred in an irregular manner in the 1980s and 1990s."),
                           p("Even with a brutal war with Iran (1980-1988), subsequently followed by the Persian Gulf War (1990-1991) terrorist attacks in Iraq never surged beyond 35 in a year (1992).  
                             When Saddam's regime was overthrown during OIF, the bandages covering social conflicts in Iraq were ripped off.  
                             As a result, ethnic, tribal, religious and political scores were to be settled via terrorist attacks."),
                           p("From 2003-2014, annual terrorist attacks in Iraq increased linearly from 87 (2003) to a staggering 3,926 (2014).  
                             Much of this violence was the anti-American insurgency being waged by a variety of actors, including Iranian proxy groups, Islamist terrorist groups such as Al Qaeda in Iraq, local militias and common criminals.
                             However, once American troops left Iraq at the end of 2011, many were hopeful that the Iraqi government could continue the positive momentum to build even more security."),
                           p("Unfortunately, Prime Minister Nouri al-Maliki's authoritarian tendencies and clear anti-Sunni sectarian biases inflamed the social wounds.  Much like in Syria, the cause of these social issues was not examined; 
                             rather, the Iraqi Security Forces used lethal violence against Sunni demonstrators in order to solve the problem.  
                             This, of course, only inflamed the situation, creating more violence on both sides.  Ultimately, the Islamic State of Iraq and Syria (ISIS) took advantage of the situation and ostensibly took up arms to defend fellow Sunnis."),
                           p("While ISIS may have acted as the protector of Sunni Iraqis, it actually sought to physically annex Iraq through chaos via mass casualty terrorist attacks and guerilla warfare.
                             By the summer of 2014, ISIS had control of large portions of Iraqi territory, and had routed the Iraqi Security Forces on numerous occasions, taking abandoned weaponry and vehicles as spoils of war."),
                           p("Eventually, ISIS emir Abu Bakr al-Baghdadi declared portions of Iraq and Syria controlled by ISIS a new Caliphate and directed like minded adherents
                             from across the globe to report for duty in the war against the regimes in Iraq, Syria and the US-led Coalition which had arrived to bomb ISIS and train the Iraqi military.
                             This is reflected in the data, with 2015 showing a decrease in attacks (2744), while 2016 increased yet again (3356)."),
                           p("Currently, ISIS has mostly been militarily defeated and expelled from most of the territory it controlled in Iraq.  
                             Yet, the conditions that allowed ISIS to fester and wage a campaign of terror still exist.  
                             Unless and until those conditions improve, it is quite likely that Iraq, which has endured 22,130 terrorist attacks since 1975, will continue to be plagued by terrorism and political violence.")
                           
                           ),
                  
                  tabPanel("Key Trends",
                           h1("Themes Discovered In The START Global Terrorism Database"),
                           tags$ol(
                             tags$li("As noted previously, Iraqi terrorism consists of two main trends.  The first trend, is a near absence of terrorism, with a maximum annual amount of 35 in 1992.
                                     This trend, which often contained years with zero attacks, lasted through 2002 (6 attacks).  The second trend is a linear explosion of terrorism, from 2003 through 2016.
                                     To be clear, the attack trend was linear from 2003-2014, with a decrease in 2015, followed by an increase in 2016.  
                                     It would be fair to assess that it will be quite some time before Iraq experiences decreased levels of attacks on the order of 2002 or even 2003."),
                             tags$li("As the number of attacks increases significant from 2003 on, we see that bombing becomes extremely widespread as the weapon of choice for terrorists in Iraq.
                                     In 2003, Iraq had 70 recorded terrorist bombings, while bombings peaked at 2,984 in 2015.  
                                     This is very telling, because even in the worst days of OIF with the IEDs and targeted bombings of civilians, terrorists did not even come close to this level of carnage.
                                     Explosives are often the preferred weapon of terrorists, due to its destruction and psychological impact, and Iraq is certainly proof of this."),
                             tags$li("With such prolific bombing, deaths from terrorism in Iraq are surprisingly stable, statistically speaking.
                                     Generally, a baseline of sorts exists beginning in 2003 where deaths are steady at 250 and under.  Let us not get lost in the data though. 
                                     Even with a decently transparent scatterplot, it is obvious that the data has a certain level of overplotting and what appears to exist is a
                                     large number of events with relatively low numbers of deaths per event. However, in 2013 we see outliers at 500 and above, 
                                     including a point at 1500 deaths in 2014.  Iraq has endured significant carnage, but with such high levels of attacks, it is astonishing that the baseline level of deaths is not higher."),
                             tags$li("As was the case in Syria, the clear majority of terrorist victims in Iraq are Iraqi.  It is worth noting what appears to be two trends in the data.
                                     The first trend runs from 1975-2002 in the low-frequency period of attacks.  Though it appears that Iraqi's are the majority, 
                                     the remainder of targets in this period appear to be a mixed bag, with heavy representation of Turkish, Iranian and European victims.
                                     From 2003-2016, we see what appears to be two primary targets: Iraqis and Americans."),
                             tags$li("Continuing to view the data chronologically in the low-frequency (1975-2002) and high-frequency (2003-2016) periods, it is interesting to assess terrorist attack locations.
                                     Specifically, the low-frequency period does not appear to have a singularly dominant province, though Baghdad province seems to have a significant amount.
                                     Yet, the remainder of the low-frequency attacks appear to be well distributed throughout much of Iraq.  However, the high-frequency period is dominated by five main locations: Al-Anbar, Baghdad, Diyala, Kirkuk, Nineveh and Saladin.
                                     These appear to synch up with historically high density attack locations during OIF and the recent reign of ISIS terrorism.")
                             )
                           
                             ),
                  
                  
                  "Geospatial Data",
                  tabPanel("Interactive Map of Syria", leafletOutput("topomap1")),
                  tabPanel("Interactive Map of Iraq", leafletOutput("topomap2")),
                  "Interactive Visualizations",
                  tabPanel("Syria Attacks Per Year", plotlyOutput("year1")),
                  tabPanel("Iraq Attacks Per Year", plotlyOutput("year2")),
                  tabPanel("Weapons Used in Syria", plotlyOutput("weapons1")),
                  tabPanel("Weapons Used in Iraq", plotlyOutput("weapons2")),
                  tabPanel("Syria Attack Type", plotlyOutput("attacktype1")),
                  tabPanel("Iraq Attack Type", plotlyOutput("attacktype2")),
                  tabPanel("Deaths in Syria", plotlyOutput("deaths1")),
                  tabPanel("Deaths in Iraq", plotlyOutput("deaths2")),
                  tabPanel("Nationality Targeted in Syria", plotlyOutput("tgtnation1")),
                  tabPanel("Nationality Targeted in Iraq", plotlyOutput("tgtnation2")),
                  tabPanel("Syria Attacks By Province", plotlyOutput("province1")),
                  tabPanel("Iraq Attacks By Province", plotlyOutput("province2")),
                  tabPanel("Syria Interactive Data Table", 
                           sliderInput("myslider", "Number of columns to display", min = 1, max = 19,
                                       value = 19),
                           shiny::dataTableOutput("dataset")),
                  tabPanel("Iraq Interactive Data Table",
                           sliderInput("myslider1", "Number of columns to display", min = 1,
                                       max = 19, value = 19),
                           shiny::dataTableOutput("dataset1"))
                             )
                
                             )






server <- function(input, output) ({
  
  syria_stats <- syria_terror3 %>% 
    mutate(popup_info = paste("<b>Target:<b>", primary_target, "<br />",
                              "<b>Terrorist Group:<b>", primary_group, "<br />",
                              "<b>Weapon Used:<b>", weaptype1, "<br />",
                              "<b>Year:<b>", year, "<br />"))
  
  iraq_stats <- iraq_geodata %>% 
    mutate(popup_info = paste("<b>Target:<b>", primary_target, "<br />",
                              "<b>Terrorist Group:<b>", primary_group, "<br />",
                              "<b>Weapon Used:<b>", weaptype1, "<br />",
                              "<b>Year:<b>", year, "<br />"))
  
  
  output$topomap1 <- renderLeaflet({
    syria_stats %>% 
      leaflet() %>% 
      addProviderTiles(providers$Stamen.Terrain) %>% 
      addMarkers(popup = ~popup_info, clusterOptions = markerClusterOptions())
  })
  
  output$topomap2 <- renderLeaflet({
    iraq_stats %>% 
      leaflet() %>% 
      addProviderTiles(providers$Stamen.Terrain) %>% 
      addMarkers(popup = ~popup_info, clusterOptions = markerClusterOptions())
  })
  
  
  output$year1 <- renderPlotly({
    ggplot(syria_terror3, aes(x = year)) +
      geom_bar(position = "identity", fill = "red") + 
      labs(title = "Syrian Terrorism Since 1974", x = "Attacks Per Year") + 
      theme_tufte()
    ggplotly()
  })
  
  output$year2 <- renderPlotly({
    ggplot(iraq_terror2, aes(x = year)) +
      geom_bar(position = "identity", fill = "red") + 
      labs(title = "Iraqi Terrorism Since 1975", x = "Attacks Per Year") + 
      theme_tufte()
    ggplotly()
  })
  
  output$weapons1 <- renderPlotly({
    ggplot(syria_terror3, aes(x = year, fill = weaptype1)) + 
      geom_bar() + scale_fill_brewer(palette = "Set1") + 
      labs(title = "Weapons Used In Attacks (2010-2016)") + 
      theme_tufte() +
      scale_x_continuous(limits = c(2010, 2016))
    ggplotly()
  })
  
  output$weapons2 <- renderPlotly({
    ggplot(iraq_terror2, aes(x = year, fill = weaptype1)) + 
      geom_bar() + scale_fill_brewer(palette = "Set1") + 
      labs(title = "Weapons Used In Attacks (2001-2016)") + 
      theme_tufte() +
      scale_x_continuous(limits = c(2001, 2016))
    ggplotly()
  })
  
  output$attacktype1 <- renderPlotly({
    ggplot(syria_terror3, aes(x = year, fill = primary_attacktype)) + 
      geom_bar() + scale_fill_brewer(palette = "Set1") + 
      labs(title = "Type Of Attack (2010-2016)") + 
      theme_tufte() + 
      scale_x_continuous(limits = c(2010, 2016))
    ggplotly()
  })
  
  output$attacktype2 <- renderPlotly({
    ggplot(iraq_terror2, aes(x = year, fill = primary_attacktype)) + 
      geom_bar() + scale_fill_brewer(palette = "Set1") + 
      labs(title = "Type Of Attack (2001-2016)") +
      theme_tufte() + 
      scale_x_continuous(limits = c(2001, 2016))
    ggplotly()
  })
  
  output$deaths1 <- renderPlotly({
    ggplot(syria_terror3, aes(x = year, y = nkill)) + 
      geom_jitter(alpha = 0.6, shape = 1, color = "#FF1A1A") + 
      labs(title = "Deaths Per Year (2010-2016)") + 
      theme_tufte() + 
      scale_x_continuous(limits = c(2010, 2016))
    ggplotly()
  })
  
  output$deaths2 <- renderPlotly({
    ggplot(iraq_terror2, aes(x = year, y = nkill)) + 
      geom_jitter(alpha = 0.6, shape = 1, color = "#FF1A1A") + 
      labs(title = "Deaths Per Year (2001-2016)") + 
      theme_tufte() + 
      scale_x_continuous(limits = c(2001, 2016))
    ggplotly()
  })
  
  output$tgtnation1 <- renderPlotly({
    ggplot(syria_geodata, aes(x = provstate, y = year, color = target_nationality)) + 
      geom_jitter() + coord_flip() + 
      labs(title = "Target Nationality") + 
      theme_tufte()
    ggplotly()
  })
  
  output$tgtnation2 <- renderPlotly({
    ggplot(iraq_geodata, aes(x = provstate, y = year, color = target_nationality)) + 
      geom_jitter() + coord_flip() + 
      labs(title = "Target Nationality") + 
      theme_tufte()
    ggplotly()
  })
  
  
  output$province1 <- renderPlotly({
    ggplot(syria_geodata, aes(x = provstate, y = year, color = primary_attacktype)) + 
      geom_jitter() + coord_flip() + 
      labs(title = "Attacks Per Location") + 
      theme_tufte() 
    ggplotly()
  })
  
  output$province2 <- renderPlotly({
    ggplot(iraq_geodata, aes(x = provstate, y = year, color = primary_attacktype)) + 
      geom_jitter() + coord_flip() + 
      labs(title = "Attacks Per Location") + 
      theme_tufte()
    ggplotly()
  })
  
  
  df <- reactive({
    input$myslider
    syria_final[1:input$myslider]
  })
  
  output$dataset <- shiny::renderDataTable(df())
  
  df1 <- reactive({
    input$myslider1
    iraq_geodata[1:input$myslider]
  })
  
  output$dataset1 <- shiny::renderDataTable(df1())
  
})




shinyApp(ui, server)


