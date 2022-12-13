# Required libraries
library(shiny)
library(rvest)
library(xml2)
library(tidyverse)

# Process the data
webpage <- read_html("https://www.pordata.pt/portugal/peixe+capturado+total+e+por+principais+especies-3450")
data <- webpage %>% html_elements("table") %>% .[[13]] %>% html_table()
data <- data[3:55, ]
data[1:53, ] <- apply(data[1:53, ], 2, function(x)(gsub("\\.", "", x, perl=TRUE)))
data[1:53, ] <- apply(data[1:53, ], 2, function(x)(gsub("x", NA, x, perl=TRUE)))
data[, 1:21] <- apply(data, 2, as.numeric)
colnames(data) <- c("ano", "total", "Todos.peixes.de.água.doce", "Lampreia", "Sável", "Enguias",
                    "Todos.peixes.marinhos", "Atum", "Biqueirão", "Carapau", "Cavala", "Peixe.espada.preto", "Sardinha",
                    "Todos.crustáceos", "Gambas", "Lagostim", "Caranguejo",
                    "Todos.moluscos", "Berbigão", "Choco", "Polvo")

data.total <- data[1:53, 1:2]

data.all <- data[34:53, -2]
data.all$Outros.peixes.de.água.doce <- apply(data.all, 1, function(table)(table["Todos.peixes.de.água.doce"] - table["Lampreia"] - table["Sável"] - table["Enguias"]))
data.all$Outros.peixes.marinhos <- apply(data.all, 1, function(table)(table["Todos.peixes.marinhos"] - table["Atum"] - table["Biqueirão"] - table["Carapau"] - table["Cavala"] - table["Peixe.espada.preto"] - table["Sardinha"]))
data.all$Outros.crustáceos <- apply(data.all, 1, function(table)(table["Todos.crustáceos"] - table["Gambas"] - table["Lagostim"] - table["Caranguejo"]))
data.all$Outros.moluscos <- apply(data.all, 1, function(table)(table["Todos.moluscos"] - table["Berbigão"] - table["Choco"] - table["Polvo"]))

data.all.long <- data.all %>%
  pivot_longer(cols=-ano, names_to="especie", values_to="toneladas")

data.all.long$especie <- factor(data.all.long$especie,
                                levels=c("Todos.peixes.de.água.doce", "Enguias", "Lampreia", "Sável", "Outros.peixes.de.água.doce",
                                         "Todos.peixes.marinhos", "Atum", "Biqueirão", "Carapau", "Cavala", "Peixe.espada.preto", "Sardinha", "Outros.peixes.marinhos",
                                         "Todos.crustáceos", "Caranguejo", "Gambas", "Lagostim",  "Outros.crustáceos",
                                         "Todos.moluscos", "Berbigão", "Choco", "Polvo", "Outros.moluscos"))


# Define UI for application
ui <- fluidPage(
    tags$style(
        HTML('
             #tittleRow {
             height:100px;
             }
             
             #panel1 {
             align-items: center;
             justify-content: center;
             margin-top:280px
             }
             
             #rowmargintop {
             margin-top:25px
             }
             
             #panel2 {
             padding: 20px 40px 10px 40px;
             }
             
             #centerText {
             align-items: center;
             justify-content: center;
             }
             
             #panelsmallmarginleft {
             margin-left:30px
             }
             
             #panelsmallmargintop {
             margin-top:30px
             }
             
             ')
    ),
    fluidRow(id="tittleRow",
        titlePanel(
            h1("Peixe Capturado em Portugal",
               align = "center")
        ),
    ),
    # 1st row
    fluidRow(
        # 1st col
        column(2),
        # 2nd col
        column(2, align="center", id="panel1",
            wellPanel(align="center",
                sliderInput("years_total",
                            "Intervalo de anos",
                            value = c(1969, 2021),
                            min = 1969,
                            max = 2021,
                            sep = "",
                            ticks = FALSE
                )
            )
        ),
        # 3rd col
        column(8,
            plotOutput("totalPlot",
                       width="800px",
                       height="400px")
        )
    ),
    # 2nd row
    fluidRow(id="rowmargintop",
        # 1st col
        column(2),
        # 2nd col
        column(2,
            wellPanel(id="panel2",
                fluidRow(id="panelsmallmarginleft",
                    checkboxGroupInput("vars_fresh",
                                       "Peixes de água doce",
                                       choiceNames = c("Todos", "Enguias", "Lampreias", "Sável", "Outros"),
                                       choiceValues = c("Todos.peixes.de.água.doce","Enguias", "Lampreia", "Sável", "Outros.peixes.de.água.doce")
                    )
                ),
                fluidRow(align="center",
                    column(12, align="center", id="centerText",
                        fluidRow(align="center",
                            actionButton("run_fresh", "Gerar gráfico", 
                            )
                        ),
                        fluidRow(align="center", id="panelsmallmargintop",
                            sliderInput("years_fresh",
                                       "Intervalo de anos",
                                       value = c(2002, 2021),
                                       min = 2002,
                                       max = 2021,
                                       sep = "",
                                       ticks = FALSE
                            )
                        )
                    )
                )
            )
        ),
        # 3rd col
        column(8,
            plotOutput("freshPlot",
                       width="800px",
                       height="400px")
        )
    ),
    # 3rd row
    fluidRow(id="rowmargintop",
        # 1st col
        column(2),
        # 2nd col
        column(2,
            wellPanel(id="panel2",
                fluidRow(id="panelsmallmarginleft",
                    checkboxGroupInput("vars_salt",
                                       "Peixes marinhos",
                                       choiceNames = c("Todos", "Atum", "Biqueirão", "Carapau", "Cavala", "Peixe espada preto", "Sardinha", "Outros"),
                                       choiceValues = c("Todos.peixes.marinhos", "Atum", "Biqueirão", "Carapau", "Cavala", "Peixe.espada.preto", "Sardinha", "Outros.peixes.marinhos")
                    )
                ),
                fluidRow(align="center",
                    column(12, align="center", id="centerText",
                        fluidRow(align="center",
                            actionButton("run_salt", "Gerar gráfico"
                            )
                        ),
                        fluidRow(align="center", id="panelsmallmargintop",
                            sliderInput("years_salt",
                                        "Intervalo de anos",
                                        value = c(2002, 2021),
                                        min = 2002,
                                        max = 2021,
                                        sep = "",
                                        ticks = FALSE
                            )
                        )
                    )
                )
            )
        ),
        # 3rd col
        column(8,
            plotOutput("saltPlot",
                       width="800px",
                       height="400px")
        )
    ),
    # 4th row
    fluidRow(id="rowmargintop",
        # 1st col
        column(2),
        # 2nd col
        column(2,
            wellPanel(id="panel2",
                fluidRow(id="panelsmallmarginleft",
                    checkboxGroupInput("vars_crust",
                                       "Crustáceos",
                                       choiceNames = c("Todos", "Caranguejo", "Gambas", "Lagostim",  "Outros"),
                                       choiceValues = c("Todos.crustáceos", "Caranguejo", "Gambas", "Lagostim",  "Outros.crustáceos")
                    )
                ),
                fluidRow(align="center",
                    column(12, align="center", id="centerText",
                        fluidRow(align="center",
                            actionButton("run_crust", "Gerar gráfico"
                            )
                        ),
                        fluidRow(align="center", id="panelsmallmargintop",
                            sliderInput("years_crust",
                                        "Intervalo de anos",
                                        value = c(2002, 2021),
                                        min = 2002,
                                        max = 2021,
                                        sep = "",
                                        ticks = FALSE
                            )
                        )
                    )
                )
            )
        ),
        # 3rd col
        column(8,
            plotOutput("crustPlot",
                       width="800px",
                       height="400px")
        )
    ),
    # 5th row
    fluidRow(id="rowmargintop",
        # 1st col
        column(2),
        # 2nd col
        column(2,
            wellPanel(id="panel2",
                fluidRow(id="panelsmallmarginleft",
                    checkboxGroupInput("vars_moll",
                                       "Moluscos",
                                       choiceNames = c("Todos", "Berbigão", "Choco", "Polvo", "Outros"),
                                       choiceValues = c("Todos.moluscos", "Berbigão", "Choco", "Polvo", "Outros.moluscos")
                    )
                ),
                fluidRow(align="center",
                    column(12, align="center", id="centerText",
                        fluidRow(align="center",
                            actionButton("run_moll", "Gerar gráfico"
                            )
                        ),
                        fluidRow(align="center", id="panelsmallmargintop",
                            sliderInput("years_moll",
                                        "Intervalo de anos",
                                        value = c(2002, 2021),
                                        min = 2002,
                                        max = 2021,
                                        sep = "",
                                        ticks = FALSE
                            )
                        )
                    )
                )
            )
        ),
        # 3rd col
        column(8,
            plotOutput("mollPlot",
                       width="800px",
                       height="400px")
        )
    )
)


# Define server logic
server <- function(input, output) {
    
    output$totalPlot <- renderPlot({
        ggplot(data.total, aes(ano, total)) +
        ggtitle("Peixe Total Capturado em Portugal") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.title.y = element_text(margin=margin(r=20)),
              axis.title.x = element_text(margin=margin(t=10))) +
        geom_point(col="#3D9CFFFF") +
        geom_line(col="#3D9CFFFF") +
        ylab("Toneladas") +
        scale_y_continuous(limits=c(0, 400000),
                           labels=c("0", "100 mil", "200 mil", "300 mil", "400 mil")) +
        xlab("Ano") +
        xlim(c(input$years_total[1], input$years_total[2]))
    })
    
    fresh_filtered <- eventReactive(input$run_fresh, {
        filter(data.all.long, especie %in% input$vars_fresh)
    })
    output$freshPlot <- renderPlot({
        ggplot(fresh_filtered(), aes(x=ano, y=toneladas, color=especie)) +
        ggtitle("Peixe de Água Doce Capturado em Portugal") +
        theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin=margin(r=20)), axis.title.x = element_text(margin=margin(t=10))) +
        geom_point() +
        geom_line() +
        ylab("Toneladas") +
        xlab("Ano") +
        xlim(c(input$years_fresh[1], input$years_fresh[2])) +
        scale_color_discrete(name = "Grupos")
    })

    salt_filtered <- eventReactive(input$run_salt, {
        filter(data.all.long, especie %in% input$vars_salt)
    })
    output$saltPlot <- renderPlot({
        ggplot(salt_filtered(), aes(x=ano, y=toneladas, color=especie)) +
            ggtitle("Peixe Marinho Capturado em Portugal") +
            theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin=margin(r=20)), axis.title.x = element_text(margin=margin(t=10))) +
            geom_point() +
            geom_line() +
            ylab("Toneladas") +
            xlab("Ano") +
            xlim(c(input$years_salt[1], input$years_salt[2])) +
            scale_color_discrete(name = "Grupos")
    })
    
    crust_filtered <- eventReactive(input$run_crust, {
        filter(data.all.long, especie %in% input$vars_crust)
    })
    output$crustPlot <- renderPlot({
        ggplot(crust_filtered(), aes(x=ano, y=toneladas, color=especie)) +
            ggtitle("Crustáceos Capturados em Portugal") +
            theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin=margin(r=20)), axis.title.x = element_text(margin=margin(t=10))) +
            geom_point() +
            geom_line() +
            ylab("Toneladas") +
            xlab("Ano") +
            xlim(c(input$years_crust[1], input$years_crust[2])) +
            scale_color_discrete(name = "Grupos")
    })
    
    moll_filtered <- eventReactive(input$run_moll, {
        filter(data.all.long, especie %in% input$vars_moll)
    })
    output$mollPlot <- renderPlot({
        ggplot(moll_filtered(), aes(x=ano, y=toneladas, color=especie)) +
            ggtitle("Molusco Capturado em Portugal") +
            theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin=margin(r=20)), axis.title.x = element_text(margin=margin(t=10))) +
            geom_point() +
            geom_line() +
            ylab("Toneladas") +
            xlab("Ano") +
            xlim(c(input$years_moll[1], input$years_moll[2])) +
            scale_color_discrete(name = "Grupos")
    })
}

# Run the application 
shinyApp(ui, server)
