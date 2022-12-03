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
colnames(data) <- c("ano", "total", "todos_água_doce", "lampreia", "sável", "enguias",
                    "todos_marinhos", "atum", "biqueirão", "carapau", "cavala", "peixe_espada_preto", "sardinha",
                    "todos_crustáceos", "gambas", "lagostim", "caranguejo",
                    "todos_moluscos", "berbigão", "choco", "polvo")

data.total <- data[1:53, 1:2]

data.all <- data[34:53, -2]
data.all$outros_água_doce <- apply(data.all, 1, function(table)(table["todos_água_doce"] - table["lampreia"] - table["sável"] - table["enguias"]))
data.all$outros_marinhos <- apply(data.all, 1, function(table)(table["todos_marinhos"] - table["atum"] - table["biqueirão"] - table["carapau"] - table["cavala"] - table["peixe_espada_preto"] - table["sardinha"]))
data.all$outros_crustáceos <- apply(data.all, 1, function(table)(table["todos_crustáceos"] - table["gambas"] - table["lagostim"] - table["caranguejo"]))
data.all$outros_moluscos <- apply(data.all, 1, function(table)(table["todos_moluscos"] - table["berbigão"] - table["choco"] - table["polvo"]))

data.all.long <- data.all %>%
  pivot_longer(cols=-ano, names_to="Espécie", values_to="toneladas")

data.all.long$Espécie <- factor(data.all.long$Espécie,
                                levels=c("todos_água_doce", "enguias", "lampreia", "sável", "outros_água_doce",
                                         "todos_marinhos", "atum", "biqueirão", "carapau", "cavala", "peixe_espada_preto", "sardinha", "outros_marinhos",
                                         "todos_crustáceos", "caranguejo", "gambas", "lagostim",  "outros_crustáceos",
                                         "todos_moluscos", "berbigão", "choco", "polvo", "outros_moluscos"))


# Define UI for application
ui <- fluidPage(

    titlePanel(
        h1("Peixe Capturado em Portugal", align = "center")
    ),
    fluidRow(
        column(1, div(style = "height:20px;"),),
        column(4, div(style = "height:20px;"),
            sliderInput("years_total",
                "Intervalo de anos",
                value = c(1969, 2021),
                min = 1969,
                max = 2021,
                sep = "",
                ticks = FALSE
            )
        ),
        column(7, div(style = "height:20px;"),
            plotOutput("totalPlot", width="600px", height="300px")
        )
    ),
    fluidRow(
        column(1, div(style = "height:20px;"),),
        column(4, div(style = "height:20px;"),
            checkboxGroupInput("vars_fresh",
                "Peixes de água doce",
                choiceNames = c("todos", "enguias", "lampreias", "sável", "outros"),
                choiceValues = c("todos_água_doce","enguias", "lampreia", "sável", "outros_água_doce")
            ),
            sliderInput("years_fresh",
                "Intervalo de anos",
                value = c(2002, 2021),
                min = 2002,
                max = 2021,
                sep = "",
                ticks = FALSE
            ),
            actionButton("run_fresh", "Criar gráfico")
        ),
        column(7, div(style = "height:20px;"),
            plotOutput("freshPlot", width="600px", height="300px")
        )
    ),
    fluidRow(
        column(1, div(style = "height:20px;"),),
        column(4, div(style = "height:20px;"),
            checkboxGroupInput("vars_salt",
                "Peixes marinhos",
                choiceNames = c("todos", "atum", "biqueirão", "carapau", "cavala", "peixe espada preto", "sardinha", "outros"),
                choiceValues = c("todos_marinhos", "atum", "biqueirão", "carapau", "cavala", "peixe_espada_preto", "sardinha", "outros_marinhos")
            ),
            sliderInput("years_salt",
                "Intervalo de anos",
                value = c(2002, 2021),
                min = 2002,
                max = 2021,
                sep = "",
                ticks = FALSE
            ),
            actionButton("run_salt", "Criar gráfico")
        ),
        column(7, div(style = "height:20px;"),
            plotOutput("saltPlot", width="600px", height="300px")
        )
    ),
    fluidRow(
        column(1, div(style = "height:20px;"),),
        column(4, div(style = "height:20px;"),
            checkboxGroupInput("vars_crust",
                "Crustáceos",
                choiceNames = c("todos", "caranguejo", "gambas", "lagostim",  "outros"),
                choiceValues = c("todos_crustáceos", "caranguejo", "gambas", "lagostim",  "outros_crustáceos")
             ),
             sliderInput("years_crust",
                "Intervalo de anos",
                value = c(2002, 2021),
                min = 2002,
                max = 2021,
                sep = "",
                ticks = FALSE
             ),
             actionButton("run_crust", "Criar gráfico")
        ),
        column(7, div(style = "height:20px;"),
            plotOutput("crustPlot", width="600px", height="300px")
        )
    ),
    fluidRow(
        column(1, div(style = "height:20px;"),),
        column(4, div(style = "height:20px;"),
            checkboxGroupInput("vars_moll",
                "Moluscos",
                choiceNames = c("todos", "berbigão", "choco", "polvo", "outros"),
                choiceValues = c("todos_moluscos", "berbigão", "choco", "polvo", "outros_moluscos")
            ),
            sliderInput("years_moll",
                "Intervalo de anos",
                value = c(2002, 2021),
                min = 2002,
                max = 2021,
                sep = "",
                ticks = FALSE
            ),
            actionButton("run_moll", "Criar gráfico")
        ),
        column(7, div(style = "height:20px;"),
            plotOutput("mollPlot", width="600px", height="300px")
        )
    )
)


# Define server logic
server <- function(input, output) {
    
    output$totalPlot <- renderPlot({
        ggplot(data.total, aes(ano, total)) +
        ggtitle("Peixe total capturado em Portugal") +
        theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin=margin(r=20)), axis.title.x = element_text(margin=margin(t=10))) +
        geom_point(col="#3D9CFFFF") +
        geom_line(col="#3D9CFFFF") +
        ylab("Toneladas") +
        scale_y_continuous(limits=c(0, 400000), labels=c("0", "100 mil", "200 mil", "300 mil", "400 mil")) +
        xlab("Ano") +
        xlim(c(input$years_total[1], input$years_total[2]))
    })
    
    fresh_filtered <- eventReactive(input$run_fresh, {
        filter(data.all.long, Espécie %in% input$vars_fresh)
    })
    output$freshPlot <- renderPlot({
        ggplot(fresh_filtered(), aes(x=ano, y=toneladas, color=Espécie)) +
        ggtitle("Peixe de água doce capturado em Portugal") +
        theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin=margin(r=20)), axis.title.x = element_text(margin=margin(t=10))) +
        geom_point() +
        geom_line() +
        ylab("Toneladas") +
        xlab("Ano") +
        xlim(c(input$years_fresh[1], input$years_fresh[2]))
    })

    salt_filtered <- eventReactive(input$run_salt, {
        filter(data.all.long, Espécie %in% input$vars_salt)
    })
    output$saltPlot <- renderPlot({
        ggplot(salt_filtered(), aes(x=ano, y=toneladas, color=Espécie)) +
            ggtitle("Peixe marinho capturado em Portugal") +
            theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin=margin(r=20)), axis.title.x = element_text(margin=margin(t=10))) +
            geom_point() +
            geom_line() +
            ylab("Toneladas") +
            xlab("Ano") +
            xlim(c(input$years_salt[1], input$years_salt[2]))
    })
    
    crust_filtered <- eventReactive(input$run_crust, {
        filter(data.all.long, Espécie %in% input$vars_crust)
    })
    output$crustPlot <- renderPlot({
        ggplot(crust_filtered(), aes(x=ano, y=toneladas, color=Espécie)) +
            ggtitle("Crustáceos capturados em Portugal") +
            theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin=margin(r=20)), axis.title.x = element_text(margin=margin(t=10))) +
            geom_point() +
            geom_line() +
            ylab("Toneladas") +
            xlab("Ano") +
            xlim(c(input$years_crust[1], input$years_crust[2]))
    })
    
    moll_filtered <- eventReactive(input$run_moll, {
        filter(data.all.long, Espécie %in% input$vars_moll)
    })
    output$mollPlot <- renderPlot({
        ggplot(moll_filtered(), aes(x=ano, y=toneladas, color=Espécie)) +
            ggtitle("Moluscos capturados em Portugal") +
            theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(margin=margin(r=20)), axis.title.x = element_text(margin=margin(t=10))) +
            geom_point() +
            geom_line() +
            ylab("Toneladas") +
            xlab("Ano") +
            xlim(c(input$years_moll[1], input$years_moll[2]))
    })
}

# Run the application 
shinyApp(ui, server)
