library(shiny)
library(ggplot2)

processos <- read.csv('https://raw.githubusercontent.com/johnmbf/gov-dash/main/dados/processos.csv')

ui <- fluidPage(
  titlePanel("Governadores"),
  sidebarLayout(
    sidebarPanel(
      selectInput('gov', 'Governador', choices = unique(processos$leg_governador))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Gráfico', plotOutput('grafico_gov')),
        tabPanel('Dados', DT::DTOutput('dt_gov'))
      )
    )
  )
)

server <- function(input, output, session) {
  output$grafico_gov <- renderPlot({
    processos |>
      dplyr::mutate(cor = dplyr::case_when(
        leg_governador == input$gov ~ '#D5A57C',
        TRUE ~ '#DADCD0'
      )) |>
      ggplot() +
      aes(x = lubridate::year(dt_autuacao), fill = cor) +
      geom_bar() +
      scale_fill_identity('Governador', guide = 'legend', labels = c('#D5A57C' = input$gov, '#DADCD0' = 'Outros')) +
      scale_y_continuous(expand = c(0,0), limits = c(0,20)) +
      scale_x_continuous(expand = c(0,0.5)) +
      labs(
        x = 'Ano',
        y = 'Nº de ações'
      ) +
      theme(
        plot.background = element_rect(fill = 'white'),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_line(),
        panel.grid.major.y = element_line(color = 'grey30', linetype = 2),
        legend.title = element_blank(),
        legend.key = element_blank())
  })
  
  output$dt_gov <- DT::renderDT({
    processos |> DT::datatable(
      rownames = FALSE,
      options = list(
        'dom' = 'Bp', 
        'language' = list('url' = 'https://cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json'),
        'rowReorder' = FALSE,
        'ordering' = FALSE),
      style = 'bootstrap5'
    ) |>
      DT::formatDate(6:7, 'toLocaleDateString') |>
      DT::formatStyle(1:10, `font-size` = '10px')
  })
}

shinyApp(ui, server)