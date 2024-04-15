library(shiny)
library(ggplot2)

processos <- read.csv('https://raw.githubusercontent.com/johnmbf/gov-dash/main/dados/processos.csv')
legitimados <- read.csv('https://raw.githubusercontent.com/johnmbf/gov-dash/main/dados/legitimados.csv')

ui <- fluidPage(
  titlePanel("Governadores"),
  sidebarLayout(
    sidebarPanel(
      selectInput('gov', 'Governador', choices = unique(processos$leg_governador)),
      selectInput('leg_tipo', 'Tipo de Legitimado', choices = c(
        "Todos" = "all",
        "Legislativo" = 'l',
        "Executivo" = 'e',
        "Judiciário" = 'j'
      )),
      selectInput('leg_fed', 'Unidade Federativa', choices = c(
        "Todos" = "all",
        "Rio Grande do Sul" = 'rs',
        "São Paulo" = 'sp',
        "Santa Catarina" = 'sc',
        "Paraná" = 'pr',
        "União" = 'u'
      ))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Ajuizamento', plotOutput('grafico_gov')),
        tabPanel('Dados', DT::DTOutput('dt_gov')),
        tabPanel('Legitimados por Tipo', plotOutput('grafico_leg1')),
        tabPanel('Legitimados por UF', plotOutput('grafico_leg2'))
      )
    )
  )
)

server <- function(input, output, session) {
  output$grafico_leg1 <- renderPlot({
    if (input$leg_tipo != "all"){
      legitimados |>
        dplyr::filter(!is.na(leg_passivo_tipo)) |>
        dplyr::mutate(cor = dplyr::case_when(
          leg_passivo_tipo == input$leg_tipo ~ "#D5A57C",
          TRUE ~ '#DADCD0'
        )) |>
        ggplot() +
        aes(x = leg_passivo_tipo, fill = cor) +
        geom_bar(show.legend = FALSE) +
        scale_fill_identity('Tipo de Lgitimado', guide = 'legend', labels = c('#D5A57C' = input$leg_tipo, '#DADCD0' = 'Outros')) +
        scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
        labs(
          x = 'Tipo',
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
    } else {
      legitimados |>
        dplyr::filter(!is.na(leg_passivo_tipo)) |>
        ggplot() +
        aes(x = leg_passivo_tipo) +
        geom_bar(fill = '#D5A57C', show.legend = FALSE) +
        scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
        labs(
          x = 'Tipo',
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
    }
  })
  
  
  output$grafico_leg2 <- renderPlot({
    if (input$leg_fed != "all"){
      legitimados |>
        dplyr::filter(!is.na(leg_passivo_fed)) |>
        dplyr::mutate(cor = dplyr::case_when(
          leg_passivo_fed == input$leg_fed ~ "#D5A57C",
          TRUE ~ '#DADCD0'
        )) |>
        ggplot() +
        aes(x = leg_passivo_fed, fill = cor) +
        geom_bar(show.legend = FALSE) +
        scale_fill_identity('Tipo de Lgitimado', guide = 'legend', labels = c('#D5A57C' = input$leg_fed, '#DADCD0' = 'Outros')) +
        scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
        labs(
          x = 'Tipo',
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
    } else {
      legitimados |>
        dplyr::filter(!is.na(leg_passivo_fed)) |>
        ggplot() +
        aes(x = leg_passivo_fed) +
        geom_bar(fill = '#D5A57C', show.legend = FALSE) +
        scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
        labs(
          x = 'Tipo',
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
    }
  })
  
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