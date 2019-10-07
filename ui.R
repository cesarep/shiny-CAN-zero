library(shiny)

source('precision.r', encoding = 'utf-8')
source('interval.r', encoding = 'utf-8')
source('katex.r', encoding = 'utf-8')

groupAddon <- function(..., inputId=NULL, class=NULL, style=NULL){
   elem <- tagList(...)
   for(i in 1:length(elem)){
      elem[[i]]$attribs$class = paste(elem[[i]]$attribs$class, "input-group-addon", collapse = ' ')
      elem[[i]]$attribs$style = paste(elem[[i]]$attribs$style, "width: auto;", collapse = ' ')
   }
   tags$div(id = inputId, class=paste(class, "input-group form-group", collapse = ' '), elem)
}

shinyUI(fluidPage(
      titlePanel("Métodos Numéricos para zeros de funções"),
      KaTeX(),
      sidebarLayout(
         sidebarPanel(
            verbatimTextOutput('rv'),
            #busca de raizes inicial
            textInput('fx', '\\(f(x)\\)', 'exp(x)+x-2'),
            includeCSS("www/estilo.css"),
            includeHTML('www/ajuda.htm'),
            intervalInput("intervalo", 'Intervalo de Plotagem', -2, 2, NULL, "≤ x ≤", step = 0.5),
            actionButton('plotar', 'Plotar f(x)', width = '40%', style='margin: 0 30%;'),
            hr(), #metodos de calculo
            selectInput('metodo', 'Método', 
                        list('Bisecção' = 1,
                             'Iteração Linear' = 2,
                             'Newton-Raphson' = 3,
                             'Falsa Posição' = 4
                        )
            ),
            precisionInput('prec', 'Precisão', 0.001),
            conditionalPanel('input.metodo == 1 || input.metodo == 4',
                intervalInput("intInicial", 'Intervalo Inicial', -2, 2, "x0", "x1", step = 0.5)
            ),
            conditionalPanel('input.metodo == 2',
               textInput('gx', '\\(g(x)\\)', 'log(2-x)'),
               a("(?)", id = "ajudaBotao", onclick="$('#ajuda').slideToggle()")
            ),
            conditionalPanel('input.metodo == 2 || input.metodo == 3',
               numericInput('x0', '\\(x_0\\)', 1.5, step = 0.5)
            ),
            actionButton('calcular', 'Calcular raiz', width = '40%', style='margin: 0 30%;')
         ),
         mainPanel(
            tabsetPanel(id = "Abas",
               tabPanel("Gráfico", value = "aba1",
                  div(plotOutput('plot', height = '500px'), onwheel="
                  event.preventDefault();
                  Shiny.setInputValue('zoom', {dir: event.deltaY/3, ts: event.timeStamp%10000})
                  "), #cuida do zoom no plot
                  #groupAddon( #controlar animação
                       # actionButton('avança', '', icon('fast-backward', )),
                       # actionButton('avança2', '', icon('step-backward', )),
                       # actionButton('avança2', '', icon('play', )),
                       # actionButton('avança2', '', icon('pause', )),
                       # actionButton('avança2', '', icon('step-forward', )),
                       # actionButton('avança3', '', icon('fast-forward', ))
                  #),
                  div(uiOutput('raiz'), style='text-align: center;')
               ),tabPanel("Resultados", value = "aba2",
                    uiOutput('tabela'),
                    tags$style(type='text/css', "#tabela table {margin: 10px auto;}"),
                    uiOutput('resultado')
               )
            )
         )
      ),
      hr(),
      flowLayout(id = "cabecario",
                 p(strong("Apoio:"), br(), img(src="NEPESTEEM.png")),
                 p(strong("Agradecimento:"), br(), img(src="FAPESC.png")),
                 p(strong("Desenvolvido por:"), br(), "César Eduardo Petersen")
      )
))
