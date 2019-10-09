library(shiny)

shinyServer(function(input, output, session) {
   #valores reativos
   rv <- reactiveValues(tabela = data.frame(), status = list(s=0), modo = 0, 
                        fx = function(x) exp(x)+x-2, 
                        gx = function(x) log(2-x),
                        k=0
                        ) 
   
   #parametros da URL TESTAR REESCREVER
   observeEvent(session$clientData, {
      query <- getQueryString()
      if(!is.null(query$fx)) updateTextInput(session, 'fx', value = query$fx)
   })
   
   # output$rv <- renderPrint(
   #    print(rv$k)
   # )
   
   #define funções
   fTest <- function(x) {}
   observeEvent(input$fx, {
      #preenche * entre numero e x ou função
      if(grepl("(\\d)(\\w|\\()", input$fx))
         updateTextInput(session, 'fx', value = gsub("(\\d)(\\w|\\()", "\\1*\\2", input$fx))
      
      try({
         body(fTest) <- parse(text = paste0(input$fx))
         if( !is.primitive(fTest(1)) )
            body(rv$fx) <- parse(text = paste0(input$fx))
         }, silent = T)
   })
   observeEvent(input$gx, {
      if(grepl("(\\d)(\\w|\\()", input$gx))
         updateTextInput(session, 'gx', value = gsub("(\\d)(\\w|\\()", "\\1*\\2", input$gx))
      try({
         body(fTest) <- parse(text = paste0(input$gx))
         if( !is.primitive(fTest(1)) )
            body(rv$gx) <- parse(text = paste0(input$gx))
      }, silent = T)
   })
   
   #se encarrega do zoom no plot
   observeEvent(input$zoom, {
      rv$k <- rv$k-input$zoom$dir
   })
   #delta = raiz-intervalo [vetor com as duas distancias dos intervalos até raizes]
   #zoom com centralizador
   #intervalo zoom = raiz - delta*zoom*centr
   zoom = function(k) exp(-k/5)
   #Im(zoom) = (0,+∞) tal que
   #zoom→0 intervalo→raiz
   #zoom=1 sem zoom
   #zoom>1 afastado
   centr = function(x, d) {
      D = sum(abs(d))/2
      dm = abs(d)
      (1-D/dm)*exp(-abs(x)*D/(5*dm))+D/dm
   }
   #Im(centr)= (1/delta, 1)
   #centr=1 sem centralizar
   #centr=||delta||/delta raiz centralizada
   
   
   #plotagem da função
   #é acionado por mudanças em fx, no intervalo de plotagem e pelo botao plotar
   #muda para modo plotagem (rv$modo = 0)
   observeEvent({input$intervalo; input$plotar; rv$fx}, {
      updateTabsetPanel(session, "Abas", 'aba1')
      rv$modo <- 0
      rv$k <- 0 #reseta zoom
   })
   observeEvent({input$calcular; rv$gx; input$x0; input$intInicial; input$metodo},{
      rv$k <- 0 #reseta zoom
   })

   #calcula os resultados
   #se estiver em modo plotagem, é acionado apenas pelo botao calcular
   observeEvent(input$calcular, {
      rv$modo <- 1
   })
   
   #se não, por mudanças em g(x), x0, intervalo inicial, g(x) e método
   observeEvent({input$calcular; rv$gx; input$prec; input$x0; input$intInicial; input$metodo}, { 
      #se nao estiver no modo resultado sai do bloco
      if(rv$modo == 0) return()
      
      #define função
      f = rv$fx
      g = rv$gx
      
      #faz calculos e tabela
      prec = input$prec
      xmin = input$intInicial[1]
      xmax = input$intInicial[2]
      
      #define variaveis locais
      tabela <- NULL
      status <- NULL
      switch(as.integer(input$metodo),{
         #bisec
         source('metodos/bisec.R', local = T, encoding = 'UTF-8')
      },{
         #it lin
         x0 = input$x0
         source('metodos/it_lin.R', local = T, encoding = 'UTF-8')
      },{
         #newt
         x0 = input$x0
         dfdx <- function(x){}
         body(dfdx) <- D(parse(text = input$fx), 'x')
         source('metodos/new_rap.R', local = T, encoding = 'UTF-8')
      },{
         #falsa pos
         source('metodos/falsa_pos.R', local = T, encoding = 'UTF-8')
      })
      #atribui variaveis localis às reativas
      rv$tabela <- tabela 
      rv$status <- status
   })
   
   #plota gráficos
   output$plot <- renderPlot({
      f = rv$fx
      if(rv$modo == 0) 
         int = input$intervalo #zoom nao funciona no modo plotagem
      else{
         int0 = input$intInicial
         deltaR = rv$status$xf-int0
         int = rv$status$xf - deltaR*centr(rv$k, deltaR)*zoom(rv$k)
         #int = input$intInicial + (rv$status$xf-input$intInicial)*zoom(rv$k)
      }
      min = optimize(f, int, maximum = F)$objective
      max = optimize(f, int, maximum = T)$objective
      curve(f, int[1], int[2], ylim=range((c(min, max, 0))))
      abline(h=0, lty=2, col='grey')
     
      #se está no modo resultado
      if(rv$modo == 1){
         #parametros de escala
         eX = abs(par('fin')[1]/(par('usr')[2]-par('usr')[1]))
         eY = abs(par('fin')[2]/(par('usr')[4]-par('usr')[3]))
         
         #etapas
         #implantar animação, puxar n de slider
         n = nrow(rv$tabela)
         
         #tabela
         tab = rv$tabela
         
         #valores iniciais
         val0 = rv$tabela[1,]
         
         switch(as.integer(input$metodo),{ 
            #bisec
            abline(v = c(val0['XE'], val0['XD']), col='red', lty=2)
            for(i in 1:n){
               comp = abs(tab[i, 'XE']-tab[i, 'XD'])*eX
               if(comp > 0.1){
                  abline(v = tab[i, 'XM'], col='red', lty=2)
                  if(comp > 0.2)
                     mtext(i, 3, 0, F, tab[i, 'XM'])
               }
            }
         },{ 
            #it lin
            d = abs(rv$status$xf-val0['X'])
            int0 = range(pretty(tab[,'X']))
            int = rv$status$xf-(rv$status$xf-int0)*zoom(rv$k)
            g = rv$gx
            curve(g, int[1], int[2], col='red')
            
            abline(0, 1, col='blue') #plota f(x)=x
            
            arrows(val0['X'], -2*d, val0['X'], val0['GX'], 0.1, col='darkgrey')
            arrows(val0['X'], val0['GX'], val0['GX'], val0['GX'], 0.05, col='darkgrey')

            for(i in 2:n){
               comp = abs(tab[i, 'X']-tab[i, 'GX'])*eX
               if(comp > 0.1){
                  arrows(tab[i, 'X'], tab[i, 'X'], tab[i, 'X'], tab[i, 'GX'], 0.05, col='darkgrey')
                  arrows(tab[i, 'X'], tab[i, 'GX'], tab[i, 'GX'], tab[i, 'GX'], 0.05, col='darkgrey')
               }
            }
            legend('left', legend = c(quote(g(x)), quote(x)), lty=c(1,1), col=c('red', 'blue'), inset = 0.05)
         },{
            #newt
            points(val0['X'], val0['FX'], pch=16)
            arrows(val0['X'], 0, val0['X'], val0['FX'], 0.075, col='red')
            for(i in 1:n){
               if(abs(tab[i, 'FX'])*eY > 0.075){
                  abline(tab[i, 'FX']-tab[i, 'X']*tab[i, 'DX'], tab[i, 'DX'], lty=3, col='red')
                  
                  points(tab[i, 'X'], tab[i, 'FX'], pch=20)
                  arrows(tab[i, 'X'], 0, tab[i, 'X'], tab[i, 'FX'], 0.075, col='red')
               }
            }
         },{
            #falsa pos
            lines(c(val0['XE'], val0['XD']), c(val0['FXE'], val0['FXD']), lty=2, col='red')
            arrows(val0['XR'], 0, val0['XR'], val0['FXR'], 0.075, col='red')
            for(i in 2:n){
               comp = (tab[i, 'XR']-tab[i-1, 'XR'])
               if(abs(comp)*eX > 0.1){ #projeção maior que
                  lines(c(tab[i, 'XE'], tab[i, 'XD']), c(tab[i, 'FXE'], tab[i, 'FXD']), lty=2, col='red')
                  if(abs(tab[i, 'FXR'])*eY > 0.075)
                     arrows(tab[i, 'XR'], 0, tab[i, 'XR'], tab[i, 'FXR'], 0.075, col='red')
               }
            }
         })
      }
   })

   observeEvent(rv$tabela, {
      if(!is.null(rv$status$s) && rv$status$s == TRUE){
         tabela = rv$tabela
         #expandir/contrair tabela
         modTab = list(pos=list(0), command=rv$status$cab)
         if(nrow(tabela)>10){
         	modTab$pos[2] = nrow(tabela)
         	modTab$command[2] = paste0("<tr id='tabexp'><td align='center' colspan='", ncol(tabela),"'></td></tr>")
         }
         output$tabela <- renderTable({
            rv$tabela
            }, align='c', digits = 4, striped = T, include.colnames=FALSE, add.to.row = modTab)
         
         output$resultado <- renderUI(
            sprintf('$$f(%.*f)\\approx%.*f$$', 4, rv$status$xf, 4, rv$status$fxf)
         )
      } else {
         output$tabela <- renderUI(tagList(br(),p(rv$status$msg, align='center')))
      }
   })
})
