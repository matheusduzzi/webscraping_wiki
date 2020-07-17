library(tidyverse)
library(tidyr)
library(dplyr)
library(rvest)
library(dygraphs)
library(BatchGetSymbols)
library(shiny)
library(dygraphs)
library(shinythemes)
library(forecast)

# tabela de acoes
wiki1 <- "https://pt.wikipedia.org/wiki/Lista_de_companhias_citadas_no_Ibovespa"
doc1 <- read_html(wiki1) 
doc1 %>% 
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
    html_table() -> tabela_b3 
tabela_acoes <- as.data.frame(tabela_b3) 
tabela_acoes <- tabela_acoes 

wiki1 <- "https://pt.wikipedia.org/wiki/Lista_de_companhias_citadas_no_Ibovespa"
doc1 <- read_html(wiki1) 
links = doc1 %>% html_nodes(xpath = "//td/a") %>% 
    html_attr("href")
links <- c(links[1:5],"/wiki/Bradesco",links[6:52],"/wiki/Petrobras",links[53:73])

acesso = cbind(tabela_acoes,links)
acesso = acesso[1:13,] #arrumar ordem

ui <- fluidPage(
    theme = shinythemes::shinytheme("united"),

    titlePanel("Conhecendo a B3"),

    sidebarLayout(
        sidebarPanel(
            h3("Escolha uma ação"),
            selectInput("escolha_acao", "Ação:", 
                        choices = acesso$Código)
        ),
        mainPanel(
            textOutput("nome_empresa"),
            tags$hr(style="border-color: white;"),tags$hr(style="border-color: white;"),
            textOutput("primeiro_paragrafo"),
            tags$hr(style="border-color: white;"),tags$hr(style="border-color: white;"),
            DT::dataTableOutput("card_wiki"),
            tags$hr(style="border-color: white;"),tags$hr(style="border-color: white;"),
            h3("Cotação Histórica da empresa"),
            dygraphOutput("candle"),
            tags$hr(style="border-color: white;"),tags$hr(style="border-color: white;"),
            h3("Previsão para os próximos dias (fechamento)"),
            tags$hr(style="border-color: white;"),
            DT::dataTableOutput("prev"),
        )
    )
)

server <- function(input, output) {
    
    output$card_wiki = DT::renderDataTable({
        
        acesso = acesso %>% filter(`Código`== input$escolha_acao)
        
        wiki1 <- paste0("https://pt.wikipedia.org/",acesso$links[1])
        doc1 <- read_html(wiki1) 
        doc1 %>% 
            html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
            html_table() -> tabela_carac_acao 
        tabela_carac_acao  <- as.data.frame(tabela_carac_acao)
        tabela_carac_acao <- tabela_carac_acao[-1,]
        
        DT::datatable(
            tabela_carac_acao , options = list(paging = FALSE,searching = FALSE))
    })
    
    output$primeiro_paragrafo <- renderText({

        acesso = acesso %>% filter(`Código`== input$escolha_acao)
        
        wiki2 <- paste0("https://pt.wikipedia.org/",acesso$links[1])
        doc2 <- read_html(wiki2) 
        doc2 %>% 
            html_nodes(xpath = '//*[@id="mw-content-text"]/div/p[1]') %>%
            html_text() -> primeiro_par
        primeiro_par
    })
    
    output$nome_empresa <- renderText({
        
        acesso = acesso %>% filter(`Código`== input$escolha_acao)
        
        wiki2 <- paste0("https://pt.wikipedia.org/",acesso$links[1])
        doc2 <- read_html(wiki2) 
        doc2 %>% 
            html_nodes(xpath = '//*[@id="firstHeading"]') %>%
            html_text() -> nome_empresa
        nome_empresa
    })
    
    output$candle <- renderDygraph({
        
        acesso = acesso %>% filter(`Código`== input$escolha_acao)
        
        inicio <- "2018-01-01" 
        final <- Sys.Date()
        bench.ticker <- "^BVSP"
        saida <- BatchGetSymbols(tickers = paste0(acesso$Código,".SA"), first.date = inicio, last.date = final, 
                                 bench.ticker = bench.ticker)
        saida <- as.data.frame(saida$df.tickers)
        
        row.names(saida) <- saida$ref.date
        m <- saida %>% dplyr::select(-ref.date,-price.adjusted,-volume,-ticker,-ret.adjusted.prices,-ret.closing.prices)
        colnames(m) <- c("Open","High","Low","Close")
        
        dygraph(m) %>%
            dyCandlestick()
        
    })
    
    output$prev = DT::renderDataTable({
        
        acesso = acesso %>% filter(`Código`== input$escolha_acao)
        
        inicio <- "2018-01-01" 
        final <- Sys.Date()
        bench.ticker <- "^BVSP"
        saida <- BatchGetSymbols(tickers = paste0(acesso$Código,".SA"), first.date = inicio, last.date = final, 
                                 bench.ticker = bench.ticker)
        saida <- as.data.frame(saida$df.tickers)
        
        row.names(saida) <- saida$ref.date
        d <- saida %>% dplyr::select(-ref.date,-price.adjusted,-volume,-ticker,-ret.adjusted.prices,-ret.closing.prices)
        colnames(d) <- c("Open","High","Low","Close")
        
        tabela = as.data.frame(forecast(nnetar(ts(d$Close,start = c(2018), frequency = 365)),h=4))
        tabela = format(round(tabela, 2))
        rownames(tabela) <- c(Sys.Date()+1,Sys.Date()+2,Sys.Date()+3,Sys.Date()+4)
        
        DT::datatable(tabela, options = list(paging = FALSE,searching = FALSE))
    })

}

shinyApp(ui = ui, server = server)
