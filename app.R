# .libPaths("/home/ming/miniconda3/envs/shiny/lib/R/library/")
library(shiny)
library(shinythemes)
library(readxl)
library(ggplot2)
library(stringr)
library(DT)
library(patchwork)
library(RColorBrewer)
library(dplyr)
library(ggsci)
library(reshape2)
library(scales)

options(shiny.maxRequestSize = 100*1024^2)

# Define UI for application that draws a histogram
ui <- navbarPage(title = "MD helper",
                 tabPanel(title = "Introduction", fluidPage(theme = shinytheme("flatly")),
                          icon = icon("wand-magic-sparkles"),
                          p("Welcome to ",a("minglab.tech",href="http://minglab.tech/",style="color: #386cb0; font-weight: bold; text-decoration: underline;"),
                            "! \nPlease enjoy the ",span("MD helpers",style="font-weight: bold; font-size:30px; color:#b22222"),"!",style = "font-size:30px; color:#18bc9c"),
                          # p("Welcome to use the Laboratory Web Applications!",style = "font-size:30px; color:#18bc9c"),
                          column(12,
                                 fluidRow(
                                   column(6,
                                          p(strong("01. Value Projection"),style="font-size:25px;color:#b22222"),
                                          img(src = "01_value_projection.png",width=2480/4,height=1064/4),
                                          
                                   ),),
                                 br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                 br(),br(),br(),br(),br(),br(),br(),br(),
                                 fluidRow(p("Minglab小明实验室 @ 2024 | ",a("沪ICP备2024063283号-1", target="_blank",href="https://beian.miit.gov.cn/"),style = "font-size:25px; text-align:center;"))
                          )),
                 tabPanel(title = "Support",icon = icon("battery-half"),
                          p(a("minglab.tech",href="http://minglab.tech/",style="color: #386cb0; font-weight: bold; text-decoration: underline;"),
                            a("小明实验室"),style = "font-size:25px"),
                          br(),
                          p(a("打赏")," 可以使网站功能增多，运行时间更久哦~",style = "font-size:25px"),
                          p("如果有个性化分析需求或者bug，欢迎向 小明师兄 ",a("发邮件")," (e-mail: wgm657158702@163.com)，小明师兄制作或修改完成后会很快部署的~！",style = "font-size:25px"),
                          br(),
                          img(src="wxzzm.jpg",width=300)
                          
                 ),
                 navbarMenu("APPs",icon = icon("list"),
                            # 01. Value Projection ===========================================================   
                            tabPanel(title = "01. Value Projection",
                                     # key: hyELISA
                                     tags$head(
                                       tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                                     pageWithSidebar(
                                       headerPanel(p("Value Projection",br(), strong("Inspired by momo, launched on 2024-05-24",
                                                                                     style = "font-size:25px; color:#18bc9c;"))),
                                       sidebarPanel(
                                         width = 4,
                                         fluidRow(
                                           column(12,
                                                  fileInput(inputId = "file_valproj",label = "Upload your file",
                                                            multiple = FALSE, buttonLabel = "Browse...",
                                                            placeholder = "No file selected"))
                                         ),
                                         uiOutput(outputId = "side_valproj"),
                                         fluidRow(column(6,
                                                         actionButton(inputId = "calculate_valproj",label = "Submit & Calculate",icon = icon("calculator"),
                                                                      class = "btn-info"))
                                         )
                                       ),
                                       
                                       mainPanel(
                                         tabsetPanel(
                                           type = "tabs",
                                           tabPanel(title = "Introduction",icon = icon("tv"),
                                                    uiOutput(outputId = "ui_valproj_intro")),
                                           tabPanel(title = "Data", icon = icon("square-poll-vertical"),
                                                    uiOutput(outputId = "ui_valproj_data")),
                                           tabPanel(title = "Pymol",icon = icon("image"),
                                                    uiOutput(outputId = "ui_valproj_pymol")),
                                           tabPanel(title = "ChimeraX",icon = icon("images"),
                                                    uiOutput(outputId = "ui_valproj_chimerax")
                                           ),
                                           tabPanel(title = "ColorBar",icon = icon("layer-group"),
                                                    uiOutput(outputId = "ui_valproj_colorbar_control"),
                                                    br(),
                                                    uiOutput(outputId = "ui_valproj_colorbar_plot")
                                           ),
                                         ),
                                       )
                                     )
                            ),
                            ),
                 tabPanel(title = "Palette",icon = icon("palette"),
                          img(src = "00_palette.png",width=6000/4,height=6000/4)
                 ),
                 
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # 00. functions initialization
  palette_in_discrete <- reactive({c("Lancet (9)","NPG (10)", "NEJM (8)","JCO (10)","JAMA (7)",
                                     "AAAS (10)","D3 (10)","Futurama (12)","GSEA (12)","IGV (51)",
                                     "LocusZoom (7)","Rick and Morty (12)","Simpsons (16)",
                                     "Star Trek (7)","Tron Legacy (7)","Chicago (9)","UCSC (26)",
                                     "Set1 (9)","Set2 (8)","Set3 (12)","Pastel1 (9)","Pastel2 (8)",
                                     "Paired (12)","Dark2 (8)","Accent (8)")})
  palette_in_continuous <- reactive({
    div_palette <- paste0("Div_",rownames(RColorBrewer::brewer.pal.info %>% filter(category %in% c("div"))))
    seq_palette <- paste0("Seq_",rownames(RColorBrewer::brewer.pal.info %>% filter(category %in% c("seq"))))
    seq_palette_colorbrewer2 <- paste0("Seq_",
                                       c("BuGn","BuPu","GnBu","OrRd","PuBu","PuBuGn",
                                         "PuRd","RdPu","YlGn","YlGnBu","YlOrBr","YlOrRd",
                                         "Blues","Greens","Greys","Oranges","Purples","Reds"),"3")
    div_palette_colorbrewer2 <- paste0("Div_",
                                       c("BrBG","PiYG","PRGn","PuOr","RdBu",
                                         "RdGy","RdYlBu","RdYlGn","Spectral"),"3")
    viridis_palette <- c("Viridis_magma","Viridis_inferno","Viridis_plasma",
                         "Viridis_viridis","Viridis_cividis","Viridis_rocket",
                         "Viridis_mako","Viridis_turbo")
    palette <- c("navy white firebrick3",div_palette_colorbrewer2,seq_palette_colorbrewer2,
                               viridis_palette,div_palette,seq_palette)
    return(palette)
  })
  palette_all <- reactive({c(palette_in_continuous(),palette_in_discrete())})
  palette_word <- reactive({c("black","white","red2","green3","blue","cyan2","magenta3","orange","gray")})
  palette_trans_fun <- reactive({
    function(palette){
      switch (palette,
              "Lancet (9)" = ggsci::pal_lancet()(9),
              "NPG (10)" = ggsci::pal_npg()(10),
              "NEJM (8)" = ggsci::pal_nejm()(8),
              "JCO (10)" = ggsci::pal_jco()(10),
              "JAMA (7)" = ggsci::pal_jama()(7),
              "AAAS (10)" = ggsci::pal_aaas()(10),
              "D3 (10)" = ggsci::pal_d3()(10),
              "Futurama (12)" = ggsci::pal_futurama()(12),
              "GSEA (12)" = ggsci::pal_gsea()(12),
              "IGV (51)" = ggsci::pal_igv()(51),
              "LocusZoom (7)" = ggsci::pal_locuszoom()(7),
              "Rick and Morty (12)" = ggsci::pal_rickandmorty()(12),
              "Simpsons (16)" = ggsci::pal_simpsons()(16),
              "Star Trek (7)" = ggsci::pal_startrek()(7),
              "Tron Legacy (7)" = ggsci::pal_tron()(7),
              "Chicago (9)" = ggsci::pal_uchicago()(9),
              "UCSC (26)" = ggsci::pal_ucscgb()(26),
              "Set1 (9)" = RColorBrewer::brewer.pal(9,"Set1"),
              "Set2 (8)" = RColorBrewer::brewer.pal(8,"Set2"),
              "Set3 (12)" = RColorBrewer::brewer.pal(12,"Set3"),
              "Pastel1 (9)" = RColorBrewer::brewer.pal(9,"Pastel1"),
              "Pastel2 (8)" = RColorBrewer::brewer.pal(8,"Pastel2"),
              "Paired (12)" = RColorBrewer::brewer.pal(12,"Paired"),
              "Dark2 (8)" = RColorBrewer::brewer.pal(8,"Dark2"),
              "Accent (8)" = RColorBrewer::brewer.pal(8,"Accent"),
              "Div_BrBG" = RColorBrewer::brewer.pal(11,"BrBG"),
              "Div_PiYG" = RColorBrewer::brewer.pal(11,"PiYG"),
              "Div_PRGn" = RColorBrewer::brewer.pal(11,"PRGn"),
              "Div_PuOr" = RColorBrewer::brewer.pal(11,"PuOr"),
              "Div_RdBu" = RColorBrewer::brewer.pal(11,"RdBu"),
              "Div_RdGy" = RColorBrewer::brewer.pal(11,"RdGy"),
              "Div_RdYlBu" = RColorBrewer::brewer.pal(11,"RdYlBu"),
              "Div_RdYlGn" = RColorBrewer::brewer.pal(11,"RdYlGn"),
              "Div_Spectral" = RColorBrewer::brewer.pal(11,"Spectral"),
              "Seq_Blues" = RColorBrewer::brewer.pal(9,"Blues"),
              "Seq_BuGn" = RColorBrewer::brewer.pal(9,"BuGn"),
              "Seq_BuPu" = RColorBrewer::brewer.pal(9,"BuPu"),
              "Seq_GnBu" = RColorBrewer::brewer.pal(9,"GnBu"),
              "Seq_Greens" = RColorBrewer::brewer.pal(9,"Greens"),
              "Seq_Greys" = RColorBrewer::brewer.pal(9,"Greys"),
              "Seq_Oranges" = RColorBrewer::brewer.pal(9,"Oranges"),
              "Seq_OrRd" = RColorBrewer::brewer.pal(9,"OrRd"),
              "Seq_PuBu" = RColorBrewer::brewer.pal(9,"PuBu"),
              "Seq_PuBuGn" = RColorBrewer::brewer.pal(9,"PuBuGn"),
              "Seq_PuRd" = RColorBrewer::brewer.pal(9,"PuRd"),
              "Seq_Purples" = RColorBrewer::brewer.pal(9,"Purples"),
              "Seq_RdPu" = RColorBrewer::brewer.pal(9,"RdPu"),
              "Seq_Reds" = RColorBrewer::brewer.pal(9,"Reds"),
              "Seq_YlGn" = RColorBrewer::brewer.pal(9,"YlGn"),
              "Seq_YlGnBu" = RColorBrewer::brewer.pal(9,"YlGnBu"),
              "Seq_YlOrBr" = RColorBrewer::brewer.pal(9,"YlOrBr"),
              "Seq_YlOrRd" = RColorBrewer::brewer.pal(9,"YlOrRd"),
              "Viridis_magma" = viridis::viridis_pal(option = "A")(12),
              "Viridis_inferno" = viridis::viridis_pal(option = "B")(12),
              "Viridis_plasma" = viridis::viridis_pal(option = "C")(12),
              "Viridis_viridis" = viridis::viridis_pal(option = "D")(12),
              "Viridis_cividis" = viridis::viridis_pal(option = "E")(12),
              "Viridis_rocket" = viridis::viridis_pal(option = "F")(12),
              "Viridis_mako" = viridis::viridis_pal(option = "G")(12),
              "Viridis_turbo" = viridis::viridis_pal(option = "H")(12),
              "Div_BrBG3" = c("#d8b365","#f5f5f5","#5ab4ac"),
              "Div_PiYG3" = c("#e9a3c9","#f7f7f7","#a1d76a"),
              "Div_PRGn3" = c("#af8dc3","#f7f7f7","#7fbf7b"),
              "Div_PuOr3" = c("#f1a340","#f7f7f7","#998ec3"),
              "Div_RdBu3" = c("#ef8a62","#f7f7f7","#67a9cf"),
              "Div_RdGy3" = c("#ef8a62","#ffffff","#999999"),
              "Div_RdYlBu3" = c("#fc8d59","#ffffbf","#91bfdb"),
              "Div_RdYlGn3" = c("#fc8d59","#ffffbf","#91cf60"),
              "Div_Spectral3" = c("#fc8d59","#ffffbf","#99d594"),
              "Seq_BuGn3" = c("#e5f5f9","#99d8c9","#2ca25f"),
              "Seq_BuPu3" = c("#e0ecf4","#9ebcda","#8856a7"),
              "Seq_GnBu3" = c("#e0f3db","#a8ddb5","#43a2ca"),
              "Seq_OrRd3" = c("#fee8c8","#fdbb84","#e34a33"),
              "Seq_PuBu3" = c("#ece7f2","#a6bddb","#2b8cbe"),
              "Seq_PuBuGn3" = c("#ece2f0","#a6bddb","#1c9099"),
              "Seq_PuRd3" = c("#e7e1ef","#c994c7","#dd1c77"),
              "Seq_RdPu3" = c("#fde0dd","#fa9fb5","#c51b8a"),
              "Seq_YlGn3" = c("#f7fcb9","#addd8e","#31a354"),
              "Seq_YlGnBu3" = c("#edf8b1","#7fcdbb","#2c7fb8"),
              "Seq_YlOrBr3" = c("#fff7bc","#fec44f","#d95f0e"),
              "Seq_YlOrRd3" = c("#ffeda0","#feb24c","#f03b20"),
              "Seq_Blues3" = c("#deebf7","#9ecae1","#3182bd"),
              "Seq_Greens3" = c("#e5f5e0","#a1d99b","#31a354"),
              "Seq_Greys3" = c("#f0f0f0","#bdbdbd","#636363"),
              "Seq_Oranges3" = c("#fee6ce","#fdae6b","#e6550d"),
              "Seq_Purples3" = c("#efedf5","#bcbddc","#756bb1"),
              "Seq_Reds3" = c("#fee0d2","#fc9272","#de2d26"),
              "navy white firebrick3" = c("navy","white","firebrick3")
      )
  }
  })
  # 01. Value Projection +++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 01. key: _valproj
  # read_data
  infile_valproj <- reactive({
    input$file_valproj$datapath
  })
  rawdata_valproj <- reactive({
    data_read <- function(path1){
      filetype_split <- unlist(strsplit(path1,split = "[.]"))
      filetype <- filetype_split[length(filetype_split)]
      dat <- switch (filetype,
                     "dat" = read.table(path1,header = F),
                     "csv" = read.csv(path1,header = F),
                     "xlsx" = as.data.frame(readxl::read_excel(path1,sheet = 1,col_names = F)),
                     "xls" = as.data.frame(readxl::read_excel(path1,sheet = 1,col_names = F)),
                     "txt" = read.table(path1,header = F)
      )
      for (i in 1:ncol(dat)) {
        dat[,i] <- as.numeric(dat[,i])
      }
      dat <- na.omit(dat)
      rownames(dat) <- 1:nrow(dat)
      colnames(dat) <- c("res1","value")
      return(dat)
    }
    data_read(path1 = infile_valproj())
  })
  
  output$ui_valproj_data_DT <- renderDataTable({
      dat <- data.frame()
      if(!is.null(infile_valproj())){
        dat <- rawdata_valproj()
      }
      DT::datatable(data = dat,
                    options = list(pageLength = 25))
    })
    
  output$ui_valproj_data <- renderUI({
    dataTableOutput(outputId = "ui_valproj_data_DT",width = "50%")
  })
    
  
  output$side_valproj <- renderUI({
    column(12, 
           fluidRow(
             column(6,
                    numericInput(inputId = "midpoint_valproj",label = "Midpoint:",
                                 value = 0,step = 1)),
             column(6,
                    numericInput(inputId = "half_range_valproj",label = "Half Range:",
                                 value = 1,min = 0.0000001,step = 1)),
           ),
           fluidRow(
             column(6,
                    selectInput(inputId = "palette_valproj",label = "Palette:",
                                choices = palette_all(),selected = "navy white firebrick3",multiple = F)
                    ),
             column(6,
                    selectInput(inputId = "palette_reverse_valproj",label = "Reverse Palette:",
                                choices = c("No","Yes"),selected = "No",multiple = F)
                    )
           )
    )
  })
  
  # parameters
  midpoint_valproj <- eventReactive(input$calculate_valproj,{input$midpoint_valproj})
  half_range_valproj <- eventReactive(input$calculate_valproj,{input$half_range_valproj})
  color_valproj <- reactive({
    color <- palette_trans_fun()(input$palette_valproj)
    if(input$palette_reverse_valproj == "Yes"){
      color <- rev(color)
    }
    return(color)
  })
  
  # analysis
  res_valproj <- eventReactive(input$calculate_valproj,{
    # print("ctg_analysis_2_start...")
    analysis_fun_valproj <- function(dat,midpoint,half_range,color){
      # parameters
      fold <- 50 / half_range
      limit_lower <- -50
      limit_upper <- 50
      color_gradient <- colorRampPalette(color)(101)
      
      dat$color_index <- round((dat$value-midpoint)*fold)
      dat[which(dat$color_index > limit_upper),"color_index"] <- limit_upper
      dat[which(dat$color_index < limit_lower),"color_index"] <- limit_lower
      dat$color_index <- dat$color_index+51
      dat$color <- color_gradient[dat$color_index]
      cols_rgb <- as.data.frame(t(col2rgb(dat$color)))
      dat$red <- cols_rgb$red
      dat$green <- cols_rgb$green
      dat$blue <- cols_rgb$blue
      dat$color_pymol <- paste0("[",dat$red,",",dat$green,",",dat$blue,"]")
      
      cmd_pymol <- data.frame(cmd1 = paste0("select resi ",dat$res1,";"),
                              cmd2 = paste0("set_color color",dat$res1,",",dat$color_pymol,";"),
                              cmd3 = paste0("color color",dat$res1,",sele;"))
      cmd_chimerax <- data.frame(cmd1 = paste0("color :",dat$res1," ",dat$color,";"))
      
      return(list("dat"=dat,"cmd_pymol"=cmd_pymol,"cmd_chimerax"=cmd_chimerax,"color_gradient"=color_gradient))
    }
    analysis_fun_valproj(dat = rawdata_valproj(),midpoint = midpoint_valproj(),
                         half_range = half_range_valproj(),color = color_valproj())
  })
  
  ### pymol_valproj for display
  pymol_valproj <- reactive({
    res_valproj()$cmd_pymol
  })
  output$pymol_valproj <- renderDataTable({
    DT::datatable(data = pymol_valproj(),options = list(pageLength=25))
  })
  down_pymol_label_valproj <- eventReactive(eventExpr = input$calculate_valproj,
                                      {"Download Script"})
  output$ui_valproj_pymol <- renderUI({
    if(!is.null(infile_valproj())){
      column(12,
             fluidRow(downloadButton(outputId = "down_pymol_valproj",label = down_pymol_label_valproj())),
             br(),
             dataTableOutput("pymol_valproj",width = "80%"))
    }
  })
  
  ### chimerax_valproj for display
  chimerax_valproj <- reactive({
    res_valproj()$cmd_chimerax
  })
  output$chimerax_valproj <- renderDataTable({
    DT::datatable(data = chimerax_valproj(),options = list(pageLength=25))
  })
  down_chimerax_label_valproj <- eventReactive(eventExpr = input$calculate_valproj,
                                            {"Download Script"})
  output$ui_valproj_chimerax <- renderUI({
    if(!is.null(infile_valproj())){
      column(12,
             fluidRow(downloadButton(outputId = "down_chimerax_valproj",label = down_chimerax_label_valproj())),
             br(),
             dataTableOutput("chimerax_valproj",width = "50%"))
    }
  })
  
  # ColorBar_valproj for display
  output$ui_valproj_colorbar_control <- renderUI({
    if(!is.null(infile_valproj())){
      pattern <- c("vertical","horizontal")
      eventReactive(input$calculate_valproj,{
        column(12,
               fluidRow(
                 column(6,selectInput(inputId = "colorbar_pattern_valproj",label = "Pattern:",
                                      choices = pattern, 
                                      selected = "vertical",multiple = FALSE)),
                 column(6,numericInput(inputId = "n_break_valproj",label = "Number of breaks:",
                                       value = 5,min = 3,max = 100,step = 1)
                        ),
                 ),
               fluidRow(
                 column(6,numericInput(inputId = "colorbar_width_valproj",label = "Width (Download):",
                                       value = 1.5,min = 0,step = 1)),
                 column(6,numericInput(inputId = "colorbar_height_valproj",label = "Height (Download):",
                                       value = 6,min = 0,step = 1)
                 ),
               ),
               )
      })()
    }
    
  })
  
  colorbar_valproj <- reactive({
    colorbar_plot_fun <- function(midpoint,half_range,n_break,color_gradient,pattern){
      label <- seq(-half_range,half_range,length.out=101)
      breaks <- label[seq(1,length(label),length.out=n_break)]
      dat_bar <- data.frame(label = label)
      
      if(pattern == "horizontal"){
        p <- ggplot(dat_bar,aes(x=label,y=1))+
          geom_tile(aes(fill=label))+
          scale_fill_gradient2(low = color_gradient[1],mid = color_gradient[51],high = color_gradient[101],midpoint = midpoint,
                               limits=c(-half_range,half_range),oob=squish,breaks=c(-half_range,midpoint,half_range))+
          scale_x_continuous(expand = c(0,0),breaks = breaks)+
          scale_y_continuous(expand = c(0,0))+
          theme(legend.position = "none",
                axis.line.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                panel.border = element_rect(colour = "black",linewidth = 0.5,fill = NA),
                plot.margin = margin(20,20,20,20))+
          labs(x=NULL,y=NULL)
      }
      
      if(pattern == "vertical"){
        p <- ggplot(dat_bar,aes(y=label,x=1))+
          geom_tile(aes(fill=label))+
          scale_fill_gradient2(low = color_gradient[1],mid = color_gradient[51],high = color_gradient[101],midpoint = midpoint,
                               limits=c(-half_range,half_range),oob=squish)+
          scale_x_continuous(expand = c(0,0))+
          scale_y_continuous(expand = c(0,0),sec.axis = sec_axis(~.,breaks =  breaks))+
          theme(legend.position = "none",
                axis.line.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.y.left = element_blank(),
                axis.ticks.y.left = element_blank(),
                axis.text.y.left = element_blank(),
                panel.border = element_rect(colour = "black",linewidth = 0.5,fill = NA),
                plot.margin = margin(20,20,20,20))+
          labs(x=NULL,y=NULL)
      }
      return(p)
    }
    
    colorbar_plot_fun(midpoint = input$midpoint_valproj,
                      half_range = input$half_range_valproj,
                      n_break = input$n_break_valproj,
                      color_gradient = res_valproj()$color_gradient,
                      pattern = input$colorbar_pattern_valproj)
  })
  
  output$colorbar_valproj <- renderPlot({
    colorbar_valproj()
  })
  
  output$ui_valproj_colorbar_plot <- renderUI({
    if(!is.null(infile_valproj())){
      if(input$colorbar_pattern_valproj == "horizontal"){
        width1 <- 6*100
        height1 <- 1*100
      }
      
      if(input$colorbar_pattern_valproj == "vertical"){
        width1 <- 1.1*100
        height1 <- 6*100
      }
      
      eventReactive(input$calculate_valproj,{
        column(12,
               h3("ColorBar:"),
               h4("recommended width and height: (1.5, 6) for vertical, (6, 1) for horizontal"),
               downloadButton(outputId = "down_valproj_colorbar_png",label = "Download PNG"), 
               downloadButton(outputId = "down_valproj_colorbar_pdf",label = "Download PDF"),
               br(),
               plotOutput("colorbar_valproj",width = width1,height = height1)
        )
      })()
    }
  })
  
  # download valproj
  output$down_pymol_valproj <- downloadHandler(
    filename = paste0("Valproj_pymol_",Sys.time(),".pml"),
    content = function(file){
      write.table(pymol_valproj(),file = file,quote = F,sep = " ",row.names = F,col.names = F)
    }
  )
  
  output$down_chimerax_valproj <- downloadHandler(
    filename = paste0("Valproj_chimerax_",Sys.time(),".cxc"),
    content = function(file){
      write.table(chimerax_valproj(),file = file,quote = F,sep = " ",row.names = F,col.names = F)
    }
  )
  
  output$down_valproj_colorbar_png <- downloadHandler(
    filename = paste0("Valproj_colorbar_",Sys.time(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$colorbar_width_valproj, height = input$colorbar_height_valproj,
             dpi = 300, plot = colorbar_valproj())
    }
  )
  output$down_valproj_colorbar_pdf <- downloadHandler(
    filename = paste0("Valproj_colorbar_",Sys.time(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$colorbar_width_valproj, height = input$colorbar_height_valproj,
             plot = colorbar_valproj())
    }
  )
  
  # 01. Value Projection Introduction
  output$ui_valproj_intro <- renderUI({
    fold <- 2
    column(12,
           fluidRow(
             h2(strong("Value Projection")),
             p("Project some numeric features such as RMSF onto the protein structure",style="font-size:25px"),
             img(src = "01_value_projection.png",width=2480/4,height=1064/4),
             h3(strong("Usage:")),
             h4(strong("1. Prepare file like bellow:")),
             p("Support filetypes including ",strong(".csv"),", ",strong(".dat"),", ",strong(".txt"),
               ", ",strong(".xls"),", and ",strong(".xlsx"),style="font-size:20px"),
             p("Column names can be casual and even null, but not numeric",style="font-size:20px"),
             img(src = "01_intro_file.png",width=221/fold,height=339/fold),
             br(),
             
             h4(strong("2. Generate the script:")),
             img(src = "01_intro_sidebar.png",width=847/fold,height=608/fold),
             p("Upload your file, select value range including midpoint and half range for visualization;",style="font-size:20px"),
             p("Choose palette and direction of palette;",style="font-size:20px"),
             p("Then, click the 'Submit & Calculate' button to generate the script and colorBar.",style="font-size:20px"),
             br(),
             
             h4(strong("3. Download and run the script:")),
             h4("3.1 Pymol:"),
             img(src = "01_intro_pymol_0.png",width=1417/fold,height=611/fold),
             img(src = "01_intro_pymol_1.png",width=232/fold,height=536/fold),
             img(src = "01_intro_pymol_2.png",width=1068/fold,height=455/fold),
             br(),br(),
             h4("3.2 ChimeraX:"),
             img(src = "01_intro_chimerax_0.png",width=883/fold,height=498/fold),
             img(src = "01_intro_chimerax_1.png",width=503/fold,height=240/fold),
             img(src = "01_intro_chimerax_2.png",width=1059/fold,height=704/fold),
             br(),
             
             h4(strong("3. Download the ColorBar:")),
             img(src = "01_intro_colorbar.png",width=1260/fold,height=565/fold),
             p("You can download the ColorBar with either horizontal or vertical pattern, and edit it with image editing softwares",style="font-size:20px"),
           ))
    
  })

}
# Run the application 
shinyApp(ui = ui, server = server)


