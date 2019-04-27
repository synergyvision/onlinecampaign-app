shinyUI( dashboardPage(title='Synergy Vision', skin = "purple",
                    dashboardHeader(title=tags$img(src="img/vision.png", width=100)),
                    dashboardSidebar(
                      sidebarMenu(
                        h4("Campaña Online",style="text-align:center"),
                        menuItem("Introducción",tabName = 'intro',icon = icon("align-justify")),
                        menuItem("Datos",tabName = 'datos',icon = icon("folder-open")),
                        menuItem("ACP",tabName = 'acp',icon = icon("calculator")),
                        menuItem("Agrupación",tabName = 'agrup',icon = icon("th",lib = "glyphicon")),
                        menuItem("Resumen",tabName = 'resu',icon = icon("signal",lib = "glyphicon")),
                        menuItem("Modelos lineales",tabName = 'Glm',icon = icon("signal",lib = "glyphicon"),
                                 menuSubItem("Generalizado", tabName = "GLM", icon = icon("circle-o"))),
                        
                        menuItem("Series Temporales",tabName = 'series',icon = icon("external-link"))
                        
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'intro',
                                fluidRow(box(title = tags$b(h3("Introducción")),width = 12,status = "primary",p("El marketing digital (o marketing online) engloba todas aquellas acciones y estrategias publicitarias o comerciales que se ejecutan en los medios y canales de internet. El marketing digital pone a nuestra disposición una serie de herramientas de gran diversidad desde las que pueden realizarse desde pequeñas acciones a prácticamente coste cero hasta complejas estrategias (y obviamente más costosas) en las que se pueden combinar infinidad de técnicas y recursos. Los buscadores, como Google, Yahoo o Bing, son herramientas que permiten a los usuarios de internet encontrar contenidos relacionados con aquello que están buscando. 
                                                                                                                Para poder posicionar con éxito una página o blog en las primeras posiciones de los buscadores y conseguir visitantes, es imprescindible realizar acciones de posicionamiento orgánico (SEO) o de pago (SEM) en una estrategia de marketing online.",style = "font-size: 16px"),div(img(src="img/seo.png",width=500),style="text-align: center;"))),
                                fluidRow(column(width=4,box(width = 12,title = tags$b(h3("Análisis de componentes principales")),status='warning',p('El análisis de componentes principales (ACP), PCA en sus siglas en Inglés, es una técnica
                                                                                                                                                    multivariada de datos que se basa principalmente en la reducción de dimensionalidad de un
                                                                                                                                                    conjunto de datos. Las componentes principales son combinaciones lineales de las variables
                                                                                                                                                    originales, las cuales no son correlacionadas y ordenadas de modo que los primeros pocos
                                                                                                                                                    representen la mayor variabilidad de todas las variables originales y
                                                                                                                                                    en consecuencia proveen una base más simple para el tratamiento de los datos.',style = "font-size: 16px"),div(img(src="img/pca.png",width=250),style="text-align: center;"))),
                                         column(width = 4,box(width = 12,title = tags$b(h3("Agrupaciones")),status="warning",p('El análisis de grupos es uno de los métodos impotantes de la minería de datos para
                                                                                                                               descubrir conocimiento de un conjunto de datos multidimensional. El objetivo de agrupar es
                                                                                                                               identificar patrones o grupos de objetos similares dentro de un conjunto de dato de interés.',style = "font-size: 16px"),div(img(src="img/cluster.jpg",width=250),style="text-align: center;"))),
                                         column(width = 4,box(width = 12,title = tags$b(h3("Series Temporales")),status='warning',p('El análisis de los datos experimentales que se han observado en diferentes puntos en
                                                                                                                                    el tiempo conduce a problemas nuevos y únicos en la modelización e inferencia estadística. El enfoque sistemático por el cual se trata de
                                                                                                                                    responder a las preguntas matemáticas y estadísticas planteadas por estas correlaciones se
                                                                                                                                    conoce comúnmente como análisis de series de tiempo. El objetivo primario en el análisis de series de tiempo es desarrollar modelos matemáticos
                                                                                                                                    que provean una descripción apropiada para los datos muestrales.',style = "font-size: 16px"),div(img(src="img/serie.jpg",width=250),style="text-align: center;"))))),
                        tabItem(tabName = 'datos',
                                fluidRow(box(width = 3,
                                             title = 'Cargar Datos',
                                             fileInput(inputId = 'cargardatos',
                                                       label = "Seleccionar archivo:",
                                                       buttonLabel = "Buscar...",
                                                       placeholder = "Aun no seleccionas el archivo..."),
                                             tags$hr(),
                                             p('Cargar datos en formato .xlsx'),
                                             
                                             checkboxGroupInput(inputId = 'columnas',
                                                                label = 'Seleccione de lo siguiente las columnas numéricas que desee usar para los análisis posteriores',
                                                                choices = ''
                                             )
                                ),
                                box(width = 9,
                                    h3('Datos:'),
                                    dataTableOutput('datos1')))
                        ),
                        tabItem(tabName = "acp",
                                fluidRow(tabBox(width = 12,
                                                title = "",id="tab1",
                                                tabPanel(h4("Métodos"),
                                                         fluidRow(column(width=3,fluidRow(column(width=12,radioButtons(inputId = "metodo1",label = "Elegir método",selected = '',
                                                                                                                       choices = c("Matriz de Correlación","Matriz de Covarianza")
                                                         ))),fluidRow(box(title = "Observación",width = 12, solidHeader = TRUE,status = "primary",'La matriz de correlación se usa cuando los datos no son dimensionalmente homogéneos o el orden de magnitud de las variables no es el mismo, en cambio, la matriz de covarianza se usa cuando los valores son medios similares',collapsible = TRUE,collapsed =TRUE))
                                                         ),
                                                         conditionalPanel(condition = "input.metodo1=='Matriz de Correlación'",column(width = 9,h3("Matriz de Correlación"),div(style='overflow-x: scroll',
                                                                                                                                                                                tableOutput("datos2")
                                                         ))
                                                         ),
                                                         conditionalPanel(condition = "input.metodo1=='Matriz de Covarianza'",column(width = 9,h3("Matriz de Covarianza"),div(style='overflow-x: scroll',
                                                                                                                                                                              tableOutput("datos3")
                                                         ))
                                                         )
                                                         ),
                                                         fluidRow(
                                                           column(width=6,h3("Porcentaje de varianza por componente principal"),plotOutput("imagen1")),
                                                           column(width=6,h3("Porcentaje de varianza Acumulada por cada componente"),plotOutput("imagen2"))
                                                         )
                                                ),
                                                tabPanel(h4("Variables Representativas"),fluidRow(box(solidHeader = TRUE,width=12,uiOutput("slid"))),
                                                         fluidRow(box(verbatimTextOutput("variable"),title = "Variables Representativas de las Componentes Seleccionadas",width = 12,status = "primary")),
                                                         fluidRow(box(title = "Observación",width = 8,solidHeader = TRUE,status = "primary","Se recomienda usar el número de componenentes que representen un 80% de variabilidad de los datos. El porcentaje de variabilidad de las componentes principales lo observamos anteriormente.",collapsible = TRUE,collapsed =TRUE),
                                                                  box(background = 'purple',width=4,div(img(src="img/vision.png", width=200),style="text-align: center;")))),
                                                
                                                tabPanel(h4("Proyecciones"),fluidRow(column(width = 5,h3('Componente 1 Vs. Componente 2'),plotOutput('imagen3')),column(width = 5,h3('Componente 2 Vs. Componente 1'),plotOutput('imagen4'))))))
                        ),
                        tabItem(tabName = 'agrup',
                                fluidRow(tabBox(width = 12,
                                                title = '',id='tab2',
                                                tabPanel(h4('Elección de grupos'),fluidRow(column(width=3,fluidRow(box(width = 12,title = 'Variables Númericas',uiOutput('colun')))
                                                ),
                                                column(width = 6,h3("Diagrama de Codo"),plotOutput("elbow"))
                                                ),
                                                fluidRow(box(width = 3,title = 'Observación',solidHeader = TRUE,status = "primary","Se recomienda usar las variables representativas que se consideran en el Análisis de Componentes Principales",collapsible = TRUE,collapsed =TRUE),box(width = 4,title = 'Observación',solidHeader = TRUE,status = "primary","El diagrama de codo recomienda el número apropiado de grupos a usar para la agrupación",collapsible = TRUE,collapsed =TRUE)),
                                                fluidRow(column(width=4,sliderInput(inputId = 'cantidadgrupos',label = 'Elija la cantidad de grupos a formar:',min = 2,max = 15,value = 2))),
                                                fluidRow(column(width = 10,h4('Agrupación'),plotOutput('clusplot')))),
                                                tabPanel(h4('Resultados'),fluidRow(column(width = 8,h3('Método de la Silueta'),plotOutput('silueta')),column(width=4,box(title = "Observación",width = 12, solidHeader = TRUE,status = "primary",'El analisis de la Silueta mide que tan bien se agrupo una observacion comparando su similitud con el resto de observaciones de su cluster frente a las de los otros clusters. El valor del índice de la silueta esta entre los valores -1 y 1, siendo el valor 1 un indicativo que la observacion se ha asignado al grupo correcto y -1 como una mala asignacion. El método de la silueta consiste en promediar todos estos índices.',collapsible = TRUE,collapsed =TRUE))),
                                                         fluidRow(column(width = 6,h3('Componente 1 Vs. Componente 2'),plotOutput('comp1')),
                                                                  column(width = 6,h3('Componente 2 Vs. Componente 1'),plotOutput('comp2'))))))),
                        tabItem(tabName = 'resu',
                                fluidRow(
                                  box(width = 6,uiOutput('elecciongrupos'),
                                      title = 'Resumen de los grupos resultantes',status = 'primary',solidHeader = TRUE,
                                      div(style='overflow-x: scroll',tableOutput('holaprueba')),tags$hr(),h3('Estructura de grupos'),tableOutput('ngruposss')),
                                  box(width = 6,h3('Datos del grupo seleccionado:'),dataTableOutput('grupoelegido')))),
                        
                        
                        tabItem(tabName = 'GLM',
                                
                                fluidRow(column(4,box(width = 10,title = "Datos",selectInput("dat","Selecione",choices = c("Originales","ACP","Cluster"))),conditionalPanel( condition = "input.dat=='Cluster'", box(width = 12,selectInput("select", h3("Escoga grupo"), choices = list("Choice 1" = 1), selected = 1),
                                                                                                                                                                                                                     title = 'Resumen de los grupos resultantes',status = 'primary',solidHeader = TRUE
                                                                                                                                                                                                                     ,tags$hr(),h3('Estructura de grupos'),tableOutput('ngruposssglm')) )
                                ),column(8,box(width = 10,title = "datos",dataTableOutput("datMod")))
                                
                                
                                ),
                                fluidRow( box( background="yellow",width=120,status = "warning",
                                               selectInput('columns4', 'Selecciona variable de estudio', "Seleccione primero los datos"))),
                                
                                
                                fluidRow( box( background="yellow",width=12,status = "warning",plotlyOutput('Histograma2'))),
                                fluidRow(
                                  box(title = h3("Prueba de hipótesis de normalidad"), style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatable12'))
                                ),
                                
                                
                                fluidRow(column(4,box(title = h3("Seleccione variables independientes"), style = "overflow-x:scroll",width=12,status = "warning",checkboxGroupInput("selec2",c("Seleccione")))),
                                         column(4,box(title = h3("Tipo de Modelo"), style = "overflow-x:scroll",width=12,status = "warning",radioButtons("selec3","Familia",
                                                                                                                                                         choices = list("binomial"=1,"gaussian"=2, "Gamma"=3,"inverse.gaussian"=4,"poisson"=5,"quasi"=6,"quasibinomial"=7,"quasipoisson"=8),selected = 2))),
                                         column(4,box(title = h3("Enlace"), style = "overflow-x:scroll",width=12,status = "warning",radioButtons("enlace","Enlace",
                                                                                                                                                 choices = c(""))))
                                ),
                                
                                fluidRow(
                                  box(title = h3("Resultados gráficos"), style = "overflow-x:scroll",width=12,status = "warning",plotOutput("grafi2"))
                                ),
                                
                                fluidRow(
                                  box(title = h3("Resumen"), style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput("summar2"))
                                ),
                                
                                
                                fluidRow(
                                  box(title = h3("Coeficientes del modelo generalizado"), style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput("coeficien2"))
                                )
                                
                                
                        ),
                        
                        tabItem(tabName = 'series',
                                fluidRow(tabBox(width = 12,
                                                title = '',id='tab3',
                                                tabPanel(h4('Elección de Variables'),fluidRow(column(width=3,fluidRow(box(width = 12,title = 'Variables Númericas',uiOutput('colun1'))),
                                                                                                     fluidRow(box(width = 12,title = "Grupo a analizar",uiOutput("gr1")))                                       
                                                ),column(width = 9,h3("Serie Temporal",plotOutput("ser"))))),
                                                tabPanel(h4("Descomposición Serie"),fluidRow(column(width = 8,h3("Descomposición"),plotOutput("descom")),column(width=4,box(title = "Observación",width = 12, solidHeader = TRUE,status = "primary",'El análisis clásico de las series de tiempo se basa en la suposición de que los valores que toma la variable de observación es la consecuencia de tres componentes, cuya actuación conjunta da como resultados los valores medidos, estas componentes son: Componente tendencia (trend), Componente estacional (seasonal), Componente aleatoria (random).',collapsible = TRUE,collapsed =TRUE)))),
                                                tabPanel(h4("Modelo"),fluidRow(column(width = 10,h3("Modelo Sugerido"),verbatimTextOutput("modelo"))),fluidRow(column(width = 10,h3("Serie Vs. Modelo fijado"),plotOutput("fijado")))),
                                                tabPanel(h4("Residuales"),fluidRow(column(width = 10,h3("Residuales"),plotOutput("resi"))),fluidRow(column(width = 10,h3("Q-Q Normal"),plotOutput("qq")))),
                                                tabPanel(h4("Predicción"),fluidRow(box(width = 10,uiOutput("nrodepred"),title = "Cantidad a predecir")),fluidRow(column(width = 10,h3("Predicción"),plotOutput("predic")))))))
                                         )
                      
                                         )
                                         )
)