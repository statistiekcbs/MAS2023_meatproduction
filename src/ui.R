#' Naam        : ui.R
#' Auteurs     : Nicolette de Bruijn (NBUN) en Kenneth Chin-A-Fat (KCIT)
#' Systeem     : Systeem verwerking Statistiek Slachtingen
#' Omschrijving: Deze module stuurt de grafische user interface aan.
#'


fluidPage(
  includeCSS(file.path(App$jpgmap, "cbs.css")), # importeer CBS opmaak
  tags$head(tags$style(type='text/css', ".action{ font-size: 16px;} .action{height: 40px;  width: 100px;}")),
  # Titel ----------------------------------------------------------------------
  titlePanel("Slachtingen"),
  # Sidebar Bronbestanden ------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      width=5,
      fluidRow(
        column(width=9, h3("Bronbestanden")),
        column(width=1, h3(textOutput(outputId="txt_vwmd")))
        ),
      fluidRow(
        h3(""),
        column(width=12, textInput("dir_input", label=NULL,
                                   value=alg_vind_default_inputmap())),
        tags$head(tags$style("#dir_input{text-align:right; font-size: 16px; }"))
        ),
      fluidRow(
        column(width=9, h3("")),
        column(width=1, align='center', actionButton(inputId="btn_inputdir",
                                                     icon("folder-open-o"),
                                                     label="Bladeren",
                                                     class="action"))
        ),
      fluidRow(
        column(width=4, h4("RVO slachtingen")),
        column(width=8, selectInput(inputId="input_rvo_slachtingen",
                                    label="", width="100%",
                                    choices=c("<Selecteer...>"="Empty")))
      ),
      fluidRow(
        column(width=4, h4("RVO slachthuizen")),
        column(width=8, selectInput(inputId="input_rvo_slachthuizen",
                                    label="", width="100%",
                                    choices=c("<Selecteer...>"="Empty")))
      ),
      hr(
        style = "border-top: 1px solid #a9a9a9;"
      ),
      fluidRow(
        column(width=4, h4("RVO slachtingen per slachthuis")),
        column(width=8, selectInput(inputId="output_rvo_naar_spek",
                                    label="", width="100%",
                                    choices=c("<Selecteer...>"="Empty")))
      ),
      fluidRow(
        column(width=4, h4("Aantal roodvlees")),
        column(width=8, selectInput(inputId="input_aant_rdvl",
                                    label="", width="100%",
                                    choices=c("<Selecteer...>"="Empty")))
        ),
      fluidRow(
        column(width=4, h4("Aantal witvlees")),
        column(width=8, selectInput(inputId="input_aant_wtvl",
                                    label="", width="100%",
                                    choices=c("<Selecteer...>"="Empty")))
        ),
      fluidRow(
        column(width=4, h4("Gewicht en verdeling roodvlees")),
        column(width=8, selectInput(inputId="input_gemg_rdvl",
                                    label="", width="100%",
                                    choices=c("<Selecteer...>"="Empty")))
        ),
      fluidRow(
        column(width=4, h4("Gewicht kalveren")),
        column(width=8, selectInput(inputId="input_gemg_kalveren",
                                    label="", width="100%",
                                    choices=c("<Selecteer...>"="Empty")))
        ),
      fluidRow(
        column(width=4, h4("Gewicht kalveren vorig jaar")),
        column(width=8, selectInput(inputId="input_gemg_kalveren_vj",
                                    label="", width="100%",
                                    choices=c("<Selecteer...>"="Empty")))
        ),
      fluidRow(
        column(width=2),
        column(width=8, imageOutput(outputId="plaatje", height="420", width="420"))
        ),
      fluidRow(
        column(width=8),
        actionButton(inputId="btn_ververs_plaatje", icon("refresh"))
        ),
      fluidRow(
        column(width=10),
        actionButton(inputId="btn_help", icon("book"), label="Help", class="action")
        )        
    ),
    mainPanel(
      width=6,
      h3("RVO naar SPEK"),
      br(),
      fluidRow(
        column(width=1, align='left', actionButton(inputId="btn_spek_input_maken",
                                                   icon("table"),
                                                   label="Maken",
                                                   class="action")),
        column(width=1),
        column(width=10, align='left', h4(textOutput(outputId="txt_spek_input_1")))
      ),
      h3("Controle en correctie"),
      h3(""),
      fluidRow(
        column(width=1, align='left', actionButton(inputId="btn_ctcr_maken",
                                                   icon("table"),
                                                   label="Maken",
                                                   class="action")),
        column(width=1),
        column(width=10, align='left', h4(textOutput(outputId="txt_ctcr_1")))
        ),
      h3(""),
      fluidRow(
        column(width=1, align='left', actionButton(inputId="btn_ctcr_openen",
                                                   icon("folder-open-o"),
                                                   label="Openen",
                                                   class="action")),
        column(width=1),
        column(width=10, align='left', h4(textOutput(outputId="txt_ctcr_2")))
        ),
      h3(""),
      fluidRow(
        column(width=2, style="padding:20px;"),
        column(width=10, align='left', h4(textOutput(outputId="txt_ctcr_3")))
        ),
      h3(""),
      fluidRow(
        column(width=2, style="padding:20px;"),
        column(width=10, align='left', h4(textOutput(outputId="txt_ctcr_4")))
        ),
      h1("", style="padding:20px;"),
      h3("Opslag in database"),
      h3(""),
      fluidRow(
        column(width=1, align='left', actionButton(inputId="btn_opslaan",
                                                   icon("database"),
                                                   label="Opslaan",
                                                   class="action")),
        column(width=1),
        column(width=10, align='left', h4(textOutput(outputId="txt_opslag")))
        ),
      h1("", style="padding:20px;"),
      h3("Analyse"),
      h3(""),
      fluidRow(
        column(width=1, align='left', actionButton(inputId="btn_ana_maken",
                                                   icon("table"),
                                                   label="Maken",
                                                   class="action")),
        column(width=1),
        column(width=10, align='left', h4(textOutput(outputId="txt_analyse_1")))
        ),
      h3(""),
      fluidRow(
        column(width=1, align='left', actionButton(inputId="btn_ana_openen",
                                                   icon("folder-open-o"),
                                                   label="Openen",
                                                   class="action")),
        column(width=1),
        column(width=10, align='left', h4(textOutput(outputId="txt_analyse_2")))
        ),
      h1("", style="padding:20px;"),
      h3("Output"),
      h3(""),
      fluidRow(
        column(width=1, align='left', actionButton(inputId="btn_output_maken",
                                                   icon("table"),
                                                   label="Maken",
                                                   class="action")),
        column(width=1),
        column(width=10, align='left', h4(textOutput(outputId="txt_output_1")))
        ),
      h3(""),
      fluidRow(
        column(width=2, style="padding:20px;"),
        column(width=10, align='left', h4(textOutput(outputId="txt_output_2")))
        ),
      h3(""),
      fluidRow(
        column(width=2, style="padding:20px;"),
        column(width=10, align='left', h4(textOutput(outputId="txt_output_3")))
        ),
      h3(""),
      fluidRow(
        column(width=2, style="padding:20px;"),
        column(width=10, align='left', h4(textOutput(outputId="txt_output_4")))
        ),
      h3(""),
      fluidRow(
        column(width=2, style="padding:20px;"),
        column(width=10, align='left', h4(textOutput(outputId="txt_output_5")))
        ),
      h1("", style="padding:20px;")
    )
    )
  )

