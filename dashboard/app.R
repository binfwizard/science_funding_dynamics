library(shiny)
library(tidyverse)
library(readr)
library(plotly)
library(DT)
library(scales)
library(DescTools)   
library(here)

# ---- load processed analysis data ----
grants <- read_csv(here("data", "processed", "grants_analysis.csv")) %>%
  mutate(
    fiscal_year      = as.integer(fiscal_year),
    cancelled        = as.logical(cancelled),
    cancelled_label  = factor(cancelled_label),
    total_obligated_amount = as.numeric(total_obligated_amount)
  ) %>%
  filter(fiscal_year >= 2018, fiscal_year <= 2025)

# static logistic regression model on full data
#logit_model <- glm(
#  cancelled ~ fiscal_year + log_funding,
#  data   = grants,
#  family = binomial()
# )

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
     body {
  background-color: #fff7fb !important; 
     }
     
  /*TABSET STYLING */
    /* base tab look (inactive) */
    .nav-tabs > li > a {
      color: #666666;             
      font-weight: 500;
      border-radius: 8px 8px 0 0;  
      border: 1px solid transparent;
      padding: 8px 20px;
    }

    /* hover on inactive tabs */
    .nav-tabs > li > a:hover {
      background-color: #f5f5f5;
      border-color: #dddddd #dddddd transparent;
      color: #333333;
    }

    /* ACTIVE tab */
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background-color: #fff8fb;               /* light pink background */
      border-color: #f2b5d4 #f2b5d4 transparent; /* pink border on top/sides */
      color: #b81f74;                           /* dark pink text */
      font-weight: 600;
    }

    /* move the content up a bit so it touches the active tab nicely */
    .tab-content {
      border-top: 1px solid #f2f2f2;
      margin-top: -1px;
      padding-top: 15px;
    }
    
      /* Hypothesis cards */
      details.hypo-card {
        border: 1px solid #f2b5d4;
        border-radius: 10px;
        padding: 8px 12px;
        margin-bottom: 8px;
        background-color: #fff8fb;
      }
      details.hypo-card > summary {
        font-weight: 600;
        color: #b81f74;
        cursor: pointer;
        list-style: none;
      }
      details.hypo-card > summary::-webkit-details-marker {
        display: none;
      }
      details.hypo-card > summary::before {
        content: '▸ ';
        color: #b81f74;
      }
      details.hypo-card[open] > summary::before {
        content: '▾ ';
      }

      /* KPI cards */
      .kpi-card {
        border: 1px solid #f2b5d4;
        border-radius: 14px;
        background-color: #fafafa;
        text-align: center;
        padding: 14px 8px;
        min-height: 110px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        transition: transform 0.18s ease-out, box-shadow 0.18s ease-out;
      }
      
      .kpi-card:hover {
      transform: translateY(-2px);
      box-shadow: 0 8px 18px rgba(0, 0, 0, 0.08);
      }

      /* KPI label (Total grants, etc.) */
      .kpi-card h4 {
        font-weight: 500;
        font-size: 1.05em;
        margin-top: 0;
        margin-bottom: 4px;
        color: #333333;
      }

      /* KPI value */
      .kpi-card .shiny-text-output {
        color: #b81f74;
        font-weight: 600;
        font-size: 1.4em;
        margin-top: 2px;
      }
      
      .click-summary {
      color: #b81f74;
      font-weight: 600;
      cursor: pointer;
      }
      
      .app-title {
      width: 100%;
      display: block;
      text-align: center;
      font-size: 2.4em;
      font-weight: 700;
      color: #b81f74;
      margin-top: 10px;
      margin-bottom: 25px;
      white-space: nowrap;
      overflow: hidden;
      font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI',
             Roboto, Helvetica, Arial, sans-serif;
      }
      
      body, label, input, button, select, p, h1, h2, h3, h4, h5 {
      font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI',
               Roboto, Helvetica, Arial, sans-serif;
      }
      
      /*SIDEBAR*/
.sidebar-card {
  background-color: #fafafa;
  border: 1px solid #e3e3e3;
  border-radius: 14px;
  padding: 18px 16px;
  margin-top: 10px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
}

/* spacing between logical sections */
.sidebar-section {
  margin-bottom: 18px;
}

/* section titles */
.sidebar-card h4 {
  margin-top: 0;
  margin-bottom: 8px;
  font-weight: 600;
  font-size: 0.98em;
  letter-spacing: 0.01em;
  color: #b81f74;
}

/* checkbox & radio labels */
.sidebar-card .checkbox label,
.sidebar-card .radio label {
  font-size: 0.92em;
  color: #333333;
}

/* tighten vertical spacing a bit */
.sidebar-card .checkbox,
.sidebar-card .radio {
  margin-bottom: 6px;
}

/* ---------- SLIDER (pink theme) ---------- */

/* Main bar + active bar */
.sidebar-card .irs--shiny .irs-bar,
.sidebar-card .irs--shiny .irs-line {
  background-color: #b81f74 !important;
  border-color: #b81f74 !important;
}

/* Pink handle */
.sidebar-card .irs--shiny .irs-handle > i:first-child {
  background-color: #b81f74 !important;
  border-color: #b81f74 !important;
}

/* Hover handle */
.sidebar-card .irs--shiny .irs-handle:hover > i:first-child {
  background-color: #8c1259 !important;
}

/* ---------- Pink YEAR BUBBLES above handles ---------- */
.sidebar-card .irs--shiny .irs-single,
.sidebar-card .irs--shiny .irs-from,
.sidebar-card .irs--shiny .irs-to {
  background: #b81f74 !important;
  border-color: #b81f74 !important;
  color: white !important;
  font-weight: 600;
}

/* Pink min/max labels (left/right year text) */
.sidebar-card .irs--shiny .irs-min,
.sidebar-card .irs--shiny .irs-max {
  color: #b81f74 !important;
  font-weight: 600;
}

/* Pink tick labels under the slider */
.sidebar-card .irs--shiny .irs-grid-text {
  color: #b81f74 !important;
  font-size: 0.8em;
}

/* ---------- Pink CHECKBOXES ---------- */
.sidebar-card input[type=checkbox] {
  -webkit-appearance: none;
  appearance: none;
  width: 16px;
  height: 16px;
  border: 2px solid #b81f74;
  border-radius: 4px;
  margin-right: 6px;
  position: relative;
  cursor: pointer;
}

/* checked state */
.sidebar-card input[type=checkbox]:checked {
  background-color: #b81f74;
  border-color: #b81f74;
}

.sidebar-card input[type=checkbox]:checked::after {
  content: \"✔\";
  color: white;
  font-size: 12px;
  position: absolute;
  top: -1px;
  left: 2px;
}

/* hover effect */
.sidebar-card input[type=checkbox]:hover {
  border-color: #8c1259;
}

/* ---------- Pink RADIO BUTTONS (SIDEBAR) ---------- */
.sidebar-card input[type=radio] {
  -webkit-appearance: none;
  appearance: none;
  width: 16px;
  height: 16px;
  border: 2px solid #b81f74;
  border-radius: 50%;
  margin-right: 6px;
  position: relative;
  cursor: pointer;
}

.sidebar-card input[type=radio]:checked {
  border-color: #b81f74;
  background-color: #b81f74;
}

.sidebar-card input[type=radio]:checked::after {
  content: '';          /* <— single quotes */
  width: 8px;
  height: 8px;
  background: white;
  border-radius: 50%;
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
}

.sidebar-card input[type=radio]:hover {
  border-color: #8c1259;
}

/* ===== GLOBAL PINK RADIO BUTTONS (for main panel tabs) ===== */
input[type=radio] {
  -webkit-appearance: none;
  appearance: none;
  width: 16px;
  height: 16px;
  border: 2px solid #b81f74;
  border-radius: 50%;
  margin-right: 6px;
  position: relative;
  cursor: pointer;
}

/* Checked state */
input[type=radio]:checked {
  border-color: #b81f74;
  background-color: #b81f74;
}

/* White dot centered */
input[type=radio]:checked::after {
  content: '';
  width: 8px;
  height: 8px;
  background: white;
  border-radius: 50%;
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%);
}

/* Hover */
input[type=radio]:hover {
  border-color: #8c1259;
}
    "))
  ),
  div(
    class = "app-title",
    HTML("Science Funding Dynamics: Federal Science Grants 2018-2025")
  ),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-card",

      div(
        class = "sidebar-section",
        h4("Filter by agency"),
        checkboxGroupInput(
          "agency",
          label   = NULL,  # label handled by h4 above
          choices  = sort(unique(grants$awarding_agency_name)),
          selected = sort(unique(grants$awarding_agency_name))
        )
      ),
      
      div(
        class = "sidebar-section",
        h4("Fiscal year range"),
        sliderInput(
          "year_range",
          label = NULL,
          min   = min(grants$fiscal_year, na.rm = TRUE),
          max   = max(grants$fiscal_year, na.rm = TRUE),
          value = c(min(grants$fiscal_year, na.rm = TRUE),
                    max(grants$fiscal_year, na.rm = TRUE)),
          step  = 1,
          sep   = ""
        )
      ),
      
      div(
        class = "sidebar-section",
        h4("Cancellation status"),
        radioButtons(
          "cancel_filter",
          label   = NULL,
          choices = c("All grants"        = "all",
                      "Cancelled only"    = "cancelled",
                      "Active only"       = "active"),
          selected = "all"
        )
      ),
      
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        # ---- Overview tab ----
        tabPanel(
          "Overview",
          br(),
          
          # KPI row
          fluidRow(
            column(
              3,
              wellPanel(
                class = "kpi-card",
                h4("Total grants"),
                textOutput("kpi_total_grants")
              )
            ),
            column(
              3,
              wellPanel(
                class = "kpi-card",
                h4("Total obligations"),
                textOutput("kpi_total_dollars")
              )
            ),
            column(
              3,
              wellPanel(
                class = "kpi-card",
                h4("Cancelled dollars"),
                textOutput("kpi_cancel_dollars")
              )
            ),
            column(
              3,
              wellPanel(
                class = "kpi-card",
                h4("Cancellation rate"),
                textOutput("kpi_cancel_rate")
              )
            )
          ),
          hr(),
          # Background + research question
          fluidRow(
            column(
              6,
              h3("Background"),
              p("From fiscal years 2018-2025, major U.S. science agencies \
(HHS, NSF, DOE, NASA, USDA, EPA, and DoD) awarded hundreds of thousands of \
federal assistance grants to support research and related activities."),
              p("These awards span the pre-COVID period, the COVID-19 pandemic, and the \
recent 2024 administration transition period, providing an opportunity to explore \
how funding volumes, dollar amounts, and cancellation patterns evolved over time.")
            ),
            column(
              6,
              h3("Research Question"),
              p(strong("How have federal science grant allocations (volume, funding levels, \
and cancellations) changed over fiscal years 2018–2025, particularly during and \
after the COVID-19 pandemic and the 2024 administration transition period (2023–2025)?")),
              br(),
              uiOutput("overview_sentence")
            )
          ),
          
          hr(),
          
          # Hypotheses (clickable cards)
          h3("Hypotheses"),
          tags$details(
            class = "hypo-card",
            tags$summary("Hypothesis 1: Funding levels over time"),
            p("H₀: Median obligated funding per grant does not differ across fiscal years 2018–2025."),
            p("H₁: At least one fiscal year has a different typical funding level.")
          ),
          tags$details(
            class = "hypo-card",
            tags$summary("Hypothesis 2: Funding volume over time"),
            p("H₀: The number of science grants awarded per fiscal year is constant over 2018–2025."),
            p("H₁: Funding volume (grant counts) changes across fiscal years.")
          ),
          tags$details(
            class = "hypo-card",
            tags$summary("Hypothesis 3: Cancellation rates over time"),
            p("H₀: The proportion of cancelled grants is constant across fiscal years 2018–2025."),
            p("H₁: At least one fiscal year has a different cancellation proportion \
(or a systematic trend over time).")
          ),
          
          hr(),
          
          # Data & analysis bullets
          h3("Data & Analysis"),
          fluidRow(
            column(
              6,
              h4("Data collection"),
              tags$ul(
                tags$li("Prime award summary records downloaded from USAspending.gov \
for FY 2018-2025."),
                tags$li("Restricted to major science-focused agencies \
(HHS, NSF, DOE, NASA, USDA, EPA, DoD)."),
                tags$li("Excluded loans and certain assistance types (such as large formula/block grants) \
to focus on research-style awards."),
                tags$li("Defined a grant as \"cancelled\" when total obligated amount was \
negative (net de-obligation).")
              )
            ),
            column(
              6,
              h4("Analysis workflow"),
              tags$ul(
                tags$li("Cleaned and merged yearly CSV files into a single dataset."),
                tags$li("Created derived variables including fiscal year, cancellation status, \
and log-transformed funding."),
                tags$li("Built interactive Shiny views for time trends, agency breakdowns, \
and filtered grant tables."),
                tags$li("Applied chi-square tests and Cochran-Armitage trend tests to evaluate \
changes in cancellation rates over time.")
              )
            )
          ),
          
          hr(),
          
          # Results for H3 + stats
          h3("Results Snapshot (Hypothesis 3: Cancellation Rates)"),
          tags$ul(
            tags$li("Cancellation rates differ significantly by fiscal year (chi-square test of independence, p < 0.001), demonstrating that cancellation behavior is not constant over time."),
            tags$li("Although the overall association is statistically small (Cramer's V ≈ 0.09), the pattern is directional rather than random; early years (2018-2021) show higher cancellation rates, while recent years (2023-2025) show sharply lower rates."),
            tags$li("A Cochran–Armitage trend test shows a strong downward trend in cancellation probability (p < 0.001), indicating that cancellations consistently declined year over year rather than fluctuating unpredictably."),
            tags$li("Together, these results show that cancellation rates decreased dramatically despite rising grant volume, suggesting increased award stability or changes in administrative or funding practices."),
            tags$li("The 'Cancellations Over Time' tab visualizes year-to-year changes in cancellation counts and total cancelled dollars."),
            tags$li("The 'Agency Breakdown' tab highlights how cancellation rates and funding levels differ across agencies."),
            tags$li("The fiscal-year line chart shows the sharp decline in cancellation rates from 2018 to 2025.")
          ),
          tags$details(
            class = "hypo-card",
            tags$summary(
              HTML("<span class='click-summary'>View full statistical output for Hypothesis 3</span>")
            ),
            
            h5("Per-year cancellation summary"),
            DTOutput("cancel_summary_table"),
            
            h5("Chi-square Test & Cramer's V"),
            verbatimTextOutput("chisq_output"),
            
            h5("Post-hoc pairwise proportion tests"),
            verbatimTextOutput("pairwise_output"),
            
            h5("Cochran-Armitage Trend Test"),
            verbatimTextOutput("trend_test_output")
          ),
          
          hr(),
          
          # Next steps + references
          h3("Next Steps"),
          tags$ul(
            tags$li("Stratify trends by agency to see whether specific agencies drive changes in cancellation rates."),
            tags$li("Incorporate award duration and recipient characteristics to explain high-risk cancellation profiles."),
            tags$li("Extend the analysis beyond 2025 as new fiscal year data become available."),
            tags$li("Incorporate unweighted logistic regression modeling to predict cancellation probability.")
          ),
          
          h3("References"),
          tags$ul(
            tags$li("USAspending.gov. U.S. Department of the Treasury, Bureau of the Fiscal Service. https://www.usaspending.gov/search
. Accessed 22 Nov. 2025."),
            tags$li("“Get Started.” Shiny by Posit, https://shiny.posit.co/py/get-started/
. Accessed 1 Dec. 2025."),
            tags$li("“Plotly for R.” Plotly, https://plotly.com/r/
                       . Accessed 1 Dec. 2025.")
          )
        ),
        
        # ---- Time tab ----
        tabPanel(
          "Cancellations Over Time",
          br(),
          radioButtons(
            "time_metric",
            "Metric",
            choices = c("Count of cancelled grants" = "count",
                        "Cancelled dollars"        = "dollars"),
            inline = TRUE
          ),
          plotlyOutput("time_plot", height = "450px")
        ),
        
        # ---- Agency tab ----
        tabPanel(
          "Agency Breakdown",
          br(),
          plotlyOutput("agency_plot", height = "450px")
        ),
        
        # ---- Details tab ----
        tabPanel(
          "Explore Details",
          br(),
          downloadButton("download_data", "Download filtered data (CSV)"),
          br(), br(),
          DTOutput("details_table")
        )
      )
    )))

server <- function(input, output, session) {
  
  # ---- filtered data based on sidebar ----
  filtered_data <- reactive({
    dat <- grants %>%
      filter(
        fiscal_year >= input$year_range[1],
        fiscal_year <= input$year_range[2]
      )
    
    if (!is.null(input$agency) && length(input$agency) > 0) {
      dat <- dat %>% filter(awarding_agency_name %in% input$agency)
    }
    
    if (input$cancel_filter == "cancelled") {
      dat <- dat %>% filter(cancelled)
    } else if (input$cancel_filter == "active") {
      dat <- dat %>% filter(!cancelled)
    }
    
    dat
  })
  
  # ========= OVERVIEW =========
  
  output$kpi_total_grants <- renderText({
    nrow(filtered_data())
  })
  
  output$kpi_total_dollars <- renderText({
    base <- grants %>%
      filter(
        fiscal_year >= input$year_range[1],
        fiscal_year <= input$year_range[2],
        awarding_agency_name %in% input$agency
      )
    if (nrow(base) == 0) return("$0")
    
    total_oblig_all   <- sum(pmax(base$total_obligated_amount, 0), na.rm = TRUE)
    cancelled_idx     <- base$cancelled %in% TRUE
    cancelled_dollars <- sum(abs(base$total_obligated_amount[cancelled_idx]), na.rm = TRUE)
    active_dollars    <- total_oblig_all - cancelled_dollars
    
    val <- switch(
      input$cancel_filter,
      "all"       = total_oblig_all,
      "cancelled" = cancelled_dollars,
      "active"    = active_dollars
    )
    
    dollar(val)
  })
  
  output$kpi_cancel_rate <- renderText({
    dat <- filtered_data()
    if (nrow(dat) == 0) return("NA")
    rate <- mean(dat$cancelled, na.rm = TRUE)
    paste0(round(100 * rate, 1), "%")
  })
  
  output$kpi_cancel_dollars <- renderText({
    dat <- filtered_data()
    cancelled_dollars <- dat %>%
      filter(cancelled) %>%
      summarize(total = sum(abs(total_obligated_amount), na.rm = TRUE)) %>%
      pull(total)
    dollar(cancelled_dollars)
  })
  
  output$kpi_avg_amount <- renderText({
    dat <- filtered_data()
    avg <- dat %>%
      summarize(avg_amt = mean(total_obligated_amount, na.rm = TRUE)) %>%
      pull(avg_amt)
    dollar(round(avg, 0))
  })
  
  output$overview_sentence <- renderUI({
    # base slice: agency + year, ignore cancel_filter
    base <- grants %>%
      filter(
        fiscal_year >= input$year_range[1],
        fiscal_year <= input$year_range[2],
        awarding_agency_name %in% input$agency
      )
    
    if (nrow(base) == 0) {
      return(HTML("No grants match the current filters."))
    }
    
    # year range
    yr_min <- min(base$fiscal_year, na.rm = TRUE)
    yr_max <- max(base$fiscal_year, na.rm = TRUE)
    
    # counts
    total_grants_all   <- nrow(base)
    cancelled_idx      <- base$cancelled %in% TRUE
    cancelled_grants   <- sum(cancelled_idx, na.rm = TRUE)
    active_grants      <- total_grants_all - cancelled_grants
    
    # dollars
    total_oblig_all    <- sum(pmax(base$total_obligated_amount, 0), na.rm = TRUE)
    cancelled_dollars  <- sum(abs(base$total_obligated_amount[cancelled_idx]), na.rm = TRUE)
    active_dollars     <- total_oblig_all - cancelled_dollars
    
    # choose numbers based on cancel filter
    if (input$cancel_filter == "all") {
      total_grants <- total_grants_all
      total_dollars <- total_oblig_all
      cancel_rate <- if (total_grants_all > 0) cancelled_grants / total_grants_all else NA_real_
    } else if (input$cancel_filter == "cancelled") {
      total_grants <- cancelled_grants
      total_dollars <- cancelled_dollars      # <- use cancelled dollars
      cancel_rate <- if (cancelled_grants > 0) 1 else NA_real_
    } else {  # "active"
      total_grants <- active_grants
      total_dollars <- active_dollars         # <- total minus cancelled
      cancel_rate <- if (active_grants > 0) 0 else NA_real_
    }
    
    # pretty printing
    total_grants_txt <- comma(total_grants)
    total_dollars_txt <- dollar(total_dollars)
    cancel_rate_txt <- if (is.na(cancel_rate)) "NA" else paste0(round(100 * cancel_rate, 1), "%")
    
    HTML(paste0(
      "Under the current filters (agency/year/status), from ",
      "<b><span style='color:#b81f74;'>FY", yr_min, "</span></b> – ",
      "<b><span style='color:#b81f74;'>FY", yr_max, "</span></b>, ",
      
      "<b><span style='color:#b81f74;'>", total_grants_txt, "</span></b> federal science grants totaling ",
      "<b><span style='color:#b81f74;'>", total_dollars_txt, "</span></b> were awarded; ",
      
      "<b><span style='color:#b81f74;'>", cancel_rate_txt,
      "</span></b> were later classified as net de-obligations (\"cancelled\")."
    ))
  })
  
  # ---- Stats: chi-square, Cramer's V, trend, logistic ----
  # (full dataset, not filtered_data)
  
  output$chisq_output <- renderPrint({
    tab <- table(grants$fiscal_year, grants$cancelled)
    cat("Chi-square test of independence (cancellation vs. year):\n\n")
    print(chisq.test(tab))
    cat("\nCramer's V effect size:\n")
    print(DescTools::CramerV(tab))
  })
  
  output$trend_test_output <- renderPrint({
    tab <- table(grants$fiscal_year, grants$cancelled)
    CochranArmitageTest(tab)
  })
  
  # ---- H3 summary table (full dataset, not filtered) ----
  yearly_cancel_summary <- grants %>%
    filter(fiscal_year >= 2018, fiscal_year <= 2025) %>%
    group_by(fiscal_year) %>%
    summarise(
      total_grants      = n(),
      cancelled_grants  = sum(cancelled, na.rm = TRUE),
      cancellation_rate = round(mean(cancelled, na.rm = TRUE) * 100, 3),
      .groups = "drop"
    )
  
  output$cancel_summary_table <- DT::renderDT({
    datatable(
      yearly_cancel_summary,
      options = list(pageLength = 10,
                     searching  = FALSE,
                     dom        = "t"),
      rownames = FALSE
    )
  })
  
  # ---- Chi-square + Cramer's V ----
  output$chisq_output <- renderPrint({
    tab <- table(grants$fiscal_year, grants$cancelled)
    cat("Chi-square test of independence (cancellation vs. year):\n\n")
    print(chisq.test(tab))
    
    cat("\nCramer's V effect size:\n")
    print(DescTools::CramerV(tab))
  })
  
  # ---- Pairwise proportion tests ----
  output$pairwise_output <- renderPrint({
    tab <- table(grants$fiscal_year, grants$cancelled)
    
    pairwise.prop.test(
      x = tab[, "TRUE"],
      n = rowSums(tab),
      p.adjust.method = "holm"
    )
  })
  
  # ---- Trend test (unchanged) ----
  output$trend_test_output <- renderPrint({
    tab <- table(grants$fiscal_year, grants$cancelled)
    CochranArmitageTest(tab)
  })
  
  #output$logit_output <- renderPrint({
   # summary(logit_model)
  #})
  
  # ========= TIME TAB =========
  
  time_summary <- reactive({
    dat <- filtered_data()
    
    dat %>%
      group_by(fiscal_year) %>%
      summarize(
        grants_total      = n(),
        grants_cancelled  = sum(cancelled, na.rm = TRUE),
        dollars_total     = sum(pmax(total_obligated_amount, 0), na.rm = TRUE),
        # take abs value
        dollars_cancelled = sum(
          if_else(cancelled, abs(total_obligated_amount), 0),
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      arrange(fiscal_year)
  })
  
  output$time_plot <- renderPlotly({
    dat <- time_summary()
    if (nrow(dat) == 0) return(NULL)
    
    pink_line <- "#cd5c8f"   
    pink_point <- "#b44874"
    
    if (input$time_metric == "count") {
      p <- ggplot(dat, aes(x = fiscal_year, y = grants_cancelled)) +
        geom_line(color = pink_line, size = 1.3) +
        geom_point(color = pink_point, size = 3) +
        scale_x_continuous(breaks = dat$fiscal_year) +
        labs(
          title = "Cancelled Grants Over Fiscal Years 2018-2025",
          x = "Fiscal Year",
          y = "Number of Cancelled Grants"
        ) +
        theme_minimal(base_size = 13) + 
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              panel.grid.major = element_line(color = "gray85"),
              panel.grid.minor = element_blank(),
              plot.margin = margin(10, 20, 10, 10))
      
      return(ggplotly(p, tooltip = c("x", "y")) %>% 
               style(hovertemplate = paste(
                 "<b>Fiscal Year:</b> %{x}<br>",
                 "<b>Cancelled Grants:</b> %{y}<extra></extra>")) %>% 
               layout(hoverlabel = list(font = list(size = 13)),
                      margin = list(l = 60, r = 40, b = 60, t = 80)))
      
    } else {
      p <- ggplot(dat, aes(x = fiscal_year, y = dollars_cancelled)) +
        geom_line(color = pink_line, size = 1.3) +
        geom_point(color = pink_point, size = 3) +
        scale_x_continuous(breaks = dat$fiscal_year) +
        scale_y_continuous(labels = dollar) +
        labs(
          title = "Cancelled Dollars Over Fiscal Years 2018-2025",
          x = "Fiscal Year",
          y = "Total Cancelled Dollars (absolute value)"
        ) +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              panel.grid.major = element_line(color = "gray85"),
              panel.grid.minor = element_blank(),
              plot.margin = margin(10, 20, 10, 10))
      
      return(ggplotly(p, tooltip = c("x", "y")) %>% 
               style(hovertemplate = paste(
                 "<b>Fiscal Year:</b> %{x}<br>",
                 "<b>Cancelled Dollars:</b> %{y}<extra></extra>")) %>% 
               layout(hoverlabel = list(font = list(size = 13)),
                      margin = list(l = 60, r = 40, b = 60, t = 80)))
 }})
  
  # ========= AGENCY TAB =========
  
  agency_summary <- reactive({
    dat <- filtered_data()
    
    dat %>%
      mutate(
        agency_acronym = dplyr::recode(
          awarding_agency_name,
          "Department of Agriculture"                        = "USDA",
          "Department of Defense"                           = "DoD",
          "Department of Energy"                            = "DOE",
          "Department of Health and Human Services"         = "HHS",
          "Environmental Protection Agency"                 = "EPA",
          "National Aeronautics and Space Administration"   = "NASA",
          "National Science Foundation"                     = "NSF",
          .default = awarding_agency_name
        )
      ) %>%
      group_by(agency_acronym) %>%
      summarize(
        grants_total      = n(),
        grants_cancelled  = sum(cancelled, na.rm = TRUE),
        dollars_total     = sum(pmax(total_obligated_amount, 0), na.rm = TRUE),
        # again use absolute value for cancelled dollars
        dollars_cancelled = sum(
          if_else(cancelled, abs(total_obligated_amount), 0),
          na.rm = TRUE
        ),
        cancel_rate       = grants_cancelled / grants_total,
        .groups = "drop"
      ) %>%
      arrange(desc(dollars_total))
  })
  
  output$agency_plot <- renderPlotly({
    dat <- agency_summary()
    if (nrow(dat) == 0) return(NULL)
    
    pink_low  <- "#fde0ec"
    pink_high <- "#b81f74"
    
    max_rate <- max(dat$cancel_rate, na.rm = TRUE)
    
    p <- ggplot(
      dat,
      aes(
        x    = reorder(agency_acronym, dollars_total),
        y    = dollars_total / 1e9,
        fill = cancel_rate,
        text = paste0(
          "Total obligations: ", scales::dollar(dollars_total), "<br>",
          "Cancelled dollars: ", scales::dollar(dollars_cancelled), "<br>",
          "Cancellation rate: ",
          scales::percent(cancel_rate, accuracy = 0.1)
        )
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = function(x) paste0(x, "B")) +
      scale_fill_gradient(name   = "Cancellation rate",
                          low    = pink_low,
                          high   = pink_high,
                          limits = c(0, max_rate),
                          labels = scales::percent_format(accuracy = 0.1)) +
      labs(
        title = "Total Obligated Funding by Agency",
        x = "Agency",
        y = "Total Obligations (billions)",
        fill = "Cancellation rate"
      ) +
      theme_minimal(base_size = 13) + 
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text  = element_text(size = 12),
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_blank(),
            plot.margin = margin(10, 20, 10, 10))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(font = list(size = 13)),
        margin = list(l = 70, r = 60, b = 60, t = 80))
  })
  
  # ========= DETAILS TAB =========
  
  output$details_table <- renderDT({
    dat <- filtered_data()
    
    cols_to_show <- intersect(
      c(
        "assistance_award_unique_key",
        "award_id_fain",
        "awarding_agency_name",
        "fiscal_year",
        "total_obligated_amount",
        "cancelled_label",
        "recipient_state_code",
        "duration_days",
        "usaspending_permalink"
      ),
      names(dat)
    )
    
    datatable(
      dat[, cols_to_show, drop = FALSE],
      options = list(pageLength = 15),
      filter  = "top",
      escape  = FALSE
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("grants_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
}

shinyApp(ui, server)
