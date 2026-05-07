# ==============================================================================
# 🎛️ 05_shiny_app.R — PRODUCTION READY
# Title: Educafin Policy Simulator | H1-H4 Framework + Fiscal ROI
# ==============================================================================

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(bslib) # For modern UI

# ==============================================================================
# UI - Using bslib for a professional "Academic" look
# ==============================================================================
ui <- page_sidebar(
  title = "🎯 Educafin Policy Simulator",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 350,
    title = "Model Parameters",
    # Model Inputs
    numericInput("beta", "Literacy Coeff (H1 β)", value = -0.0051, step = 0.001),
    numericInput("se_beta", "Std Error (β)", value = 0.002, step = 0.001),
    hr(),
    # Intervention Inputs
    sliderInput("pts", "Intervention Strength (+pts)", min = 1, max = 25, value = 10),
    numericInput("n_hh", "Target Population (M Households)", value = 1.5, min = 0.1),
    hr(),
    # ROI Inputs
    numericInput("cost_pt", "Cost per Point per HH ($)", value = 15, min = 1),
    actionButton("run", "🚀 Run Policy Simulation", class = "btn-primary w-100")
  ),
  
  layout_column_wrap(
    width = 1/2,
    # Tab 1: Hypothesis & Probability
    card(
      card_header("📊 H1-H2: Debt Risk Projection"),
      plotlyOutput("projPlot"),
      card_footer("Monte Carlo Simulation (R=1,000) showing 95% Confidence Intervals.")
    ),
    # Tab 2: Fiscal ROI
    card(
      card_header("💰 H4: Fiscal ROI Analysis"),
      plotOutput("roi_plot"),
      verbatimTextOutput("roi_summary")
    )
  ),
  
  layout_column_wrap(
    width = 1/2,
    # Tab 3: Hypothesis Checklist
    card(
      card_header("✅ Hypothesis Validation Checklist"),
      uiOutput("hypothesis_results")
    ),
    # Tab 4: Policy Brief
    card(
      card_header("📋 Executive Policy Brief"),
      verbatimTextOutput("policy_brief")
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  # 🔧 HELPER: Safe Fiscal Formatting
  fmt_cad <- function(x) {
    if (is.na(x) || !is.finite(x)) return("N/A")
    paste0("$", format(round(x), big.mark = ",", scientific = FALSE), " CAD")
  }
  
  # 1. SIMULATION ENGINE
  sim_results <- eventReactive(input$run, {
    set.seed(42)
    # Simulate Beta distribution for uncertainty
    beta_sim <- rnorm(1000, mean = input$beta, sd = input$se_beta)
    
    # Calculate impacts
    impact_sim <- beta_sim * input$pts
    
    # Fiscal Math
    # Cost = Points * Cost_per_Point * Population
    total_cost <- input$pts * input$cost_pt * (input$n_hh * 1e6)
    # Savings = abs(Impact) * Avg_Debt * Population * Social_Cost_Friction (assume 8%)
    avg_hh_debt <- 22000 
    total_savings <- abs(mean(impact_sim)) * avg_hh_debt * (input$n_hh * 1e6) * 0.08
    
    list(
      mean_impact = mean(impact_sim),
      ci = quantile(impact_sim, c(0.025, 0.975)),
      is_sig = abs(input$beta / input$se_beta) >= 1.96,
      cost = total_cost,
      savings = total_savings,
      net = total_savings - total_cost
    )
  })
  
  # 2. OUTPUT: PROJECTION PLOT
  output$projPlot <- renderPlotly({
    res <- sim_results()
    
    df_plot <- tibble(
      Scenario = c("Baseline", "Post-Intervention"),
      Value = c(0.20, 0.20 + res$mean_impact), # Assuming 20% baseline risk
      Lower = c(0.20, 0.20 + res$ci[1]),
      Upper = c(0.20, 0.20 + res$ci[2])
    )
    
    plot_ly(df_plot, x = ~Scenario, y = ~Value, type = "bar", 
            error_y = ~list(array = Upper - Value, arrayminus = Value - Lower, color = '#000000'),
            marker = list(color = c("#bdc3c7", "#2ecc71"))) %>%
      layout(yaxis = list(title = "Probability of Over-Indebtedness", tickformat = ".1%"))
  })
  
  # 3. OUTPUT: ROI PLOT
  output$roi_plot <- renderPlot({
    res <- sim_results()
    df_roi <- tibble(
      Type = c("Program Cost", "Fiscal Savings"),
      Amount = c(res$cost, res$savings)
    )
    
    ggplot(df_roi, aes(x = Type, y = Amount, fill = Type)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = c("#e74c3c", "#27ae60")) +
      theme_minimal() +
      scale_y_continuous(labels = scales::label_dollar()) +
      labs(title = "H4: Cost-Benefit Comparison", x = "", y = "")
  })
  
  # 4. OUTPUT: ROI SUMMARY
  output$roi_summary <- renderText({
    res <- sim_results()
    paste0(
      "NET IMPACT ANALYSIS\n",
      "----------------------------\n",
      "Total Cost:    ", fmt_cad(res$cost), "\n",
      "Total Savings: ", fmt_cad(res$savings), "\n",
      "Net ROI:       ", fmt_cad(res$net), "\n",
      "Status:        ", ifelse(res$net > 0, "✅ PROFITABLE", "⚠️ FISCAL DEFICIT")
    )
  })
  
  # 5. OUTPUT: HYPOTHESIS CHECKLIST
  output$hypothesis_results <- renderUI({
    res <- sim_results()
    
    tagList(
      div(style = "margin-bottom: 15px;",
          h6("H1: Literacy reduces Debt Ratio"),
          span(class = "badge", 
               style = paste0("background-color:", ifelse(input$beta < 0 && res$is_sig, "#27ae60", "#e74c3c")),
               ifelse(input$beta < 0 && res$is_sig, "Supported", "Not Supported"))),
      div(style = "margin-bottom: 15px;",
          h6("H4: Positive Fiscal Return"),
          span(class = "badge", 
               style = paste0("background-color:", ifelse(res$net > 0, "#27ae60", "#e74c3c")),
               ifelse(res$net > 0, "Supported", "Not Supported")))
    )
  })
  
  # 6. OUTPUT: POLICY BRIEF
  output$policy_brief <- renderText({
    res <- sim_results()
    paste0(
      "EXECUTIVE SUMMARY\n",
      "=================\n",
      "The simulation suggests that a ", input$pts, "-point increase in literacy scores\n",
      "results in a ", round(abs(res$mean_impact)*100, 2), "% reduction in over-indebtedness risk.\n\n",
      "With an estimated net benefit of ", fmt_cad(res$net), ",\n",
      "this intervention is ", ifelse(res$net > 0, "economically viable.", "not recommended in its current form."),
      "\n\nCaveat: Based on provincial-aggregate data (N=49)."
    )
  })
}

shinyApp(ui, server)