
ui <- page_navbar(
  title = "Clone-Censor-Weight",
  theme = bs_theme(bootswatch = "flatly"),
  nav_panel(
    title = "Study",
    card()
  ),
  nav_panel(
    title = "Codelists"
  ),
  nav_panel(
    title = "Cohorts",
    navset_bar(
      nav_panel(
        title = "Cohort definition"
      ),
      nav_panel(
        title = "Cohort count"
      ),
      nav_panel(
        title = "Cohort attrition"
      )
    )
  ),
  nav_panel(
    title = "Characterisation",
    navset_bar(
      nav_panel(
        title = "Cohort Characteristics"
      ),
      nav_panel(
        title = "Large Scale Characteristics"
      )
    )
  ),
  nav_panel(
    title = "Diagnostics",
    navset_bar(
      nav_panel(
        title = "Number subjects"
      ),
      nav_panel(
        title = "Events"
      ),
      nav_panel(
        title = "Follow-up"
      ),
      nav_panel(
        title = "PS model coefficients"
      ),
      nav_panel(
        title = "PS model result"
      ),
      nav_panel(
        title = "Weights"
      )
    )
  ),
  nav_panel(
    title = "Results",
    navset_bar(
      nav_panel(
        title = "Kaplan-Meier"
      ),
      nav_panel(
        title = "Hazard Ratio"
      )
    )
  ),
  nav_spacer(),
  nav_item(a(
    icon("github"),
    href = "https://github.com/oxford-pharmacoepi/ProstateCancerTTE",
    target = "_blank"
  ))
)
