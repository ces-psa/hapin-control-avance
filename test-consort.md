---
title: "Reporte CONSORT"
output: 
  html_document:
    keep_md: true
---


```r
library(DiagrammeR)

library(DiagrammeR)

# Crea el gráfico con DiagrammeR
graph <- create_graph() %>%
  add_node("Assessed", label = "1. Assessed for eligibility (S4): 750") %>%
  add_node("Consent", label = "2. Consent (S4) : 31") %>%
  add_node("Exit", label = "3. Exit after enrollment (E3)") %>%
  add_node("Death", label = "a. Death (E2, E3 - during versus not during study [only E3]) : 2") 
# Renderiza el gráfico
render_graph(graph)
```

```{=html}
<div class="grViz html-widget html-fill-item" id="htmlwidget-ee5a06f34c70fc17d4e0" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-ee5a06f34c70fc17d4e0">{"x":{"diagram":"digraph {\n\ngraph [layout = \"neato\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\"]\n\n  \"1\" [label = \"1. Assessed for eligibility (S4): 750\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"2\" [label = \"2. Consent (S4) : 31\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"3\" [label = \"3. Exit after enrollment (E3)\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n  \"4\" [label = \"a. Death (E2, E3 - during versus not during study [only E3]) : 2\", fillcolor = \"#F0F8FF\", fontcolor = \"#000000\"] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
```
