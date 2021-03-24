
### Draw diagrams using semTool

nine-multicorrelated model with some removed items

{r, fig.height=12, fig.width=12}

```
semPlot::semPaths(fit1b)
for (mdl in mdls) {
  cat(mdl,"\n"); summary(cfa_results[[mdl]]$cfa, standardized = T)
  semPaths(cfa_results[[mdl]]$cfa,  "std"
           , curvePivot = T, layout = "circle", rotation = 3, fade = F, intercepts = F, residuals = F
           , sizeLat = 4, sizeLat2 = 3, sizeMan = 4, sizeMan2 = 2, curvature = 2.5, esize = 1.5, asize = 1.5
           , edge.label.cex = 0.35, edge.label.position = 0.5, levels = c(9.5,10,10,10))
}
```


Eight-multicorrelated model with some removed items

```{r}

```

Alternative short version of FSS-BR

```{r}

```
