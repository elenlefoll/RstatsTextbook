```{r}
#| include: false
# https://github.com/quarto-dev/quarto-cli/issues/4492#issuecomment-1548655951
# https://ctan.math.washington.edu/tex-archive/macros/luatex/latex/emoji/emoji-doc.pdf (to find aliases)

emoji <- function(x) {
  if (knitr::is_latex_output()) {
 stringr::str_c("\\emoji{", stringr::str_replace_all(x, "_", "-"), "}")
  } else if (knitr::is_html_output()) {
 stringr::str_c(":", x, ":")
  } else x
}
```

