KaTeX <- function(){
	css = list(
		url="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.css",
		hash="sha384-zB1R0rpPzHqg7Kpt0Aljp8JPLqbXI3bhnPWROx27a9N0Ll6ZP/+DiW/UqRcLbRjq"
	)
	js1 = list(
		url="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.js",
		hash="sha384-y23I5Q6l+B6vatafAwxRu/0oK/79VlbSz7Q9aiSZUvyWYIYsd+qj+o24G5ZU2zJz"
	)
	js2 = list(
		url="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/contrib/auto-render.min.js",
		hash="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI"
	)
	return(
		tags$head(
		singleton(tagList(
			tags$link(rel='stylesheet', href=css$url, 'integrity'=css$hash, 'crossorigin'="anonymous"),
			tags$script('defer'=NA, src=js1$url, 'integrity'=js1$hash, 'crossorigin'="anonymous"),
			tags$script('defer'=NA, src=js2$url, 'integrity'=js2$hash, 'crossorigin'="anonymous", 'onload'=HTML('"renderMathInElement(document.body);"')),
			tags$script(HTML(
			'$(document).on("DOMContentLoaded shiny:value shiny:visualchange", function(){
		        renderMathInElement(document.body);
		      })'
			))
		))
	)
	)
}