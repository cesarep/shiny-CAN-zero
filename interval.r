#falta fazer updateInput

intervalInput <- function(inputId, label, x1, x2, label1 = "De", label2 = "à", label3 = NULL, min=NULL, max=NULL, step=NULL){
	shiny::tagList(
		shiny::singleton(shiny::tags$head(
			shiny::tags$script(HTML(intervalJS))
		)),
		shiny::tags$div(class='shiny-input-container form-group intinput', id=inputId,
							 shiny::tags$label('for' = inputId, label),
							 shiny::tags$div(class='input-group',
							 					 if(!is.null(label1)) shiny::tags$span(class='input-group-addon', label1) else NULL,
							 					 shiny::tags$input(class="form-control", type='number', value=x1, min=min, max=max, step=step),
							 					 if(!is.null(label2)) shiny::tags$span(class='input-group-addon', label2) else NULL,
							 					 shiny::tags$input(class="form-control", type='number', value=x2, min=min, max=max, step=step),
							 			 		 if(!is.null(label3)) shiny::tags$span(class='input-group-addon', label3) else NULL
							 )
		)
	)
}

intervalJS <- "var intrvBinding = new Shiny.InputBinding();
$.extend(intrvBinding, {
  find: function(scope) {
    return $(scope).find('.intinput');
  },
  getValue: function(el) {
	  var output = []
	  output = $(el).find('input').map(function() {return $(this).val()}).get();
	  output = output.map((i)=>parseFloat(i))
  	  return(output)
	},
  subscribe: function(el, callback) {
  	 $(el).on('change.input', function(event) {
     callback(false);
   }); 
  },
  unsubscribe: function(el) {
    $(el).off('.intinput');
  }
});

Shiny.inputBindings.register(intrvBinding);"